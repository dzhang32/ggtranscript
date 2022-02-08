
#' @keywords internal
#' @noRd
to_reduced_gap <- function(x, x_intron, group_var = NULL, target_gap_width = 100L) {
    .check_coord_object(x)
    .check_coord_object(x_intron)
    .check_group_var(x, group_var)
    .check_group_var(x_intron, group_var)
    target_gap_width <- .check_target_gap_width(target_gap_width)

    # to_intron obtains exon boundaries as intron definition
    # we need to convert this to the gap definition to match
    # downstream GenomicRanges::gaps
    x_intron <- x_intron %>%
        dplyr::mutate(
            start = start + 1,
            end = end - 1
        )

    # needs to be a genomic range for downstream processing
    x_gr <- GenomicRanges::GRanges(x)
    x_intron_gr <- GenomicRanges::GRanges(x_intron)

    intron_gaps <- .get_gaps(x_gr)

    gap_map <- .get_gap_map(x_intron_gr, intron_gaps)

    x_intron_reduced <- .get_reduced_introns(x_intron, intron_gaps, target_gap_width)

    x_rescaled <- .get_rescaled_transcripts(x, x_intron_reduced)

    return(x_rescaled)
}

#' @keywords internal
#' @noRd
.get_gaps <- function(x_gr) {
    orig_seqnames <- x_gr %>%
        GenomicRanges::seqnames() %>%
        as.character() %>%
        unique()

    orig_strand <- x_gr %>%
        GenomicRanges::strand() %>%
        as.character() %>%
        unique()

    # make sure we only have exons from a single transcript
    .check_len_1_strand_seqnames(orig_seqnames, orig_strand)

    # "reduce" exons - here meaning to collapse into single meta transcript
    x_gr_reduced <- x_gr %>% GenomicRanges::reduce()

    # keep only the relevant seqnames, otherwise gaps includes all seqlevels
    GenomeInfoDb::seqlevels(x_gr_reduced, pruning.mode = "coarse") <-
        orig_seqnames

    # obtain intronic gaps of the meta transcript
    intron_gaps <- x_gr_reduced %>%
        GenomicRanges::gaps(
            start = min(GenomicRanges::start(x_gr_reduced)),
            end = max(GenomicRanges::end(x_gr_reduced))
        )

    # creates a gap per strand too, let's keep only those for the
    intron_gaps <- intron_gaps %>%
        .[GenomicRanges::strand(intron_gaps) == orig_strand]

    return(intron_gaps)
}

#' map the gaps back to introns
#' @keywords internal
#' @noRd
.get_gap_map <- function(x_intron_gr, intron_gaps) {

    # when we reduce the length of the intronic gaps,
    # we need to make sure the exons/introns remain aligned
    # to do this, we need to map the intronic gaps back onto the introns

    # the simplest case is when gaps are identical to original introns
    equal_hits <- GenomicRanges::findOverlaps(
        intron_gaps,
        x_intron_gr,
        type = "equal"
    )

    # often, the gaps don't map identically
    # this occurs due to the exons of one tx overlapping the intron of another tx
    # need to find cases when the gaps are completely contained an original intron
    # type = "within" does the job, but also catches the "equal"
    within_hits <- GenomicRanges::findOverlaps(
        intron_gaps,
        x_intron_gr,
        type = "within"
    )

    # convert to data.frame() to use dplyr::anti_join()
    equal_hits <- equal_hits %>% as.data.frame()
    within_hits <- within_hits %>% as.data.frame()

    # remove the "equal" hits from the "within"
    pure_within_hits <- within_hits %>%
        dplyr::anti_join(equal_hits)

    # need both equal and pure within hits
    gap_map <- list(
        equal = equal_hits,
        pure_within = pure_within_hits
    )

    return(gap_map)
}

#' @keywords internal
#' @noRd
.get_reduced_introns <- function(x_intron, intron_gaps, target_gap_width) {

    # we need the intron widths (to reduce them)
    x_intron <- x_intron %>%
        dplyr::mutate(width = end - start)

    # characterise introns by scaling type
    x_intron_reduced <- x_intron %>%
        dplyr::mutate(
            reduce_type = dplyr::case_when(
                dplyr::row_number() %in% gap_map[["equal"]][["subjectHits"]] ~ "equal",
                dplyr::row_number() %in% gap_map[["pure_within"]][["subjectHits"]] ~ "pure_within",
                TRUE ~ "none"
            )
        )

    # for the "equal" cases we can simply shorten the widths
    x_intron_reduced <- x_intron_reduced %>%
        dplyr::mutate(
            reduced_width = ifelse(
                (reduce_type == "equal") & (width > target_gap_width),
                target_gap_width,
                width
            )
        )

    # for the "within" cases we need to shorten the widths by the amount the
    # overlapping gaps are shortened
    # first, add the width of the overlapping gaps
    intron_indexes <- gap_map[["pure_within"]][["subjectHits"]]
    overlapping_gap_indexes <- gap_map[["pure_within"]][["queryHits"]]
    x_intron_reduced[["gap_width"]][intron_indexes] <-
        GenomicRanges::width(intron_gaps)[overlapping_gap_indexes]

    # then, calculate the reduction in gap
    # and minus this from the overlapping intron
    x_intron_reduced <- x_intron_reduced %>%
        dplyr::mutate(
            reduced_gap_width = dplyr::case_when(
                is.na(gap_width) ~ NA_integer_,
                gap_width > target_gap_width ~ target_gap_width,
                TRUE ~ gap_width
            ),
            reduced_gap_diff = gap_width - reduced_gap_width,
        )

    # one intron may overlap multiple gaps
    # in which case we need to sum the reduction in overlapping gaps
    x_intron_reduced <- x_intron_reduced %>%
        dplyr::group_by_at(.vars = c(
            "start",
            "end",
            group_var
        )) %>%
        dplyr::summarise(
            width = width,
            reduced_width = reduced_width,
            sum_reduced_gap_diff = sum(reduced_gap_diff)
        ) %>%
        dplyr::ungroup()

    stopifnot(nrow(x_intron_reduced) == nrow(x_intron))

    # now actually do reduction for introns with "pure_within" gaps
    x_intron_reduced <- x_intron_reduced %>%
        dplyr::mutate(
            reduced_width = ifelse(
                is.na(sum_reduced_gap_diff),
                reduced_width,
                width - sum_reduced_gap_diff
            )
        ) %>%
        dplyr::select(-sum_reduced_gap_diff, -width, width = reduced_width)

    return(x_intron_reduced)
}

#' @keywords internal
#' @noRd
.get_rescaled_transcripts <- function(x, x_intron_reduced) {

    # calculate the rescaled exon/intron start/ends using
    # the widths of the exons and reduced introns
    x_rescaled <- x %>% dplyr::mutate(width = end - start)

    x_rescaled <- x_rescaled %>%
        dplyr::bind_rows(x_intron_reduced) %>%
        dplyr::arrange_at(.vars = c(group_var, "start", "end")) %>%
        dplyr::group_by_at(.vars = c(
            group_var
        )) %>%
        dplyr::mutate(
            rescaled_end = cumsum(width),
            rescaled_start = rescaled_end - (width - 1)
        ) %>%
        dplyr::ungroup()

    # need to scale the transcript starts so transcripts align correctly
    tx_start_rescaled <-
        x %>%
        dplyr::group_by_at(.vars = c(
            group_var
        )) %>%
        dplyr::summarise(
            rescaled_tx_start = min(start) - min(x[["start"]])
        ) %>%
        dplyr::ungroup()

    x_rescaled <- x_rescaled %>%
        dplyr::left_join(
            tx_start_rescaled,
            by = c(group_var)
        ) %>%
        dplyr::mutate(
            rescaled_end = rescaled_end + rescaled_tx_start,
            rescaled_start = rescaled_start + rescaled_tx_start
        )

    return(x_rescaled)
}

#' we expect the exons to originate from a single gene Hence, unique strand and
#' seqnames should be of length 1
#' @keywords internal
#' @noRd
.check_len_1_strand_seqnames <- function(orig_seqnames, orig_strand) {
    ab_1_uniq <- "of x contains more than 1 unique value. "
    reason <- "x is expected to contain exons from a single gene."

    if (length(orig_seqnames) != 1) {
        stop("seqnames ", ab_1_uniq, reason)
    }

    if (length(orig_strand) != 1) {
        stop("seqnames ", ab_1_uniq, reason)
    }
}

.check_target_gap_width <- function(target_gap_width) {
    if (!is.integer(target_gap_width)) {
        warning("target_gap_width must be an integer, coercing...")
        target_gap_width <- target_gap_width %>%
            as.integer()
    }

    return(target_gap_width)
}


#' @keywords internal
#' @noRd
shorten_gaps <- function(x,
                         x_intron,
                         group_var = NULL,
                         target_gap_width = 100L) {

    # input checks
    .check_coord_object(x, check_seqnames = TRUE, check_strand = TRUE)
    .check_coord_object(x_intron, check_seqnames = TRUE, check_strand = TRUE)
    .check_group_var(x, group_var)
    .check_group_var(x_intron, group_var)
    target_gap_width <- .check_target_gap_width(target_gap_width)

    # to_intron() obtains exon boundaries as intron definition
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
    gap_map_intron <- .get_gap_map(x_intron_gr, intron_gaps)
    x_intron_reduced <- .get_reduced_gaps(
        x_intron,
        intron_gaps,
        gap_map_intron,
        group_var,
        target_gap_width
    )

    if (!is.null(group_var)) {
        tx_start_gaps <- .get_tx_start_gaps(x, group_var)
        gap_map_tx_start_gaps <- .get_gap_map(
            tx_start_gaps %>% GenomicRanges::GRanges(),
            intron_gaps
        )
        tx_start_gaps_reduced <- .get_reduced_gaps(
            tx_start_gaps,
            intron_gaps,
            gap_map_tx_start_gaps,
            group_var,
            target_gap_width
        ) %>%
            dplyr::select(-start, -end, -strand, -seqnames, -strand)
    }

    x_rescaled <- .get_rescaled_transcripts(
        x,
        x_intron_reduced,
        tx_start_gaps_reduced,
        group_var
    )

    # convert intron back to be defined by exon boundaries
    x_rescaled <- x_rescaled %>%
        dplyr::mutate(
            start = ifelse(type == "intron", start - 1, start),
            end = ifelse(type == "intron", end + 1, end)
        )

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

#' @keywords internal
#' @noRd
.get_tx_start_gaps <- function(x, group_var) {

    # need to scale the transcript starts so transcripts align correctly
    # importantly, this tx start also has to take into account
    # whether gaps that overlap it's start have been reduced
    # get the start gap, i.e. from 1 to the transcript tx as GRanges
    tx_start_gaps <-
        x %>%
        dplyr::group_by_at(.vars = c(
            group_var
        )) %>%
        dplyr::summarise(
            seqnames = unique(seqnames),
            strand = unique(strand),
            end = min(start), # min start of this transcript
            start = min(x[["start"]]) # min start of all transcripts
        )

    return(tx_start_gaps)
}

#' map the gaps back to introns/transcript start gaps
#' @keywords internal
#' @noRd
.get_gap_map <- function(y, intron_gaps) {

    # when we reduce the length of the intronic gaps,
    # we need to make sure the exons/introns remain aligned
    # to do this, we need to map the intronic gaps back onto the introns

    # the simplest case is when gaps are identical to original introns
    equal_hits <- GenomicRanges::findOverlaps(
        intron_gaps,
        y,
        type = "equal"
    )

    # often, the gaps don't map identically
    # this occurs due to the exons of one tx overlapping the intron of another tx
    # need to find cases when the gaps are completely contained an original intron
    # type = "within" does the job, but also catches the "equal"
    within_hits <- GenomicRanges::findOverlaps(
        intron_gaps,
        y,
        type = "within"
    )

    # convert to data.frame() to use dplyr::anti_join()
    equal_hits <- equal_hits %>% as.data.frame()
    within_hits <- within_hits %>% as.data.frame()

    # remove the "equal" hits from the "within"
    pure_within_hits <- within_hits %>%
        dplyr::anti_join(equal_hits, by = c("queryHits", "subjectHits"))

    # need both equal and pure within hits
    gap_map <- list(
        equal = equal_hits,
        pure_within = pure_within_hits
    )

    return(gap_map)
}

#' @keywords internal
#' @noRd
.get_reduced_gaps <- function(y, intron_gaps, gap_map, group_var, target_gap_width) {

    # we need the intron/tx start gap widths (to reduce them)
    y <- y %>% dplyr::mutate(width = (end - start) + 1)

    # characterise introns by scaling type
    y_reduced <- y %>%
        dplyr::mutate(
            reduce_type = dplyr::case_when(
                dplyr::row_number() %in% gap_map[["equal"]][["subjectHits"]] ~ "equal",
                dplyr::row_number() %in% gap_map[["pure_within"]][["subjectHits"]] ~ "pure_within",
                TRUE ~ "none"
            )
        )

    # for the "equal" cases we can simply shorten the widths
    y_reduced <- y_reduced %>%
        dplyr::mutate(
            reduced_width = ifelse(
                (reduce_type == "equal") & (width > target_gap_width),
                target_gap_width,
                width
            )
        )

    # for the "within" cases we need to shorten the intron widths
    # by the total amount the overlapping gaps are shortened

    overlapping_gap_indexes <- gap_map[["pure_within"]][["queryHits"]]

    # only have to this if there are gaps that are "pure_within"
    if (length(overlapping_gap_indexes) > 0) {

        # one intron may overlap multiple gaps
        # first, calculate the sum of the reduction in gap
        sum_gap_diff <- dplyr::tibble(
            intron_indexes = gap_map[["pure_within"]][["subjectHits"]],
            gap_width = GenomicRanges::width(intron_gaps)[overlapping_gap_indexes]
        ) %>%
            dplyr::mutate(
                reduced_gap_width = ifelse(
                    gap_width > target_gap_width,
                    target_gap_width,
                    gap_width
                ),
                reduced_gap_diff = gap_width - reduced_gap_width,
            ) %>%
            dplyr::group_by(intron_indexes) %>%
            dplyr::summarise(
                sum_reduced_gap_diff = sum(reduced_gap_diff)
            )

        # now actually do reduction for introns with "pure_within" gaps
        y_reduced[["sum_reduced_gap_diff"]][sum_gap_diff[["intron_indexes"]]] <-
            sum_gap_diff[["sum_reduced_gap_diff"]]

        # now actually do reduction for introns with "pure_within" gaps
        y_reduced <- y_reduced %>%
            dplyr::mutate(
                reduced_width = ifelse(
                    is.na(sum_reduced_gap_diff),
                    reduced_width,
                    width - sum_reduced_gap_diff
                )
            ) %>%
            dplyr::select(-sum_reduced_gap_diff)
    }

    y_reduced <- y_reduced %>%
        dplyr::select(
            -reduce_type,
            -width,
            width = reduced_width
        )

    return(y_reduced)
}

#' @keywords internal
#' @noRd
.get_rescaled_transcripts <- function(x,
                                      x_intron_reduced,
                                      tx_start_gaps_reduced,
                                      group_var) {

    # calculate the rescaled exon/intron start/ends using
    # the widths of the exons and reduced introns
    x_rescaled <- x %>% dplyr::mutate(
        type = "exon",
        width = (end - start) + 1
    )

    # bind together exons and introns and arrange into genomic order
    x_rescaled <- x_rescaled %>%
        dplyr::bind_rows(x_intron_reduced %>% dplyr::mutate(type = "intron")) %>%
        dplyr::arrange_at(.vars = c(group_var, "start", "end"))

    # calculate the rescaled coords using cum of the widths of introns/exons
    x_rescaled <- x_rescaled %>%
        dplyr::group_by_at(.vars = c(
            group_var
        )) %>%
        dplyr::mutate(
            rescaled_end = cumsum(width),
            rescaled_start = rescaled_end - (width - 1)
        ) %>%
        dplyr::ungroup()

    # account for the tx starts being in different places, to keep the
    if (is.null(group_var)) {
        x_rescaled <- x_rescaled %>%
            dplyr::mutate(width_tx_start = 1)
    } else {
        x_rescaled <- x_rescaled %>%
            dplyr::left_join(
                tx_start_gaps_reduced,
                by = c(group_var),
                suffix = c("", "_tx_start")
            )
    }

    x_rescaled <- x_rescaled %>%
        dplyr::mutate(
            rescaled_end = rescaled_end + width_tx_start,
            rescaled_start = rescaled_start + width_tx_start
        ) %>%
        dplyr::select(-dplyr::contains("width"), -start, -end) %>%
        dplyr::select(
            seqnames,
            start = rescaled_start,
            end = rescaled_end,
            strand,
            dplyr::everything()
        )

    return(x_rescaled)
}

#' we expect the exons to originate from a single gene.
#' therefore, unique strand and seqnames should be of length 1
#' @keywords internal
#' @noRd
.check_len_1_strand_seqnames <- function(orig_seqnames, orig_strand) {
    ab_1_uniq <- "of object contains more than 1 unique value. "
    reason <- "object is expected to contain exons from a single gene."

    if (length(orig_seqnames) != 1) {
        stop("seqnames ", ab_1_uniq, reason)
    }

    if (length(orig_strand) != 1) {
        stop("strand ", ab_1_uniq, reason)
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

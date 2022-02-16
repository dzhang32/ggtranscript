#' Improve transcript structure visualization by shortening gaps
#'
#' For a given set of exons and introns, `shorten_gaps()` reduces the width of
#' gaps (regions that do not overlap any `exons`) to a user-inputted
#' `target_gap_width`. This can be useful when visualizing transcripts that have
#' long introns, to hone in on the regions of interest (i.e. exons) and better
#' compare between transcript structures.
#'
#' After `shorten_gaps()` reduces the size of gaps, it will re-scale `exons` and
#' `introns` to preserve exon alignment. This process will only reduce the width
#' of input `introns`, never `exons`. Importantly, the outputted re-scaled
#' co-ordinates should only be used for visualization as they will not match the
#' original genomic coordinates.
#'
#' @inheritParams to_diff
#' @param introns `data.frame()` the intron co-ordinates corresponding to the
#'   input `exons`. This can be created by applying `to_intron()` to the
#'   `exons`. If a user is not using `to_intron()`, they must make sure intron
#'   start/ends are defined precisely as the adjacent exon boundaries (rather
#'   than exon end + 1 and exon start - 1).
#' @param target_gap_width `integer()` the width in base pairs to shorten the
#'   gaps to.
#'
#' @return `data.frame()` contains the re-scaled co-ordinates of `introns` and
#'   `exons` of each input transcript with shortened gaps.
#'
#' @export
#' @examples
#'
#' library(magrittr)
#'
#' gba_ens_105_exons <- gba_ens_105 %>%
#'     dplyr::filter(type == "exon")
#'
#' # first, let's use an example of a single transcript
#' single_tx <- gba_ens_105_exons %>%
#'     dplyr::filter(transcript_name %in% c("GBA-203"))
#'
#' single_tx
#'
#' single_tx_rescaled <- shorten_gaps(
#'     single_tx,
#'     to_intron(single_tx),
#'     group_var = NULL,
#'     target_gap_width = 100L
#' )
#'
#' # rescaled output contains both introns and exons
#' single_tx_rescaled
#'
#' # create function to compare transcript visualisation
#' # before and after shortening gaps
#' plot_before_after <- function(before_shorten_gaps,
#'                               after_shorten_gaps,
#'                               group_var) {
#'     before_plot <- before_shorten_gaps %>%
#'         ggplot2::ggplot(ggplot2::aes_string(
#'             xstart = "start",
#'             xend = "end",
#'             y = group_var
#'         )) +
#'         geom_range() +
#'         geom_intron(
#'             data = to_intron(before_shorten_gaps, group_var),
#'             strand = "-",
#'             arrow.min.intron.length = 500
#'         )
#'
#'     after_plot <- after_shorten_gaps %>%
#'         dplyr::filter(type == "exon") %>%
#'         ggplot2::ggplot(ggplot2::aes_string(
#'             xstart = "start",
#'             xend = "end",
#'             y = group_var
#'         )) +
#'         geom_range() +
#'         geom_intron(
#'             data = after_shorten_gaps %>%
#'                 dplyr::filter(type == "intron"),
#'             strand = "-",
#'             arrow.min.intron.length = 200
#'         )
#'
#'     before_after_plotlist <- list(before_plot, after_plot)
#'
#'     before_after_plot <- ggpubr::ggarrange(
#'         plotlist = before_after_plotlist,
#'         nrow = 2,
#'         labels = c("Before shorten_gaps()", "After shorten_gaps()")
#'     )
#'
#'     return(before_after_plot)
#' }
#'
#' plot_before_after(
#'     before_shorten_gaps = single_tx,
#'     after_shorten_gaps = single_tx_rescaled,
#'     group_var = "transcript_name"
#' )
#'
#' # exons can also contain multiple transcripts
#' multi_tx <- gba_ens_105_exons %>%
#'     dplyr::filter(
#'         transcript_name %in% paste0("GBA-20", 2:8)
#'     )
#'
#' multi_tx_rescaled <- shorten_gaps(
#'     multi_tx,
#'     to_intron(multi_tx, "transcript_name"),
#'     group_var = "transcript_name",
#'     target_gap_width = 100L
#' )
#'
#' plot_before_after(
#'     before_shorten_gaps = multi_tx,
#'     after_shorten_gaps = multi_tx_rescaled,
#'     group_var = "transcript_name"
#' )
shorten_gaps <- function(exons,
                         introns,
                         group_var = NULL,
                         target_gap_width = 100L) {

    # input checks
    .check_coord_object(exons, check_seqnames = TRUE, check_strand = TRUE)
    .check_coord_object(introns, check_seqnames = TRUE, check_strand = TRUE)
    .check_group_var(exons, group_var)
    .check_group_var(introns, group_var)
    target_gap_width <- .check_target_gap_width(target_gap_width)

    # to_intron() defines introns using the exon boundaries
    # we need to convert this to the actual gap definition to make sure
    # comparison to GenomicRanges::gaps() when using "equal" works correctly
    introns <- introns %>%
        dplyr::mutate(
            start = start + 1,
            end = end - 1
        )

    # we use GenomicRanges methods for downstream processing
    exons_gr <- GenomicRanges::GRanges(exons)
    introns_gr <- GenomicRanges::GRanges(introns)

    # obtain actual gaps, i.e. regions that overlap no exons
    intron_gaps <- .get_gaps(exons_gr)

    # by mapping gaps back to introns, we can then shorten overlapping gaps
    gap_map_intron <- .get_gap_map(introns_gr, intron_gaps)
    introns_shortened <- .get_shortened_gaps(
        introns,
        intron_gaps,
        gap_map_intron,
        group_var,
        target_gap_width
    )

    # don't have to take tx_start_gaps into account if only 1 tx
    if (!is.null(group_var)) {
        # because we're shortening intron_gaps, we also need to shorten the
        # region from start of the plot and start of each tx (tx_start_gaps)
        tx_start_gaps <- .get_tx_start_gaps(exons, group_var)
        gap_map_tx_start_gaps <- .get_gap_map(
            tx_start_gaps %>% GenomicRanges::GRanges(),
            intron_gaps
        )
        tx_start_gaps_shortened <- .get_shortened_gaps(
            tx_start_gaps,
            intron_gaps,
            gap_map_tx_start_gaps,
            group_var,
            target_gap_width
        ) %>%
            dplyr::select(-start, -end, -strand, -seqnames, -strand)
    }

    rescaled_tx <- .get_rescaled_txs(
        exons,
        introns_shortened,
        tx_start_gaps_shortened,
        group_var
    )

    # convert introns back to be defined by exon boundaries
    rescaled_tx <- rescaled_tx %>%
        dplyr::mutate(
            start = ifelse(type == "intron", start - 1, start),
            end = ifelse(type == "intron", end + 1, end)
        )

    return(rescaled_tx)
}

#' @keywords internal
#' @noRd
.get_gaps <- function(exons_gr) {
    orig_seqnames <- exons_gr %>%
        GenomicRanges::seqnames() %>%
        as.character() %>%
        unique()

    orig_strand <- exons_gr %>%
        GenomicRanges::strand() %>%
        as.character() %>%
        unique()

    # make sure we only have exons from a single transcript
    .check_len_1_strand_seqnames(orig_seqnames, orig_strand)

    # "reduce" exons - here meaning to collapse into single meta transcript
    exons_gr_reduced <- exons_gr %>% GenomicRanges::reduce()

    # keep only the relevant seqnames, otherwise gaps includes all seqlevels
    GenomeInfoDb::seqlevels(exons_gr_reduced, pruning.mode = "coarse") <-
        orig_seqnames

    # obtain intronic gaps of the meta transcript
    intron_gaps <- exons_gr_reduced %>%
        GenomicRanges::gaps(
            start = min(GenomicRanges::start(exons_gr_reduced)),
            end = max(GenomicRanges::end(exons_gr_reduced))
        )

    # gaps creates a gap per strand too, keep only those from the original strand
    intron_gaps <- intron_gaps %>%
        .[GenomicRanges::strand(intron_gaps) == orig_strand]

    return(intron_gaps)
}

#' @keywords internal
#' @noRd
.get_tx_start_gaps <- function(exons, group_var) {

    # need to scale the transcript starts so scaled introns/exons align
    # importantly, this tx start also has to take into account
    # whether intron_gaps that overlap it have been shortened
    # here, get the tx_start_gap - the region between
    # 1. the start of plot (smallest start position of all txs)
    # 2. the start of each tx
    tx_start_gaps <-
        exons %>%
        dplyr::group_by_at(.vars = c(
            group_var
        )) %>%
        dplyr::summarise(
            seqnames = unique(seqnames),
            strand = unique(strand),
            end = min(start), # min start of this transcript
            start = min(exons[["start"]]) # min start of all transcripts
        )

    return(tx_start_gaps)
}

#' map the gaps back to introns/transcript start gaps
#' @keywords internal
#' @noRd
.get_gap_map <- function(y, intron_gaps) {

    # when we reduce the length of the intron_gaps, whilst making sure
    # whilst making sure the exons/introns remain aligned
    # to do this, we need to map the intron_gaps back onto the introns

    # the simplest case is when gaps are identical to original introns
    equal_hits <- GenomicRanges::findOverlaps(
        intron_gaps,
        y,
        type = "equal"
    )

    # often, the intron_gaps don't map identically
    # this occurs due to the exons of one tx overlapping the intron of another
    # we find cases when the gaps are completely contained an original intron
    # using type = "within", but this also catches the "equal" intron_gaps
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

    # need both "equal" and "pure_within" hits
    gap_map <- list(
        equal = equal_hits,
        pure_within = pure_within_hits
    )

    return(gap_map)
}

#' @keywords internal
#' @noRd
.get_shortened_gaps <- function(y,
                                intron_gaps,
                                gap_map,
                                group_var,
                                target_gap_width) {

    # we need the intron/tx_start_gap widths (to shorten them)
    y <- y %>% dplyr::mutate(width = (end - start) + 1)

    # characterise introns by shortening type
    y_shortened <- y %>%
        dplyr::mutate(
            shorten_type = dplyr::case_when(
                dplyr::row_number() %in% gap_map[["equal"]][["subjectHits"]] ~
                "equal",
                dplyr::row_number() %in% gap_map[["pure_within"]][["subjectHits"]] ~
                "pure_within",
                TRUE ~ "none"
            )
        )

    # for the "equal" cases, simply shorten the widths to the target_gap_width
    y_shortened <- y_shortened %>%
        dplyr::mutate(
            shortened_width = ifelse(
                (shorten_type == "equal") & (width > target_gap_width),
                target_gap_width,
                width
            )
        )

    # for the "within" cases we need to shorten the intron widths
    # by the !total! amount the overlapping gaps are shortened
    overlapping_gap_indexes <- gap_map[["pure_within"]][["queryHits"]]

    # only have to this if there are gaps that are "pure_within"
    if (length(overlapping_gap_indexes) > 0) {

        # one intron may overlap multiple gaps
        # first, calculate the sum of the reduction in gap widths
        sum_gap_diff <- dplyr::tibble(
            intron_indexes = gap_map[["pure_within"]][["subjectHits"]],
            gap_width = GenomicRanges::width(intron_gaps)[overlapping_gap_indexes]
        ) %>%
            dplyr::mutate(
                shortened_gap_width = ifelse(
                    gap_width > target_gap_width,
                    target_gap_width,
                    gap_width
                ),
                shortened_gap_diff = gap_width - shortened_gap_width,
            ) %>%
            dplyr::group_by(intron_indexes) %>%
            dplyr::summarise(
                sum_shortened_gap_diff = sum(shortened_gap_diff)
            )

        # now actually do reduction for introns with "pure_within" gaps
        y_shortened[["sum_shortened_gap_diff"]][sum_gap_diff[["intron_indexes"]]] <-
            sum_gap_diff[["sum_shortened_gap_diff"]]

        y_shortened <- y_shortened %>%
            dplyr::mutate(
                shortened_width = ifelse(
                    is.na(sum_shortened_gap_diff),
                    shortened_width,
                    width - sum_shortened_gap_diff
                )
            ) %>%
            dplyr::select(-sum_shortened_gap_diff)
    }

    # remove unecessary intermediate cols
    y_shortened <- y_shortened %>%
        dplyr::select(
            -shorten_type,
            -width,
            width = shortened_width
        )

    return(y_shortened)
}

#' @keywords internal
#' @noRd
.get_rescaled_txs <- function(exons,
                              introns_shortened,
                              tx_start_gaps_shortened,
                              group_var) {

    # calculate the rescaled exon/intron start/ends using
    # the widths of the exons and reduced introns
    rescaled_tx <- exons %>% dplyr::mutate(
        type = "exon",
        width = (end - start) + 1
    )

    # bind together exons and introns and arrange into genomic order
    rescaled_tx <- rescaled_tx %>%
        dplyr::bind_rows(
            introns_shortened %>%
                dplyr::mutate(type = "intron")
        ) %>%
        dplyr::arrange_at(.vars = c(group_var, "start", "end"))

    # calculate the rescaled coords using cumsum of the widths of introns/exons
    rescaled_tx <- rescaled_tx %>%
        dplyr::group_by_at(.vars = c(
            group_var
        )) %>%
        dplyr::mutate(
            rescaled_end = cumsum(width),
            rescaled_start = rescaled_end - (width - 1)
        ) %>%
        dplyr::ungroup()

    # account for the tx starts being in different places
    # to keep everything aligned
    if (is.null(group_var)) {
        # if only 1 tx, we use 1 as the dummy rescaled tx_start
        rescaled_tx <- rescaled_tx %>%
            dplyr::mutate(width_tx_start = 1)
    } else {
        rescaled_tx <- rescaled_tx %>%
            dplyr::left_join(
                tx_start_gaps_shortened,
                by = c(group_var),
                suffix = c("", "_tx_start")
            )
    }

    rescaled_tx <- rescaled_tx %>%
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

    return(rescaled_tx)
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

#' @keywords internal
#' @noRd
.check_target_gap_width <- function(target_gap_width) {
    if (!is.integer(target_gap_width)) {
        warning("target_gap_width must be an integer, coercing...")
        target_gap_width <- target_gap_width %>%
            as.integer()
    }

    return(target_gap_width)
}

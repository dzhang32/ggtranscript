#' Obtain the differences between transcript structure
#'
#' `to_diff()` obtains the difference between `exons` from a set of transcripts
#' to a reference transcript (`ref_exons`). This can be useful when visualizing
#' the differences between transcript structure. `to_diff()` expects two sets of
#' input exons; 1. `exons` - exons from any number of transcripts that will be
#' compared to `ref_exons` and 2. `ref_exons` - exons from a single transcript
#' which acts as the reference to compare against.
#'
#' @param exons `data.frame()` contains exons which can originate from multiple
#'   transcripts differentiated by `group_var`.
#' @param ref_exons `data.frame()` contains exons that originate from a single
#'   transcript, which `exons` will be compared against.
#' @param group_var `character()` if input data originates from more than 1
#'   transcript, `group_var` must specify the column that differentiates
#'   transcripts (e.g. "transcript_id").
#'
#' @return `data.frame()` details the differences between `exons` and
#'   `ref_exons`.
#'
#' @export
#' @examples
#'
#' library(magrittr)
#'
#' gba_ens_105_exons <- gba_ens_105 %>%
#'     dplyr::filter(type == "exon")
#'
#' gba_ens_105_exons
#'
#' # for example, let's compare other transcripts to the MANE-select transcript
#' mane <- gba_ens_105_exons %>%
#'     dplyr::filter(transcript_name == "GBA-202")
#'
#' single_tx <- gba_ens_105_exons %>%
#'     dplyr::filter(transcript_name %in% c("GBA-203"))
#'
#' single_tx_diffs <- to_diff(
#'     exons = single_tx,
#'     ref_exons = mane
#' )
#'
#' single_tx_diffs
#'
#' # exons can also contain multiple transcripts
#' multi_tx <- gba_ens_105_exons %>%
#'     dplyr::filter(transcript_name %in% c("GBA-203", "GBA-201", "GBA-204"))
#'
#' multi_tx_diffs <- to_diff(
#'     exons = multi_tx,
#'     ref_exons = mane,
#'     group_var = "transcript_name"
#' )
#'
#' multi_tx_diffs
#'
#' # an example of visualising differences
#' mane %>%
#'     dplyr::bind_rows(multi_tx) %>%
#'     dplyr::mutate(
#'         transcript_name = transcript_name %>%
#'             factor(levels = c("GBA-202", "GBA-201", "GBA-203", "GBA-204"))
#'     ) %>%
#'     ggplot2::ggplot(
#'         ggplot2::aes(
#'             xstart = start,
#'             xend = end,
#'             y = transcript_name
#'         )
#'     ) +
#'     geom_range() +
#'     geom_range(
#'         data = multi_tx_diffs,
#'         ggplot2::aes(fill = diff_type),
#'         alpha = 0.2,
#'     ) +
#'     ggplot2::scale_y_discrete(
#'         labels = c("MANE Select", "GBA-201", "GBA-203", "GBA-204")
#'     )
to_diff <- function(exons, ref_exons, group_var = NULL) {
    .check_coord_object(exons, check_seqnames = TRUE, check_strand = TRUE)
    .check_coord_object(ref_exons, check_seqnames = TRUE, check_strand = TRUE)
    .check_group_var(exons, group_var)

    # need to remember if group is NULL for downstream
    null_group <- is.null(group_var)

    # we have to create dummy group if there is no group for .get_diff
    if (null_group) {
        exons <- exons %>% dplyr::mutate(dummy_group = "A")
        group_var <- "dummy_group"
    }

    diffs <- .get_diff(exons, ref_exons, group_var)

    # remove the dummy_group if created
    if (null_group) diffs[[group_var]] <- NULL

    return(diffs)
}

#' The heavy lifting of `to_diff()` happens here.
#'
#' @keywords internal
#' @noRd
.get_diff <- function(exons, ref_exons, group_var) {
    groups <- exons[[group_var]] %>% unique()

    # needs to be a genomic range for downstream processing
    exons_gr <- GenomicRanges::GRanges(exons)
    ref_exons_gr <- GenomicRanges::GRanges(ref_exons)

    diffs <- vector("list", length = length(group_var))

    for (i in seq_along(groups)) {
        exons_gr_curr <- exons_gr %>%
            .[GenomicRanges::mcols(exons_gr)[[group_var]] == groups[i]]

        # get the disjoint pieces (flattening and breaking apart exons)
        disjoint_pieces <- GenomicRanges::disjoin(
            c(ref_exons_gr, exons_gr_curr)
        )

        # find whether the disjoint pieces overlap exons or ref_exons
        # those that only overlap 1 are the differences
        # TODO - perhaps allow modification of findOverlaps() via ... ?
        overlap_exons <- GenomicRanges::findOverlaps(
            disjoint_pieces, exons_gr_curr
        )
        overlap_ref_exons <- GenomicRanges::findOverlaps(
            disjoint_pieces, ref_exons_gr
        )

        # convert pieces back to data.frame and classify diffs
        # TODO - could improve efficiency by placing this step post-loop
        # i.e. manipulate the grs instead
        diff_curr <- disjoint_pieces %>%
            as.data.frame() %>%
            dplyr::mutate(
                index = dplyr::row_number(),
                type = "diff",
                in_exons = index %in% S4Vectors::queryHits(overlap_exons),
                in_ref_exons = index %in% S4Vectors::queryHits(overlap_ref_exons)
            ) %>%
            dplyr::mutate(
                diff_type = dplyr::case_when(
                    in_exons & in_ref_exons ~ "both",
                    in_exons & !in_ref_exons ~ "not_in_ref",
                    !in_exons & in_ref_exons ~ "in_ref"
                )
            )

        # add back in group info
        diff_curr[[group_var]] <- groups[i]

        # keep only diffs and necessary cols
        diffs[[i]] <-
            diff_curr %>%
            dplyr::filter(diff_type != "both") %>%
            dplyr::select(-in_exons, -in_ref_exons, -index)
    }

    diffs <- diffs %>% do.call(dplyr::bind_rows, .)

    return(diffs)
}

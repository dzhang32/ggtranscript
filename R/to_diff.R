#' Obtain the differences between transcripts
#'
#' `to_diff` is a helper function intended to facilitate visualizing the
#' differences between transcript structure. `to_diff` expects two sets of user
#' inputted exons; 1. `x` - exons from a single transcript which acts as the
#' reference to compare to and 2. `y` exons from number of transcripts that will
#' be compared to `x`.
#'
#' @param x `data.frame` containing ranges to compare against. `x` should
#'   originate from a single transcript.
#' @param y `data.frame` containing ranges to compare with `x`. `y` can
#'   originate from multiple transcripts.
#' @param group_var `character` if `y` contains more than 1 transcript,
#'   `group_var` should specify the column that differentiates transcripts (e.g.
#'   "transcript_id").
#'
#' @return a `data.frame` that details the differences of each `y` group to `x`.
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
#'     x = mane,
#'     y = single_tx
#' )
#'
#' single_tx_diffs
#'
#' # y can also contain multiple transcripts
#' multi_tx <- gba_ens_105_exons %>%
#'     dplyr::filter(transcript_name %in% c("GBA-203", "GBA-201", "GBA-204"))
#'
#' multi_tx_diffs <- to_diff(
#'     x = mane,
#'     y = multi_tx,
#'     group_var = "transcript_name"
#' )
#'
#' multi_tx_diffs
#'
#' # an example of visualising differences
#' mane %>%
#'     dplyr::bind_rows(multi_tx) %>%
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
#'     )
to_diff <- function(x, y, group_var = NULL) {
    .check_coord_object(x)
    .check_coord_object(y)
    .check_group_var(y, group_var)

    # need to remember if group is NULL for downstream
    null_group <- is.null(group_var)

    # we have to create dummy group if there is no group for .get_diff
    if (null_group) {
        y <- y %>% dplyr::mutate(dummy_group = "A")
        group_var <- "dummy_group"
    }

    diffs <- .get_diff(x, y, group_var)

    # remove the dummy_group if created
    if (null_group) diffs[[group_var]] <- NULL

    return(diffs)
}

#' The heavy lifting of `to_diff()` happens here.
#' @keywords internal
#' @noRd
.get_diff <- function(x, y, group_var) {
    groups <- y[[group_var]] %>% unique()

    # needs to be a genomic range for downstream processing
    x_gr <- GenomicRanges::GRanges(x)
    y_gr <- GenomicRanges::GRanges(y)

    diffs <- vector("list", length = length(group_var))

    for (i in seq_along(groups)) {
        y_gr_curr <- y_gr[GenomicRanges::mcols(y_gr)[[group_var]] == groups[i]]

        # get the disjoint pieces (flattening and breaking apart exons)
        disjoint_pieces <- GenomicRanges::disjoin(c(x_gr, y_gr_curr))

        # find whether the disjoint pieces overlap x or the current y
        # those that only overlap 1 are the differences
        # TODO - perhaps allow modification of findOverlaps() via ... ?
        overlap_x <- GenomicRanges::findOverlaps(disjoint_pieces, x_gr)
        overlap_y <- GenomicRanges::findOverlaps(disjoint_pieces, y_gr_curr)

        # convert pieces back to data.frame and classify diffs
        # TODO - could improve efficiency by placing this step post-loop
        # i.e. manipulate the grs instead
        diff_curr <- disjoint_pieces %>%
            as.data.frame() %>%
            dplyr::mutate(
                index = dplyr::row_number(),
                type = "diff",
                in_x = index %in% S4Vectors::queryHits(overlap_x),
                in_y = index %in% S4Vectors::queryHits(overlap_y)
            ) %>%
            dplyr::mutate(
                diff_type = dplyr::case_when(
                    in_x & in_y ~ "both",
                    in_x & !in_y ~ "in_x",
                    !in_x & in_y ~ "in_y"
                )
            )

        # add back in group info
        diff_curr[[group_var]] <- groups[i]

        # keep only diffs and necessary cols
        diffs[[i]] <-
            diff_curr %>%
            dplyr::filter(diff_type != "both") %>%
            dplyr::select(-in_x, -in_y, -index)
    }

    diffs <- diffs %>% do.call(dplyr::bind_rows, .)

    return(diffs)
}

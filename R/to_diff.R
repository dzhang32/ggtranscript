#' @keywords internal
#' @noRd
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

#' Convert exon co-ordinates to introns
#'
#' Given a set of `exons`, `to_intron()` will return the corresponding introns.
#' If `exons` contains data originating from more than 1 transcript, `group_var`
#' must specify the column that differentiates transcripts (e.g.
#' "transcript_id").
#'
#' It is important to note that, for visualisation purposes, `to_intron()`
#' defines introns precisely as the exon boundaries, rather than the intron
#' start/end being (exon end + 1)/(exon start - 1).
#'
#' @inheritParams to_diff
#'
#' @export
#' @examples
#' library(magrittr)
#'
#' example_exons <-
#'     dplyr::tibble(
#'         start = c(5, 10, 15, 20),
#'         end = c(7, 12, 17, 22),
#'         tx = c("A", "A", "B", "B")
#'     )
#'
#' example_exons
#'
#' to_intron(example_exons, group_var = "tx")
#'
#' # this can be convenient when plotting transcript annotation
#' example_exons %>%
#'     ggplot2::ggplot(
#'         ggplot2::aes(
#'             xstart = start,
#'             xend = end,
#'             y = tx
#'         )
#'     ) +
#'     geom_range() +
#'     geom_intron(
#'         data = to_intron(example_exons, "tx")
#'     )
to_intron <- function(exons, group_var = NULL) {
    .check_coord_object(exons)
    .check_group_var(exons, group_var)

    # TODO - add functionality to check warn if exons overlap
    # as this should could break the function
    # which expects exons in each group_var to originate from a single tx

    if (!is.null(group_var)) {
        exons <- exons %>% dplyr::group_by_at(.vars = group_var)
    }

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    exons <- exons %>%
        dplyr::arrange(start, end)

    # obtain intron start and ends
    exons <- exons %>%
        dplyr::mutate(
            intron_start := dplyr::lag(end),
            intron_end := start,
            type = "intron"
        ) %>%
        dplyr::select(-start, -end)

    # for each group, output N of introns should be N - 1 the inputted exons
    # remove the introduced artifact NAs
    # rename intron_start/intron_end to
    exons <- exons %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end)) %>%
        dplyr::rename(start = intron_start, end = intron_end)

    return(exons)
}

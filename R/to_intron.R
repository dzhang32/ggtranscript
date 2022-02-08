#' Convert exon co-ordinates to introns
#'
#' Taking a set of exons, `to_intron` will return the corresponding introns.
#' `to_intron` expects inputted exons to either 1. originate from a single
#' transcript or 2. be grouped via `group_var` such that each group corresponds
#' to a single transcript. In other words, `to_intron` assumes that exons (from
#' each group) do not overlap one another.
#'
#' @param x `data.frame` containing exons co-ordinates (and possibly associated
#'   transcript)
#' @param group_var `character` specifying the column with the
#'   group, most often the transcript id/name.
#'
#' @export
#' @examples
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
to_intron <- function(x, group_var = NULL) {
    .check_coord_object(x)
    .check_group_var(x, group_var)

    # TODO - add functionality to check warn if exons overlap
    # as this should could break the function
    # which expects exons in each group_var to originate from a single tx

    if (!is.null(group_var)) {
        x <- x %>% dplyr::group_by_at(.vars = group_var)
    }

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    x <- x %>%
        dplyr::arrange(start, end)

    # obtain intron start and ends
    x <- x %>%
        dplyr::mutate(
            intron_start := dplyr::lag(end) + 1,
            intron_end := start - 1,
            type = "intron"
        ) %>%
        dplyr::select(-start, -end)

    # for each group, output N of introns should be N - 1 the inputted exons
    # remove the introduced artifact NAs
    # rename intron_start/intron_end to
    x <- x %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end)) %>%
        dplyr::rename(start = intron_start, end = intron_end)

    return(x)
}

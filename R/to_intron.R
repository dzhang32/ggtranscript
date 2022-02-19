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
#' library(ggplot2)
#'
#' # to illustrate the package's functionality
#' # ggtranscript includes example transcript annotation
#' sod1_annotation %>% head()
#'
#' # extract exons
#' sod1_exons <- sod1_annotation %>% dplyr::filter(type == "exon")
#' sod1_exons %>% head()
#'
#' # to_intron() is a helper function included in ggtranscript
#' # which is useful for converting exon co-ordinates to introns
#' sod1_introns <- sod1_exons %>% to_intron(group_var = "transcript_name")
#' sod1_introns %>% head()
#'
#' # this can be particular useful when combined with
#' # geom_range() and geom_intron()
#' # to visualize the core components of transcript annotation
#' sod1_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_intron(
#'         data = to_intron(sod1_exons, "transcript_name")
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

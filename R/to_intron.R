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
#' @return `data.frame()` contains the intron co-ordinates.
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

    # TODO - switch this to using GenomicRanges::gaps()?

    if (!is.null(group_var)) {
        exons <- exons %>% dplyr::group_by_at(.vars = group_var)
    }

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    exons <- exons %>%
        dplyr::arrange(start, end)

    # obtain intron start and ends
    introns <- exons %>%
        dplyr::mutate(
            intron_start := dplyr::lag(end),
            intron_end := start,
            type = "intron"
        ) %>%
        dplyr::select(-start, -end)

    # remove the introduced artifact NAs
    introns <- introns %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end))

    # filter out introns with a width of 1, this should only happen when
    # utrs are included and are directly adjacent to end of cds
    introns <- introns %>% dplyr::filter(abs(intron_end - intron_start) != 1)

    introns <- introns %>% dplyr::rename(start = intron_start, end = intron_end)

    return(introns)
}

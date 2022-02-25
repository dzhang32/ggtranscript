#' Add utrs as ranges
#'
#' Placeholder
#'
#' @inheritParams to_diff
#' @param cds `data.frame()` contains the coding sequence which can originate
#'   from multiple transcripts differentiated by `group_var`.
#'
#' @export
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' # to illustrate the package's functionality
#' # ggtranscript includes example transcript annotation
#' pknox1_annotation %>% head()
#'
#' # extract exons
#' pknox1_exons <- pknox1_annotation %>% dplyr::filter(type == "exon")
#' pknox1_exons %>% head()
#'
#' # extract cds
#' pknox1_cds <- pknox1_annotation %>% dplyr::filter(type == "CDS")
#' pknox1_cds %>% head()
#'
#' # need to make sure that the CDS definition includes the stop codon
#' # as the ensembl CDS definition does not include the stop codon
#' # here, we add 3 base pairs to the end of the the CDS of each transcript
#' pknox1_cds_w_stop <- pknox1_cds %>%
#'     dplyr::group_by(transcript_name) %>%
#'     dplyr::mutate(
#'         end = ifelse(end == max(end), end + 3, end)
#'     ) %>%
#'     dplyr::ungroup()
#'
#' # add utr adds ranges that represent the utr
#' pknox1_cds_utr <- add_utr(
#'     pknox1_exons,
#'     pknox1_cds_w_stop,
#'     group_var = "transcript_name"
#' )
#'
#' pknox1_cds_utr %>% head()
#'
#' # this can be useful to visualize the utrs
#' pknox1_cds_utr %>%
#'     dplyr::filter(type == "CDS") %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_range(
#'         data = pknox1_cds_utr %>% dplyr::filter(type == "UTR"),
#'         height = 0.25,
#'         fill = "white"
#'     ) +
#'     geom_intron(
#'         data = to_intron(
#'             pknox1_cds_utr %>%
#'                 dplyr::filter(type != "intron"),
#'             "transcript_name"
#'         ),
#'     )
#'
#' # add utrs can be most useful when combined with shorten_gaps()
#' pknox1_cds_utr_rescaled <-
#'     shorten_gaps(
#'         exons = pknox1_cds_utr,
#'         introns = to_intron(pknox1_cds_utr, "transcript_name"),
#'         group_var = "transcript_name"
#'     )
#'
#' # so that you can plot the shorten_gap() output together with utrs
#' pknox1_cds_utr_rescaled %>%
#'     dplyr::filter(type == "CDS") %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_range(
#'         data = pknox1_cds_utr_rescaled %>% dplyr::filter(type == "UTR"),
#'         height = 0.25,
#'         fill = "white"
#'     ) +
#'     geom_intron(
#'         data = to_intron(
#'             pknox1_cds_utr_rescaled %>%
#'                 dplyr::filter(type != "intron"),
#'             "transcript_name"
#'         ),
#'         arrow.min.intron.length = 100
#'     )
add_utr <- function(exons,
                    cds,
                    group_var = NULL) {

    # input checks
    .check_coord_object(exons, check_seqnames = TRUE)
    .check_group_var(exons, group_var)
    .check_coord_object(cds, check_seqnames = TRUE)
    .check_group_var(cds, group_var)

    # we have to create dummy group for downstream for loop if there is no group
    null_group <- is.null(group_var)
    if (null_group) {
        exons <- exons %>% dplyr::mutate(dummy_group = "A")
        cds <- cds %>% dplyr::mutate(dummy_group = "A")
        group_var <- "dummy_group"
    }

    groups <- cds[[group_var]] %>% unique()

    # convert to GenomicRanges for downstream processing
    exons_gr <- exons %>% GenomicRanges::GRanges()
    cds_gr <- cds %>% GenomicRanges::GRanges()

    exons_w_utr <- vector("list", length = length(groups))

    for (i in seq_along(groups)) {
        exons_gr_curr <- exons_gr %>%
            .[GenomicRanges::mcols(exons_gr)[[group_var]] == groups[i]]

        cds_gr_curr <- cds_gr %>%
            .[GenomicRanges::mcols(cds_gr)[[group_var]] == groups[i]]

        # use setdiff to get regions in exon but not in cds (i.e. the utrs)
        utrs_curr <- GenomicRanges::setdiff(exons_gr_curr, cds_gr_curr)
        GenomicRanges::mcols(utrs_curr)[[group_var]] <- groups[i]

        utrs_curr$type <- "UTR"
        cds_gr_curr$type <- "CDS"

        exons_w_utr[[i]] <- c(utrs_curr, cds_gr_curr) %>% sort()
    }

    exons_w_utr <- exons_w_utr %>%
        do.call(c, .) %>%
        as.data.frame() %>%
        dplyr::as_tibble()

    # remove dummp_group if created
    if (null_group) {
        exons_w_utr <- exons_w_utr %>% dplyr::select(-dummy_group)
    }

    return(exons_w_utr)
}

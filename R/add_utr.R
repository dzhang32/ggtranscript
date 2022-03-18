#' Add untranslated regions (UTRs)
#'
#' Given a set of `exons` (encompassing the CDS and UTRs) and `cds` regions,
#' `add_utr()` will calculate and add the corresponding UTR regions as ranges.
#' This can be useful when combined with `shorten_gaps()` to visualize
#' transcripts with long introns, whilst differentiating UTRs from CDS regions.
#'
#' The definition of the inputted `cds` regions are expected to range from the
#' beginning of the start codon to the end of the stop codon. Sometimes, for
#' example in the case of Ensembl, reference annotation will omit the stop
#' codons from the CDS definition. In such cases, users should manually ensure
#' that the `cds` includes both the start and stop codons.
#'
#' @inheritParams to_diff
#' @param cds `data.frame()` contains coding sequence ranges for the transcripts
#'   in `exons`.
#'
#' @return `data.frame()` contains differentiated CDS and UTR ranges.
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
#' # the CDS definition originating from the Ensembl reference annotation
#' # does not include the stop codon
#' # we must incorporate the stop codons into the CDS manually
#' # by adding 3 base pairs to the end of the CDS of each transcript
#' pknox1_cds_w_stop <- pknox1_cds %>%
#'     dplyr::group_by(transcript_name) %>%
#'     dplyr::mutate(
#'         end = ifelse(end == max(end), end + 3, end)
#'     ) %>%
#'     dplyr::ungroup()
#'
#' # add_utr() adds ranges that represent the UTRs
#' pknox1_cds_utr <- add_utr(
#'     pknox1_exons,
#'     pknox1_cds_w_stop,
#'     group_var = "transcript_name"
#' )
#'
#' pknox1_cds_utr %>% head()
#'
#' # this can be useful when combined with shorten_gaps()
#' # to visualize transcripts with long introns whilst differentiating UTRs
#' pknox1_cds_utr_rescaled <-
#'     shorten_gaps(
#'         exons = pknox1_cds_utr,
#'         introns = to_intron(pknox1_cds_utr, "transcript_name"),
#'         group_var = "transcript_name"
#'     )
#'
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
#'             pknox1_cds_utr_rescaled %>% dplyr::filter(type != "intron"),
#'             "transcript_name"
#'         ),
#'         arrow.min.intron.length = 110
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

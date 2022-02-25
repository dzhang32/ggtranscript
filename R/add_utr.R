#' @keywords internal
#' @noRd
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

pknox1_exons <- pknox1_annotation %>% dplyr::filter(type == "exon")
pknox1_cds <- pknox1_annotation %>% dplyr::filter(type == "CDS")
pknox1_utr <- pknox1_annotation %>% dplyr::filter(grepl("utr", type))

##### add_utr #####

# add 3 bp to end of cds as stop codon not included in ensembl cds
pknox1_cds_w_stop <- pknox1_cds %>%
    dplyr::group_by(transcript_name) %>%
    dplyr::mutate(
        end = ifelse(end == max(end), end + 3, end)
    ) %>%
    dplyr::ungroup()

pknox1_cds_utr <- add_utr(
    pknox1_exons,
    pknox1_cds_w_stop,
    group_var = "transcript_name"
)

pknox1_cds_utr_1_tx <- add_utr(
    pknox1_exons %>% dplyr::filter(transcript_name == "PKNOX1-203"),
    pknox1_cds_w_stop %>% dplyr::filter(transcript_name == "PKNOX1-203"),
    group_var = "transcript_name"
)

pknox1_cds_utr_1_tx_no_group <- add_utr(
    pknox1_exons %>% dplyr::filter(transcript_name == "PKNOX1-203"),
    pknox1_cds_w_stop %>% dplyr::filter(transcript_name == "PKNOX1-203"),
    group_var = NULL
)

test_add_utrs <- function(cds_utr_add_utr, utr_annotation, cds_annotation) {
    utr_add_utr <- cds_utr_add_utr %>%
        dplyr::filter(type == "UTR") %>%
        dplyr::select(start, end) %>%
        dplyr::arrange(start, end)

    cds_add_utr <- cds_utr_add_utr %>%
        dplyr::filter(type == "CDS") %>%
        dplyr::select(start, end) %>%
        dplyr::arrange(start, end)

    utr_annotation <- utr_annotation %>%
        dplyr::select(start, end) %>%
        dplyr::arrange(start, end)

    cds_annotation <- cds_annotation %>%
        dplyr::select(start, end) %>%
        dplyr::arrange(start, end)

    no_na_type <- all(!is.na(cds_utr_add_utr[["type"]]))
    no_dummy_group <- is.null(cds_utr_add_utr[["dummy_group"]])
    correct_utrs <- all.equal(utr_add_utr, utr_annotation)
    correct_cds <- all.equal(cds_add_utr, cds_annotation)

    check_add_utr <- all(no_na_type, no_dummy_group, correct_utrs, correct_cds)

    return(check_add_utr)
}

testthat::test_that(
    "add_utr() works correctly",
    {
        expect_true(test_add_utrs(pknox1_cds_utr, pknox1_utr, pknox1_cds_w_stop))
        expect_true(test_add_utrs(
            pknox1_cds_utr_1_tx,
            pknox1_utr %>% dplyr::filter(transcript_name == "PKNOX1-203"),
            pknox1_cds_w_stop %>% dplyr::filter(transcript_name == "PKNOX1-203")
        ))
        expect_true(test_add_utrs(
            pknox1_cds_utr_1_tx_no_group,
            pknox1_utr %>% dplyr::filter(transcript_name == "PKNOX1-203"),
            pknox1_cds_w_stop %>% dplyr::filter(transcript_name == "PKNOX1-203")
        ))
    }
)

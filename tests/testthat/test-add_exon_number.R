gba_ens_105_exons <- gba_ens_105 %>%
    dplyr::filter(type == "exon")

# create dummy transcripts with both positive and minus strand
# purely for testing strand functionality
test_exons <- gba_ens_105_exons %>%
    dplyr::filter(transcript_name == "GBA-202") %>%
    dplyr::mutate(strand = "+") %>%
    dplyr::bind_rows(
        gba_ens_105_exons %>%
            dplyr::filter(transcript_name == "GBA-201")
    )

##### add_exon_number #####

testthat::test_that("add_exon_number() works correctly", {
    test_exon_number <- test_exons %>%
        add_exon_number(group_var = "transcript_name")

    test_exon_number_plus <- test_exon_number %>%
        dplyr::filter(strand == "+")
    test_exon_number_minus <- test_exon_number %>%
        dplyr::filter(strand == "-")

    expect_true("exon_number" %in% colnames(test_exon_number))
    expect_true(is.numeric(test_exon_number[["exon_number"]]))

    expect_equal(
        test_exon_number_plus[["exon_number"]],
        seq_len(nrow(test_exon_number_plus))
    )
    expect_equal(
        test_exon_number_minus[["exon_number"]],
        seq_len(nrow(test_exon_number_minus)) %>% rev()
    )

    # check order makes no difference
    set.seed(32)
    expect_equal(
        test_exons[sample(seq_len(nrow(test_exons)), nrow(test_exons)), ] %>%
            add_exon_number(group_var = "transcript_name"),
        test_exon_number
    )
})

testthat::test_that("add_exon_number(group_var = NULL) works correctly", {
    test_exon_number_plus <- test_exons %>%
        dplyr::filter(strand == "+") %>%
        add_exon_number(group_var = NULL)
    test_exon_number_minus <- test_exons %>%
        dplyr::filter(strand == "-") %>%
        add_exon_number(group_var = NULL)

    expect_equal(
        test_exon_number_plus[["exon_number"]],
        seq_len(nrow(test_exon_number_plus))
    )
    expect_equal(
        test_exon_number_minus[["exon_number"]],
        seq_len(nrow(test_exon_number_minus)) %>% rev()
    )
})

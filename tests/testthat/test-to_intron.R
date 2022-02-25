# create dummy exons for testing
test_exons <-
    dplyr::tibble(
        start = c(100, 300, 500, 650),
        end = start + 100,
        strand = c("+", "+", "-", "-"),
        tx = c("A", "A", "B", "B")
    )

# manually create the expected introns
test_introns <-
    dplyr::tibble(
        strand = c("+", "-"),
        tx = c("A", "B"),
        start = c(200, 600),
        end = c(300, 650),
        type = "intron"
    )

pknox1_cds_utr <-
    pknox1_annotation %>% dplyr::filter(
        type == "CDS" | grepl("utr", type)
    )

##### to_intron #####

testthat::test_that("to_intron() obtains introns correctly", {
    # with group_var
    expect_equal(
        test_introns,
        test_exons %>% to_intron(group_var = "tx")
    )
    # without group_var
    expect_equal(
        test_introns %>% dplyr::filter(tx != "B"),
        test_exons %>%
            dplyr::filter(tx != "B") %>%
            to_intron()
    )
})

testthat::test_that(
    "to_intron() obtains introns correctly, regardless of exon order",
    {
        set.seed(32)

        expect_equal(
            test_introns,
            test_exons %>%
                .[sample(seq_len(nrow(test_exons))), ] %>%
                to_intron(group_var = "tx")
        )
    }
)

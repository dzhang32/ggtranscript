##### to_intron #####

# create dummy exons for testing
test_exons <-
    dplyr::tibble(
        start = c(5, 10, 15, 20),
        end = c(7, 12, 17, 22),
        tx = c("A", "A", "B", "B")
    )

# manually create the expected introns
test_introns <-
    dplyr::tibble(
        tx = c("A", "B"),
        intron_start = c(8, 18),
        intron_end = c(9, 19)
    )

testthat::test_that("to_intron() obtains introns correctly", {
    # with group_var
    expect_equal(
        test_introns,
        test_exons %>% to_intron(group_var = tx)
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
                to_intron(group_var = tx)
        )
    }
)

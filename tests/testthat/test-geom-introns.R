##### to_introns #####

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

testthat::test_that("to_introns() obtains introns correctly", {
    expect_equal(
        test_introns,
        test_exons %>% to_introns(group_var = tx)
    )
})

testthat::test_that(
    "to_introns() obtains introns correctly, regardless of exon order",
    {
        expect_equal(
            test_introns,
            test_exons %>%
                dplyr::arrange(dplyr::desc(start)) %>%
                to_introns(group_var = tx)
        )
    }
)

testthat::test_that(
    "to_introns() obtains introns correctly, regardless of exon order",
    {
        set.seed(32)

        expect_equal(
            test_introns,
            test_exons %>%
                .[sample(seq_len(nrow(test_exons))), ] %>%
                to_introns(group_var = tx)
        )
    }
)

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

##### geom_intron #####

testthat::test_that(
    "geom_intron() catches strand input errors",
    {
        na_strand <- test_introns %>%
            ggplot2::ggplot() +
            geom_intron(ggplot2::aes(
                x_start = intron_start, x_end = intron_end,
                y = tx
            ),
            strand = NA
            )
        a_strand <- test_introns %>%
            ggplot2::ggplot() +
            geom_intron(ggplot2::aes(
                x_start = intron_start, x_end = intron_end,
                y = tx
            ),
            strand = "a"
            )
        # seems to require print to catch error
        expect_error(
            print(na_strand),
            "strand values must be one of"
        )
        expect_error(
            print(a_strand),
            "strand values must be one of"
        )
    }
)

# test_introns %>%
#   ggplot2::ggplot() +
#   geom_intron(ggplot2::aes(
#     x_start = intron_start, x_end = intron_end,
#     y = tx
#   ),
#   strand = "+"
#   )

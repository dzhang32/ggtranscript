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
        start = c(201, 601),
        end = c(299, 649),
        type = "intron"
    )

# create base plot to be used in downstream tests
test_introns_plot <- test_introns %>%
    ggplot2::ggplot(aes(
        xstart = start,
        xend = end,
        y = tx
    ))

##### to_intron #####

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
    "geom_intron() works correctly",
    {
        base_geom_intron <- test_introns_plot +
            geom_intron()
        w_param_geom_intron <- test_introns_plot +
            geom_intron(colour = "blue", size = 2)
        w_aes_geom_intron <- test_introns_plot +
            geom_intron(aes(colour = tx, size = c(1L, 2L)))
        w_facet_geom_intron <- test_introns_plot +
            geom_intron() +
            ggplot2::facet_wrap(~tx)

        vdiffr::expect_doppelganger(
            "Base geom_intron plot",
            base_geom_intron
        )
        vdiffr::expect_doppelganger(
            "With param geom_intron plot",
            w_param_geom_intron
        )
        vdiffr::expect_doppelganger(
            "With aes geom_intron plot",
            w_aes_geom_intron
        )
        vdiffr::expect_doppelganger(
            "With facet geom_intron plot",
            w_facet_geom_intron
        )
    }
)

testthat::test_that(
    "geom_intron(strand = x) works correctly",
    {
        minus_strand <- test_introns_plot +
            geom_intron(strand = "-")
        as_aes_strand <- test_introns_plot +
            geom_intron(aes(strand = strand))

        vdiffr::expect_doppelganger(
            "Minus strand plot",
            minus_strand
        )
        vdiffr::expect_doppelganger(
            "As aes strand plot",
            as_aes_strand
        )
    }
)

testthat::test_that(
    "geom_intron(arrow.min.intron.length = x) works correctly",
    {
        base_arrow.min <- test_introns_plot +
            geom_intron(arrow.min.intron.length = 50)
        w_strand_arrow_min <- test_introns_plot +
            geom_intron(arrow.min.intron.length = 50, strand = "-")

        vdiffr::expect_doppelganger(
            "base arrow.min plot",
            base_arrow.min
        )
        vdiffr::expect_doppelganger(
            "with strand arrow.min plot",
            w_strand_arrow_min
        )
    }
)

testthat::test_that(
    "geom_intron() catches strand input errors",
    {
        na_strand <- test_introns_plot +
            geom_intron(strand = c(NA, rep("+", nrow(test_introns) - 1)))
        a_strand <- test_introns_plot +
            geom_intron(strand = "a")
        int_strand <- test_introns_plot +
            geom_intron(aes(strand = start))
        # seems to require print to catch error
        expect_error(
            print(na_strand),
            "strand values must be one of"
        )
        expect_error(
            print(a_strand),
            "strand values must be one of"
        )
        expect_error(
            print(int_strand),
            "strand values must be one of"
        )
    }
)

testthat::test_that(
    "geom_intron() catches arrow.min.intron.length input errors",
    {
        neg_arrow.min <- test_introns_plot +
            geom_intron(arrow.min.intron.length = -1)
        chr_arrow.min <- test_introns_plot +
            geom_intron(arrow.min.intron.length = "1")
        # seems to require print to catch error
        expect_error(
            print(neg_arrow.min),
            "arrow.min.intron.length must be "
        )
        expect_error(
            print(chr_arrow.min),
            "arrow.min.intron.length must be "
        )
    }
)

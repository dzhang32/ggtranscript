# create dummy exons for testing
test_exons <-
    dplyr::tibble(
        start = c(100, 300, 500, 650),
        end = start + 100,
        strand = c("+", "+", "-", "-"),
        tx = c("A", "A", "B", "B")
    )

# create base plot to be used in downstream tests
test_exons_plot <- test_exons %>%
    ggplot2::ggplot(aes(
        xstart = start,
        xend = end,
        y = tx
    ))

##### geom_range #####

testthat::test_that(
    "geom_range() works correctly",
    {
        base_geom_range <- test_exons_plot +
            geom_range()
        w_param_geom_range <- test_exons_plot +
            geom_range(colour = "red", fill = "blue")
        w_aes_geom_range <- test_exons_plot +
            geom_range(aes(fill = tx))
        w_facet_geom_range <- test_exons_plot +
            geom_range() +
            ggplot2::facet_wrap(~tx)

        vdiffr::expect_doppelganger(
            "Base geom_range plot",
            geom_range
        )
        vdiffr::expect_doppelganger(
            "With param geom_range plot",
            w_param_geom_range
        )
        vdiffr::expect_doppelganger(
            "With aes geom_range plot",
            w_aes_geom_range
        )
        vdiffr::expect_doppelganger(
            "With facet geom_range plot",
            w_facet_geom_range
        )
    }
)

testthat::test_that(
    "geom_range(vjust = x) works correctly",
    {
        w_vjust_geom_range <- test_exons_plot +
            geom_range(vjust = 1.5, height = 0.25)

        vdiffr::expect_doppelganger(
            "With vjust geom_range plot",
            w_vjust_geom_range
        )
    }
)

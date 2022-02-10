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

##### geom_half_range #####

testthat::test_that(
    "geom_half_range() works correctly",
    {
        base_geom_half_range <- test_exons_plot +
            geom_half_range()
        w_param_geom_half_range <- test_exons_plot +
            geom_half_range(colour = "red", fill = "blue")
        w_aes_geom_half_range <- test_exons_plot +
            geom_half_range(aes(fill = tx))
        w_facet_geom_half_range <- test_exons_plot +
            geom_half_range() +
            ggplot2::facet_wrap(~tx)

        vdiffr::expect_doppelganger(
            "Base geom_half_range plot",
            base_geom_half_range
        )
        vdiffr::expect_doppelganger(
            "With param geom_half_range plot",
            w_param_geom_half_range
        )
        vdiffr::expect_doppelganger(
            "With aes geom_half_range plot",
            w_aes_geom_half_range
        )
        vdiffr::expect_doppelganger(
            "With facet geom_half_range plot",
            w_facet_geom_half_range
        )
    }
)

testthat::test_that(
    "geom_half_range(range.orientation = x) works correctly",
    {
        w_top_geom_half_range <- test_exons_plot +
            geom_half_range(range.orientation = "top")
        w_both_geom_half_range <- test_exons_plot +
            geom_half_range(range.orientation = "top", fill = "red") +
            geom_half_range(range.orientation = "bottom", fill = "blue")

        vdiffr::expect_doppelganger(
            "With top geom_half_range plot",
            w_top_geom_half_range
        )
        vdiffr::expect_doppelganger(
            "With both geom_half_range plot",
            w_both_geom_half_range
        )
    }
)

testthat::test_that(
    "geom_half_range() catches user input errors",
    {
        a_range.orientation <- test_exons_plot +
            geom_half_range(range.orientation = "a")

        expect_error(
            print(a_range.orientation),
            "range.orientation must be one of"
        )
    }
)

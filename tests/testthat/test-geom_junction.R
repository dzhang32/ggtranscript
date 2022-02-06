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

##### geom_junction #####

testthat::test_that(
    "geom_junction() works correctly",
    {
        base_geom_junction <- test_introns_plot +
            geom_junction()
        w_param_geom_junction <- test_introns_plot +
            geom_junction(colour = "red", size = 2)
        w_aes_geom_junction <- test_introns_plot +
            geom_junction(aes(colour = tx))
        w_facet_geom_junction <- test_introns_plot +
            geom_junction() +
            ggplot2::facet_wrap(~tx)

        vdiffr::expect_doppelganger(
            "Base geom_junction plot",
            geom_junction
        )
        vdiffr::expect_doppelganger(
            "With param geom_junction plot",
            w_param_geom_junction
        )
        vdiffr::expect_doppelganger(
            "With aes geom_junction plot",
            w_aes_geom_junction
        )
        vdiffr::expect_doppelganger(
            "With facet geom_junction plot",
            w_facet_geom_junction
        )
    }
)

testthat::test_that(
    "geom_junction(junction.orientation = x) works correctly",
    {
        top_junction.orientation <- test_introns_plot +
            geom_junction(junction.orientation = "top")
        bottom_junction.orientation <- test_introns_plot +
            geom_junction(junction.orientation = "bottom")
        w_aes_param_top_junction.orientation <- test_introns_plot +
            geom_junction(aes(colour = tx), size = 1, junction.orientation = "top")

        vdiffr::expect_doppelganger(
            "top junction.orientation plot",
            top_junction.orientation
        )
        vdiffr::expect_doppelganger(
            "bottom junction.orientation plot",
            bottom_junction.orientation
        )
        vdiffr::expect_doppelganger(
            "with aes and param top junction.orientation plot",
            w_aes_param_top_junction.orientation
        )
    }
)

testthat::test_that(
    "geom_junction() catches junction_orientation input errors",
    {
        a_junction.orientation <- test_introns_plot +
            geom_junction(junction.orientation = "a")

        expect_error(
            print(a_junction.orientation),
            "junction.orientation must be one of"
        )
    }
)

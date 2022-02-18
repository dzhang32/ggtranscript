# manually create the expected introns
test_introns <-
    sod1_annotation %>%
    dplyr::filter(type == "exon") %>%
    to_intron(group_var = "transcript_name")

# create base plot to be used in downstream tests
test_introns_plot <- test_introns %>%
    ggplot2::ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    ))

##### geom_junction #####

testthat::test_that(
    "geom_junction() works correctly",
    {
        base_geom_junction <- test_introns_plot +
            geom_junction()
        w_param_geom_junction <- test_introns_plot +
            geom_junction(colour = "red")
        w_aes_geom_junction <- test_introns_plot +
            geom_junction(aes(colour = transcript_name))
        w_facet_geom_junction <- test_introns_plot +
            geom_junction() +
            ggplot2::facet_wrap(~transcript_biotype)

        vdiffr::expect_doppelganger(
            "Base geom_junction plot",
            base_geom_junction
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
            geom_junction(
                aes(colour = transcript_name),
                size = 1,
                junction.orientation = "top"
            )

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
    "geom_junction(junction.y.max = x) works correctly",
    {
        junction.y.max_0.5 <- test_introns_plot +
            geom_junction(junction.y.max = 0.5)
        w_aes_param_junction.y.max_0.5 <- test_introns_plot +
            geom_junction(
                aes(colour = transcript_name),
                size = 1,
                junction.y.max = 0.5
            )
        w_facet_junction.y.max_0.5 <- test_introns_plot +
            geom_junction(junction.y.max = 0.5) +
            ggplot2::facet_wrap(~transcript_biotype)

        vdiffr::expect_doppelganger(
            "0.5 junction.y.max plot",
            junction.y.max_0.5
        )
        vdiffr::expect_doppelganger(
            "with aes and param 0.5 junction.y.max plot",
            w_aes_param_junction.y.max_0.5
        )
        vdiffr::expect_doppelganger(
            "with facet 0.5 junction.y.max plot",
            w_facet_junction.y.max_0.5
        )
    }
)

testthat::test_that(
    "geom_junction() catches junction.orientation input errors",
    {
        a_junction.orientation <- test_introns_plot +
            geom_junction(junction.orientation = "a")

        expect_error(
            print(a_junction.orientation),
            "junction.orientation must be one of"
        )
    }
)

testthat::test_that(
    "geom_junction() catches junction.y.max input errors",
    {
        len_2_junction.y.max <- test_introns_plot +
            geom_junction(junction.y.max = c(1, 2))
        a_junction.y.max <- test_introns_plot +
            geom_junction(junction.y.max = "a")

        expect_error(
            print(len_2_junction.y.max),
            "junction.y.max must have a length of 1"
        )
        expect_error(
            print(a_junction.y.max),
            "junction.y.max must be a numeric value"
        )
    }
)

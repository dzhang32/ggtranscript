# manually create the expected introns
test_introns <-
    gba_ens_105 %>%
    dplyr::filter(type == "exon") %>%
    to_intron(group_var = transcript_name)


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
            geom_junction(colour = "red", curvature = 0.25)
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
    "geom_junction() catches junction_orientation input errors",
    {
        a_junction.orientation <- test_introns_plot +
            geom_junction(junction.orientation = "a")
        neg_curvature_junction.orientation <- test_introns_plot +
            geom_junction(curvature = -0.5)

        expect_error(
            print(a_junction.orientation),
            "junction.orientation must be one of"
        )
        expect_warning(
            print(neg_curvature_junction.orientation),
            "Setting curvature of < 0 will flip"
        )
    }
)

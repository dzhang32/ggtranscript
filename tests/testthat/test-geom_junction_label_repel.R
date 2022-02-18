# manually create the expected introns
test_introns <-
    sod1_annotation %>%
    dplyr::filter(
        type == "exon",
        transcript_name %in% c("SOD1-201", "SOD1-202")
    ) %>%
    to_intron(group_var = "transcript_name") %>%
    dplyr::mutate(
        count = dplyr::row_number()
    )

# create base plot to be used in downstream tests
test_introns_plot <- test_introns %>%
    ggplot2::ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    ))

##### geom_junction_label_repel #####

# testthat::test_that(
#     "geom_junction() works correctly",
#     {
#         base_geom_junction_labels <- test_introns_plot +
#             geom_junction() +
#             geom_junction_label_repel(
#                 aes(label = count),
#                 seed = 32
#             )
#         w_param_geom_junction_labels <- test_introns_plot +
#             geom_junction(
#                 junction.y.max = 0.5
#             ) +
#             geom_junction_label_repel(
#                 aes(label = count),
#                 junction.y.max = 0.5,
#                 seed = 32
#             )
#         w_aes_geom_junction_labels <- test_introns_plot +
#             geom_junction(aes(colour = transcript_name)) +
#             geom_junction_label_repel(
#                 aes(
#                     label = count,
#                     colour = transcript_name
#                 ),
#                 seed = 32
#             )
#         w_facet_geom_junction_labels <- test_introns_plot +
#             geom_junction() +
#             geom_junction_label_repel(
#                 aes(label = count),
#             ) +
#             ggplot2::facet_wrap(transcript_name ~ ., drop = TRUE)
#
#         vdiffr::expect_doppelganger(
#             "Base geom_junction_label_repel plot",
#             base_geom_junction_labels
#         )
#         vdiffr::expect_doppelganger(
#             "With param geom_junction_label_repel plot",
#             w_param_geom_junction_labels
#         )
#         vdiffr::expect_doppelganger(
#             "With aes geom_junction_label_repel plot",
#             w_aes_geom_junction_labels
#         )
#         vdiffr::expect_doppelganger(
#             "With facet geom_junction_label_repel plot",
#             w_facet_geom_junction_labels
#         )
#     }
# )

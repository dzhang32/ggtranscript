sod1_exons <- sod1_annotation %>%
    dplyr::filter(type == "exon")

mane <- sod1_exons %>%
    dplyr::filter(transcript_name == "SOD1-201")

single_tx <- sod1_exons %>%
    dplyr::filter(transcript_name %in% c("SOD1-202"))

multi_tx <- sod1_exons %>%
    dplyr::filter(transcript_name %in% c("SOD1-202", "SOD1-203", "SOD1-204"))

##### to_diff #####

testthat::test_that("to_diff() works correctly", {
    test_diffs <- to_diff(
        exons = single_tx,
        ref_exons = mane
    )
    expect_true(is.data.frame(test_diffs))
    expect_true(nrow(test_diffs) > 0)
    expect_true(all(
        c("seqnames", "start", "end", "strand", "type", "diff_type") %in%
            colnames(test_diffs)
    ))
})

testthat::test_that("to_diff() works correctly for single transcripts", {
    test_diffs <- to_diff(
        exons = single_tx,
        ref_exons = mane,
        group_var = "transcript_name"
    )
    # think the easiest way to check diffs is via plotting
    single_tx_diff_plot <- mane %>%
        dplyr::bind_rows(single_tx) %>%
        ggplot2::ggplot(
            aes(
                xstart = start,
                xend = end,
                y = transcript_name
            )
        ) +
        geom_range() +
        geom_range(
            data = test_diffs,
            alpha = 0.2,
            fill = "red"
        )

    vdiffr::expect_doppelganger(
        "single tx diff plot",
        single_tx_diff_plot
    )
})

testthat::test_that("to_diff() works correctly for multiple transcripts", {
    test_diffs <- to_diff(
        exons = multi_tx,
        ref_exons = mane,
        group_var = "transcript_name"
    )
    multi_tx_diff_plot <- mane %>%
        dplyr::bind_rows(multi_tx) %>%
        ggplot2::ggplot(
            aes(
                xstart = start,
                xend = end,
                y = transcript_name
            )
        ) +
        geom_range() +
        geom_range(
            data = test_diffs,
            aes(fill = diff_type),
            alpha = 0.2,
        )

    vdiffr::expect_doppelganger(
        "multi tx diff plot",
        multi_tx_diff_plot
    )
})

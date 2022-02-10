test_exons <-
    dplyr::tibble(
        seqnames = "1",
        start = c(100, 300, 500, 650),
        end = start + 100,
        strand = "+",
        tx = c("A", "A", "B", "B")
    )

gba_ens_105_exons <- gba_ens_105 %>% dplyr::filter(type == "exon")
gba_ens_105_introns <- gba_ens_105_exons %>%
    to_intron("transcript_name")

##### .get_gaps #####

# need to create gaps globally for downstream tests
gba_ens_105_intron_gaps <- .get_gaps(GenomicRanges::GRanges(gba_ens_105_exons))
test_intron_gaps <- .get_gaps(GenomicRanges::GRanges(test_exons))

test_.get_gaps <- function(exons, intron_gaps) {

    # intron_gaps should not overlap any exons
    exons_gap_hits <- GenomicRanges::findOverlaps(
        GenomicRanges::GRanges(exons),
        intron_gaps
    )

    overlap_exons <- length(exons_gap_hits) == 0

    return(overlap_exons)
}

testthat::test_that(".get_gaps() works correctly", {
    expect_true(test_.get_gaps(
        gba_ens_105_exons, gba_ens_105_intron_gaps
    ))
    expect_true(test_.get_gaps(
        test_exons, test_intron_gaps
    ))
})

##### .get_tx_start_gaps #####

gba_ens_105_tx_start_gaps <-
    .get_tx_start_gaps(gba_ens_105_exons, "transcript_name")

test_exons_tx_start_gaps <-
    .get_tx_start_gaps(test_exons, NULL)

test_.get_tx_start_gaps <- function(exons, tx_start_gaps, group_var) {
    unique_start <- length(unique(tx_start_gaps[["start"]])) == 1
    correct_start <- all(tx_start_gaps[["start"]] == min(exons[["start"]]))
    correct_end <- exons %>%
        dplyr::group_by_at(.vars = group_var) %>%
        dplyr::summarise(tx_start = min(start))
    correct_end <- all(tx_start_gaps[["end"]] == correct_end[["tx_start"]])

    correct_all <- all(
        unique_start, correct_start, correct_end
    )

    return(correct_all)
}

testthat::test_that(".get_tx_start_gaps() works correctly", {
    expect_true(test_.get_tx_start_gaps(
        gba_ens_105_exons,
        gba_ens_105_tx_start_gaps,
        "transcript_name"
    ))
    expect_true(test_.get_tx_start_gaps(
        test_exons,
        test_exons_tx_start_gaps,
        NULL
    ))
})

##### .check_len_1_strand_seqnames #####

testthat::test_that(
    ".check_len_1_strand_seqnames() catches user input errors",
    {
        expect_error(
            .check_len_1_strand_seqnames(1:2, 1),
            "seqnames of object contains more than 1 unique value"
        )
        expect_error(
            .check_len_1_strand_seqnames(1, 1:2),
            "strand of object contains more than 1 unique value"
        )
    }
)

##### .check_target_gap_width #####

testthat::test_that(".check_target_gap_width() catches user input errors", {
    expect_warning(
        .check_target_gap_width(100),
        "target_gap_width must be an integer, coercing"
    )
})


##### shorten_gaps #####

gba_ens_105_rescaled_tx <- shorten_gaps(
    gba_ens_105_exons,
    gba_ens_105_introns,
    group_var = "transcript_name",
    target_gap_width = 100L
)

gba_ens_105_exons_1_tx <- gba_ens_105_exons %>%
    dplyr::filter(transcript_name == "GBA-202")
gba_ens_105_introns_1_tx <- gba_ens_105_introns %>%
    dplyr::filter(transcript_name == "GBA-202")

gba_ens_105_rescaled_1_tx <- shorten_gaps(
    gba_ens_105_exons_1_tx,
    gba_ens_105_introns_1_tx,
    group_var = "transcript_name",
    target_gap_width = 100L
)

gba_ens_105_rescaled_1_tx_no_group <- shorten_gaps(
    gba_ens_105_exons_1_tx,
    gba_ens_105_introns_1_tx,
    group_var = NULL,
    target_gap_width = 100L
)

test_rescaled_tx <- shorten_gaps(
    test_exons,
    to_intron(test_exons, "tx"),
    group_var = "tx",
    target_gap_width = 50L
)

test_shorten_gaps <- function(exons, rescaled_tx) {

    # should never shorten exons
    exon_widths_before <- exons[["end"]] - exons[["start"]]
    exon_widths_after <- rescaled_tx %>%
        dplyr::filter(type == "exon") %>%
        dplyr::mutate(width = end - start) %>%
        .[["width"]]


    unchanged_exon_widths <- all.equal(
        sort(exon_widths_before), sort(exon_widths_after)
    )

    return(unchanged_exon_widths)
}

testthat::test_that("shorten_gaps() never modifies exons", {
    expect_true(test_shorten_gaps(
        gba_ens_105_exons,
        gba_ens_105_rescaled_tx
    ))
    expect_true(test_shorten_gaps(
        gba_ens_105_exons_1_tx,
        gba_ens_105_rescaled_1_tx
    ))
    expect_true(test_shorten_gaps(
        gba_ens_105_exons_1_tx,
        gba_ens_105_rescaled_1_tx_no_group
    ))
    expect_true(test_shorten_gaps(
        test_exons,
        test_rescaled_tx
    ))
})

# add labels helps manual checking
plot_rescaled_tx <- function(exons, rescaled_tx, group_var, add_labels = FALSE) {
    before_rescaling <- exons %>%
        ggplot2::ggplot(ggplot2::aes_string(
            xstart = "start",
            xend = "end",
            y = group_var
        )) +
        geom_range() +
        geom_intron(
            data = to_intron(exons, group_var),
            strand = "-",
            arrow.min.intron.length = 500
        )

    after_rescaling <- rescaled_tx %>%
        dplyr::filter(type == "exon") %>%
        ggplot2::ggplot(ggplot2::aes_string(
            xstart = "start",
            xend = "end",
            y = group_var
        )) +
        geom_range() +
        geom_intron(
            data = rescaled_tx %>%
                dplyr::filter(type == "intron"),
            strand = "-",
            arrow.min.intron.length = 500
        )

    before_after_list <- list(before_rescaling, after_rescaling)

    if (add_labels) {
        for (i in seq_len(length(before_after_list))) {
            before_after_list[[i]] <- before_after_list[[i]] +
                ggrepel::geom_label_repel(
                    ggplot2::aes_string(x = "end", y = group_var, label = "end"),
                    size = 2,
                    min.segment.length = 0
                )
        }
    }

    before_after_plot <- ggpubr::ggarrange(plotlist = before_after_list, nrow = 2)

    return(before_after_plot)
}

testthat::test_that(
    "shorten_gaps works correctly",
    {
        test_rescaled_plot <- plot_rescaled_tx(test_exons, test_rescaled_tx, "tx")
        gba_rescaled_plot <- plot_rescaled_tx(
            gba_ens_105_exons, gba_ens_105_rescaled_tx, "transcript_name"
        )
        gba_rescaled_plot_1_tx <- plot_rescaled_tx(
            gba_ens_105_exons_1_tx, gba_ens_105_rescaled_1_tx, "transcript_name"
        )
        # make sure everything works okay even if group is set to NULL
        gba_rescaled_plot_1_tx_no_group <- plot_rescaled_tx(
            gba_ens_105_exons_1_tx,
            gba_ens_105_rescaled_1_tx_no_group,
            "transcript_name"
        )

        vdiffr::expect_doppelganger(
            "test exons rescaled plot",
            test_rescaled_plot
        )
        vdiffr::expect_doppelganger(
            "gba rescaled plot",
            gba_rescaled_plot
        )
        vdiffr::expect_doppelganger(
            "gba rescaled plot 1 tx",
            gba_rescaled_plot_1_tx
        )
    }
)

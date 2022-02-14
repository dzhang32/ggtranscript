#' Add exon number
#'
#' `add_exon_number` is a helper function that adds the exon number, i.e. the
#' order the exons are transcribed within a transcript, as a column in the
#' `exons`. Importantly, a "strand" column must be present within `exons`
#' in order to differentiate whether exon number should be calculated according
#' to ascending ("+") or descending ("-") genomic co-ordinates. A transcript
#' with an ambiguous strand i.e. "*" will be assumed to be "+".
#'
#' @inheritParams to_diff
#'
#' @export
#' @examples
#' library(magrittr)
#'
#' gba_ens_105_exons <- gba_ens_105 %>%
#'     dplyr::filter(
#'         type == "exon",
#'         transcript_name %in% paste0("GBA-20", 2:8)
#'     )
#'
#' gba_ens_105_exons
#'
#' gba_ens_105_exons %>%
#'     add_exon_number(group_var = "transcript_name")
#'
#' # this can be useful to label exons with their order/number
#' base <- gba_ens_105_exons %>%
#'     add_exon_number(group_var = "transcript_name") %>%
#'     ggplot2::ggplot(
#'         ggplot2::aes(
#'             xstart = start,
#'             xend = end,
#'             y = transcript_name
#'         )
#'     ) +
#'     geom_range() +
#'     geom_intron(
#'         data = to_intron(gba_ens_105_exons, "transcript_name"),
#'         strand = "-",
#'         arrow.min.intron.length = 500
#'     )
#'
#' base +
#'     ggplot2::geom_text(ggplot2::aes(
#'         x = (start + end) / 2, # plot label at midpoint of exon
#'         label = exon_number
#'     ),
#'     size = 1.5
#'     )
#'
#' # for complex transcript structures or small exons, it can be useful to
#' # set nudge_y to plot exon numbers above their respective exons
#' base +
#'     ggplot2::geom_text(ggplot2::aes(
#'         x = (start + end) / 2, # plot label at midpoint of exon
#'         label = exon_number
#'     ),
#'     size = 2.5,
#'     nudge_y = 0.4
#'     )
#'
#' # or use ggrepel::geom_label_repel to separate labels from exons
#' base +
#'     ggrepel::geom_label_repel(ggplot2::aes(
#'         x = (start + end) / 2,
#'         label = exon_number
#'     ),
#'     size = 2,
#'     min.segment.length = 0
#'     )
add_exon_number <- function(exons, group_var = NULL) {
    .check_coord_object(exons, check_strand = TRUE)
    .check_group_var(exons, group_var)

    if (!is.null(group_var)) {
        exons <- exons %>% dplyr::group_by_at(.vars = group_var)
    }

    # arrange to make sure order reflects genomic position
    exons <- exons %>%
        dplyr::arrange_at(c(.vars = c(group_var, "start", "end")))

    # add exon number, assuming all plus strand at start
    exons <- exons %>%
        dplyr::mutate(
            exon_number = dplyr::row_number(),
            n_exons = dplyr::n()
        ) %>%
        dplyr::ungroup()

    # convert exon number for minus strand
    exons <- exons %>%
        dplyr::mutate(
            exon_number = ifelse(
                strand == "-",
                n_exons - exon_number + 1,
                exon_number
            )
        ) %>%
        dplyr::select(-n_exons)

    return(exons)
}

#' Add exon number
#'
#' `add_exon_number()` adds the exon number (the order the exons are transcribed
#' within each transcript) as a column in `exons`. This can be useful when
#' visualizing long, complex transcript structures, in order to keep track of
#' specific exons of interest.
#'
#' To note, a "strand" column must be present within `exons`. The strand is used
#' to differentiate whether exon numbers should be calculated according to
#' ascending ("+") or descending ("-") genomic co-ordinates. For ambiguous
#' strands ("*"), `add_exon_number()` will be assumed the strand be "+".
#'
#' @inheritParams to_diff
#'
#' @return `data.frame()` equivalent to input `exons`, with the additional
#'   column "exon_number".
#'
#' @export
#' @examples
#' library(magrittr)
#' library(ggplot2)
#'
#' # to illustrate the package's functionality
#' # ggtranscript includes example transcript annotation
#' sod1_annotation %>% head()
#'
#' # extract exons
#' sod1_exons <- sod1_annotation %>% dplyr::filter(type == "exon")
#' sod1_exons %>% head()
#'
#' # add the exon number for each transcript
#' sod1_exons <- sod1_exons %>% add_exon_number(group_var = "transcript_name")
#'
#' base <- sod1_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_intron(
#'         data = to_intron(sod1_exons, "transcript_name"),
#'         strand = "+"
#'     )
#'
#' # it can be useful to annotate exons with their exon number
#' # using ggplot2::geom_text()
#' base +
#'     geom_text(aes(
#'         x = (start + end) / 2, # plot label at midpoint of exon
#'         label = exon_number
#'     ),
#'     size = 3.5,
#'     nudge_y = 0.4
#'     )
#'
#' # Or alternatively, using ggrepel::geom_label_repel()
#' # to separate labels from exons
#' base +
#'     ggrepel::geom_label_repel(ggplot2::aes(
#'         x = (start + end) / 2,
#'         label = exon_number
#'     ),
#'     size = 3.5,
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

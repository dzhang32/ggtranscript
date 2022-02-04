#' @noRd
geom_intron <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        arrow = grid::arrow(ends = "last", length = grid::unit(0.1, "inches")),
                        arrow.fill = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = FALSE,
                        strand = "+",
                        arrow.min.intron.length = 0,
                        show.legend = NA,
                        inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomIntron,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            arrow.fill = arrow.fill,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            strand = strand,
            arrow.min.intron.length = arrow.min.intron.length,
            ...
        )
    )
}

#' @noRd
GeomIntron <- ggplot2::ggproto("GeomIntron", ggplot2::GeomSegment,
    required_aes = c("xstart", "xend", "y"),
    default_aes = ggplot2::aes(
        colour = "black", size = 0.5, linetype = 1, alpha = NA
    ),
    setup_params = function(data, params) {
        # check that strand is scalar and one of "+" or "-"
        strand_len_1 <- length(params$strand) != 1
        strand_any_na <- any(is.na(params$strand))
        strand_chr <- !is.character(params$strand)
        strand_plus_minus <- !(all(params$strand %in% c("+", "-")))

        if (strand_len_1 | strand_any_na | strand_chr | strand_plus_minus) {
            stop("strand values must be one of '+' and '-'")
        }

        # check that arrow.min.intron.length numeric is >= 0
        arrow.min_numeric <- is.numeric(params$arrow.min.intron.length)
        arrow.min_neg <- params$arrow.min.intron.length < 0

        if (!arrow.min_numeric | arrow.min_neg) {
            stop("arrow.min.intron.length must be a numeric > 0")
        }

        params
    },
    setup_data = function(data, params) {
        # needed to permit usage of xstart/xend
        transform(
            data,
            x = xstart,
            xend = xend,
            y = y,
            yend = y,
            xstart = NULL
        )
    },
    draw_panel = function(data,
                          panel_params,
                          coord,
                          arrow = NULL,
                          arrow.fill = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          na.rm = FALSE,
                          strand = "+",
                          arrow.min.intron.length = 0) {

        # first, create the intron grob, which is just a pure line (no arrow)
        intron_grob <- ggplot2::GeomSegment$draw_panel(
            data = data,
            panel_params = panel_params,
            coord = coord,
            arrow = NULL,
            arrow.fill = NULL,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm
        )

        # then, create the arrow grobs
        # this involves making a line that ends in the middle of the intron
        # as the arrow can only be placed at either end of a geom_segment/path
        # the strand changes the x/xends around, shifting the arround direction
        if (strand == "+") {
            arrow_data <- transform(
                data,
                xend = (x + xend) / 2
            )
        } else {
            arrow_data <- transform(
                data,
                mid = (x + xend) / 2,
                x = xend
            )
            arrow_data <- transform(
                arrow_data,
                xend = mid
            )
        }

        # only plot arrows for intron above arrow.min.intron.length
        ab_min <- abs(arrow_data$x - arrow_data$xend) > arrow.min.intron.length
        arrow_data <- arrow_data[ab_min, ]

        # if there are no arrows to plot, use a nullGrob() to add nothing
        if (nrow(arrow_data) > 0) {
            arrow_grob <- ggplot2::GeomSegment$draw_panel(
                data = arrow_data,
                panel_params = panel_params,
                coord = coord,
                arrow = arrow,
                arrow.fill = arrow.fill,
                lineend = lineend,
                linejoin = linejoin,
                na.rm = na.rm
            )
        } else {
            arrow_grob <- grid::nullGrob()
        }

        # draw_panel expects return of a grob
        # here, as we build multiple grobs (i.e. intron lines + arrows)
        # we use a grobTree to combine the two
        grid::grobTree(
            intron_grob,
            arrow_grob
        )
    }
)

#' Convert exon co-ordinates to introns
#'
#' Taking a set of exons, `to_intron` will return the corresponding introns.
#' `to_intron` expects inputted exons to either 1. originate from a single
#' transcript or 2. be grouped via `group_var` such that each group corresponds
#' to a single transcript. In other words, `to_intron` assumes that exons (from
#' each group) do not overlap one another.
#'
#' @param x `data.frame` containing exons co-ordinates (and possibly associated
#'   transcript)
#' @param group_var variable name (not in quotes) specifying the column with the
#'   group, most often the transcript id/name.
#' @param start_var variable name (not in quotes) specifying the column with the
#'   start co-ordinate(s).
#' @param end_var variable name (not in quotes) specifying the column with the
#'   end co-ordinate(s).
#'
#' @examples
#'
#' example_exons <-
#'     dplyr::tibble(
#'         start = c(5, 10, 15, 20),
#'         end = c(7, 12, 17, 22),
#'         tx = c("A", "A", "B", "B")
#'     )
#'
#' to_intron(example_exons, tx)
#' @export
to_intron <- function(x, group_var = NULL, start_var = start, end_var = end) {

    # TODO - add functionality to check warn if exons overlap
    # as this should could break the function
    # which expects exons in each group_var to originate from a single tx

    # grouping by NULL (default) does nothing
    x <- x %>%
        dplyr::group_by({{ group_var }})

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    x <- x %>%
        dplyr::arrange({{ start_var }}, {{ end_var }})

    # obtain intron start and ends
    x <- x %>%
        dplyr::mutate(
            intron_start := dplyr::lag({{ end_var }}) + 1,
            intron_end := {{ start_var }} - 1
        ) %>%
        dplyr::select(-{{ start_var }}, -{{ end_var }})

    # for each group, output N of introns should be N - 1 the inputted exons
    # remove the introduced artifact NAs
    x <- x %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end))

    return(x)
}

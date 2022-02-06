#' Plot intron lines with strand arrows
#'
#' `geom_intron()` draws a horizontal line specifying introns between two points
#' (xstart, xend) for each e.g. transcript (y). The `strand` option (one of "+"
#' or "-") enables arrows to be plotted at the centre of the introns to indicate
#' the direction of transcription.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_segment
#' @param arrow.min.intron.length `integer` the minimum required length of an
#'   intron for an arrow to be drawn. Depending on the length of the overall
#'   transcript, this can be useful to remove cluttering of arrows on short
#'   introns.
#'
#' @export
#' @examples
#'
#' example_introns <-
#'     dplyr::tibble(
#'         strand = c("+", "-"),
#'         tx = c("A", "B"),
#'         start = c(201, 601),
#'         end = c(299, 649),
#'         type = "intron"
#'     )
#'
#' example_introns
#'
#' base <-
#'     ggplot2::ggplot(
#'         example_introns,
#'         ggplot2::aes(
#'             xstart = start,
#'             xend = end,
#'             y = tx
#'         )
#'     )
#'
#' base + geom_intron()
#' base + geom_intron(strand = "-")
#'
#' # strand can also be mapped as an aesthetic
#' base + geom_intron(ggplot2::aes(strand = strand))
#'
#' base + geom_intron(ggplot2::aes(colour = tx, strand = strand))
#' base + geom_intron(ggplot2::aes(
#'     colour = tx,
#'     strand = strand
#' ),
#' arrow.min.intron.length = 50
#' )
geom_intron <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        arrow = grid::arrow(ends = "last", length = grid::unit(0.1, "inches")),
                        arrow.fill = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = FALSE,
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
            arrow.min.intron.length = arrow.min.intron.length,
            ...
        )
    )
}

#' `GeomIntron` is pretty much `ggplot2::GeomSegment` with the `required_aes`
#' changed to `xstart`/`xend` to match genetic nomenclature and the added arrows
#' to indicate direction of transcription (configured with `strand` and
#' `arrow.min.intron.length`)
#' @noRd
GeomIntron <- ggplot2::ggproto("GeomIntron", ggplot2::GeomSegment,
    required_aes = c("xstart", "xend", "y"),
    default_aes = aes(
        colour = "black",
        size = 0.5,
        linetype = 1,
        alpha = NA,
        strand = "+"
    ),
    setup_params = function(data, params) {
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
                          arrow.min.intron.length = 0) {

        # check that strand is scalar and one of "+" or "-"
        .check_strand(data$strand)

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

        # then, create the arrow grobs, one per strand
        # need both as the direction of arrow (as far I can tell) is
        # is dependent on the orientation of the x/xend
        strand_arrow_plus_grob <- .create_strand_arrow_grob(
            target_strand = "+",
            arrow.min.intron.length = arrow.min.intron.length,
            data = data,
            panel_params = panel_params,
            coord = coord,
            arrow = arrow,
            arrow.fill = arrow.fill,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm
        )

        strand_arrow_minus_grob <- .create_strand_arrow_grob(
            target_strand = "-",
            arrow.min.intron.length = arrow.min.intron.length,
            data = data,
            panel_params = panel_params,
            coord = coord,
            arrow = arrow,
            arrow.fill = arrow.fill,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm
        )

        # draw_panel expects return of a grob
        # here, as we build multiple grobs (i.e. intron lines + arrows)
        # we use a grobTree to combine the two
        grid::grobTree(
            intron_grob,
            strand_arrow_plus_grob,
            strand_arrow_minus_grob
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
#' @param group_var `character` specifying the column with the
#'   group, most often the transcript id/name.
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
#' example_exons
#'
#' to_intron(example_exons, group_var = "tx")
#' @export
to_intron <- function(x, group_var = NULL) {
    .check_coord_object(x)
    .check_group_var(x, group_var)

    # TODO - add functionality to check warn if exons overlap
    # as this should could break the function
    # which expects exons in each group_var to originate from a single tx

    if (!is.null(group_var)) {
        x <- x %>% dplyr::group_by_at(.vars = group_var)
    }

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    x <- x %>%
        dplyr::arrange(start, end)

    # obtain intron start and ends
    x <- x %>%
        dplyr::mutate(
            intron_start := dplyr::lag(end) + 1,
            intron_end := start - 1,
            type = "intron"
        ) %>%
        dplyr::select(-start, -end)

    # for each group, output N of introns should be N - 1 the inputted exons
    # remove the introduced artifact NAs
    # rename intron_start/intron_end to
    x <- x %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end)) %>%
        dplyr::rename(start = intron_start, end = intron_end)

    return(x)
}

#' @keywords internal
#' @noRd
.check_strand <- function(strand) {
    any_na <- any(is.na(strand))
    plus_minus <- !(all(strand %in% c("+", "-")))

    if (any_na | plus_minus) {
        stop("strand values must be one of '+' and '-'")
    }

    return(invisible())
}

#' @keywords internal
#' @noRd
.create_strand_arrow_grob <- function(target_strand,
                                      arrow.min.intron.length,
                                      data,
                                      panel_params,
                                      coord,
                                      arrow,
                                      arrow.fill,
                                      lineend,
                                      linejoin,
                                      na.rm) {

    # filter for introns that match target strand
    # and have a length above arrow.min.intron.length
    match_strand <- data$strand == target_strand
    ab_min <- abs(data$x - data$xend) > arrow.min.intron.length
    arrow_data <- data[match_strand & ab_min, ]

    # if there are no arrows to plot, use a nullGrob() to add nothing
    if (nrow(arrow_data) == 0) {
        arrow_grob <- grid::nullGrob()
    } else {

        # obtain the the correct orientation of arrow (dependent on strand)
        # as the arrow can only be placed at either end of a geom_segment/path
        # the strand changes the x/xends around, shifting the around direction
        if (target_strand == "+") {
            arrow_data <- transform(
                arrow_data,
                xend = (x + xend) / 2
            )
        } else {
            arrow_data <- transform(
                arrow_data,
                mid = (x + xend) / 2,
                x = xend
            )
            arrow_data <- transform(
                arrow_data,
                xend = mid
            )
        }

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
    }

    return(arrow_grob)
}

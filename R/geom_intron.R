#' Plot intron lines with strand arrows
#'
#' `geom_intron()` draws horizontal lines with central arrows that are designed
#' to represent introns. In combination with `geom_range()`/`geom_half_range()`,
#' these geoms form the core components for visualizing transcript
#' annotation.
#'
#' `geom_intron()`  requires the following `aes()`; `xstart`, `xend` and `y`
#' (e.g. transcript name). The `strand` option (one of "+" or "-") adjusts the
#' arrow direction to match the direction of transcription. the
#' `arrow.min.intron.length` parameter can be useful to remove strand arrows
#' overlapping exons, which can be a problem if plotted introns include those
#' that are relatively short.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_segment
#' @param arrow.min.intron.length `integer()` the minimum required width of an
#'   intron for a strand arrow to be drawn. This can be useful to remove strand
#'   arrows on short introns that overlap adjacent exons.
#'
#' @export
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' # to illustrate the package's functionality
#' # ggtranscript includes example transcript annotation
#' pknox1_annotation
#'
#' # extract exons
#' pknox1_exons <- pknox1_annotation %>% dplyr::filter(type == "exon")
#' pknox1_exons
#'
#' # to_intron() is a helper function included in ggtranscript
#' # which is useful for converting exon co-ordinates to introns
#' pknox1_introns <- pknox1_exons %>% to_intron(group_var = "transcript_name")
#' pknox1_introns
#'
#' base <- pknox1_introns %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     ))
#'
#' # by default, geom_intron() assumes introns originate from the "+" strand
#' base + geom_intron()
#'
#' # however this can be modified using the strand option
#' base + geom_intron(strand = "-")
#'
#' # strand can also be set as an aes()
#' base + geom_intron(aes(strand = strand))
#'
#' # as a ggplot2 extension, ggtranscript geoms inherit the
#' # the functionality from the parameters and aesthetics in ggplot2
#' base + geom_intron(
#'     aes(colour = transcript_name),
#'     size = 1
#' )
#'
#' # together, geom_range() and geom_range() are designed to visualize
#' # the core components of transcript annotation
#' pknox1_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_intron(
#'         data = pknox1_introns
#'     )
#'
#' # for short introns, sometimes strand arrows will overlap exons
#' # to avoid this, users can set the arrow.min.intron.length parameter
#' pknox1_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_intron(
#'         data = pknox1_introns,
#'         arrow.min.intron.length = 3500
#'     )
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

#' @keywords internal
#' @noRd
.check_strand <- function(strand) {
    # TODO - add option for "*" arrow?
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

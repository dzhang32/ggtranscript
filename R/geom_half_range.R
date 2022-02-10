#' Plot genomic ranges
#'
#' `geom_range()` draws blocks/tiles with a width specified by their `xstart`
#' and `xend` position. The other required `aes`, `y`, is expected to be a
#' `character` or `factor` (e.g. a transcript id/name). This `geom` is designed
#' to represent genomic annotations that cover a genomic range (e.g. exons,
#' CDS).
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_tile
#' @inheritParams ggplot2::geom_segment
#' @inheritParams grid::rectGrob
#'
#' @export
#' @rdname geom_range
#'
#' @examples
#'
#' example_exons <-
#'     dplyr::tibble(
#'         start = c(100, 300, 500, 650),
#'         end = start + 100,
#'         strand = c("+", "+", "-", "-"),
#'         tx = c("A", "A", "B", "B")
#'     )
#'
#' example_exons
#'
#' base <-
#'     ggplot2::ggplot(
#'         example_exons,
#'         ggplot2::aes(
#'             xstart = start,
#'             xend = end,
#'             y = tx
#'         )
#'     )
#'
#' base + geom_range()
#' base + geom_range(ggplot2::aes(fill = tx))
geom_half_range <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            range.orientation = "bottom",
                            linejoin = "mitre",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomHalfRange,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            range.orientation = range.orientation,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @include geom_range.R
#' `GeomHalfRange` is `GeomRange` with the added parameter `range.orientation`
#' @keywords internal
#' @noRd
GeomHalfRange <- ggplot2::ggproto("GeomHalfRange", GeomRange,
    setup_data = function(data, params) {
        # check that range.orientation is one of possible options
        .check_range.orientation(params)

        # modified from ggplot2::GeomTile
        data$height <- data$height %||% params$height %||% 0.25

        transform(
            data,
            xmin = xstart,
            xmax = xend,
            ymin = y - height / 2,
            ymax = y + height / 2,
            height = NULL
        )
    },
    draw_panel = function(data,
                          panel_params,
                          coord,
                          range.orientation = "bottom",
                          lineend = "butt",
                          linejoin = "mitre") {
        vjust <- ifelse(
            range.orientation == "bottom",
            1.5,
            0.5
        )

        GeomRange$draw_panel(
            data = data,
            panel_params = panel_params,
            coord = coord,
            vjust = vjust,
            lineend = lineend,
            linejoin = linejoin
        )
    }
)

#' @keywords internal
#' @noRd
.check_range.orientation <- function(params) {
    not_orient_option <-
        !(params$range.orientation %in% c("top", "bottom"))

    if (not_orient_option) {
        stop(
            "range.orientation must be one of ",
            "'alternating', 'top' or 'bottom'"
        )
    }
}

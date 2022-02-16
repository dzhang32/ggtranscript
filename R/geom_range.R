#' Plot genomic ranges
#'
#' `geom_range()` and `geom_half_range()` draw tiles that are designed to
#' represent range-based genomic features, such as exons. In combination with
#' `geom_intron()`, these geoms form the core components for visualizing
#' transcript annotation.
#'
#' `geom_range()` and `geom_half_range()` require the following `aes()`;
#' `xstart`, `xend` and `y` (e.g. transcript name). `geom_half_range()` takes
#' advantage of the vertical symmetry of transcript annotation by plotting only
#' half of a range on the top or bottom of a transcript structure. This can be
#' useful to free up plotting space for other transcript annotations (e.g.
#' `geom_junction()`).
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_tile
#' @inheritParams ggplot2::geom_segment
#' @inheritParams grid::rectGrob
#'
#' @export
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
#'
#' base + geom_half_range()
#' base + geom_half_range(range.orientation = "top")
geom_range <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       vjust = NULL,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRange,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            vjust = vjust,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' `GeomRange` is `ggplot2::GeomTile` with modified `aes` to match genetic
#' nomenclature (`xstart`/`xend`)
#' @keywords internal
#' @noRd
GeomRange <- ggplot2::ggproto("GeomRange", ggplot2::GeomTile,
    required_aes = c("xstart", "xend", "y"),
    default_aes = aes(
        fill = "grey",
        colour = "black",
        size = 0.25,
        linetype = 1,
        alpha = NA,
        height = NA
    ),
    setup_data = function(data, params) {
        # modified from ggplot2::GeomTile
        data$height <- data$height %||% params$height %||% 0.5

        transform(
            data,
            xmin = xstart,
            xmax = xend,
            ymin = y - height / 2,
            ymax = y + height / 2,
            height = NULL
        )
    },
    draw_panel = function(self,
                          data,
                          panel_params,
                          coord,
                          vjust = NULL,
                          lineend = "butt",
                          linejoin = "mitre") {
        if (!coord$is_linear()) {
            # prefer to match geom_curve and warn
            # rather than copy the implementation from GeomRect for simplicity
            # also don'think geom_range would be used for non-linear coords
            warn("geom_ is not implemented for non-linear coordinates")
        }

        coords <- coord$transform(data, panel_params)
        grid::rectGrob(
            coords$xmin, coords$ymax,
            width = coords$xmax - coords$xmin,
            height = coords$ymax - coords$ymin,
            default.units = "native",
            just = c("left", "top"),
            vjust = vjust,
            gp = grid::gpar(
                col = coords$colour,
                fill = ggplot2::alpha(coords$fill, coords$alpha),
                lwd = coords$size * ggplot2::.pt,
                lty = coords$linetype,
                linejoin = linejoin,
                lineend = lineend
            )
        )
    }
)

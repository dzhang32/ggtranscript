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
#' base + geom_range(ggplot2::aes(colour = tx))
geom_range <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
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
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @noRd
GeomRange <- ggplot2::ggproto("GeomRange", ggplot2::GeomTile,
    setup_data = function(data, params) {
        # i think, height has to be created here (rather than in default_aes)
        # as default_aes takes effect AFTER setup_data
        # alternatively, I think we could move the data processing to draw_panel
        # but this would be more complex
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
    default_aes = aes(
        fill = "grey",
        colour = "black",
        size = 0.25,
        linetype = 1,
        alpha = NA,
        height = NA
    ),
    required_aes = c("xstart", "xend", "y")
)

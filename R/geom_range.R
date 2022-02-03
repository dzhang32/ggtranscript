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
        data$height <- data$height %||% params$height %||% 0.5

        transform(data,
            xmin = x_start,
            xmax = x_end,
            ymin = y - height / 2,
            ymax = y + height / 2,
            height = NULL
        )
    },
    default_aes = ggplot2::aes(
        fill = "grey20", colour = NA,
        size = 0.1, linetype = 1,
        alpha = NA, height = NA
    ),
    required_aes = c("x_start", "x_end", "y")
)

#' @noRd
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

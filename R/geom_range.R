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
GeomRange <- ggplot2::ggproto("GeomRange", ggplot2::GeomRect,

    # TODO - check whether this is appropriate
    extra_params = c("na.rm"),
    setup_data = function(data, params) {
        browser()

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
    required_aes = c("x_start", "x_end", "y"),

    # Blindly copied from GeomTile, justification:
    # https://github.com/tidyverse/ggplot2/blob/c89c265a57fd71f8a0288ce81037296aadc0a012/R/geom-tile.r#L116-L118
    non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
    draw_key = ggplot2::draw_key_polygon
)

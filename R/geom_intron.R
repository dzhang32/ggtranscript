geom_intron <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        arrow = NULL,
                        arrow.fill = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = FALSE,
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
            ...
        )
    )
}

GeomIntron <- ggplot2::ggproto("GeomIntron", ggplot2::GeomSegment,
    required_aes = c("x_start", "x_end", "y"),
    setup_data = function(data, params) {
        transform(data,
            x = x_start,
            xend = x_end,
            y = y,
            yend = y,
            x_start = NULL,
            x_end = NULL
        )
    },
    draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                          lineend = "butt", linejoin = "round", na.rm = FALSE) {
        browser()

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

        arrow_data <- transform(
            data,
            xend = (xend + x) / 2
        )

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

        grid::grobTree(
            intron_grob,
            arrow_grob
        )
    }
)

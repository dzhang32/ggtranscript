geom_intron <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
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
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

GeomIntron <- ggplot2::ggproto("GeomIntron", ggplot2::GeomSegment,
    required_aes = c("x_start", "x_end", "y"),
    default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, strand = "+"),
    setup_params = function(data, params) {
        strand_any_na <- any(is.na(data$strand))
        strand_chr <- !is.character(data$strand)
        strand_plus_minus <- !all(data$strand %in% c("+", "-"))

        if (strand_any_na | strand_chr | strand_plus_minus) {
            stop("strand values must be one of '+' and '-'")
        }
        params
    },
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
    draw_panel = function(data, panel_params, coord, lineend = "butt", linejoin = "round", na.rm = FALSE) {
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
            arrow = grid::arrow(ends = "last", length = grid::unit(0.1, "inches")),
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

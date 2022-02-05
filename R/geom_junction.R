#' @noRd
geom_junction <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          curvature = -0.5,
                          angle = 90,
                          ncp = 5,
                          arrow = NULL,
                          arrow.fill = NULL,
                          lineend = "butt",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomJunction,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            arrow.fill = arrow.fill,
            curvature = curvature,
            angle = angle,
            ncp = ncp,
            lineend = lineend,
            na.rm = na.rm,
            ...
        )
    )
}

#' @noRd
GeomJunction <- ggplot2::ggproto("GeomJunction", ggplot2::GeomCurve,
    required_aes = c("xstart", "xend", "y"),
    setup_data = function(data, params) {
        # needed to permit usage of xstart/xend
        transform(
            data,
            x = xstart,
            xend = xend,
            y = y,
            yend = y,
            xstart = NULL
        )
    }
)

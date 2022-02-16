#' @param range.orientation `character()` one of "top" or "bottom", specifying
#'   where the half ranges will be plotted with respect to each transcript
#'   (`y`).
#'
#' @export
#' @rdname geom_range
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

#' `GeomHalfRange` is `GeomRange` with default parameters for `vjust` and
#' `height` as well as the added parameter `range.orientation`
#'
#' @include geom_range.R
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

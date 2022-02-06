#' @noRd
geom_junction <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          junction.orientation = "alternating",
                          curvature = 0.5,
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
            junction.orientation = junction.orientation,
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
        # check that junction.orientation is length 1 and one of possible options
        .check_junction.orientation(params)

        if (params$curvature < 0) {
            warning("Setting curvature of < 0 will flip junction.orientation")
        }

        # needed to permit usage of xstart/xend
        transform(
            data,
            x = xstart,
            yend = y,
            xstart = NULL
        )
    },
    draw_panel = function(data,
                          panel_params,
                          coord,
                          junction.orientation = "alternating",
                          curvature = 0.5,
                          angle = 90,
                          ncp = 5,
                          arrow = NULL,
                          arrow.fill = NULL,
                          lineend = "butt",
                          na.rm = FALSE) {
        if (junction.orientation == "alternating") {

            # to create alternating top/bottom junctions
            # we need to split the data
            # and plot the alternating indexes with curvature -0.5/0.5
            # we group_by y (e.g. tx) to ensure the alternating junctions
            # occurs within the y groups
            data <- data %>%
                dplyr::group_by(y) %>%
                dplyr::mutate(
                    odd_index = as.logical(dplyr::row_number() %% 2)
                ) %>%
                dplyr::ungroup()

            even_index_data <- data[!data$odd_index, ]
            odd_index_data <- data[data$odd_index, ]

            # if there's 1 junction, even_index_data will have 0 rows
            # .create_junction_grob will return nullGrob in that case
            even_bottom_junction_grob <- .create_junction_grob(
                data = even_index_data,
                panel_params = panel_params,
                coord = coord,
                curvature = curvature,
                angle = angle,
                ncp = ncp,
                arrow = arrow,
                arrow.fill = arrow.fill,
                lineend = lineend,
                na.rm = na.rm
            )

            odd_top_junction_grob <- .create_junction_grob(
                data = odd_index_data,
                panel_params = panel_params,
                coord = coord,
                curvature = -curvature,
                angle = angle,
                ncp = ncp,
                arrow = arrow,
                arrow.fill = arrow.fill,
                lineend = lineend,
                na.rm = na.rm
            )

            grid::grobTree(
                even_bottom_junction_grob,
                odd_top_junction_grob
            )
        } else {
            # if "top" we need to flip the orientation to -0.5
            curvature <- ifelse(
                junction.orientation == "top",
                -curvature,
                curvature
            )

            .create_junction_grob(
                data = data,
                panel_params = panel_params,
                coord = coord,
                curvature = curvature,
                angle = angle,
                ncp = ncp,
                arrow = arrow,
                arrow.fill = arrow.fill,
                lineend = lineend,
                na.rm = na.rm
            )
        }
    }
)

.check_junction.orientation <- function(params) {
    not_orient_option <-
        !(params$junction.orientation %in% c("alternating", "top", "bottom"))

    if (not_orient_option) {
        stop(
            "junction.orientation must be one of ",
            "'alternating', 'top' or 'bottom'"
        )
    }
}

.create_junction_grob <- function(data,
                                  panel_params,
                                  coord,
                                  curvature,
                                  angle,
                                  ncp = 5,
                                  arrow,
                                  arrow.fill,
                                  lineend,
                                  na.rm) {
    if (nrow(data) == 0) {
        return(grid::nullGrob())
    }

    ggplot2::GeomCurve$draw_panel(
        data = data,
        panel_params = panel_params,
        coord = coord,
        curvature = curvature,
        angle = angle,
        ncp = ncp,
        arrow = arrow,
        arrow.fill = arrow.fill,
        lineend = lineend,
        na.rm = na.rm
    )
}

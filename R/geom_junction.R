#' Plot junction curves
#'
#' `geom_junction()` draws curves that are designed to represent junction reads
#' from RNA-sequencing data. The curves can be modified using `curvature`,
#' `angle` and `ncp` parameters. By default, the junctions will alternate
#' between being plotted on the top and bottom of each `y` group, however this
#' can be changed via `junction.orientation`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_segment
#' @inheritParams grid::curveGrob
#' @param junction.orientation `character` one of "alternating", "top" or
#'   "bottom". Specifies where the junctions will be plotted with respect to
#'   each `y` group.
#'
#' @export
#' @examples
#'
#' library(magrittr)
#'
#' example_introns <-
#'     gba_ens_105 %>%
#'     dplyr::filter(type == "exon") %>%
#'     to_intron(group_var = "transcript_name")
#'
#' example_introns
#'
#' base <- example_introns %>%
#'     dplyr::filter(transcript_name == "GBA-202") %>%
#'     ggplot2::ggplot(ggplot2::aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     ))
#'
#' base + geom_junction()
#' base + geom_junction(junction.orientation = "top")
#' base + geom_junction(junction.orientation = "bottom")
#'
#' # for multiple transcripts, sometimes the junctions will overlap
#' base_multi_transcript <- example_introns %>%
#'     ggplot2::ggplot(ggplot2::aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     ))
#'
#' base_multi_transcript + geom_junction()
#'
#' # this can be corrected using the curvature parameter
#' base_multi_transcript + geom_junction(curvature = 0.25)
#'
#' base_multi_transcript + geom_junction(
#'     ggplot2::aes(colour = transcript_name),
#'     curvature = 0.25
#' )
#' @export
#' @rdname geom_path
geom_junction <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          junction.orientation = "alternating",
                          junction.y.max = 1,
                          angle = 90,
                          ncp = 15,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomJunction,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            junction.orientation = junction.orientation,
            junction.y.max = junction.y.max,
            angle = angle,
            ncp = ncp,
            na.rm = na.rm,
            orientation = orientation,
            ...
        )
    )
}

#' @keywords internal
#' @noRd
GeomJunction <- ggplot2::ggproto("GeomJunction", ggplot2::GeomLine,
    required_aes = c("xstart", "xend", "y"),
    setup_data = function(data, params) {
        # check that junction.orientation is length 1 + one of possible options
        .check_junction.orientation(params)
        # check that junction.y.max is length 1 + one of possible options
        .check_junction.y.max(params)


        # we need a unique group id per junction, rather than per transcript
        # similar to spring example from ggplot2 book
        # https://ggplot2-book.org/spring1.html#spring3
        if (is.null(data$group)) {
            data$group <- seq_len(nrow(data))
        }
        if (anyDuplicated(data$group)) {
            data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
        }

        # needed to permit usage of xstart/xend
        transform(
            data,
            x = xstart,
            xstart = NULL
        )
    },
    draw_panel = function(data,
                          panel_params,
                          coord,
                          junction.orientation = "alternating",
                          junction.y.max = 1,
                          angle = 90,
                          ncp = 15) {
        data <- data %>%
            dplyr::group_by(y) %>%
            dplyr::mutate(junction_index = dplyr::row_number()) %>%
            dplyr::ungroup()

        # again, very similar to springs example
        # create the junction points, whilst preserving aes
        # https://ggplot2-book.org/spring1.html#spring3
        cols_to_keep <- setdiff(names(data), c("x", "xend", "y"))
        junctions <- lapply(seq_len(nrow(data)), function(i) {
            junction_curve <- .get_junction_curve(
                data$x[i], data$xend[i], data$y[i],
                angle, ncp
            )
            cbind(junction_curve, unclass(data[i, cols_to_keep]))
        })

        junctions <- do.call(rbind, junctions)
        junctions <- .get_normalised_curve(junctions, junction.orientation, junction.y.max)

        ggplot2::GeomLine$draw_panel(junctions, panel_params, coord)
    }
)

#' @keywords internal
#' @noRd
.get_junction_curve <- function(x, xend, y, angle, ncp) {
    curve_points <- grid:::calcControlPoints(
        x1 = x, x2 = xend,
        y1 = y, y2 = y,
        angle = angle,
        curvature = -0.5,
        ncp = ncp
    )

    junction_curve <- data.frame(
        x_points = c(x, curve_points$x, xend),
        y_points = c(y, curve_points$y, y),
        y_orig = y
    ) %>%
        dplyr::rename(
            x = x_points,
            y = y_points
        )

    return(junction_curve)
}

#' @keywords internal
#' @noRd
.get_normalised_curve <- function(junctions,
                                  junction.orientation,
                                  junction.y.max) {
    sf <- 1 / junction.y.max

    if (junction.orientation == "top") {
        junctions <- junctions %>% dplyr::mutate(
            y = ifelse(y == y_orig, y, y_orig + (y / max(y)) / sf)
        )
    } else if (junction.orientation == "bottom") {
        junctions <- junctions %>% dplyr::mutate(
            y = ifelse(y == y_orig, y, y_orig - (y / max(y)) / sf)
        )
    } else if (junction.orientation == "alternating") {
        junctions <- junctions %>% dplyr::mutate(y = dplyr::case_when(
            y == y_orig ~ y,
            junction_index %% 2 == 0 ~ y_orig - (y / max(y) / sf),
            junction_index %% 2 == 1 ~ y_orig + (y / max(y) / sf)
        ))
    }

    return(junctions)
}

#' @keywords internal
#' @noRd
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

#' @keywords internal
#' @noRd
.check_junction.y.max <- function(params) {
    if (length(params$junction.y.max) != 1) {
        stop(
            "junction.y.max must have a length of 1"
        )
    }
    if (!is.numeric(params$junction.y.max)) {
        stop(
            "junction.y.max must be a numeric value (integer/double)"
        )
    }
}

#' Label junction curves
#'
#' `geom_junction_label_repel()` labels junction curves at their midpoint using
#' `ggrepel::geom_label_repel()`. This can be useful to label and compare
#' junctions (plotted using `geom_junction()`) with metrics of their usage (e.g.
#' count or percent-spliced-in).
#'
#' `geom_junction_label_repel()` requires the following `aes()`; `xstart`,
#' `xend`, `y` (e.g. transcript name) and `label`. Under the hood,
#' `geom_junction_label_repel()` generates the same junction curves as
#' `geom_junction()` to obtain curve midpoints for labeling. Therefore, it is
#' important that users use the same input data and parameters that alter
#' junction curves (namely `junction.orientation`, `junction.y.max`, `angle`,
#' `ncp`) for `geom_junction_label_repel()` that they have used for
#' `geom_junction()`.
#'
#' @inheritParams ggrepel::geom_text_repel
#' @inheritParams grid::curveGrob
#' @inheritParams geom_junction
#'
#' @export
#' @examples
#' library(magrittr)
#' library(ggplot2)
#'
#' # to illustrate the package's functionality
#' # ggtranscript includes example transcript annotation
#' sod1_annotation %>% head()
#'
#' # as well as a set of example (unannotated) junctions
#' # originating from GTEx and downloaded via the Bioconductor package snapcount
#' sod1_junctions
#'
#' # extract exons
#' sod1_exons <- sod1_annotation %>% dplyr::filter(
#'     type == "exon",
#'     transcript_name == "SOD1-201"
#' )
#' sod1_exons %>% head()
#'
#' # add transcript_name to junctions for plotting
#' sod1_junctions <- sod1_junctions %>%
#'     dplyr::mutate(transcript_name = "SOD1-201")
#'
#' # geom_junction_label_repel() can be used to label junctions
#' base <- sod1_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range() +
#'     geom_intron(
#'         data = to_intron(sod1_exons, "transcript_name")
#'     )
#'
#' # this can be useful to label junctions with their counts
#' base +
#'     geom_junction(
#'         data = sod1_junctions,
#'         junction.y.max = 0.5
#'     ) +
#'     geom_junction_label_repel(
#'         data = sod1_junctions,
#'         aes(label = round(mean_count, 2)),
#'         junction.y.max = 0.5
#'     )
geom_junction_label_repel <- function(mapping = NULL,
                                      data = NULL,
                                      stat = "identity",
                                      position = "identity",
                                      parse = FALSE,
                                      ...,
                                      junction.orientation = "alternating",
                                      junction.y.max = 1,
                                      angle = 90,
                                      ncp = 15,
                                      box.padding = 0.25,
                                      label.padding = 0.25,
                                      point.padding = 1e-6,
                                      label.r = 0.15,
                                      label.size = 0.25,
                                      min.segment.length = 0,
                                      arrow = NULL,
                                      force = 1,
                                      force_pull = 1,
                                      max.time = 0.5,
                                      max.iter = 10000,
                                      max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                                      nudge_x = 0,
                                      nudge_y = 0,
                                      xlim = c(NA, NA),
                                      ylim = c(NA, NA),
                                      na.rm = FALSE,
                                      show.legend = NA,
                                      direction = c("both", "y", "x"),
                                      seed = NA,
                                      verbose = FALSE,
                                      inherit.aes = TRUE) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
        }
        position <- position_nudge_repel(nudge_x, nudge_y)
    }
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomJunctionLabelRepel,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            parse = parse,
            junction.orientation = junction.orientation,
            junction.y.max = junction.y.max,
            angle = angle,
            ncp = ncp,
            box.padding  = to_unit(box.padding),
            label.padding = to_unit(label.padding),
            point.padding  = to_unit(point.padding),
            label.r = to_unit(label.r),
            label.size = label.size,
            min.segment.length = to_unit(min.segment.length),
            arrow = arrow,
            na.rm = na.rm,
            force = force,
            force_pull = force_pull,
            max.time = max.time,
            max.iter = max.iter,
            max.overlaps = max.overlaps,
            nudge_x = nudge_x,
            nudge_y = nudge_y,
            xlim = xlim,
            ylim = ylim,
            direction = match.arg(direction),
            seed = seed,
            verbose = verbose,
            ...
        )
    )
}

#' @include geom_junction.R
#' @keywords internal
#' @noRd
GeomJunctionLabelRepel <- ggplot2::ggproto(
    "GeomJunctionLabelRepel", ggrepel::GeomLabelRepel,
    required_aes = c("xstart", "xend", "y", "label"),
    # copied from ggrepel::GeomLabelRepel with segment.colour and segment.alpha
    # defaults set to appropriate values, rather than NULL
    # this avoid warnings e.g. Unknown or uninitialised column: `segment.alpha`
    # but does cause issues when setting e.g. aes(colour = tx)
    # TODO - resolve either warning or make segment.colour borrow colour aes
    default_aes = aes(
        colour = "black",
        fill = "white",
        size = 3.88,
        angle = 0,
        alpha = NA,
        family = "",
        fontface = 1,
        lineheight = 1.2,
        hjust = 0.5,
        vjust = 0.5,
        point.size = 1,
        segment.linetype = 1,
        segment.colour = "black",
        segment.size = 0.5,
        segment.alpha = NA,
        segment.curvature = 0,
        segment.angle = 90,
        segment.ncp = 1,
        segment.shape = 0.5,
        segment.square = TRUE,
        segment.squareShape = 1,
        segment.inflect = FALSE,
        segment.debug = FALSE
    ),
    setup_data = GeomJunction$setup_data,
    draw_panel = function(data, panel_scales, coord,
                          parse = FALSE,
                          na.rm = FALSE,
                          junction.orientation = "alternating",
                          junction.y.max = 1,
                          angle = 90,
                          ncp = 15,
                          box.padding = 0.25,
                          label.padding = 0.25,
                          point.padding = 1e-6,
                          label.r = 0.15,
                          label.size = 0.25,
                          min.segment.length = 0,
                          arrow = NULL,
                          force = 1,
                          force_pull = 1,
                          max.time = 0.5,
                          max.iter = 10000,
                          max.overlaps = 10,
                          nudge_x = 0,
                          nudge_y = 0,
                          xlim = c(NA, NA),
                          ylim = c(NA, NA),
                          direction = "both",
                          seed = NA,
                          verbose = FALSE) {

        # junction_index represents the order of each junction within tx
        # needed for junction.orientation = "alternating"
        data <- data %>%
            dplyr::group_by(y) %>%
            dplyr::mutate(junction_index = dplyr::row_number()) %>%
            dplyr::ungroup()

        # obtain the midpoints of junction curves (where we want label)
        junction_midpoints <-
            to_junction_midpoints(
                data,
                angle,
                ncp,
                junction.orientation,
                junction.y.max
            )

        ggrepel::GeomLabelRepel$draw_panel(
            data = junction_midpoints,
            panel_scales = panel_scales,
            coord = coord,
            parse = parse,
            na.rm = na.rm,
            box.padding = box.padding,
            label.padding = label.padding,
            point.padding = point.padding,
            label.r = label.r,
            label.size = label.size,
            min.segment.length = min.segment.length,
            arrow = arrow,
            force = force,
            force_pull = force_pull,
            max.time = max.time,
            max.iter = max.iter,
            max.overlaps = max.overlaps,
            nudge_x = nudge_x,
            nudge_y = nudge_y,
            xlim = xlim,
            ylim = ylim,
            direction = direction,
            seed = seed,
            verbose = verbose
        )
    }
)

#' Wrapper for obtaining junction curve midpoints
#'
#' @keywords internal
#' @noRd
to_junction_midpoints <- function(data,
                                  angle,
                                  ncp,
                                  junction.orientation,
                                  junction.y.max) {
    # TODO - maybe export this as helper?
    junctions <- .get_junction_curves(data, angle, ncp)
    junctions <- .get_normalised_curve(
        junctions,
        junction.orientation,
        junction.y.max
    )
    junction_midpoints <- .get_curve_midpoints(junctions)

    return(junction_midpoints)
}

#' @keywords internal
#' @noRd
.get_curve_midpoints <- function(junctions) {

    # get the mid points of each curve for labeling junctions
    # these are the points with the x value closest to median(x)
    # this cannot be == median(x), this will not pick up point for even ncp's
    junctions_mid <- junctions %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(
            median_x = stats::median(x),
            median_diff = abs(x - median_x)
        ) %>%
        dplyr::filter(median_diff == min(median_diff)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-median_x, -median_diff)

    return(junctions_mid)
}


to_unit <- ggrepel:::to_unit

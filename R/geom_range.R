#' Plot genomic ranges
#'
#' `geom_range()` and `geom_half_range()` draw tiles that are designed to
#' represent range-based genomic features, such as exons. In combination with
#' `geom_intron()`, these geoms form the core components for visualizing
#' transcript annotation.
#'
#' `geom_range()` and `geom_half_range()` require the following `aes()`;
#' `xstart`, `xend` and `y` (e.g. transcript name). `geom_half_range()` takes
#' advantage of the vertical symmetry of transcript annotation by plotting only
#' half of a range on the top or bottom of a transcript structure. This can be
#' useful to free up plotting space for other transcript annotations (e.g.
#' `geom_junction()`).
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_tile
#' @inheritParams ggplot2::geom_segment
#' @inheritParams grid::rectGrob
#'
#' @export
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' # to illustrate the package's functionality
#' # ggtranscript includes example transcript annotation
#' sod1_annotation
#'
#' # extract exons
#' sod1_exons <- sod1_annotation %>% dplyr::filter(type == "exon")
#' sod1_exons
#'
#' base <- sod1_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     ))
#'
#' # geom_range() is designed to visualise range-based annotation such as exons
#' base + geom_range()
#'
#' # geom_half_range() allows users to plot half ranges
#' # on the top or bottom of the transcript
#' base + geom_half_range()
#'
#' # where the half ranges are plotted can be adjusted using range.orientation
#' base + geom_half_range(range.orientation = "top")
#'
#' # as a ggplot2 extension, ggtranscript geoms inherit the
#' # the functionality from the parameters and aesthetics in ggplot2
#' base + geom_range(
#'     aes(fill = transcript_name),
#'     size = 1
#' )
#'
#' # together, geom_range() and geom_range() are designed to visualize
#' # the core components of transcript annotation
#' base + geom_range(
#'     aes(fill = transcript_biotype)
#' ) +
#'     geom_intron(
#'         data = to_intron(sod1_exons, "transcript_name")
#'     )
#'
#' # for protein coding transcripts
#' # geom_range() be useful for visualizing UTRs that lie outside of the CDS
#' sod1_exons_prot_coding <- sod1_exons %>%
#'     dplyr::filter(transcript_biotype == "protein_coding")
#'
#' # extract cds
#' sod1_cds <- sod1_annotation %>%
#'     dplyr::filter(type == "CDS")
#'
#' sod1_exons_prot_coding %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = transcript_name
#'     )) +
#'     geom_range(
#'         fill = "white",
#'         height = 0.25
#'     ) +
#'     geom_range(
#'         data = sod1_cds
#'     ) +
#'     geom_intron(
#'         data = to_intron(sod1_exons_prot_coding, "transcript_name")
#'     )
#'
#' # one use case of geom_half_range() is to compare between two transcripts
#' # by visualising one on the top and the other on the bottom
#' sod1_201_exons <- sod1_exons %>% dplyr::filter(transcript_name == "SOD1-201")
#' sod1_201_cds <- sod1_cds %>% dplyr::filter(transcript_name == "SOD1-201")
#' sod1_202_exons <- sod1_exons %>% dplyr::filter(transcript_name == "SOD1-202")
#' sod1_202_cds <- sod1_cds %>% dplyr::filter(transcript_name == "SOD1-202")
#'
#' sod1_201_plot <- sod1_201_exons %>%
#'     ggplot(aes(
#'         xstart = start,
#'         xend = end,
#'         y = "SOD1-201/202"
#'     )) +
#'     geom_half_range(
#'         fill = "white",
#'         height = 0.125
#'     ) +
#'     geom_half_range(
#'         data = sod1_201_cds
#'     ) +
#'     geom_intron(
#'         data = to_intron(sod1_201_exons, "transcript_name")
#'     )
#'
#' sod1_201_plot
#'
#' sod1_201_202_plot <- sod1_201_plot +
#'     geom_half_range(
#'         data = sod1_202_exons,
#'         range.orientation = "top",
#'         fill = "white",
#'         height = 0.125
#'     ) +
#'     geom_half_range(
#'         data = sod1_202_cds,
#'         range.orientation = "top",
#'         fill = "red"
#'     ) +
#'     geom_intron(
#'         data = to_intron(sod1_202_exons, "transcript_name")
#'     )
#'
#' sod1_201_202_plot
#'
#' # leveraging existing ggplot2 functionality vis coord_cartesian()
#' # can be useful to zoom in on areas of interest
#' sod1_201_202_plot + coord_cartesian(xlim = c(31659500, 31660000))
geom_range <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       vjust = NULL,
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
            vjust = vjust,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' `GeomRange` is `ggplot2::GeomTile` with modified `aes` to match genetic
#' nomenclature (`xstart`/`xend`)
#' @keywords internal
#' @noRd
GeomRange <- ggplot2::ggproto("GeomRange", ggplot2::GeomTile,
    required_aes = c("xstart", "xend", "y"),
    default_aes = aes(
        fill = "grey",
        colour = "black",
        size = 0.25,
        linetype = 1,
        alpha = NA,
        height = NA
    ),
    setup_data = function(data, params) {
        # modified from ggplot2::GeomTile
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
    draw_panel = function(self,
                          data,
                          panel_params,
                          coord,
                          vjust = NULL,
                          lineend = "butt",
                          linejoin = "mitre") {
        if (!coord$is_linear()) {
            # prefer to match geom_curve and warn
            # rather than copy the implementation from GeomRect for simplicity
            # also don'think geom_range would be used for non-linear coords
            warn("geom_ is not implemented for non-linear coordinates")
        }

        coords <- coord$transform(data, panel_params)
        grid::rectGrob(
            coords$xmin, coords$ymax,
            width = coords$xmax - coords$xmin,
            height = coords$ymax - coords$ymin,
            default.units = "native",
            just = c("left", "top"),
            vjust = vjust,
            gp = grid::gpar(
                col = coords$colour,
                fill = ggplot2::alpha(coords$fill, coords$alpha),
                lwd = coords$size * ggplot2::.pt,
                lty = coords$linetype,
                linejoin = linejoin,
                lineend = lineend
            )
        )
    }
)

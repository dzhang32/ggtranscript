geom_intron <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = FALSE,
                        strand = "+",
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
            strand = strand,
            ...
        )
    )
}

GeomIntron <- ggplot2::ggproto("GeomIntron", ggplot2::GeomSegment,
    required_aes = c("x_start", "x_end", "y"),
    default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
    setup_params = function(data, params) {
        strand_len_1 <- length(params$strand) != 1
        strand_any_na <- any(is.na(params$strand))
        strand_chr <- !is.character(params$strand)
        strand_plus_minus <- !(all(params$strand %in% c("+", "-")))

        if (strand_len_1 | strand_any_na | strand_chr | strand_plus_minus) {
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
    draw_panel = function(data, panel_params, coord, lineend = "butt", linejoin = "round", na.rm = FALSE, strand = "+") {
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
        if (strand == "+") {
            arrow_data <- transform(
                data,
                xend = (x + xend) / 2
            )
        } else {
            arrow_data <- transform(
                data,
                mid = (x + xend) / 2,
                x = xend
            )
            arrow_data <- transform(
                arrow_data,
                xend = mid
            )
        }

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

#' @noRd
to_intron <- function(x, group_var = NULL, start_var = start, end_var = end) {

    # grouping by NULL (default) does nothing
    x <- x %>%
        dplyr::group_by({{ group_var }})

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    x <- x %>%
        dplyr::arrange({{ start_var }}, {{ end_var }})

    # obtain intron start and ends
    x <- x %>%
        dplyr::mutate(
            intron_start := dplyr::lag({{ end_var }}) + 1,
            intron_end := {{ start_var }} - 1
        ) %>%
        dplyr::select(-{{ start_var }}, -{{ end_var }})

    # for each group, output N of introns should be N - 1 the inputted exons
    # remove the introduced artifact NAs
    x <- x %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end))

    return(x)
}

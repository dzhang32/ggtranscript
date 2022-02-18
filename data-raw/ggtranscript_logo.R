
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(hexSticker)
library(showtext)
devtools::load_all(".")

# Main --------------------------------------------------------------------

logo_exons <- tribble(
    ~start, ~end, ~tx, ~letter,
    #-------|-----|----|--------
    150, 200, "J", "T",
    500, 550, "J", "T",
    300, 310, "I", "T",
    350, 400, "I", "T",
    300, 350, "H", "T",
    390, 400, "H", "T",
    300, 310, "G", "T",
    350, 400, "G", "T",
    700, 800, "J", "X_top",
    1100, 1200, "J", "X_top",
    700, 800, "G", "X_bot",
    1100, 1200, "G", "X_bot"
) %>%
    dplyr::mutate(
        tx = tx %>% factor(
            levels = LETTERS[1:17]
        )
    )

logo_utr <- tribble(
    ~start, ~end, ~tx, ~letter,
    #-------|-----|----|--------
    100, 150, "J", "T",
    550, 600, "J", "T",
)

logo_introns <- logo_exons %>%
    dplyr::filter(letter == "T") %>%
    to_intron(group_var = "tx")

logo_junctions <- logo_exons %>%
    dplyr::filter(letter %in% c("X_top", "X_bot")) %>%
    to_intron(group_var = "tx")

size <- 0.3
colour <- "black"
fill <- ggpubr::get_palette("jco", 10)[10]

# create T
ggtranscript_logo <- logo_exons %>%
    dplyr::filter(letter == "T") %>%
    ggplot(aes(
        xstart = start,
        xend = end,
        y = tx
    )) +
    geom_range(
        fill = fill,
        size = size,
        colour = colour
    ) +
    geom_range(
        data = logo_utr,
        fill = "white",
        height = 0.25,
        size = size,
        colour = colour
    ) +
    geom_intron(
        data = logo_introns,
        size = size,
        colour = colour,
        arrow.min.intron.length = 100
    )

ggtranscript_logo <- ggtranscript_logo +
    geom_half_range(
        data = logo_exons %>% dplyr::filter(letter == "X_top"),
        range.orientation = "top",
        fill = fill,
        size = size,
        colour = colour,
    ) +
    geom_half_range(
        data = logo_exons %>% dplyr::filter(letter == "X_bot"),
        fill = fill,
        size = size,
        colour = colour,
    ) +
    geom_junction(
        data = logo_junctions %>% dplyr::filter(letter == "X_bot"),
        size = size,
        colour = colour,
        junction.orientation = "top",
        junction.y.max = 1.4,
        ncp = 50
    ) +
    geom_junction(
        data = logo_junctions %>% dplyr::filter(letter == "X_top"),
        size = size,
        colour = colour,
        junction.orientation = "bottom",
        junction.y.max = 1.4,
        ncp = 50
    )

ggtranscript_logo <- ggtranscript_logo +
    scale_x_continuous(
        limits = c(-300, 1600),
        minor_breaks = seq(-300, 1500, 100)
    ) +
    scale_y_discrete(drop = FALSE) +
    theme_bw() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(size = size, colour = ggpubr::get_palette("Greys", 10)[2]),
        panel.grid.minor = element_line(size = size, colour = ggpubr::get_palette("Greys", 10)[2])
    )

ggtranscript_logo

# Save data ---------------------------------------------------------------

# use font from https://fonts.google.com
font_add_google(name = "Raleway", family = "Raleway")
showtext_auto()

ggtranscript_logo_hex <- hexSticker::sticker(
    # the plot (TX)
    subplot = ggtranscript_logo,
    s_x = 0.98,
    s_y = 1.2,
    s_width = 2.8,
    s_height = 3,
    # the package
    package = "ggtranscript",
    p_x = 1,
    p_y = 0.65,
    p_size = 35,
    p_family = "Raleway",
    p_fontface = "bold",
    p_color = ggpubr::get_palette("jco", 10)[6],
    # hex border
    h_color = ggpubr::get_palette("jco", 10)[6],
    h_fill = "white",
    h_size = 2,
    # url
    url = "https://github.com/dzhang32/ggtranscript",
    u_family = "Raleway",
    u_color = ggpubr::get_palette("jco", 10)[6],
    u_size = 6.5,
    # general
    filename = here::here("man", "figures", "ggtranscript_logo.png"),
    dpi = 600,
    white_around_sticker = TRUE
)

plot(ggtranscript_logo_hex)

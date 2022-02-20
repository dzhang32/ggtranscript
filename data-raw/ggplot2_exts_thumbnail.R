
# Load libraries ----------------------------------------------------------

library(tidyverse)
devtools::load_all(".")

# Main --------------------------------------------------------------------

sod1_201_exons <- sod1_annotation %>%
    dplyr::filter(
        type == "exon",
        transcript_name == "SOD1-201"
    )

sod1_201_cds <- sod1_annotation %>%
    dplyr::filter(
        type == "CDS",
        transcript_name == "SOD1-201"
    )

sod1_junctions <- sod1_junctions %>% dplyr::mutate(transcript_name = "SOD1-201")

ggplot2_exts_figure <- sod1_201_exons %>%
    ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    )) +
    geom_range(
        fill = "white",
        height = 0.125
    ) +
    geom_range(
        data = sod1_201_cds,
        height = 0.25
    ) +
    geom_intron(
        data = to_intron(sod1_201_exons, "transcript_name")
    ) +
    geom_junction(
        data = sod1_junctions,
        aes(size = mean_count),
        junction.y.max = 0.25,
        ncp = 30,
        colour = "purple"
    ) +
    scale_size_continuous(range = c(0.1, 1), guide = "none") +
    xlab("Genomic position (chr21)") +
    ylab("Transcript name") +
    theme_bw() +
    theme(
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank()
    )

ggplot2_exts_figure

# Save data ---------------------------------------------------------------

ggsave(
    plot = ggplot2_exts_figure,
    filename = here::here("man", "figures", "dzhang32-ggtranscript.png"),
    height = 3,
    width = 3.5,
    dpi = 600
)

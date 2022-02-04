# library(tidyverse)
#
# gba_ens_105 %>%
#   dplyr::filter(type == "exon") %>%
#   ggplot() +
#   geom_range(
#     aes(
#       xstart = start,
#       xend = end,
#       y = transcript_name,
#       fill = transcript_name,
#       height = 1
#     ),
#     colour = "black"
#   )

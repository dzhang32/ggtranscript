# library(tidyverse)
#
# gba_ens_105 %>%
#   dplyr::filter(type == "exon") %>%
#   ggplot() +
#   geom_range(
#     aes(
#       x_start = start,
#       x_end = end,
#       y = transcript_name,
#       fill = transcript_name,
#       height = 1
#     ),
#     colour = "black"
#   )
#
# gba_ens_105 %>%
#   dplyr::filter(type == "exon") %>%
#   dplyr::mutate(width = 100) %>%
#   ggplot() +
#   geom_tile(
#     aes(x = start,
#         y = transcript_name,
#         width = 100,
#         height = 0.1),
#   )

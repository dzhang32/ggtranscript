# bypass R CMD Check notes, related to tidyverse non-standard evaluation
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
utils::globalVariables(c(
    "start",
    "end",
    ":=",
    "intron_start",
    "intron_end",
    "x",
    "xend",
    "mid",
    "index",
    "diff_type",
    "in_x",
    "in_y",
    "."
))

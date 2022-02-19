
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(snapcount)
library(recount)
library(SummarizedExperiment)

# Main --------------------------------------------------------------------

# obtain gtex junctions across SOD1
sod1_query <- snapcount::QueryBuilder(compilation="gtex", regions = "SOD1")

# keeping only unannotated junctions
# from liver where SOD1 is highly expressed
# https://gtexportal.org/home/gene/SOD1
sod1_query <- set_row_filters(sod1_query, annotated == 0)
sod1_query <- set_column_filters(sod1_query, SMTS == "Liver")

sod1_junctions <- snapcount::query_jx(sod1_query)

# obtain mean counts
mean_counts <-
  sod1_junctions %>%
  SummarizedExperiment::assays() %>%
  .[["counts"]] %>%
  as.matrix() %>%
  rowMeans()

sod1_junctions <- sod1_junctions %>%
  SummarizedExperiment::rowRanges() %>%
  as.data.frame() %>%
  dplyr::as_tibble()

# minor QC and tidying of the junctions
sod1_junctions <-
  sod1_junctions %>%
  dplyr::mutate(mean_count = mean_counts) %>%
  dplyr::filter(mean_count > 0.3) %>%
  dplyr::select(
    seqnames,
    start,
    end,
    strand,
    mean_count
  )

# Save data ---------------------------------------------------------------

usethis::use_data(
  sod1_junctions,
  compress = "gzip",
  overwrite = TRUE
)

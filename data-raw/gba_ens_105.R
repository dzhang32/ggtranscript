
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rtracklayer)
library(R.utils)

# Main --------------------------------------------------------------------

gtf_path <- file.path(tempdir(), "Homo_sapiens.GRCh38.105.chr.gtf.gz")

# download ens 105 gtf
download.file(
    stringr::str_c(
        "http://ftp.ensembl.org/pub/release-105/gtf/homo_sapiens/",
        "Homo_sapiens.GRCh38.105.chr.gtf.gz"
    ),
    destfile = gtf_path
)

# unzip gtf
R.utils::gunzip(gtf_path)

gtf_path <- gtf_path %>%
    stringr::str_remove("\\.gz$")

gtf <- rtracklayer::import(gtf_path)

# extract gba transcripts
# convert to data.frame()
gba_ens_105 <-
    gtf[!is.na(gtf$gene_name) & gtf$gene_name == "GBA"] %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::select(seqnames, start, end, strand, type)



# Save data ---------------------------------------------------------------

usethis::use_data(
    gba_ens_105,
    compress = "gzip",
    overwrite = TRUE
)

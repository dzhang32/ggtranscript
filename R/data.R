#' Example transcript annotation
#'
#' Co-ordinates (hg38) of the genes, transcripts, exons for an example gene
#' (GBA) originating from Ensembl v105 reference annotation.
#'
#' @format A `tibble::tibble()` with 26 rows and 4 columns:
#' \describe{
#'   \item{seqnames}{`factor()` chromosome.}
#'   \item{start}{`integer()` start position.}
#'   \item{end}{`integer()` end position.}
#'   \item{strand}{`factor()` strand.}
#'   \item{type}{`factor()` type of data, one of gene, transcript, exon or CDS.}
#' }
#'
#' @source generated using `ggtranscript/data-raw/gba_ens_105.R`
"gba_ens_105"

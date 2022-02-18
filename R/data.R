#' Example transcript annotation
#'
#' Co-ordinates (hg38) of the genes, transcripts, exons for an example genes
#' (SOD1, PKNOX1) originating from Ensembl v105 reference annotation.
#'
#' @format A `tibble::tibble()` with 26 rows and 4 columns:
#' \describe{
#'   \item{seqnames}{`factor()` chromosome.}
#'   \item{start}{`integer()` start position.}
#'   \item{end}{`integer()` end position.}
#'   \item{strand}{`factor()` strand.}
#'   \item{type}{`factor()` type of data, one of gene, transcript, exon or CDS.}
#'   \item{gene_name}{`character()` name of gene (GBA).}
#'   \item{transcript_name}{`character()` name of transcript.}
#'   \item{transcript_biotype}{`character()` biotype of transcript.}
#' }
#'
#' @source generated using `ggtranscript/data-raw/sod1_pknox1_annotation.R`
"sod1_annotation"

#' @rdname sod1_annotation
"pknox1_annotation"

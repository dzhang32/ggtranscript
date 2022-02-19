#' Example transcript annotation
#'
#' Transcript annotation including the co-ordinates (hg38) of the genes,
#' transcripts, exons and CDS regions for \emph{SOD1} and \emph{PKNOX1}, which
#' originate from version 105 of the Ensembl reference annotation.
#'
#' @format A `tibble::tibble()`:
#' \describe{
#'   \item{seqnames}{`factor()` chromosome.}
#'   \item{start}{`integer()` start position.}
#'   \item{end}{`integer()` end position.}
#'   \item{strand}{`factor()` strand.}
#'   \item{type}{`factor()` E.g.gene, transcript, exon or CDS.}
#'   \item{gene_name}{`character()` name of gene (GBA).}
#'   \item{transcript_name}{`character()` name of transcript.}
#'   \item{transcript_biotype}{`character()` biotype of transcript.}
#' }
#'
#' @source generated using `ggtranscript/data-raw/sod1_pknox1_annotation.R`
"sod1_annotation"

#' @rdname sod1_annotation
"pknox1_annotation"

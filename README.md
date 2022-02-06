
# ggtranscript

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/dzhang32/ggtranscript)](https://github.com/dzhang32/ggtranscript/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/dzhang32/ggtranscript)](https://github.com/dzhang32/ggtranscript/pulls)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/dzhang32/ggtranscript/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/dzhang32/ggtranscript/actions)
[![Codecov test
coverage](https://codecov.io/gh/dzhang32/ggtranscript/branch/main/graph/badge.svg)](https://app.codecov.io/gh/dzhang32/ggtranscript?branch=main)
<!-- badges: end -->

`ggtranscript` is a `ggplot2` extension that makes it easy visualize
transcript structure and annotation.

## Installation

``` r
devtools::install_github("dzhang32/ggtranscript")
```

## Examples

`ggtranscript` introduces 3 new `geom`s designed to visualise transcript
annotation; `geom_range()`, `geom_intron()` and `geom_junction()`.

`geom_range()` and `geom_intron()` enable the plotting of exons and
introns. `ggtranscript` also provides a useful helpful function
`to_intron` to convert exon co-ordinates to the corresponding introns.

``` r
library(magrittr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(ggtranscript)

# gene annotation for the an example gene (GBA)
gba_ens_105 %>% head()
#> # A tibble: 6 × 8
#>   seqnames  start    end strand type  gene_name transcript_name transcript_biot…
#>   <fct>     <int>  <int> <fct>  <fct> <chr>     <chr>           <chr>           
#> 1 1        1.55e8 1.55e8 -      gene  GBA       <NA>            <NA>            
#> 2 1        1.55e8 1.55e8 -      tran… GBA       GBA-202         protein_coding  
#> 3 1        1.55e8 1.55e8 -      exon  GBA       GBA-202         protein_coding  
#> 4 1        1.55e8 1.55e8 -      CDS   GBA       GBA-202         protein_coding  
#> 5 1        1.55e8 1.55e8 -      star… GBA       GBA-202         protein_coding  
#> 6 1        1.55e8 1.55e8 -      exon  GBA       GBA-202         protein_coding

# obtain exons
gba_ens_105_exons <- gba_ens_105 %>%
    dplyr::filter(type == "exon")

gba_ens_105_exons %>%
    ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    )) +
    geom_range(
        aes(fill = transcript_biotype)
    ) +
    geom_intron(
        data = to_intron(gba_ens_105_exons, transcript_name),
        aes(strand = strand),
        arrow.min.intron.length = 500,
    )
```

<img src="man/figures/README-tx-annot-base-1.png" width="100%" />

`geom_range` can be used for any genomic range based annotation. For
example, when plotting protein-coding transcripts, it can be useful to
visually distinguish the coding segments from UTRs.

``` r
# keeping only the exons from protein coding transcripts
gba_ens_105_exons_prot_cod <- gba_ens_105_exons %>%
    dplyr::filter(transcript_biotype == "protein_coding")

# obtain cds
gba_ens_105_cds <- gba_ens_105 %>%
    dplyr::filter(type == "CDS")

gba_ens_105_exons_prot_cod %>%
    ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    )) +
    geom_range(
        fill = "white",
        height = 0.25
    ) +
    geom_range(
        data = gba_ens_105_cds
    ) +
    geom_intron(
        data = to_intron(gba_ens_105_exons_prot_cod, transcript_name),
        aes(strand = strand),
        arrow.min.intron.length = 500,
    )
```

<img src="man/figures/README-tx-annot-w-cds-1.png" width="100%" />

When working with short-read RNA-sequencing data, it can be useful to
check whether a known transcript structure has junction support using
`geom_junction()`.

``` r
# using two transcripts as an example
gba_ens_105_201_exons <- gba_ens_105_exons %>%
    dplyr::filter(transcript_name == c("GBA-201"))

gba_ens_105_201_cds <- gba_ens_105_cds %>%
    dplyr::filter(transcript_name == "GBA-201")

# simulate junction data, randomly keeping half of the junctions
gba_ens_105_201_introns <- gba_ens_105_201_exons %>%
    to_intron(transcript_name)

set.seed(32)

gba_ens_105_201_junctions <-
    gba_ens_105_201_introns[sample(seq_len(nrow(gba_ens_105_201_introns)), 6), ]

gba_ens_105_201_exons %>%
    ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    )) +
    geom_range(
        fill = "white",
        height = 0.25
    ) +
    geom_range(
        data = gba_ens_105_201_cds
    ) +
    geom_intron(
        data = gba_ens_105_201_introns,
        aes(strand = strand),
        arrow.min.intron.length = 500,
    ) +
    geom_junction(
        data = gba_ens_105_201_junctions,
        colour = "red"
    )
```

<img src="man/figures/README-tx-annot-w-junction-1.png" width="100%" />

## Code of Conduct

Please note that the `ggtranscript` project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

-   Continuous code testing is possible thanks to [GitHub
    actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
    through *[usethis](https://CRAN.R-project.org/package=usethis)*,
    *[remotes](https://CRAN.R-project.org/package=remotes)*, and
    *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)*
    customized to use [Bioconductor’s docker
    containers](https://www.bioconductor.org/help/docker/) and
    *[BiocCheck](https://bioconductor.org/packages/3.14/BiocCheck)*.
-   Code coverage assessment is possible thanks to
    [codecov](https://codecov.io/gh) and
    *[covr](https://CRAN.R-project.org/package=covr)*.
-   The [documentation website](http://dzhang32.github.io/ggtranscript)
    is automatically updated thanks to
    *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
-   The code is styled automatically thanks to
    *[styler](https://CRAN.R-project.org/package=styler)*.
-   The documentation is formatted thanks to
    *[devtools](https://CRAN.R-project.org/package=devtools)* and
    *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.14/biocthis)*.

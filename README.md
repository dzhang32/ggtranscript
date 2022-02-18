
# ggtranscript <img src="man/figures/ggtranscript_logo_cropped.svg" align="right" height="139" />

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

## Overview

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

# gene annotation for the an example gene (SOD1)
sod1_annotation %>% head()
#> # A tibble: 6 × 8
#>   seqnames  start    end strand type  gene_name transcript_name transcript_biot…
#>   <fct>     <int>  <int> <fct>  <fct> <chr>     <chr>           <chr>           
#> 1 21       3.17e7 3.17e7 +      gene  SOD1      <NA>            <NA>            
#> 2 21       3.17e7 3.17e7 +      tran… SOD1      SOD1-202        protein_coding  
#> 3 21       3.17e7 3.17e7 +      exon  SOD1      SOD1-202        protein_coding  
#> 4 21       3.17e7 3.17e7 +      CDS   SOD1      SOD1-202        protein_coding  
#> 5 21       3.17e7 3.17e7 +      star… SOD1      SOD1-202        protein_coding  
#> 6 21       3.17e7 3.17e7 +      exon  SOD1      SOD1-202        protein_coding

# obtain exons
sod1_exons <- sod1_annotation %>%
    dplyr::filter(type == "exon")

sod1_exons %>%
    ggplot(aes(
        xstart = start,
        xend = end,
        y = transcript_name
    )) +
    geom_range(
        aes(fill = transcript_biotype)
    ) +
    geom_intron(
        data = to_intron(sod1_exons, "transcript_name"),
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
sod1_exons_prot_cod <- sod1_exons %>%
    dplyr::filter(transcript_biotype == "protein_coding")

# obtain cds
sod1_cds <- sod1_annotation %>%
    dplyr::filter(type == "CDS")

sod1_exons_prot_cod %>%
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
        data = sod1_cds
    ) +
    geom_intron(
        data = to_intron(sod1_exons_prot_cod, "transcript_name"),
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
sod1_201_exons <- sod1_exons %>%
    dplyr::filter(transcript_name == c("SOD1-201"))

sod1_201_cds <- sod1_cds %>%
    dplyr::filter(transcript_name == "SOD1-201")

# simulate junction data, randomly keeping half of the junctions
sod1_201_introns <- sod1_201_exons %>%
    to_intron("transcript_name")

set.seed(32)

sod1_201_junctions <-
    sod1_201_introns[sample(seq_len(nrow(sod1_201_introns)), 3), ]

sod1_201_exons %>%
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
        data = sod1_201_cds
    ) +
    geom_intron(
        data = sod1_201_introns,
        aes(strand = strand),
        arrow.min.intron.length = 500,
    ) +
    geom_junction(
        data = sod1_201_junctions,
        colour = "red",
        junction.y.max = 0.5
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

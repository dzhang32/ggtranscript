# ggtranscript 0.99.7

## NEW FEATURES

* Add `cran-comments.md` in preparation for first CRAN submission.
* Update CI to run `R CMD Check` on latest R version (4.2).

# ggtranscript 0.99.6

## NEW FEATURES

* Add `@return` documentation for `geom_*` functions for `BiocCheck`.

# ggtranscript 0.99.5

## NEW FEATURES

* Change branch to naming from main to master to match BBS.

# ggtranscript 0.99.4

## NEW FEATURES

* Change email to UCL email for Bioconductor submission. 

## NEW FEATURES

* Add `add_utr()` for adding UTRs as ranges. This helper function is designed to 
work with `shorten_gaps()`, enabling shortening of gaps whilst visually 
differentiating UTRs from the CDS.
* Allow `to_intron()` to take CDS and UTRs ranges as input. 
* Submit to Bioconductor.

# ggtranscript 0.99.2

## NEW FEATURES

* Add `geom_junction_label_repel()` for labeling junctions (e.g. with counts).
* Add `add_exon_number()` for visualizing the exon number/order.

# ggtranscript 0.99.1

## NEW FEATURES

* Implement base geoms: `geom_range()`, `geom_half_range()`, `geom_intron()`, 
`geom_junction()` and helper functions: `to_intron()`, `to_diff()` and 
`shorten_gaps()`.

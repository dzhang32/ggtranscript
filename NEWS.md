# ggtranscript 0.99.3

NEW FEATURES

* Add `add_utr()` for adding UTRs as ranges. This helper function is designed to 
work with `shorten_gaps()`, enabling shortening of gaps whilst visually 
differentiating UTRs from the CDS.
* Allow `to_intron()` to take CDS and UTRs ranges as input. 

# ggtranscript 0.99.2

NEW FEATURES

* Add `geom_junction_label_repel()` for labeling junctions (e.g. with counts).
* Add `add_exon_number()` for visualizing the exon number/order.

# ggtranscript 0.99.1

NEW FEATURES

* Implement base geoms: `geom_range()`, `geom_half_range()`, `geom_intron()`, 
`geom_junction()` and helper functions: `to_intron()`, `to_diff()` and 
`shorten_gaps()`.

# Convert arcs to a wkpool of arc segments

Convert arcs to a wkpool of arc segments

## Usage

``` r
as_arcs(x, arc_id = TRUE, quotient = FALSE)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- arc_id:

  Logical: add .arc column to track arc membership?

- quotient:

  Walk the quotient graph: duplicated shared boundaries collapse to a
  single undirected edge (first occurrence keeps its direction), so a
  boundary shared by two features becomes ONE arc rather than one per
  feature, and its interior vertices are degree-2 pass-throughs. This is
  the TopoJSON-style arc decomposition of a polygon layer. Default FALSE
  walks every segment.

## Value

A wkpool with segments grouped by arc

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
as_arcs(merged)
#> <wkpool[8 segments, 6 vertices]>
#> [1] <segment: 2->1> <segment: 1->4> <segment: 4->3> <segment: 2->3>
#> [5] <segment: 2->5> <segment: 5->6> <segment: 6->3> <segment: 2->3>
```

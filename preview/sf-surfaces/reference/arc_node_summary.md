# Summarize arc-node structure

Summarize arc-node structure

## Usage

``` r
arc_node_summary(x, quotient = FALSE)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- quotient:

  Walk the quotient graph: duplicated shared boundaries collapse to a
  single undirected edge (first occurrence keeps its direction), so a
  boundary shared by two features becomes ONE arc rather than one per
  feature, and its interior vertices are degree-2 pass-throughs. This is
  the TopoJSON-style arc decomposition of a polygon layer. Default FALSE
  walks every segment.

## Value

List with counts and degree distribution

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
arc_node_summary(merged)
#> $n_vertices
#> [1] 6
#> 
#> $n_nodes
#> [1] 2
#> 
#> $n_arcs
#> [1] 4
#> 
#> $degree_distribution
#> deg
#> 2 4 
#> 4 2 
#> 
#> $arc_length_distribution
#> arc_lengths
#> 1 3 
#> 2 2 
#> 
#> $mean_arc_length
#> [1] 2
#> 
```

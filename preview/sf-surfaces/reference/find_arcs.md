# Find arcs (maximal segment sequences between nodes)

Find arcs (maximal segment sequences between nodes)

## Usage

``` r
find_arcs(x, quotient = FALSE)
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

A list of integer vectors, each containing .vx IDs forming an arc

## Details

Arcs are maximal paths through degree-2 vertices. They start and end at
nodes (degree != 2) or form closed loops through degree-2 vertices.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
find_arcs(merged)
#> [[1]]
#> [1] 2 1 4 3
#> 
#> [[2]]
#> [1] 2 3
#> 
#> [[3]]
#> [1] 2 5 6 3
#> 
#> [[4]]
#> [1] 2 3
#> 
```

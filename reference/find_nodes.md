# Find nodes (vertices where degree != 2)

Find nodes (vertices where degree != 2)

## Usage

``` r
find_nodes(x, quotient = FALSE)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- quotient:

  Count in the quotient graph: duplicated shared boundaries (the same
  undirected vertex pair carried by more than one feature) count once. A
  vertex interior to a shared boundary has quotient degree 2 - the
  degree-2 invariant holds at this level - while true junctions (three
  or more distinct edges) stay nodes. Default FALSE counts every
  segment.

## Value

Integer vector of .vx IDs that are nodes

## Details

Nodes are branch points (degree 3+) or endpoints (degree 1). Degree-2
vertices are pass-through points within an arc.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
find_nodes(merged)
#> [1] 2 3
```

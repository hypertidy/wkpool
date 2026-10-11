# Calculate vertex degree (number of segments touching each vertex)

Calculate vertex degree (number of segments touching each vertex)

## Usage

``` r
vertex_degree(x, quotient = FALSE)
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

Named integer vector: names are .vx, values are degree

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
vertex_degree(merged)
#> 1 2 3 4 5 6 
#> 2 4 4 2 2 2 
```

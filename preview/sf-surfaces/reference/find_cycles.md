# Find closed cycles (rings) in segment graph

Find closed cycles (rings) in segment graph

## Usage

``` r
find_cycles(x)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

## Value

A list of integer vectors, each containing .vx IDs forming a closed
cycle

## Details

When the pool carries path provenance (minted by
[`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md)),
a cycle is a path whose segment chain closes: rings are recovered
exactly, in input order and input winding, robust to segment reordering,
and the result carries a `path` attribute mapping each cycle to its path
id. Pools without provenance fall back to walking the segment graph in
storage order, which relies on segments being consecutive within each
ring.

Each cycle is a vector of vertex IDs in traversal order. The cycle is
closed (first vertex connects back to last via a segment).

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
find_cycles(merged)
#> [[1]]
#> [1] 1 2 3 4
#> 
#> [[2]]
#> [1] 2 5 6 3
#> 
#> attr(,"path")
#> [1] 1 2
```

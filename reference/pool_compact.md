# Compact a pool by removing unreferenced vertices

Compact a pool by removing unreferenced vertices

## Usage

``` r
pool_compact(x)
```

## Arguments

- x:

  A wkpool

## Value

A wkpool with only referenced vertices, .vx remapped

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
compact <- pool_compact(merged)
nrow(pool_vertices(compact))
#> [1] 6
```

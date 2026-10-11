# Report topology diagnostics

Report topology diagnostics

## Usage

``` r
topology_report(x, tolerance = 1e-08, period_x = NULL)
```

## Arguments

- x:

  A wkpool

- tolerance:

  Tolerance for "near miss" detection

- period_x:

  `NULL`, or the period of x, as in
  [`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md);
  duplicates and near misses are then counted with x wrapped.

## Value

List of diagnostic information

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
topology_report(pool)
#> $n_vertices_raw
#> [1] 10
#> 
#> $n_vertices_unique
#> [1] 6
#> 
#> $n_duplicate_vertices
#> [1] 4
#> 
#> $n_near_miss_vertices
#> [1] 0
#> 
#> $n_segments
#> [1] 8
#> 
#> $n_shared_edges
#> [1] 1
#> 
#> $n_features
#> [1] 2
#> 
```

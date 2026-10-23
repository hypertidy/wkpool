# Calculate signed area of a cycle

Calculate signed area of a cycle

## Usage

``` r
cycle_signed_area(cycle, pool)
```

## Arguments

- cycle:

  Integer vector of .vx IDs forming a closed cycle

- pool:

  Vertex pool data frame (from pool_vertices)

## Value

Numeric signed area.

## Details

Uses the shoelace formula to compute signed area. The sign indicates
winding direction. For typical geographic data (Simple Features
convention):

- Negative area = outer ring

- Positive area = hole

This is intrinsic to the geometry: we observe winding, not declare it.

## Examples

``` r
x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
pool <- establish_topology(x)
merged <- merge_coincident(pool)
cycles <- find_cycles(merged)
cycle_signed_area(cycles[[1]], pool_vertices(merged))
#> [1] 1
```

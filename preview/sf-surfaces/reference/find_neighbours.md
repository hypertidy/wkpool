# Build adjacency from shared edges

Build adjacency from shared edges

## Usage

``` r
find_neighbours(x, type = c("edge", "vertex"))
```

## Arguments

- x:

  A wkpool after merge_coincident

- type:

  "edge" for features sharing an edge, "vertex" for features sharing any
  vertex

## Value

Data frame of feature pairs that are neighbours

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
find_neighbours(merged)
#>   feature_a feature_b
#> 3         1         2
```

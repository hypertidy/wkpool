# Access components of a wkpool object

Extract the vertex pool, segment table, or feature vector from a wkpool.

## Usage

``` r
pool_vertices(x)

pool_segments(x)

pool_feature(x)

pool_path(x)

pool_paths(x)
```

## Arguments

- x:

  A wkpool object.

## Value

- `pool_vertices()`: A data frame with columns `.vx` (vertex ID), `x`,
  `y`, and optionally `z`.

- `pool_segments()`: A data frame with columns `.vx0`, `.vx1`, and
  optionally `.feature` and `.path`.

- `pool_feature()`: An integer vector of feature IDs, or `NULL` if no
  feature information is present.

- `pool_path()`: An integer vector of path IDs (one per segment), or
  `NULL` if no path provenance is present.

- `pool_paths()`: A data frame describing each path (`.path`,
  `.feature`, `.part`, `.ring`, as captured from
  [`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html)
  identifiers), or `NULL`.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
pool_vertices(pool)
#>    .vx x y
#> 1    1 0 0
#> 2    2 1 0
#> 3    3 1 1
#> 4    4 0 1
#> 5    5 0 0
#> 6    6 1 0
#> 7    7 2 0
#> 8    8 2 1
#> 9    9 1 1
#> 10  10 1 0
pool_segments(pool)
#>   .vx0 .vx1 .feature .path
#> 1    1    2        1     1
#> 2    2    3        1     1
#> 3    3    4        1     1
#> 4    4    5        1     1
#> 5    6    7        2     2
#> 6    7    8        2     2
#> 7    8    9        2     2
#> 8    9   10        2     2
pool_feature(pool)
#> [1] 1 1 1 1 2 2 2 2
```

# Convert wkpool segments to WKT

Convert wkpool segments to WKT

## Usage

``` r
segments_to_wkt(x, type = c("multilinestring", "linestring", "point"))

segments_to_wkb(x, type = c("multilinestring", "linestring", "point"), ...)
```

## Arguments

- x:

  A wkpool

- type:

  Output type: "linestring" (segments as paths), "multilinestring" (all
  segments), or "point" (vertices only)

- ...:

  Passed to
  [`wk::as_wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html)

## Value

A wk_wkt vector

## Details

Geometry is assembled natively via
[`wk::wk_linestring()`](https://paleolimbot.github.io/wk/reference/wk_linestring.html)
and
[`wk::wk_collection()`](https://paleolimbot.github.io/wk/reference/wk_linestring.html)
(WKB first, WKT derived from it): coordinates are emitted at full double
precision, z is carried when the pool has it, and the pool's crs and
geodesic flag are attached to the output.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
segments_to_wkt(pool)
#> <wk_wkt[1]>
#> [1] MULTILINESTRING ((0 0, 1 0), (1 0, 1 1), (1 1, 0 1...
segments_to_wkt(pool, type = "linestring")
#> <wk_wkt[8]>
#> [1] LINESTRING (0 0, 1 0) LINESTRING (1 0, 1 1) LINESTRING (1 1, 0 1)
#> [4] LINESTRING (0 1, 0 0) LINESTRING (1 0, 2 0) LINESTRING (2 0, 2 1)
#> [7] LINESTRING (2 1, 1 1) LINESTRING (1 1, 1 0)
```

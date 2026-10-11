# CRS and geodesic handling for wkpool

A wkpool participates in wk's CRS propagation model: the CRS is an
opaque object carried in an attribute, never interpreted, and checked
for equality when pools are combined. Likewise the geodesic flag records
whether segments should be interpreted as geodesics when coordinates are
spherical. Both are captured from the input by
[`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md),
survive subsetting, merging and compaction, and are restored onto
geometry produced by
[`segments_to_wkt()`](https://hypertidy.github.io/wkpool/reference/segments_to_wkt.md),
[`arcs_to_wkt()`](https://hypertidy.github.io/wkpool/reference/arcs_to_wkt.md),
[`cycles_to_wkt()`](https://hypertidy.github.io/wkpool/reference/cycles_to_wkt.md)
and their WKB counterparts.

## Usage

``` r
# S3 method for class 'wkpool'
wk_crs(x)

# S3 method for class 'wkpool'
wk_set_crs(x, crs)

# S3 method for class 'wkpool'
wk_is_geodesic(x)

# S3 method for class 'wkpool'
wk_set_geodesic(x, geodesic)
```

## Arguments

- x:

  A wkpool object.

- crs:

  A CRS object (commonly an authority string such as "EPSG:4326"), or
  `NULL`.

- geodesic:

  `TRUE`, `FALSE`, or `NA` (inherit).

## Value

- [`wk_crs()`](https://paleolimbot.github.io/wk/reference/wk_crs.html):
  the CRS object, or `NULL`.

- [`wk_set_crs()`](https://paleolimbot.github.io/wk/reference/wk_crs.html),
  [`wk_set_geodesic()`](https://paleolimbot.github.io/wk/reference/wk_is_geodesic.html):
  `x` with the attribute replaced.

- [`wk_is_geodesic()`](https://paleolimbot.github.io/wk/reference/wk_is_geodesic.html):
  `TRUE`, `FALSE`, or `NA`.

## Examples

``` r
x <- wk::wkt("LINESTRING (0 0, 1 1)", crs = "EPSG:4326")
pool <- establish_topology(x)
wk::wk_crs(pool)
#> [1] "EPSG:4326"
wk::wk_crs(wk::wk_set_crs(pool, "EPSG:3031"))
#> [1] "EPSG:3031"
```

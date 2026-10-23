# Handle wkpool objects with wk handlers

A wkpool is wk-handleable: it presents as one LINESTRING per segment (so
the handled vector has the same length as the pool vector), with the
pool's crs and geodesic flag attached. This means the wk ecosystem's
generic conversions work directly on a pool:
[`wk::as_wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html),
[`wk::as_wkt()`](https://paleolimbot.github.io/wk/reference/wkt.html),
[`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html),
`geos::as_geos_geometry()`, plotting, and anything else built on
[`wk::wk_handle()`](https://paleolimbot.github.io/wk/reference/wk_handle.html).

## Usage

``` r
# S3 method for class 'wkpool'
wk_handle(handleable, handler, ...)
```

## Arguments

- handleable:

  A wkpool object.

- handler:

  A [wk
  handler](https://paleolimbot.github.io/wk/reference/wk_handle.html).

- ...:

  Passed on.

## Value

The result of the handler.

## Details

Vertices are emitted at full double precision, indexed directly from the
pool (no text round trip). If the pool carries `z`, coordinates are
emitted as XYZ.

Arcs and cycles remain explicit conversions
([`arcs_to_wkt()`](https://hypertidy.github.io/wkpool/reference/arcs_to_wkt.md),
[`cycles_to_wkt()`](https://hypertidy.github.io/wkpool/reference/cycles_to_wkt.md)
and friends): the handled form is deliberately the segment vector
itself.

## Examples

``` r
x <- wk::wkt(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  crs = "EPSG:4326"
)
pool <- establish_topology(x)
wk::as_wkt(pool)
#> <wk_wkt[4] with CRS=EPSG:4326>
#> [1] LINESTRING (0 0, 1 0) LINESTRING (1 0, 1 1) LINESTRING (1 1, 0 1)
#> [4] LINESTRING (0 1, 0 0)
wk::wk_coords(pool)
#>   feature_id part_id ring_id x y
#> 1          1       1       0 0 0
#> 2          1       1       0 1 0
#> 3          2       2       0 1 0
#> 4          2       2       0 1 1
#> 5          3       3       0 1 1
#> 6          3       3       0 0 1
#> 7          4       4       0 0 1
#> 8          4       4       0 0 0
wk::wk_crs(wk::as_wkb(pool))
#> [1] "EPSG:4326"
```

# Create a wkpool from a vertex table and a segment table

The user-level constructor: give a vertex table and a segment table (as
returned by
[`pool_vertices()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)
and
[`pool_segments()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md))
and get a wkpool back. It is a thin wrapper around
[`new_wkpool()`](https://hypertidy.github.io/wkpool/reference/new_wkpool.md),
which checks that every segment endpoint is a vertex id present in the
pool.

## Usage

``` r
wkpool(vertices, segments, paths = NULL, crs = NULL, geodesic = FALSE)
```

## Arguments

- vertices:

  A data.frame with columns `.vx` (integer vertex ids), `x`, `y`, and
  optionally `z`.

- segments:

  A data.frame with integer columns `.vx0` and `.vx1` (vertex ids), and
  optionally `.feature` and `.path`.

- paths:

  Optional data.frame describing the paths: columns `.path`, `.feature`,
  `.part`, `.ring` (as captured from
  [`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html)
  identifiers by
  [`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md)).

- crs:

  A CRS object (commonly an authority string such as "EPSG:4326"),
  carried but never interpreted, per wk's CRS propagation model. Use
  `NULL` for none, or
  [`wk::wk_crs_inherit()`](https://paleolimbot.github.io/wk/reference/wk_crs_inherit.html).

- geodesic:

  `TRUE` if segments should be interpreted as geodesics when coordinates
  are spherical, `FALSE` otherwise, `NA` to inherit.

## Value

A wkpool object.

## See also

[`new_wkpool()`](https://hypertidy.github.io/wkpool/reference/new_wkpool.md)
for the low-level constructor,
[`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md)
to build a pool from wk geometry.

## Examples

``` r
v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
s <- data.frame(.vx0 = 1:4, .vx1 = c(2:4, 1L), .feature = 1L)
pool <- wkpool(v, s)
pool
#> <wkpool[4 segments, 4 vertices]>
#> [1] <segment: 1->2> <segment: 2->3> <segment: 3->4> <segment: 4->1>

# round trip through the accessors
wkpool(pool_vertices(pool), pool_segments(pool))
#> <wkpool[4 segments, 4 vertices]>
#> [1] <segment: 1->2> <segment: 2->3> <segment: 3->4> <segment: 4->1>
```

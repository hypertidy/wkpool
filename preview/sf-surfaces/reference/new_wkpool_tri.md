# Construct a triangle pool

A low-level constructor for `wkpool_tri` objects: a vector of triangles
whose corners are vertex ids into a shared vertex pool, the same pool
structure a
[wkpool](https://hypertidy.github.io/wkpool/reference/new_wkpool.md)
uses for segments.

## Usage

``` r
new_wkpool_tri(
  vertices,
  vx0,
  vx1,
  vx2,
  feature = NULL,
  part = NULL,
  paths = NULL,
  crs = NULL,
  geodesic = FALSE
)
```

## Arguments

- vertices:

  A data.frame with columns `.vx` (integer vertex ids), `x`, `y`, and
  optionally `z`.

- vx0, vx1, vx2:

  Integer vectors of triangle corner vertex ids, each value present in
  `vertices$.vx`.

- feature:

  Optional integer vector of feature ids, one per triangle.

- part:

  Optional integer vector of part ids, one per triangle (the polygon
  within a multipolygon feature, as identified by
  [`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html)).

- paths:

  Optional data.frame describing the input paths (see
  [`pool_paths()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)),
  carried from the segment pool.

- crs:

  A CRS object (commonly an authority string such as "EPSG:4326"),
  carried but never interpreted, per wk's CRS propagation model. Use
  `NULL` for none, or
  [`wk::wk_crs_inherit()`](https://paleolimbot.github.io/wk/reference/wk_crs_inherit.html).

- geodesic:

  `TRUE` if segments should be interpreted as geodesics when coordinates
  are spherical, `FALSE` otherwise, `NA` to inherit.

## Value

A `wkpool_tri` object.

## Details

The `wkpool_tri` class is experimental: how a triangle records the
polygon part it came from (currently a `.part` field) may change in a
later release.

## Examples

``` r
v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
#> <wkpool_tri[2 triangles, 4 vertices]>
#> [1] <triangle: 1,2,3> <triangle: 1,3,4>
```

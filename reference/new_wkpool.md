# Construct a wkpool from validated components

A low-level constructor for wkpool objects, for use by packages that
build pools directly (for example after refining or transforming an
existing pool). Inputs are checked against the wkpool invariants: every
segment endpoint must be a vertex id present in the pool.

## Usage

``` r
new_wkpool(
  vertices,
  vx0,
  vx1,
  feature = NULL,
  path = NULL,
  paths = NULL,
  crs = NULL,
  geodesic = FALSE
)
```

## Arguments

- vertices:

  A data.frame with columns `.vx` (integer vertex ids), `x`, `y`, and
  optionally `z`.

- vx0, vx1:

  Integer vectors of segment start/end vertex ids, each value present in
  `vertices$.vx`.

- feature:

  Optional integer vector of feature ids, one per segment.

- path:

  Optional integer vector of path ids, one per segment. A path is a
  maximal run of segments minted from one input ring or linestring; it
  is the provenance that lets cycles, ring roles and feature structure
  be recovered exactly.

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

## Examples

``` r
v <- data.frame(.vx = 1:3, x = c(0, 1, 1), y = c(0, 0, 1))
new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(2L, 3L), crs = "EPSG:4326")
#> <wkpool[2 segments, 3 vertices] CRS=EPSG:4326>
#> [1] <segment: 1->2> <segment: 2->3>
```

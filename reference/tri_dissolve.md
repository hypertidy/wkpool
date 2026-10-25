# Dissolve triangles into polygons by group

Merge the triangles of a triangle pool into one MULTIPOLYGON per group,
along shared triangle edges. With the default `group`, the triangles of
each feature are dissolved back into that feature's polygons.

## Usage

``` r
tri_dissolve(x, group = NULL, ...)
```

## Arguments

- x:

  A `wkpool_tri`.

- group:

  Integer group id per triangle (1-based, `NA` to leave a triangle out).
  The default is the triangles' feature, or one group when the pool has
  no feature.

- ...:

  Unused.

## Value

A [`wk::wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html)
vector of MULTIPOLYGON, one per group id from 1 to the largest in
`group`, with the pool's crs.

## Details

`group` is one integer per triangle: a dense, 1-based group id, with
`NA` for a triangle that belongs to no group (it is left out, and its
edges become boundary). This is the `set_id` of a labelled mesh, so a
mesh labelled by the covering input features (an overlay) dissolves into
its pieces with `tri_dissolve(x, set_id)`, while the table from set to
features is kept by the caller. Element `k` of the result is group `k`;
a group id with no triangles gives `MULTIPOLYGON EMPTY`.

Triangles must be counter-clockwise and form a triangulation (no
overlaps), with adjacent triangles sharing vertex ids: run
[`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md)
first on a pool whose triangles do not already share vertices. Triangles
are joined only across an edge whose two triangles share both of its
vertex ids, so triangles that touch at a vertex or along part of an edge
stay separate.

Rings follow the edges of the triangles, so pieces that touch share
vertices exactly. Outer rings are counter-clockwise and holes clockwise.
A group has one polygon for each edge-connected part, and a boundary
that passes through a vertex twice (a hole touching its outer ring, for
example) is split there, so every ring is simple and rings meet only at
points. Only x and y are used.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.3, 0.2 0.5, 0.4 0.5, 0.2 0.3))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
if (requireNamespace("decido", quietly = TRUE)) {
  tri <- merge_coincident(pool_triangulate(establish_topology(x)))
  ## back to the input polygons
  wk::as_wkt(tri_dissolve(tri))
  ## everything as one polygon, with the hole kept
  wk::as_wkt(tri_dissolve(tri, rep(1L, length(tri))))
}
#> <wk_wkt[1]>
#> [1] MULTIPOLYGON (((0 1, 0 0, 1 0, 2 0, 2 1, 1 1...
```

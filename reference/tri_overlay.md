# Overlay two layers in one mesh

Put the features of two layers into one triangulation in which every
place they meet is a shared vertex or edge, so that relations between
them can be read from the mesh alone (see
[`tri_overlay_relate()`](https://hypertidy.github.io/wkpool/reference/tri_overlay_relate.md)).

## Usage

``` r
tri_overlay(x, y = NULL, x_group = NULL, y_group = NULL)
```

## Arguments

- x, y:

  Layers, as above. `y` may be `NULL` to put one layer alone into a mesh
  (lines or points have no mesh of their own).

- x_group, y_group:

  Groups for a polygon layer (as `group` in
  [`tri_measures()`](https://hypertidy.github.io/wkpool/reference/tri_measures.md));
  ignored for lines and points.

## Value

A `wkpool_overlay`: a list with

- `mesh`, the overlay as a `wkpool_tri` (vertex column `origin` is
  `"input"` or `"crossing"`);

- `groups`, a data frame with one row per group of the overlay: `layer`
  (`"x"` or `"y"`), `id` (the group id in its layer), `dim` (2 polygons,
  1 lines, 0 points);

- `members`, a data frame of `group` (row of `groups`), `item` and
  `item2`: a triangle row of `mesh` for polygons, the two vertex rows of
  an edge for lines, a vertex row for points (`item2` is `NA` except for
  lines).

## Details

A layer is one of:

- a `wkpool_tri`, for polygons: groups of its triangles, as in
  [`tri_measures()`](https://hypertidy.github.io/wkpool/reference/tri_measures.md);

- a [wkpool](https://hypertidy.github.io/wkpool/reference/wkpool.md) of
  segments, for lines: one line per feature (one line in all when the
  pool has no feature);

- any geometry that wk can read holding points or lines (not polygons;
  mesh those first), one feature per input geometry.

The boundary edges of every polygon group and every line segment become
constraints of one constrained Delaunay triangulation of all the layers'
vertices and points
([`cdtr::cdt_triangulate()`](https://hypertidy.github.io/cdtr/reference/cdt_triangulate.html)).
Where constraints cross, the crossing point is added as a vertex
(computed in floating point, so a relation that hinges on a crossing
point is exact for the overlay rather than for the inputs). Each
triangle of the overlay then belongs to the polygon groups that hold its
centroid in the source mesh, and each line is followed along the
overlay's fixed edges. The centroid is that of the largest triangle
among those joined to it without crossing a constraint (they all lie in
the same groups), so that slivers along a constraint are labelled from a
triangle well inside.

## See also

[`tri_overlay_relate()`](https://hypertidy.github.io/wkpool/reference/tri_overlay_relate.md)

## Examples

``` r
if (requireNamespace("cdtr", quietly = TRUE)) {
  ## a unit square as two triangles, and a line across it
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  sq <- new_wkpool_tri(v, vx0 = c(1, 1), vx1 = c(2, 3), vx2 = c(3, 4))
  ln <- wk::wkt(c("LINESTRING (-1 0.5, 2 0.5)", "LINESTRING (2 2, 3 3)"))
  ov <- tri_overlay(sq, ln)
  ov$groups
  tri_overlay_relate(ov)
}
#>   x y    relate
#> 1 1 1 1F20F1102
```

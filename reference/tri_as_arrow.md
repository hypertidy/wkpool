# Triangles as an Arrow array

Write a triangle pool as an Arrow array, built with nanoarrow, in the
layout of the experimental `geoarrow.triangles` type: each triangle is
three indices into one vertex dictionary (a struct of `x` and `y`) that
every triangle shares, so a vertex used by many triangles is stored once
and a shared boundary is a shared index.

## Usage

``` r
tri_as_arrow(x, group = NULL, layout = c("feature", "triangle"))
```

## Arguments

- x:

  A `wkpool_tri`.

- group:

  Groups of triangles. Either one integer per triangle (a dense, 1-based
  group id, `NA` to leave a triangle out), or a data frame with columns
  `triangle` (row in `x`) and `group`, for groups that overlap (a
  triangle in several groups, as in an overlay). The default is the
  triangles' feature, or one group when the pool has no feature.

- layout:

  `"feature"` (one row per group) or `"triangle"` (one row per triangle;
  `group` is ignored).

## Value

A `nanoarrow_array`.

## Details

With `layout = "feature"` (the default) each row is one group (a
polygonal feature) and its value is the list of the group's triangles:
`List<FixedSizeList<Dictionary<int32, Coordinate>>[3]>`. The list
offsets are then the feature-to-triangle ranges, so an engine that
evaluates `ST_*` per row needs no grouping column. A triangle in several
groups is written in each of their rows (three more indices, no new
vertices). With `layout = "triangle"` each row is one triangle
(`FixedSizeList<Dictionary<int32, Coordinate>>[3]`), in pool order, the
storage of the mesh itself.

Vertices are numbered by first appearance in the written triangles, and
vertices no triangle uses are not written. The extension metadata holds
`"edges": "planar"` and the pool's crs (as a string) when it has one.
The `geoarrow.triangles` type is a draft and not part of GeoArrow; the
layout may change.

## See also

[`tri_from_arrow()`](https://hypertidy.github.io/wkpool/reference/tri_from_arrow.md)
to read it back.

## Examples

``` r
if (requireNamespace("nanoarrow", quietly = TRUE)) {
  v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
  tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                        vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
  a <- tri_as_arrow(tri)
  a$length
  back <- tri_from_arrow(a)
  tri_measures(back$mesh, back$group)
}
#>   group area perimeter centroid_x centroid_y   point_x   point_y xmin ymin xmax
#> 1     1    1         4        0.5        0.5 0.6666667 0.3333333    0    0    1
#> 2     2    1         4        1.5        0.5 1.6666667 0.3333333    1    0    2
#>   ymax
#> 1    1
#> 2    1
```

# Relations between the layers of an overlay

The DE-9IM matrix (as `ST_Relate()` returns it) between features of the
two layers of a
[`tri_overlay()`](https://hypertidy.github.io/wkpool/reference/tri_overlay.md),
or between features of its one layer, read from the overlay mesh's
triangles, edges and vertices.

## Usage

``` r
tri_overlay_relate(x, pairs = NULL)
```

## Arguments

- x:

  A `wkpool_overlay` from
  [`tri_overlay()`](https://hypertidy.github.io/wkpool/reference/tri_overlay.md).

- pairs:

  `NULL` for every pair of features (one from each layer, or two of the
  one layer) whose closures share a vertex, the pairs that are not
  disjoint; or a two-column matrix or data frame of feature ids, the
  first in layer `x` and the second in layer `y` (or both in `x` for a
  single layer).

## Value

A data frame with columns `x` and `y` (feature ids in their layers) and
`relate` (the nine-character DE-9IM string, interior, boundary and
exterior of the first by those of the second).

## Details

Every point of the plane is in one open triangle, open edge or vertex of
the mesh, or outside it, and each of those is in the interior, boundary
or exterior of a feature. For polygons the classes are those of
[`tri_relate()`](https://hypertidy.github.io/wkpool/reference/tri_relate.md).
A line's interior is its edges and the vertices inside it, its boundary
the vertices where an odd number of its edges end (the mod-2 rule that
GEOS uses by default). A point set's interior is its vertices and it has
no boundary. Each entry is the largest dimension of a cell in both
classes.

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

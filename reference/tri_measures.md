# Measures of groups of triangles

Area, perimeter, centroid, a point inside, and extent of each group of
triangles, computed from the triangles without forming polygons. These
are what `ST_Area()`, `ST_Perimeter()`, `ST_Centroid()`,
`ST_PointOnSurface()` and `ST_Envelope()` return for the polygons the
groups cover.

## Usage

``` r
tri_measures(x, group = NULL)
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

## Value

A data frame with one row per group id from 1 to the largest, and
columns `group`, `area`, `perimeter`, `centroid_x`, `centroid_y`,
`point_x`, `point_y`, `xmin`, `ymin`, `xmax`, `ymax`. A group with no
triangles has zero area and perimeter and `NA` elsewhere.

## Details

The area is the sum of triangle areas and the centroid their
area-weighted mean. The perimeter is the length of the edges whose other
side is not in the group. The point inside is the centroid of the
group's largest triangle, so it is inside the group by construction.
Triangles must be counter-clockwise (a clockwise triangle counts
negative area).

## See also

[`tri_dissolve()`](https://hypertidy.github.io/wkpool/reference/tri_dissolve.md)
for the polygons themselves.

## Examples

``` r
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
tri_measures(tri)
#>   group area perimeter centroid_x centroid_y   point_x   point_y xmin ymin xmax
#> 1     1    1         4        0.5        0.5 0.6666667 0.3333333    0    0    1
#> 2     2    1         4        1.5        0.5 1.6666667 0.3333333    1    0    2
#>   ymax
#> 1    1
#> 2    1
## one group over both squares
tri_measures(tri, rep(1L, 4))
#>   group area perimeter centroid_x centroid_y   point_x   point_y xmin ymin xmax
#> 1     1    2         6          1        0.5 0.6666667 0.3333333    0    0    2
#>   ymax
#> 1    1
```

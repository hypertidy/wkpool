# Convex hulls of groups of triangles

`ST_ConvexHull()` of the polygons the groups cover, from the triangles'
vertices.

## Usage

``` r
tri_convex_hull(x, group = NULL)
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

A [`wk::wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html)
vector of POLYGON, one per group id from 1 to the largest
(`POLYGON EMPTY` for a group with fewer than three distinct points),
with the pool's crs. Rings are counter-clockwise.

## Examples

``` r
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
wk::as_wkt(tri_convex_hull(tri, rep(1L, 4)))
#> <wk_wkt[1]>
#> [1] POLYGON ((0 0, 2 0, 2 1, 0 1, 0 0))
```

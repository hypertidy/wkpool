# Locate points in a triangle pool

The triangle that holds each point: point in polygon (`ST_Intersects()`
or `ST_Contains()` of a point) answered by point location on the mesh.

## Usage

``` r
tri_locate(x, at)
```

## Arguments

- x:

  A `wkpool_tri`.

- at:

  The points: any geometry
  [`wk::wk_handle()`](https://paleolimbot.github.io/wk/reference/wk_handle.html)
  understands (its vertices are used, as
  [`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html)
  gives them), a two-column matrix, or a data frame or list with `x` and
  `y`.

## Value

An integer vector with one triangle row of `x` per point, `NA` where no
triangle holds it.

## Details

Triangles are bucketed on a uniform grid over the extent of the mesh,
and each candidate is tested with an exact orientation sign, so a point
on an edge or a vertex is decided the same way for every triangle that
holds it, and goes to the lowest triangle row among them. A point inside
no triangle, or with a missing coordinate, is `NA`. Triangles may be
either orientation; a degenerate triangle (zero area) holds no point.

The feature of the polygon each point falls in is
`pool_feature(x)[tri_locate(x, at)]`, and the group of any grouping (as
in
[`tri_measures()`](https://hypertidy.github.io/wkpool/reference/tri_measures.md))
is found the same way.

## Examples

``` r
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
pts <- wk::xy(c(0.75, 0.25, 1.5, 3), c(0.25, 0.75, 0.5, 0.5))
i <- tri_locate(tri, pts)
i
#> [1]  1  2  3 NA
## the feature each point falls in
pool_feature(tri)[i]
#> [1]  1  1  2 NA
```

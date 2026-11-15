# Clip triangles by a rectangle

Keep the parts of the triangles inside a rectangle, as triangles again:
`ST_ClipByBox2D()` that returns a mesh.

## Usage

``` r
tri_clip_rect(x, rect)
```

## Arguments

- x:

  A `wkpool_tri`.

- rect:

  The rectangle: a
  [`wk::rct()`](https://paleolimbot.github.io/wk/reference/rct.html) or
  `c(xmin, ymin, xmax, ymax)`.

## Value

A `wkpool_tri` with the input vertices plus the new ones (`origin`
column `"clip"` for new vertices when the pool has an `origin` column;
use
[`pool_compact()`](https://hypertidy.github.io/wkpool/reference/pool_compact.md)
to drop unused vertices), keeping the feature and part of each source
triangle. The source triangle row of each output triangle is in
attribute `"source"`.

## Details

Triangles inside the rectangle are kept as they are, triangles outside
are dropped, and each triangle that crosses a side is cut to a convex
polygon and fanned back into triangles. New vertices where edges cross
the sides are added to the vertex pool, and a point where a shared edge
crosses a side is computed the same way from both of its triangles, so
neighbouring pieces share it exactly. Slivers of zero area on a side are
dropped.

## Examples

``` r
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
clipped <- tri_clip_rect(tri, c(0.5, 0.25, 1.5, 0.75))
tri_measures(clipped)
#>   group area perimeter centroid_x centroid_y point_x   point_y xmin ymin xmax
#> 1     1 0.25         2       0.75        0.5    0.75 0.5000000  0.5 0.25  1.0
#> 2     2 0.25         2       1.25        0.5    1.25 0.5833333  1.0 0.25  1.5
#>   ymax
#> 1 0.75
#> 2 0.75
```

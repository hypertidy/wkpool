# Boundary edges of groups of triangles

The edges between a group and what is not in it, as a segment pool on
the triangles' vertex pool. This is `ST_Boundary()` of the polygons the
groups cover, with shared boundaries on shared vertices.

## Usage

``` r
tri_boundary(x, group = NULL, once = FALSE)
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

- once:

  Keep an edge shared by two groups only once?

## Value

A [wkpool](https://hypertidy.github.io/wkpool/reference/wkpool.md) of
segments with the group as its feature.

## Details

Each edge is directed with its group on the left, so outer boundaries
run counter-clockwise and holes clockwise. With `once = TRUE` an edge
between two groups is kept for the smaller group id only, so every line
is drawn once (see
[`find_arcs()`](https://hypertidy.github.io/wkpool/reference/find_arcs.md)
to merge the edges into arcs).

## Examples

``` r
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
tri_boundary(tri)
#> <wkpool[8 segments, 6 vertices]>
#> [1] <segment: 1->2> <segment: 2->3> <segment: 3->4> <segment: 4->1>
#> [5] <segment: 2->5> <segment: 5->6> <segment: 6->3> <segment: 3->2>
## the middle edge once
length(tri_boundary(tri, once = TRUE))
#> [1] 7
```

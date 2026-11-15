# Neighbouring triangles

The edge table of a triangle pool: for each triangle, the triangle on
the other side of each of its three edges.

## Usage

``` r
tri_neighbours(x)
```

## Arguments

- x:

  A `wkpool_tri`.

## Value

An integer matrix with one row per triangle and three columns: column
`k` is the triangle across the edge from corner `k` to corner `k + 1`
(corner 3 to corner 1 for the third), `NA` where there is none.

## Details

Two triangles are neighbours across an edge when both hold its two
vertex ids, in opposite order (as counter-clockwise triangles that share
an edge do). An edge on the outside of the mesh, or one used by more
than two triangles, has no neighbour. Run
[`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md)
first on a pool whose triangles do not already share vertex ids.

## Examples

``` r
v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
tri <- new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
tri_neighbours(tri)
#>      [,1] [,2] [,3]
#> [1,]   NA   NA    2
#> [2,]    1   NA   NA
```

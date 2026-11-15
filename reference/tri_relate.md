# Relations between groups of triangles

The DE-9IM matrix (as `ST_Relate()` returns it) between pairs of groups
of one triangle mesh, computed from shared triangles, edges and vertices
alone, without reading coordinates.

## Usage

``` r
tri_relate(x, group = NULL, pairs = NULL)
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

- pairs:

  `NULL` for every pair of groups that share a vertex (all pairs that
  are not disjoint), or a two-column matrix or data frame of group ids.

## Value

A data frame with columns `a`, `b` (group ids) and `relate` (the
nine-character DE-9IM string, interior, boundary, exterior of `a` by
those of `b`).

## Details

Every point of the plane lies in exactly one open triangle, open edge or
vertex of the mesh, or outside it, and each of those lies wholly in the
interior, boundary or exterior of a group. A triangle is interior when
it is in the group. An edge is interior when the triangles on both sides
are, boundary when one is (outside the mesh is never in a group). A
vertex is interior when every triangle around it is in the group and it
is not on the outside of the mesh, exterior when none is, and boundary
otherwise. Each matrix entry is the largest dimension of a mesh element
in both classes: `"2"` for a triangle, `"1"` for an edge, `"0"` for a
vertex, `"F"` for none.

The answer is exact for the mesh as given. It equals the relation of the
polygons the groups cover when the mesh was built so that every place
two inputs meet is a shared vertex, as a constrained triangulation that
resolves crossings does. Pairs of groups that share no vertex are
disjoint, `"FF2FF1212"`, and are not listed when `pairs` is `NULL`.

## Examples

``` r
## a 2 x 1 strip of unit squares and a group covering both
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3))
g <- data.frame(triangle = c(1, 2, 3, 4, 1, 2, 3, 4),
                group = c(1, 1, 2, 2, 3, 3, 3, 3))
tri_relate(tri, g)
#>   a b    relate
#> 1 1 2 FF2F11212
#> 2 1 3 2FF11F212
#> 3 2 3 2FF11F212
```

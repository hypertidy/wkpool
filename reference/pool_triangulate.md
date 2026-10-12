# Triangulate the polygons in a pool

Triangulate each polygon of a
[`wkpool()`](https://hypertidy.github.io/wkpool/reference/wkpool.md)
(built by
[`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md))
and return the triangles as a `wkpool_tri` on the same vertex pool.

## Usage

``` r
pool_triangulate(x, engine = c("decido"), ...)
```

## Arguments

- x:

  A wkpool with path provenance (from
  [`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md)).

- engine:

  Triangulation engine, currently only `"decido"`.

- ...:

  Unused.

## Value

A `wkpool_tri`.

## Details

The `wkpool_tri` class is experimental: how a triangle records the
polygon part it came from (currently a `.part` field) may change in a
later release.

The `"decido"` engine is ear cutting
([`decido::earcut()`](https://hypertidy.github.io/decido/reference/earcut.html)),
which never creates vertices: every triangle corner is an existing
vertex of the pool, so the vertex table is passed through unchanged.
Each polygon (each part of a multipolygon) is triangulated separately,
with its first ring as the boundary and later rings as holes, using the
path provenance recorded by
[`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md).
Paths that are not closed rings (linestrings) are ignored.

The pool can be triangulated before or after
[`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md).
On a raw pool each ring's closing coordinate is its own vertex; it is
dropped from the ring and never used by a triangle. Merging the triangle
pool later with
[`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md)
gives adjacent features shared triangle corners.

Triangles are counter-clockwise.

For constrained Delaunay triangulation, with Steiner points added for
quality or area, pass
[`as_pslg()`](https://hypertidy.github.io/wkpool/reference/as_pslg.md)
to RTriangle; see
[`vignette("triangulation", package = "wkpool")`](https://hypertidy.github.io/wkpool/articles/triangulation.md).

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.2 0.4, 0.4 0.4, 0.2 0.2))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
if (requireNamespace("decido", quietly = TRUE)) {
  tri <- pool_triangulate(pool)
  tri
  pool_triangles(tri)
  wk::as_wkt(tri)
}
#> <wk_wkt[8]>
#> [1] POLYGON ((0 1, 0 0, 0.2 0.2, 0 1))    
#> [2] POLYGON ((0.4 0.4, 0 0, 1 0, 0.4 0.4))
#> [3] POLYGON ((1 0, 1 1, 0 1, 1 0))        
#> [4] POLYGON ((0 1, 0.2 0.2, 0.2 0.4, 0 1))
#> [5] POLYGON ((0.4 0.4, 1 0, 0 1, 0.4 0.4))
#> [6] POLYGON ((0 1, 0.2 0.4, 0.4 0.4, 0 1))
#> [7] POLYGON ((2 1, 1 1, 1 0, 2 1))        
#> [8] POLYGON ((1 0, 2 0, 2 1, 1 0))        
```

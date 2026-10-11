# Merge coincident vertices in a pool

Merge coincident vertices in a pool

## Usage

``` r
merge_coincident(x, tolerance = 0, period_x = NULL)
```

## Arguments

- x:

  A wkpool, or a `wkpool_tri` (see
  [`pool_triangulate()`](https://hypertidy.github.io/wkpool/reference/pool_triangulate.md))

- tolerance:

  Numeric tolerance for coordinate matching. Default 0 means exact match
  only.

- period_x:

  `NULL` (default) for no wrapping, or a single positive number: x is
  treated as periodic with this period when deciding which vertices
  coincide (for example 360 for longitude in degrees). See 'Periodic x'
  below.

## Value

A wkpool (or `wkpool_tri`) with shared vertices merged (fewer unique
vertices)

## Details

With tolerance = 0, only exactly identical coordinates are merged -
identity is the double-precision bit pattern (grouped at C level via
vctrs), not a formatted representation, so values that differ only
beyond printable precision remain distinct. The first occurrence becomes
the canonical vertex; coordinates are never moved.

With a nonzero tolerance, coordinates are snapped to a grid of cell size
`tolerance` for matching (the canonical vertex keeps its original
coordinates). Note this is a snap grid, not a distance guarantee: two
vertices within `tolerance` of each other that straddle a grid-cell
boundary do not merge.

When the pool carries `z`, vertices are keyed on all coordinate columns:
equal x/y with different z remain distinct vertices.

This is where you discover shared boundaries between polygons, network
connectivity, mesh topology, etc.

## Periodic x

With `period_x` set, vertices are keyed on `x %% period_x` (then on the
tolerance snap grid, if any, wrapped again so that the grid cell at the
period boundary is the same cell as the one at zero). This fuses the two
copies of a seam, for example x = -180 and x = 180 for longitude, and
with `period_x = 360` data in a 0 to 360 convention fuses at 0 and 360
in the same way. Once fused, a seam is an ordinary internal boundary:
[`find_shared_edges()`](https://hypertidy.github.io/wkpool/reference/find_shared_edges.md),
[`find_internal_boundaries()`](https://hypertidy.github.io/wkpool/reference/find_internal_boundaries.md)
and the `quotient` argument of
[`find_arcs()`](https://hypertidy.github.io/wkpool/reference/find_arcs.md)
treat it like any other shared boundary.

The period is never inferred: wkpool carries the crs without
interpreting it, so it cannot know that x is longitude. The decision to
fuse is made here, explicitly.

Coordinates are still never moved: the canonical vertex keeps the x of
its first occurrence (say -180), and segments from the other side of the
seam now reference it. The topology is correct on the cylinder, but
geometry emitted from the pool (for example by
[`cycles_to_wkb()`](https://hypertidy.github.io/wkpool/reference/cycles_to_wkt.md))
has coordinate jumps of about one period across the seam. Consumers that
understand wrapping (s2, great-circle densification) are fine; a planar
consumer needs the seam split again downstream.

Segments are identified by their two vertices, so on the cylinder a
segment cannot say which way round it goes. Two segments whose ends fuse
to the same pair but which run opposite ways around become the same
edge, for example the edge from -180 to 0 in one feature and from 0 to
180 in another. This needs a segment that spans at least half the
period, so densify long segments before merging with `period_x`.

Only x is periodic. Poles (where all x coincide on the sphere), periodic
y, and any detection from the crs are out of scope.

Feature provenance (.feature) is preserved - segments keep their
original feature assignment even after vertex merging.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
topology_report(pool)
#> $n_vertices_raw
#> [1] 10
#> 
#> $n_vertices_unique
#> [1] 6
#> 
#> $n_duplicate_vertices
#> [1] 4
#> 
#> $n_near_miss_vertices
#> [1] 0
#> 
#> $n_segments
#> [1] 8
#> 
#> $n_shared_edges
#> [1] 1
#> 
#> $n_features
#> [1] 2
#> 
merged <- merge_coincident(pool)
topology_report(merged)
#> $n_vertices_raw
#> [1] 6
#> 
#> $n_vertices_unique
#> [1] 6
#> 
#> $n_duplicate_vertices
#> [1] 0
#> 
#> $n_near_miss_vertices
#> [1] 0
#> 
#> $n_segments
#> [1] 8
#> 
#> $n_shared_edges
#> [1] 1
#> 
#> $n_features
#> [1] 2
#> 

# two hemispheres, each with its own copy of the antimeridian
g <- wk::as_wkb(c(
  "POLYGON ((-180 -60, 0 -60, 0 60, -180 60, -180 -60))",
  "POLYGON ((0 -60, 180 -60, 180 60, 0 60, 0 -60))"
))
pool <- establish_topology(g)
nrow(pool_vertices(merge_coincident(pool)))
#> [1] 6
fused <- merge_coincident(pool, period_x = 360)
nrow(pool_vertices(fused))
#> [1] 4
find_shared_edges(fused)
#>   edge_key .vx0 .vx1 .feature segment_idx features
#> 1      1-2    1    2        1           1     1, 2
#> 2      2-3    2    3        1           2     1, 2
#> 3      3-4    3    4        1           3     1, 2
#> 4      1-4    1    4        1           4     1, 2
#> 5      1-2    1    2        2           5     1, 2
#> 6      1-4    1    4        2           6     1, 2
#> 7      3-4    3    4        2           7     1, 2
#> 8      2-3    2    3        2           8     1, 2
```

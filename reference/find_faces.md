# Find the faces of a segment graph

Enumerate the faces of the graph formed by a pool's segments: the
regions it encloses (street blocks between roads, parcels between
boundaries), plus one outer face for each connected component.

## Usage

``` r
find_faces(x)
```

## Arguments

- x:

  A wkpool (ideally after
  [`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md)).

## Value

A data frame with one row per half-edge, in walk order face by face:

- `.face`: face id, 1-based, in order of the lowest half-edge on it.

- `.vx0`, `.vx1`: the half-edge's start and end vertex ids.

- `.segment`: index of the input segment (in `x`) it comes from.

- `.adjacent`: the face on the other side of the edge.

- `.face_area`: signed area of the face (positive for bounded faces).

- `.bridge`: `TRUE` when `.adjacent` is the same face.

## Details

Segments are taken as undirected edges. A boundary carried by more than
one feature (the same pair of vertex ids) counts once, keeping the first
segment, and segments from a vertex to itself are dropped. Run
[`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md)
first so that segments meeting at the same coordinates share a vertex:
faces are traced through shared vertex ids only, so lines that cross
without a shared vertex (a bridge over a road) do not split each other,
and their faces can overlap.

Each edge gives two half-edges, one per direction. At each vertex the
outgoing half-edges are ordered counter-clockwise by angle, and a walk
always takes the next edge clockwise from the one it arrived on, which
keeps the face on its left. Every half-edge lies on exactly one face, so
the result has two rows per edge. Bounded faces are counter-clockwise
(positive area); the outer face of each connected component is clockwise
(negative area), or zero for a component that encloses nothing (a tree).

An edge with the same face on both sides (`.bridge`) is a dead end, or a
bridge joining two parts of the graph; its two half-edges both appear in
that face's walk. Drop those rows to keep only edges that separate two
faces.

Only x and y are used.

## See also

[`find_neighbours()`](https://hypertidy.github.io/wkpool/reference/find_neighbours.md)
for adjacency between input features,
[`find_cycles()`](https://hypertidy.github.io/wkpool/reference/find_cycles.md)
for the rings of polygon input.

## Examples

``` r
## a square with a cross inside: four blocks
x <- wk::as_wkb(c(
  "LINESTRING (0 0, 1 0, 2 0, 2 1, 2 2, 1 2, 0 2, 0 1, 0 0)",
  "LINESTRING (1 0, 1 1, 1 2)",
  "LINESTRING (0 1, 1 1, 2 1)"
))
pool <- merge_coincident(establish_topology(x))
f <- find_faces(pool)
## bounded faces and their areas
unique(f[f$.face_area > 0, c(".face", ".face_area")])
#>    .face .face_area
#> 1      1          1
#> 13     3          1
#> 17     4          1
#> 21     5          1
## which blocks share an edge
unique(f[f$.face_area > 0 & f$.face != f$.adjacent, c(".face", ".adjacent")])
#>    .face .adjacent
#> 1      1         2
#> 2      1         3
#> 3      1         5
#> 13     3         2
#> 15     3         4
#> 16     3         1
#> 17     4         2
#> 19     4         5
#> 20     4         3
#> 21     5         2
#> 23     5         1
#> 24     5         4
```

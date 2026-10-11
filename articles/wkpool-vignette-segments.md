# Segments: the atoms of geometry

## What is a segment?

Two connected vertices.

A segment is the atom of geometry. Every line is a sequence of segments.
Every polygon boundary is a cycle of segments. Every track step between
consecutive GPS fixes is a segment with a duration. It’s the simplest
spatial primitive that encodes *connection* rather than just *position*.

Simple Features — a dominant spatial data model — hides segments inside
sealed coordinate sequences. A polygon is a ring of coordinates. A line
is a list of coordinates. The connections between consecutive vertices
are implicit. To discover them you extract coordinates, pair consecutive
points, and rebuild the structure.

wkpool makes segments explicit.

## The cost of sealed geometry

Consider two adjacent polygons sharing a boundary:

``` r

library(wkpool)
library(wk)

two_squares <- c(
  wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"),
  wkt("POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))")
)
```

In Simple Features, these are two independent geometries. Each owns its
coordinates. The shared edge from (1,0) to (1,1) is duplicated — encoded
separately in both polygons with no structural record that they share a
boundary. To discover the shared edge, you’d need
`sf::st_intersection()` or equivalent, which runs the full GEOS
machinery on every coordinate.

With wkpool, decompose to segments and the structure is *visible*:

``` r

pool <- establish_topology(two_squares)
pool
#> <wkpool[8 segments, 10 vertices]>
#> [1] <segment: 1->2>  <segment: 2->3>  <segment: 3->4>  <segment: 4->5> 
#> [5] <segment: 6->7>  <segment: 7->8>  <segment: 8->9>  <segment: 9->10>
```

At this stage every coordinate has been minted as a vertex with an
integer ID, and every consecutive pair within a ring has become a
segment. But the vertices from different features are still separate —
the shared edge exists as duplicate coordinate pairs.
[`topology_report()`](https://hypertidy.github.io/wkpool/reference/topology_report.md)
shows the raw state:

``` r

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
```

Now merge coincident vertices:

``` r

pool <- merge_coincident(pool)
topology_report(pool)
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
```

After merging, the two copies of (1,0) are now the same vertex ID, and
the two copies of (1,1) are the same vertex ID. The shared boundary is a
structural fact — two segments in different features that reference the
same vertex pair.

## What becomes easy

With segments as explicit objects, operations that are expensive on
Simple Features become table lookups.

**Shared boundaries** — segments that appear in more than one feature:

``` r

find_shared_edges(pool)
#>   edge_key .vx0 .vx1 .feature segment_idx features
#> 2      2-3    2    3        1           2     1, 2
#> 8      2-3    2    3        2           8     1, 2
```

In Simple Features this requires geometric intersection tests. Here it’s
a grouped count on vertex-pair indices.

**Neighbours** — which features share structure:

``` r

find_neighbours(pool)
#>   feature_a feature_b
#> 3         1         2
```

The adjacency graph falls out of a self-join on shared segments.

**Internal boundaries** — shared edges with opposite winding, the
defining signature of a true polygon boundary (as opposed to a
self-touching ring):

``` r

find_internal_boundaries(pool)
#> <wkpool[2 segments, 6 vertices]>
#> [1] <segment: 2->3> <segment: 3->2>
```

**Topology-preserving simplification** — reduce vertex count while
keeping shared boundaries aligned. With wkpool, shared vertices are
identified by index. Simplify the vertex pool, and every feature that
references those vertices updates together. In Simple Features you
simplify each geometry independently, and shared boundaries drift apart.

## The full topology

wkpool provides the arc-node decomposition.

**Vertex degree** tells you where topology is interesting:

``` r

vertex_degree(pool)
#> 1 2 3 4 5 6 
#> 2 4 4 2 2 2
```

Degree-2 vertices are interior to an arc — they’re the “pass-through”
points. Degree != 2 means a node: a branch point, an endpoint, or a
junction where multiple features meet.

**Nodes** — the vertices where topology happens:

``` r

find_nodes(pool)
#> [1] 2 3
```

**Arcs** — maximal sequences of segments between nodes:

``` r

find_arcs(pool)
#> [[1]]
#> [1] 2 1 4 3
#> 
#> [[2]]
#> [1] 2 3
#> 
#> [[3]]
#> [1] 2 5 6 3
#> 
#> [[4]]
#> [1] 2 3
arc_node_summary(pool)
#> $n_vertices
#> [1] 6
#> 
#> $n_nodes
#> [1] 2
#> 
#> $n_arcs
#> [1] 4
#> 
#> $degree_distribution
#> deg
#> 2 4 
#> 4 2 
#> 
#> $arc_length_distribution
#> arc_lengths
#> 1 3 
#> 2 2 
#> 
#> $mean_arc_length
#> [1] 2
```

This is the arc-node model from computational geometry (de Berg et
al. 2008), expressed as data frames rather than linked-list pointer
structures. The same decomposition that PostGIS topology provides, and
Arc/INFO is probably the most famous implementation, remnants exist in
.e00 format and some other softwares. This linear-only topology also
highlights clearly how polygons are *just lines*, they aren’t composed
of 2D topology (triangles) in many modern geo software.

## Cycles and winding

Cycles are closed loops in the segment graph. For polygon data, they
correspond to rings:

``` r

cycles <- find_cycles(pool)
cycles
#> [[1]]
#> [1] 1 2 3 4
#> 
#> [[2]]
#> [1] 2 5 6 3
#> 
#> attr(,"path")
#> [1] 1 2
```

Signed area from the shoelace formula distinguishes outer rings from
holes — negative area is an outer ring in the SF convention:

``` r

classify_cycles(pool)
#>   cycle area  type .path .feature
#> 1     1    1 outer     1        1
#> 2     2    1 outer     2        2
```

This is *intrinsic* — we observe winding from the coordinate order, we
don’t declare it via metadata. The geometry tells you what it is.

## The round trip

wkpool decomposes Simple Features into vertices and segments. It goes
back too:

``` r

# Arcs as linestrings (the topological boundary representation)
arcs_to_wkt(pool)
#> <wk_wkt[4]>
#> [1] LINESTRING (1 0, 0 0, 0 1, 1 1) LINESTRING (1 0, 1 1)          
#> [3] LINESTRING (1 0, 2 0, 2 1, 1 1) LINESTRING (1 0, 1 1)

# Cycles as polygons
cycles_to_wkt(pool)
#> <wk_wkt[2]>
#> [1] POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0)) POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))
```

This is the “SF at the edges” principle. Read geometry in any format wk
can handle (WKT, WKB, sf, geos, s2). Decompose to vertices and segments
for analysis. Recompose to Simple Features when you need to write, plot,
or hand off to another tool.

Simple Features is the interchange format. Segments are the working
format.

## Integration: triangulation

The vertex pool and segment table map directly to constrained
triangulation inputs — no coordinate extraction or reformatting needed:

RTriangle PSLG format (without classing)

The pool *is* the PSLG. The mapping is structural, not a conversion.

## Segments in movement data

wkpool applies beyond polygons. Track data — animal movement, vessel
tracks, GPS logs — is inherently a sequence of segments. Each step
between consecutive fixes carries a bearing, distance, speed, and
duration.

The traipse package works directly on this segment view:

``` r

library(traipse)

# 5 GPS fixes from a Southern Ocean track
x <- c(147.0, 147.5, 148.1, 148.3, 148.0)
y <- c(-42.0, -42.3, -42.5, -42.2, -41.9)

# Each function operates on the implicit segment between consecutive points
track_bearing(x, y)
#> [1] 129.04690 114.41743  26.38024 -36.79846        NA
track_distance(x, y)
#> [1]       NA 53088.55 54163.60 37175.89 41559.40
```

traipse doesn’t construct a LINESTRING and decompose it. It works
directly on the consecutive vertex pairs. The segments *are* the data.
The trip package builds on this: a trip is a grouped tibble of ordered
coordinates, and every analytical operation runs on the segment view via
`dplyr::lead()` and `dplyr::lag()` within groups. No geometry column, no
format conversion, no decomposition/reconstruction cycle.

## Why this matters now

Apache Arrow is becoming the universal columnar memory format, and
GeoArrow is defining how geometry lives in Arrow’s layout. A GeoArrow
polygon is already stored as coordinate arrays with offset indices —
structurally closer to a vertex pool than to a sealed WKB blob.

wkpool is a proof of concept for what GeoArrow-native analysis could
look like: work on decomposed vertices and segments directly, without
round-tripping through Simple Features semantics. The same principle
applies in any language.

## The lineage

The ideas in wkpool trace back to experiences with Arc/INFO, Myriax
Eonfusion, and the silicate package
[10.32614/CRAN.package.silicate](https://CRAN.R-project.org/package=silicate),
which explored topological data models for spatial data in R using
PATH/PATH0, ARC/ARC0, SC/SC0, TRI/TRI0 (segment-and-constraint) models.
wkpool is leaner: the vertex pool and segment table, built on wk
handlers, without the full ontology of silicate. Every network edge,
every polygon boundary, every track step reduces to a segment.

## Further reading

- de Berg, M. et al. (2008). *Computational Geometry: Algorithms and
  Applications*. Springer. (Arc-node topology, DCEL)

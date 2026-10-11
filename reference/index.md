# Package index

## Build a pool

Decompose any wk-handleable geometry into a shared vertex pool and
segments that reference it, or assemble a pool from tables.

- [`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md)
  : Establish topology from any wk-handleable geometry
- [`wkpool()`](https://hypertidy.github.io/wkpool/reference/wkpool.md) :
  Create a wkpool from a vertex table and a segment table
- [`new_wkpool()`](https://hypertidy.github.io/wkpool/reference/new_wkpool.md)
  : Construct a wkpool from validated components
- [`pool_combine()`](https://hypertidy.github.io/wkpool/reference/pool_combine.md)
  : Combine wkpool objects
- [`vec_c(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/vec_c.md)
  : Combine many wkpool vectors into one vector
- [`pool_compact()`](https://hypertidy.github.io/wkpool/reference/pool_compact.md)
  : Compact a pool by removing unreferenced vertices

## Inspect and draw

Accessors for the vertices, segments and feature structure of a pool,
CRS handling, wk handlers, and plotting.

- [`pool_vertices()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)
  [`pool_segments()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)
  [`pool_feature()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)
  [`pool_path()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)
  [`pool_paths()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md)
  : Access components of a wkpool object
- [`wk_crs(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/wkpool-crs.md)
  [`wk_set_crs(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/wkpool-crs.md)
  [`wk_is_geodesic(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/wkpool-crs.md)
  [`wk_set_geodesic(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/wkpool-crs.md)
  : CRS and geodesic handling for wkpool
- [`wk_handle(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/wk_handle.wkpool.md)
  : Handle wkpool objects with wk handlers
- [`plot(`*`<wkpool>`*`)`](https://hypertidy.github.io/wkpool/reference/plot.wkpool.md)
  : Plot a wkpool object

## Vertex identity

Coincident vertex detection and merging (with optional periodic x for
the antimeridian), and topology diagnostics.

- [`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md)
  : Merge coincident vertices in a pool
- [`topology_report()`](https://hypertidy.github.io/wkpool/reference/topology_report.md)
  : Report topology diagnostics
- [`vertex_degree()`](https://hypertidy.github.io/wkpool/reference/vertex_degree.md)
  : Calculate vertex degree (number of segments touching each vertex)

## Shared edges and neighbours

Edges used by more than one feature, internal boundaries, and feature
adjacency.

- [`find_shared_edges()`](https://hypertidy.github.io/wkpool/reference/find_shared_edges.md)
  : Find shared edges between features
- [`find_internal_boundaries()`](https://hypertidy.github.io/wkpool/reference/find_internal_boundaries.md)
  : Find internal boundaries (edges shared by exactly 2 features,
  opposite direction)
- [`find_neighbours()`](https://hypertidy.github.io/wkpool/reference/find_neighbours.md)
  : Build adjacency from shared edges

## Arcs and nodes

Arc-node topology, maximal segment runs between nodes of degree other
than two.

- [`find_nodes()`](https://hypertidy.github.io/wkpool/reference/find_nodes.md)
  : Find nodes (vertices where degree != 2)
- [`find_arcs()`](https://hypertidy.github.io/wkpool/reference/find_arcs.md)
  : Find arcs (maximal segment sequences between nodes)
- [`as_arcs()`](https://hypertidy.github.io/wkpool/reference/as_arcs.md)
  : Convert arcs to a wkpool of arc segments
- [`arc_node_summary()`](https://hypertidy.github.io/wkpool/reference/arc_node_summary.md)
  : Summarize arc-node structure

## Cycles

Closed rings in the segment graph, classified by winding into outer
rings and holes.

- [`find_cycles()`](https://hypertidy.github.io/wkpool/reference/find_cycles.md)
  : Find closed cycles (rings) in segment graph
- [`classify_cycles()`](https://hypertidy.github.io/wkpool/reference/classify_cycles.md)
  : Classify cycles as outer rings or holes based on winding
- [`cycle_signed_area()`](https://hypertidy.github.io/wkpool/reference/cycle_signed_area.md)
  : Calculate signed area of a cycle
- [`reverse_cycle()`](https://hypertidy.github.io/wkpool/reference/reverse_cycle.md)
  : Reverse a cycle's winding direction

## Triangulation (experimental)

Triangle pools that share the vertex pool, and the inputs constrained
triangulators need (holes, PSLG). This interface may change.

- [`pool_triangulate()`](https://hypertidy.github.io/wkpool/reference/pool_triangulate.md)
  : Triangulate the polygons in a pool
- [`new_wkpool_tri()`](https://hypertidy.github.io/wkpool/reference/new_wkpool_tri.md)
  : Construct a triangle pool
- [`pool_triangles()`](https://hypertidy.github.io/wkpool/reference/pool_triangles.md)
  [`tri_segments()`](https://hypertidy.github.io/wkpool/reference/pool_triangles.md)
  [`wk_handle(`*`<wkpool_tri>`*`)`](https://hypertidy.github.io/wkpool/reference/pool_triangles.md)
  : Access the triangles of a triangle pool
- [`pool_holes()`](https://hypertidy.github.io/wkpool/reference/pool_holes.md)
  [`hole_points()`](https://hypertidy.github.io/wkpool/reference/pool_holes.md)
  : Get hole points for constrained triangulation
- [`as_pslg()`](https://hypertidy.github.io/wkpool/reference/as_pslg.md)
  : Convert wkpool to RTriangle pslg format

## Export

Back to WKT or WKB, as segments, arcs or polygons from cycles.

- [`segments_to_wkt()`](https://hypertidy.github.io/wkpool/reference/segments_to_wkt.md)
  [`segments_to_wkb()`](https://hypertidy.github.io/wkpool/reference/segments_to_wkt.md)
  : Convert wkpool segments to WKT
- [`arcs_to_wkb()`](https://hypertidy.github.io/wkpool/reference/arcs_to_wkt.md)
  [`arcs_to_wkt()`](https://hypertidy.github.io/wkpool/reference/arcs_to_wkt.md)
  : Convert arcs to WKT linestrings
- [`cycles_to_wkt()`](https://hypertidy.github.io/wkpool/reference/cycles_to_wkt.md)
  [`cycles_to_wkb()`](https://hypertidy.github.io/wkpool/reference/cycles_to_wkt.md)
  : Convert cycles to WKT polygons

## Package

- [`wkpool-package`](https://hypertidy.github.io/wkpool/reference/wkpool-package.md)
  : wkpool: Vertex Pool Topology for Well-Known Geometry

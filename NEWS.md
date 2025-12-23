# wkpool 0.2.0

## Round trip

From cycles -> Polygons

* Classify outer/hole
* Associate holes with containing outer (by point-in-polygon or nesting)
* Group by .feature

From arcs → Linestrings

* Each arc becomes a linestring

## Arc-node topology

* `vertex_degree()` counts segments touching each vertex
* `find_nodes()` identifies branch points and endpoints (degree ≠ 2)
* `find_arcs()` extracts maximal segment sequences between nodes
* `as_arcs()` returns wkpool with `.arc` column for arc membership
* `arc_node_summary()` reports arc/node structure statistics

Arcs are the "uninterrupted lines" — sequences of segments passing only through degree-2 vertices. Useful for line simplification, network extraction, and efficient storage.

# wkpool 0.1.0

*Like sands through the hourglass, so are the segments of our geometries.*

Initial release. Vertex pool topology for wk-handleable geometry.

## Core functions

* `establish_topology()` converts any wk-handleable geometry to segments + vertex pool
* `merge_coincident()` discovers shared vertices (exact or tolerance-based)
* `pool_vertices()`, `pool_segments()`, `pool_feature()` access the structure
* `pool_combine()` merges multiple pools with proper index remapping
* `pool_compact()` removes unreferenced vertices

## Topology discovery

* `find_shared_edges()` finds edges shared by multiple features
* `find_internal_boundaries()` finds edges traversed in opposite directions (true shared boundaries)
* `find_neighbours()` builds adjacency from shared edges or vertices
* `topology_report()` summarizes vertex/edge sharing

## Cycle and winding analysis

* `find_cycles()` discovers closed rings from segment connectivity
* `cycle_signed_area()` computes signed area (shoelace formula)
* `classify_cycles()` identifies outer rings vs holes by winding
* `reverse_cycle()` flips winding direction
* `hole_points()` extracts hole centroids for triangulation
* Convention support: `"sf"` (default) or `"ogc"` winding rules

## Triangulation export

* `as_pslg()` exports to RTriangle format (P, S matrices)
* `as_decido()` exports to decido format (0-indexed)

## vctrs integration

* wkpool is a vctrs rcrd: subset, combine operations work
* `plot.wkpool()` for quick visualization

## Design principles

* Segments are the atomic primitive — the waist of the hourglass
* Observe, don't correct — represent truth, fixing is separate
* Minted `.vx` integers for stable vertex identity
* Feature provenance tracked via `.feature`

# wkpool (development version)

## wk_handle: a wkpool is wk-handleable

* `wk::wk_handle()` is implemented for wkpool: a pool presents as one
  LINESTRING per segment (length preserved), with crs and geodesic
  attached. The wk ecosystem's generic conversions now work directly
  on a pool: `wk::as_wkb()`, `wk::as_wkt()`, `wk::wk_coords()`,
  `wk::wk_meta()`, `geos::as_geos_geometry()`, and anything else built
  on `wk_handle()`. Arcs and cycles remain explicit conversions.

* The handled path is built natively from the pool (`wk::xy()` indexed
  by the segment table, through wk's C-level linestring filter): no
  text round trip, coordinates at full double precision, XYZ when the
  pool carries z.

## CRS round trip

* A wkpool now participates in wk's CRS propagation model. The CRS and
  geodesic flag of the input are captured by `establish_topology()`
  (via `wk::wk_crs()` and `wk::wk_is_geodesic()`), carried through
  subsetting, `merge_coincident()`, `pool_compact()`, `as_arcs()` and
  `pool_combine()`, and restored onto the output of `segments_to_wkt()`,
  `arcs_to_wkt()`, `cycles_to_wkt()` and their WKB counterparts.

* Methods for `wk::wk_crs()`, `wk::wk_set_crs()`, `wk::wk_is_geodesic()`
  and `wk::wk_set_geodesic()` are provided for wkpool objects. The CRS is
  opaque and carried, never interpreted, per wk convention.

* `pool_combine()` resolves the CRS across inputs with
  `wk::wk_crs_output()`: inherit values give way to concrete values, and
  combining pools with unequal concrete CRS is an error.

* `new_wkpool()` is now exported, so downstream packages that construct
  pools directly (e.g. bigcurve) can do so through a supported
  constructor that checks invariants and carries crs/geodesic.

# wkpool 0.3.0

* Removed `as_decido()` which wasn't fully worked out. 

* Fixed `wkpool()` which never worked before. 

* Added check_wkpool helper function. 

# wkpool 0.2.0

## Round-trip to wk

* `arcs_to_wkt()` / `arcs_to_wkb()` — arcs become linestrings
* `cycles_to_wkt()` / `cycles_to_wkb()` — cycles become polygons, with hole nesting
* `segments_to_wkt()` / `segments_to_wkb()` — raw segments as linestrings, multilinestring, or points

## Arc-node topology

* `vertex_degree()` counts segments touching each vertex
* `find_nodes()` identifies branch points and endpoints (degree ≠ 2)
* `find_arcs()` extracts maximal segment sequences between nodes
* `as_arcs()` returns wkpool with `.arc` column for arc membership
* `arc_node_summary()` reports arc/node structure statistics

Arcs are the "uninterrupted lines" — sequences of segments passing only through degree-2 vertices. Useful for line simplification, network extraction, and efficient storage.

# wkpool 0.1.0

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

# wkpool (development version)

## Exact coordinate identity and linear internals

* `merge_coincident()` now groups vertices on the double-precision bit
  pattern (via `vctrs::vec_group_id()`) instead of formatted text
  keys. This fixes a silent correctness bug: the old keys went through
  `as.character()` at ~15 significant digits, so distinct doubles
  differing only beyond printable precision merged even at
  `tolerance = 0`. Exact now means exact.

* When the pool carries `z`, vertices are keyed on all coordinate
  columns: equal x/y with different z no longer merge silently.
  Tolerance is documented honestly as a snap grid (cell size
  `tolerance`), not a distance guarantee, and canonical vertices keep
  their original coordinates.

* `topology_report()` computes its counts directly from vertex groups
  instead of performing a full internal `merge_coincident()` plus
  `find_shared_edges()`.

* The quadratic idioms are gone: `find_arcs()` uses dense integer
  adjacency with amortized cursors and a preallocated walk buffer
  (was: character-keyed lists grown with `c()` in loops);
  `pool_combine()` remaps by position with per-input offsets (was:
  character-named lookup tables and growing vectors); `as_arcs()` and
  `vertex_degree()` are vectorized. On a 160x160 grid of squares
  (102400 segments) `find_arcs()` drops from minutes to under half a
  second, and all core verbs now scale linearly.

* `pool_compact()` remap corrected for pools whose segment order does
  not follow pool order (latent indexing assumption).

## Path provenance (.path)

* `establish_topology()` now mints a `.path` id per segment - one id
  per input ring or linestring - with a sidecar `paths` table
  (`.path`, `.feature`, `.part`, `.ring`, captured from
  `wk::wk_coords()` identifiers). Provenance survives subsetting,
  `merge_coincident()`, `pool_compact()` and `pool_combine()` (which
  offsets path ids to keep them unique), and is exposed via the new
  accessors `pool_path()` and `pool_paths()`. `new_wkpool()` gains
  `path` and `paths` arguments.

* `find_cycles()` uses provenance when present: a cycle is a path
  whose segment chain closes. Rings are recovered exactly, in input
  order and input winding, robust to segment reordering (chains are
  rebuilt by connectivity), and a broken ring after subsetting is
  dropped rather than mis-walked. The result carries a `path`
  attribute mapping cycles to path ids. Pools without provenance keep
  the legacy storage-order walk.

* The winding-convention issue is resolved structurally: with
  provenance, ring roles are known (the first ring of each part is the
  exterior, later rings are its holes), so `classify_cycles()` and
  `hole_points()` no longer guess from winding and the `convention`
  argument is only consulted for provenance-free pools.
  `hole_points()` on a simple polygon now returns `NULL` as intended.

* `cycles_to_wkb(feature = TRUE)` reconstructs original feature
  structure exactly when provenance is present: rings grouped into
  their original part (exterior first, then that part's own holes),
  parts grouped into POLYGON or MULTIPOLYGON features. This replaces
  the heuristics (all holes with the first outer; holes dropped for
  multi-part features) for established pools; provenance-free pools
  keep the old behaviour.

## Native emitters

* All round-trip emitters (`arcs_to_*()`, `cycles_to_*()`,
  `segments_to_*()`) are rebuilt on wk's C-level construction filters
  (`wk::wk_linestring()`, `wk::wk_polygon()`, `wk::wk_collection()`).
  WKB is now the primary product and WKT is derived from it; no
  coordinate text is assembled anywhere in wkpool.

* Consequences: WKB output preserves coordinates at full double
  precision (previously text formatting perturbed doubles at around
  the 15th significant digit); z vertex values are carried through all
  emitters (previously dropped); rings are closed by wk; crs and
  geodesic ride along by construction. WKT output uses wk's writer
  (16 significant digits, trimmed) - use the WKB emitters when
  bit-exact coordinates matter.

* Behaviour is otherwise unchanged. (The documented simplifications in
  `cycles_to_wkb(feature = TRUE)` were retained at this stage and have
  since been superseded by path provenance for established pools - see
  above; they still describe the provenance-free fallback.)

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

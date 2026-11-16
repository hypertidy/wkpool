# wkpool 0.5.0

* New `tri_as_arrow()` and `tri_from_arrow()`: a triangle pool as an Arrow
  array (built with nanoarrow, now in Suggests) of triangles that index
  one shared vertex dictionary, in the layout of the draft
  `geoarrow.triangles` type. By default each row is a feature, the list of
  its triangles (`List<FixedSizeList<Dictionary<int32, Coordinate>>[3]>`),
  so the list offsets are the feature-to-triangle ranges; a triangle in
  several features is written in each of their rows and read back as one
  triangle in several groups. `layout = "triangle"` writes one row per
  triangle.

* New `tri_prepare()`: compute the triangle rows, the edge table, the group
  membership and the boundary rings of a triangle pool once and keep them
  with it, as a prepared geometry in GEOS. The `tri_*()` functions and
  `tri_dissolve()` read the cache while the pool and `group` are
  unchanged (checked on every use). `tri_is_prepared()` says whether a pool
  has a valid cache. `tri_overlay()` returns a prepared mesh.

* New `tri_structure()`: number of parts, holes and rings per group
  (`ST_NumGeometries()`, `ST_NumInteriorRings()`), a read on a prepared
  pool.

* New `tri_overlay()` and `tri_overlay_relate()`: the full DE-9IM matrix
  between features of two different layers, polygons (triangle meshes),
  lines or points in any combination. The layers' boundaries and lines go
  through one constrained triangulation (cdtr, now in Suggests) so that
  every place they meet is a shared vertex or edge. The relation is then
  read from the overlay's triangles, edges and vertices alone, with the
  mod-2 boundary rule for lines. The tests check every combination of
  types against geos, including shared and overlapping boundaries.

* New vignette "Questions on a mesh": the `tri_*()` functions on a small
  coverage, with overlapping groups, points, boundaries and clipping, and
  a check against geos.

* New `tri_locate()`: the triangle that holds each point (point in polygon,
  `ST_Intersects()` of a point), by a grid of triangle buckets and exact
  orientation tests, so a point on an edge or vertex goes to the same
  triangle whatever the rounding (the lowest row that holds it). Points
  can be any wk geometry, a two-column matrix, or `x` and `y`. No
  dependency on a triangulation package.

* `plot()` for `wkpool` gains `add`, to draw segments over an existing plot.

* New `tri_*()` functions answer the usual `ST_*` questions about groups of
  triangles (one group per triangle, or a table of triangle and group for
  groups that overlap) without forming polygons, in compiled code:
  `tri_neighbours()` (the edge table), `tri_measures()` (area, perimeter,
  centroid, a point inside, extent), `tri_relate()` (the DE-9IM matrix
  from shared triangles, edges and vertices alone), `tri_boundary()` (the
  boundary as segments, optionally each shared edge once),
  `tri_clip_rect()` (clip by a rectangle and stay a mesh) and
  `tri_convex_hull()`. The tests check them against geos.

* `tri_dissolve()` walks its rings in compiled code (`src/dissolve.cpp`),
  about 10 times faster on a 33,000 triangle mesh. The kernel mirrors the R
  reference step for step and the tests check that both give identical
  rings.

* New `find_faces()`: the faces of a segment graph, the regions it
  encloses (street blocks between roads) plus one outer face per
  connected component. Each edge is two half-edges; at every vertex the
  outgoing half-edges are ordered by angle and the walk takes the next one
  clockwise from its arrival, so every half-edge lies on exactly one face
  and all faces come out in one pass. One row per half-edge with its
  face, input segment, the face on the other side, the face's signed area
  and whether the edge is a bridge or dead end. Compiled kernel with a
  pure-R reference implementation, tested for identical output. On the
  Greater Hobart road network (483,302 edges) it takes under a second.

* New `tri_dissolve()`: dissolve the triangles of a `wkpool_tri` into one
  MULTIPOLYGON per group, along shared triangle edges. `group` is one
  1-based id per triangle (`NA` leaves a triangle out), the `set_id` of a
  labelled mesh; the default dissolves each feature's triangles back into
  its polygons. Moved here from polymer's `fuse()`, with the next boundary
  edge at a vertex found by turning through the triangle fan instead of
  comparing angles (the output is identical on polymer's test meshes).

# wkpool 0.4.0

* New vignette "Triangulation": ear cutting with `pool_triangulate()`,
  the PSLG route (`as_pslg()` with `pool_holes()`) to RTriangle, and
  taking its triangles back onto a vertex pool with `new_wkpool_tri()`,
  on silicate's `inlandwaters`. The other vignettes now cover periodic x,
  `wkpool(vertices, segments)` and triangle pools, and point to it.

* `pool_holes()` returns a point inside each hole even when the hole is
  not convex: the centroid of an ear of the hole ring, clear of every
  pool vertex, instead of the mean of the ring's vertices, which could
  fall outside the hole and make RTriangle erase the polygon around it
  (on `inlandwaters` this lost most of the area). Holes filled exactly by
  another polygon of the pool (enclaves) are no longer returned, since
  they are not holes of the coverage.

* `plot()` for `wkpool_tri` accepts `xlab` and `ylab`, and sets the plot
  extent from the triangles drawn rather than the whole vertex pool, so a
  subset is drawn at its own extent.

* New `plot()` method for `wkpool_tri`: one `polygon()` call indexing the
  vertex pool, filled by feature (40,000 triangles draw in about 0.4 s to
  png, where casting to wk polygons and plotting those took over a minute).

## Triangles (experimental)

* New, experimental `wkpool_tri` class: a vector of triangles (`.vx0`, `.vx1`, `.vx2`,
  `.feature`, `.part`) on the same kind of vertex pool as a wkpool.
  `pool_triangulate(x, engine = "decido")` ear-cuts each polygon of a pool
  using its path provenance; ear cutting creates no vertices, so the
  vertex table is passed through unchanged. `merge_coincident()` accepts a
  `wkpool_tri` (triangulate first, merge later gives the same result as
  merging first). `pool_triangles()` and `tri_segments()` give the triangle
  table and the triangle edges as a wkpool; `wk_handle()` presents one
  POLYGON per triangle, with crs and geodesic carried. 'decido' is
  suggested.

* `wkpool_tri` is experimental and may change in a later release. In
  particular the part is stored as a `.part` field on each triangle (the
  simplest option); the alternative is a `.path` field pointing at the
  outer ring's path, recovering feature and part from `pool_paths()` as
  segments do.

## Periodic x (antimeridian)

* `merge_coincident()` and `topology_report()` gain `period_x` (default
  `NULL`, behaviour unchanged). With `period_x = 360`, vertices are keyed
  on `x %% 360`, so the two copies of a +-180 seam fuse (and 0 to 360
  data fuses at 0 and 360); after merging, the seam is an ordinary shared
  boundary for `find_shared_edges()`, `find_internal_boundaries()` and
  quotient arcs. The period is never inferred from the crs. Coordinates
  are not moved, so geometry emitted from a fused pool jumps by one period
  across the seam. With a tolerance, x is wrapped, snapped, then wrapped
  again (#3).

## Constructors and naming

* `wkpool(vertices, segments)` is now exported and documented: the
  user-level constructor from a vertex table and a segment table, as
  returned by `pool_vertices()` and `pool_segments()` (#2).

* New `pool_holes()`, the same function as `hole_points()` under a name
  that matches the other `pool_*()` accessors. `hole_points()` is kept
  (#1).

## Performance

* `cycles_to_wkb()` / `cycles_to_wkt()` build all polygons in one
  `wk::wk_polygon()` call and all multipolygons in one
  `wk::wk_collection()` call, instead of looping over features and parts
  (10,000 hexagons: 5.7 s to 0.16 s). The fallback for pools without
  path provenance matches cycle edges to segments with one
  `vctrs::vec_match()` instead of a scan per cycle. Output is unchanged.

* `establish_topology()` and `merge_coincident()` no longer build hash
  tables to look up vertex ids: pools they mint have `.vx = 1..n`, where
  an id is its own position, so validation in `new_wkpool()` and the
  segment remap use it directly (other pools fall back to `match()`).
  On 1M line coordinates `merge_coincident()` drops from 0.35 s to
  0.12 s, and the vertex pipeline is now faster than silicate's
  `PATH0()` on every benchmark input (see `bench/README.md`).

* `find_cycles()`, `classify_cycles()`, `hole_points()` and
  `find_neighbours()` no longer scale quadratically: on 10,000 hexagons
  `find_neighbours(type = "edge")` drops from 19 s (13 GB allocated) to
  0.03 s and `classify_cycles()` from 8.3 s to 0.23 s.
  `find_shared_edges()`, `find_internal_boundaries()` and
  `topology_report()` key edges with vctrs instead of `paste()`. Results
  are unchanged, except that `find_neighbours()` output no longer
  carries a stray `out.attrs` attribute and the `features` column of
  `find_shared_edges()` is a plain named list.
* `bench/` holds a benchmark against silicate (not part of the built
  package).

## Quotient boundary semantics

* `vertex_degree()`, `find_nodes()`, `find_arcs()`, `as_arcs()`,
  `arc_node_summary()`, `arcs_to_wkt()` and `arcs_to_wkb()` gain a
  `quotient` argument (default FALSE, behaviour unchanged). In the
  quotient graph, duplicated shared boundaries - the same undirected
  vertex pair carried by more than one feature - count once: the
  first occurrence keeps its direction, multiplicity is discarded
  whatever its count. A vertex interior to a shared boundary has
  quotient degree 2 (the degree-2 invariant holds at this level,
  including for vertices minted by densification), true junctions
  remain nodes, and `find_arcs(x, quotient = TRUE)` yields the
  TopoJSON-style arc decomposition of a polygon layer: each shared
  border once, not once per feature.

* Characterization guards pin the default duplicated-boundary and
  antimeridian-seam semantics as explicit contracts: boundary-interior
  vertices are degree-4 by default, seams of a +-180 split stay
  distinct through merge, and merge keys are exact coordinates with
  no wrapping of any kind.

## Compiled arc kernel (first src/)

* `find_arcs()` now runs on a cpp11 kernel (wkpool's first compiled
  code, LinkingTo: cpp11 only, no system requirements). The algorithm,
  adjacency ordering, node ordering and output are identical to the
  pure-R implementation, which is retained as the executable
  specification (`find_arcs_walk_r`, internal) and pinned to the
  kernel by conformance tests across nodes, loops, open lines, holes,
  grids, subsets and shuffled pools.

* Scale: the pathological all-nodes grid at 2.56 million segments runs
  in about 1 second (was ~10x that in R after the linear rewrite, and
  unboundedly worse before it); 2 million segments of long arcs walk
  in under a second, with remaining time dominated by the R-side
  dense-id mapping. Real-world effect: the CGAZ world admin
  boundaries (~10M segments) drop from ~44s to a few seconds.

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

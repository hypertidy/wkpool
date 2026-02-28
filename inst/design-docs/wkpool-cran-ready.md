---

## Package 3: wkpool

**Repo**: https://github.com/hypertidy/wkpool  
**Language**: Pure R  
**Dependencies**: wk

### What it does

Vertex pool + segment table decomposition of geometries via wk handlers. The "binary of geometry" — every geometry expressed as indexed vertex pairs. Provides topology discovery (shared edges, neighbours), arc-node structure, cycle detection, and round-trip back to WKT/WKB. The foundation layer between wk's coordinate streams and domain-specific structures (meshes, tracks, triangulations).

### CRAN submission checklist

- [ ] **Academic reference in DESCRIPTION**: cite the wk package paper if one exists, or a computational geometry/topology reference. Possibilities: de Berg et al. "Computational Geometry: Algorithms and Applications" for the half-edge / DCEL concepts, or Worboys & Duckham "GIS: A Computing Perspective" for topological data models. Or reference the OGC Simple Features spec for the geometry model being decomposed. At minimum: `Description: ... Topological decomposition of geometries following principles described in Worboys and Duckham (2004, ISBN:978-0415283755).`
- [ ] **All exported functions have \value{}**: ✅ all documented functions have @returns — DONE
- [ ] **Dependency on wk**: wk is on CRAN — just make sure the version pinning is correct. Check which wk version introduced the handler features you use
- [ ] **No broken URLs**
- [ ] **\examples{} on all exported functions**: use small inline geometries (like `wk::wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")`) not external data
- [ ] **Vignette**: a vignette showing the segments-as-primitive concept with inlandwaters or similar would be very compelling for reviewers
- [ ] **R CMD check --as-cran**: clean
- [ ] **Bump version** to 0.1.0

### Key considerations

- wkpool is conceptually novel enough that a good vignette will help CRAN reviewers understand the purpose. "Topological decomposition of simple feature geometries into indexed vertex-edge structures" is the pitch
- The package name is good — descriptive, no conflicts
- If any functions use R-level vertex deduplication with paste-based hashing, document the precision/tolerance behaviour clearly
- Make sure the round-trip functions (cycles_to_wkt etc.) have tests showing geometry preservation

### Agent handoff notes

This needs much documentation work. Focus on: comprehensive \value{} tags, a solid vignette (the "segments are the binary of geometry" narrative is excellent material), examples that work without external data. Run `R CMD check --as-cran`.

---

## Audit (2025-02-19)

Version is currently **0.2.0**. NEWS.md documents both 0.1.0 and 0.2.0 features.

### Documentation audit

#### `@examples` coverage — SPARSE

Only **3 of 26** exported functions have examples:

| Function | has `@examples` | Notes |
|----------|----------------|-------|
| `establish_topology` | ✅ | |
| `as_pslg` | ✅ | wrapped in `\dontrun{}` |
| `hole_points` | ✅ | calls `RTriangle::triangulate()` — needs `\dontrun{}` or `requireNamespace()` guard |
| all others | ❌ | **23 functions need examples** |

#### Undocumented exported functions

These are exported but have **no roxygen block** (no .Rd file):

| Function | Type | Action needed |
|----------|------|---------------|
| `pool_vertices()` | User-facing accessor | Needs full roxygen: title, description, @param, @returns, @examples |
| `pool_segments()` | User-facing accessor | Same |
| `pool_feature()` | User-facing accessor | Same |
| `plot.wkpool()` | S3 method | Needs @param for `col`, `...`; could share an .Rd page |

vctrs S3 methods (`format.wkpool`, `vec_ptype_abbr.wkpool`, etc.) are acceptable without docs.

#### `@returns` coverage — GOOD

All documented functions have `@returns`. ✅

### Tests — MISSING

No `tests/` directory exists. Need:

- `tests/testthat.R` boilerplate
- Test files covering at minimum:
  - round-trip: `establish_topology()` → `cycles_to_wkt()` preserves geometry
  - `merge_coincident()` reduces vertex count
  - `find_shared_edges()` / `find_neighbours()` on known adjacency
  - `find_cycles()` / `classify_cycles()` on polygon with hole
  - `pool_combine()` index remapping
  - `[` subsetting preserves structure
  - arc-node: `find_arcs()`, `vertex_degree()`, `find_nodes()`

### Vignette

`examples.qmd` exists in `inst/design-docs/` — needs to be moved to `vignettes/` and converted to `.Rmd` (CRAN standard) or kept as `.qmd` if using quarto vignette builder.

### DESCRIPTION issues

1. Version says 0.2.0 but checklist says "bump to 0.1.0" — version is ahead of checklist
2. No `URL:` or `BugReports:` fields
3. No academic reference in Description
4. No `VignetteBuilder:` field
5. `vctrs` is in Imports — confirm all vctrs generics used are re-exported or imported correctly

---

## Decisions (resolved 2025-02-19)

1. **Vignette format**: `.Rmd` (knitr). Least friction for first CRAN submission.
2. **Version number**: Ship as 0.2.0. Not bothered about version when < 1.0.0.
3. **`hole_points()` example**: Use `requireNamespace("RTriangle")` guard.
4. **Test scope**: Comprehensive. Full testthat coverage across all major functionality.
5. **Accessor docs**: Shared `?wkpool-accessors` help page for `pool_vertices()`, `pool_segments()`, `pool_feature()`.
6. **wk version pin**: `wk (>= 0.9.4)`. Pin to 0.9.5 if any issues arise.

## Design question: wk_orient and ring winding (2025-02-20)

**Status**: ⚠️ **Important open issue — must resolve before or shortly after CRAN submission**

**Finding**: `find_cycles()` traversal direction does not reliably match the SF/wk winding convention. Standard SF-wound input (CCW outer rings) produces **positive** signed area from the shoelace formula, but the original `classify_cycles()` convention treats **negative** area as "outer" for SF. This means:

- `classify_cycles()` misclassifies standard SF outer rings as "holes"
- `cycles_to_wkt(x, feature = TRUE)` returns empty output for standard SF input
- `hole_points()` returns points for simple polygons with no holes

The original design convention has been preserved for now. Tests document the current behaviour and flag these as known limitations.

**Root cause**: The segment traversal order in `find_cycles()` does not guarantee that the resulting cycle winding matches the input ring winding. The mapping from directed segments back to ring orientation needs investigation.

**Options**:

1. **Call `wk::wk_orient()` inside `establish_topology()`** — guarantees consistent winding before decomposition. Cheap and robust. But changes the contract (input is modified).
2. **Fix `find_cycles()` traversal** — ensure cycle traversal preserves input winding direction. More principled but harder.
3. **Flip the sign convention** — make `classify_cycles()` treat positive area as outer. Simple fix but may break for other input types.
4. **Document the requirement** — tell users to pre-orient with `wk_orient()`. Fragile.

**Decision**: Preserve original design for now. Flag as important issue to resolve. Tests document current behaviour.

**Affected tests** (document current behaviour, will need updating when resolved):
- `test-cycles.R`: "hole_points on simple polygon reflects winding convention"
- `test-roundtrip.R`: "cycles_to_wkt feature=TRUE is affected by winding convention"

---

## Work plan (ordered)

### Phase 1: Documentation (blocks CRAN check) — ✅ COMPLETE

1. Add roxygen for `pool_vertices()`, `pool_segments()`, `pool_feature()` — shared `wkpool-accessors.Rd` page
2. Add roxygen for `plot.wkpool()` with @param
3. Add `@examples` to all 23 undocumented functions — small inline WKT geometries
4. Guard `hole_points()` example with `requireNamespace("RTriangle")`
5. Add academic reference to DESCRIPTION Description field
6. Add `URL:` and `BugReports:` to DESCRIPTION
7. Convert `examples.qmd` → `vignettes/wkpool.Rmd`, add `VignetteBuilder: knitr` to DESCRIPTION

### Phase 2: Tests (comprehensive) — ✅ COMPLETE (138 passing)

8. ~~Create `tests/testthat.R` + `tests/testthat/` structure~~
9. ~~Write core test files:~~
   - `test-roundtrip.R` — 26 tests: establish → round-trip, WKB variants
   - `test-merge.R` — 10 tests: merge_coincident reduces vertices, tolerance
   - `test-topology.R` — 25 tests: shared edges, neighbours, internal boundaries
   - `test-cycles.R` — 24 tests: find_cycles, classify_cycles, reverse_cycle, hole_points
   - `test-arc-node.R` — 20 tests: vertex_degree, find_nodes, find_arcs
   - `test-combine.R` — 33 tests: pool_combine, subsetting, pool_compact

### Phase 3: Polish

10. Pin `wk (>= 0.9.4)` in DESCRIPTION
11. Run `R CMD check --as-cran`, fix any NOTEs/WARNINGs
12. Review all URLs (README, DESCRIPTION)
13. Final `devtools::document()` pass
14. Tag release
15. **Resolve winding convention issue** (can be post-release but should be soon)
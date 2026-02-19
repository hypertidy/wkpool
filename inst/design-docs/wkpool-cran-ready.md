---

## Package 3: wkpool

**Repo**: https://github.com/hypertidy/wkpool  
**Language**: Pure R  
**Dependencies**: wk

### What it does

Vertex pool + segment table decomposition of geometries via wk handlers. The "binary of geometry" — every geometry expressed as indexed vertex pairs. Provides topology discovery (shared edges, neighbours), arc-node structure, cycle detection, and round-trip back to WKT/WKB. The foundation layer between wk's coordinate streams and domain-specific structures (meshes, tracks, triangulations).

### CRAN submission checklist

- [ ] **Academic reference in DESCRIPTION**: cite the wk package paper if one exists, or a computational geometry/topology reference. Possibilities: de Berg et al. "Computational Geometry: Algorithms and Applications" for the half-edge / DCEL concepts, or Worboys & Duckham "GIS: A Computing Perspective" for topological data models. Or reference the OGC Simple Features spec for the geometry model being decomposed. At minimum: `Description: ... Topological decomposition of geometries following principles described in Worboys and Duckham (2004, ISBN:978-0415283755).`
- [ ] **All exported functions have \value{}**: the establish_topology, merge_coincident, cycles, arcs, round-trip functions all need documented returns
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

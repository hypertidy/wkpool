# Characterization guards: duplicated-boundary and seam semantics
#
# These tests pin CURRENT behaviour as a contract before any change to
# the pool's duplicated-boundary semantics (quotient node finding,
# antimeridian-aware merging). If one of these fails, a default
# behaviour has changed - that must be a deliberate decision, not a
# side effect.

# two features sharing a two-segment boundary through a midpoint at
# (1, 0.5): the smallest fixture where a boundary-interior vertex
# exists (the densify+merge situation, without needing bigcurve)
shared_midpoint_pool <- function() {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 0.5, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0.5, 1 0))"
  ))
  merge_coincident(establish_topology(x))
}

test_that("GUARD: boundary-interior vertices are degree-4 nodes by default", {
  m <- shared_midpoint_pool()
  v <- pool_vertices(m)
  mid <- v$.vx[v$x == 1 & v$y == 0.5]

  deg <- vertex_degree(m)
  expect_identical(unname(deg[as.character(mid)]), 4L)

  # and so find_nodes reports it, along with the two true junctions
  nodes <- find_nodes(m)
  expect_true(mid %in% nodes)
  junctions <- v$.vx[(v$x == 1 & v$y == 0) | (v$x == 1 & v$y == 1)]
  expect_true(all(junctions %in% nodes))
  expect_identical(length(nodes), 3L)
})

test_that("GUARD: default arcs traverse a shared boundary once per feature", {
  m <- shared_midpoint_pool()
  arcs <- find_arcs(m)
  # two outer arcs plus the shared boundary twice - and the midpoint
  # node splits each shared-boundary traversal into two arcs
  # (junction -> midpoint, midpoint -> junction), so 2 + 4
  expect_identical(length(arcs), 6L)
})

test_that("GUARD: seams of a +-180 split are distinct after merge", {
  # two hemispheres of a global layer, each owning its copy of the
  # antimeridian: x = -180 on one, x = 180 on the other. Coincidence
  # is exact-coordinate only: the seam columns must NOT fuse.
  g <- wk::as_wkb(wk::wkt(c(
    "POLYGON ((-180 -60, 0 -60, 0 60, -180 60, -180 -60))",
    "POLYGON ((0 -60, 180 -60, 180 60, 0 60, 0 -60))"
  ), crs = "EPSG:4326"))
  m <- merge_coincident(establish_topology(g))
  v <- pool_vertices(m)

  # both seam columns present as distinct vertices
  expect_identical(sum(v$x == -180), 2L)
  expect_identical(sum(v$x == 180), 2L)

  # the greenwich boundary IS internal (shared), the seam is NOT
  internal <- find_internal_boundaries(m)
  vi <- pool_vertices(internal)
  s <- pool_segments(internal)
  touched <- unique(c(s$.vx0, s$.vx1))
  expect_true(all(vi$x[match(touched, vi$.vx)] == 0))

  # seam vertices are degree-2 boundary corners, not fused junctions
  deg <- vertex_degree(m)
  seam_vx <- v$.vx[abs(v$x) == 180]
  expect_true(all(deg[as.character(seam_vx)] == 2L))
})

test_that("GUARD: merge keys are exact coordinates, no wrapping of any kind", {
  v <- data.frame(.vx = 1:4, x = c(-180, 180, 0, 360), y = c(0, 0, 5, 5))
  pool <- new_wkpool(v, vx0 = c(1L, 3L), vx1 = c(2L, 4L))
  m <- merge_coincident(pool)
  # -180 != 180 and 0 != 360 under exact identity
  expect_identical(nrow(pool_vertices(m)), 4L)
})

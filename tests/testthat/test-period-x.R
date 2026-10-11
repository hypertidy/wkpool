# merge_coincident(period_x = ): opt-in periodic-x coincidence (#3)
#
# The GUARD tests in test-guard-boundaries.R pin the default
# (period_x = NULL: exact coordinates, no wrapping); these tests cover
# the opt-in behaviour.

# two hemispheres of a global layer, each owning its copy of the
# antimeridian, with a vertex every 90 degrees along top and bottom
hemispheres <- function(dx = 0) {
  wkt <- c(
    "POLYGON ((-180 -60, -90 -60, 0 -60, 0 60, -90 60, -180 60, -180 -60))",
    "POLYGON ((0 -60, 90 -60, 180 -60, 180 60, 90 60, 0 60, 0 -60))"
  )
  g <- wk::as_wkb(wk::wkt(wkt, crs = "EPSG:4326"))
  if (dx != 0) g <- wk::wk_transform(g, wk::wk_affine_translate(dx, 0))
  g
}

test_that("period_x = NULL is the default exact behaviour", {
  p <- establish_topology(hemispheres())
  expect_identical(merge_coincident(p, period_x = NULL), merge_coincident(p))
})

test_that("period_x = 360 fuses the seams of a +-180 split", {
  m <- merge_coincident(establish_topology(hemispheres()), period_x = 360)
  v <- pool_vertices(m)

  # seam column appears once, keeping the first occurrence's x (-180)
  expect_identical(sum(abs(v$x) == 180), 2L)
  expect_identical(sum(v$x == 180), 0L)
  expect_identical(nrow(v), 8L)

  # both the seam and greenwich are now internal boundaries
  internal <- find_internal_boundaries(m)
  s <- pool_segments(internal)
  x_touched <- v$x[match(unique(c(s$.vx0, s$.vx1)), v$.vx)]
  expect_setequal(unique(x_touched), c(-180, 0))
  expect_identical(nrow(find_shared_edges(m)), 4L)

  # seam corners are junctions, like the greenwich corners
  nodes <- find_nodes(m)
  expect_setequal(v$x[match(nodes, v$.vx)], c(-180, -180, 0, 0))
})

test_that("the fused seam is an ordinary boundary in quotient arcs", {
  m <- merge_coincident(establish_topology(hemispheres()), period_x = 360)
  # default: seam and greenwich traversed once per feature
  expect_identical(length(find_arcs(m)), 8L)
  # quotient: each shared boundary once, plus four top/bottom arcs
  arcs <- find_arcs(m, quotient = TRUE)
  expect_identical(length(arcs), 6L)
  v <- pool_vertices(m)
  seam <- v$.vx[v$x == -180]
  is_seam <- vapply(arcs, function(a) all(a %in% seam), logical(1))
  expect_identical(sum(is_seam), 1L)
})

test_that("0..360 data fuses at greenwich equivalently", {
  m <- merge_coincident(establish_topology(hemispheres(dx = 180)),
                        period_x = 360)
  v <- pool_vertices(m)
  expect_identical(nrow(v), 8L)
  expect_identical(sum(v$x == 360), 0L)
  expect_identical(nrow(find_shared_edges(m)), 4L)
  expect_identical(length(find_arcs(m, quotient = TRUE)), 6L)
})

test_that("tolerance applies after wrapping, and wraps at the period", {
  v <- data.frame(.vx = 1:4,
                  x = c(-180, 180 + 1e-10, 359.9999999, 0),
                  y = c(0, 0, 5, 5))
  pool <- new_wkpool(v, vx0 = c(1L, 3L), vx1 = c(2L, 4L))

  # exact wrap: 180 + 1e-10 is not -180 %% 360, 359.9999999 is not 0
  expect_identical(nrow(pool_vertices(merge_coincident(pool, period_x = 360))), 4L)
  # wrap then snap fuses both pairs (the snapped 360 wraps to 0)
  m <- merge_coincident(pool, tolerance = 1e-6, period_x = 360)
  expect_identical(nrow(pool_vertices(m)), 2L)
  # tolerance alone fuses neither pair
  expect_identical(nrow(pool_vertices(merge_coincident(pool, tolerance = 1e-6))), 4L)
})

test_that("other periods work (radians)", {
  v <- data.frame(.vx = 1:3, x = c(-pi, pi, 0), y = c(0, 0, 1))
  pool <- new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(3L, 3L))
  m <- merge_coincident(pool, period_x = 2 * pi)
  expect_identical(nrow(pool_vertices(m)), 2L)
})

test_that("emitted geometry after fusing has coordinate jumps across the seam", {
  m <- merge_coincident(establish_topology(hemispheres()), period_x = 360)
  out <- wk::as_wkt(cycles_to_wkb(m))
  # the eastern hemisphere now references the -180 seam vertices
  expect_identical(
    as.character(out[2]),
    "POLYGON ((0 -60, 90 -60, -180 -60, -180 60, 90 60, 0 60, 0 -60))"
  )
  expect_identical(wk::wk_crs(out), "EPSG:4326")
})

test_that("emit, re-establish and merge with the same period is stable", {
  m <- merge_coincident(establish_topology(hemispheres()), period_x = 360)
  m2 <- merge_coincident(establish_topology(cycles_to_wkb(m)), period_x = 360)
  expect_identical(pool_vertices(m2), pool_vertices(m))
  expect_identical(pool_segments(m2)[c(".vx0", ".vx1", ".feature")],
                   pool_segments(m)[c(".vx0", ".vx1", ".feature")])
})

test_that("topology_report counts duplicates with x wrapped", {
  p <- establish_topology(hemispheres())
  expect_identical(topology_report(p)$n_shared_edges, 1L)
  r <- topology_report(p, period_x = 360)
  expect_identical(r$n_vertices_unique, 8L)
  expect_identical(r$n_shared_edges, 2L)
})

test_that("period_x applies to triangle pools", {
  skip_if_not_installed("decido")
  tri <- pool_triangulate(establish_topology(hemispheres()))
  expect_identical(nrow(pool_vertices(merge_coincident(tri))), 10L)
  m <- merge_coincident(tri, period_x = 360)
  expect_identical(nrow(pool_vertices(m)), 8L)
})

test_that("period_x is validated", {
  p <- establish_topology(hemispheres())
  expect_error(merge_coincident(p, period_x = 0), "period_x")
  expect_error(merge_coincident(p, period_x = c(360, 180)), "period_x")
  expect_error(merge_coincident(p, period_x = "360"), "period_x")
  expect_error(topology_report(p, period_x = -1), "period_x")
})

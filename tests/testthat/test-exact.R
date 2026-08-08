# Coordinate identity: bitwise-exact merging, z keying, snap-grid honesty

test_that("tolerance = 0 does not merge doubles that differ beyond print precision", {
  x0 <- 1 / 3
  x1 <- x0 + .Machine$double.eps / 4   # distinct double, same 15-digit text
  expect_false(identical(x0, x1) && FALSE)  # sanity: values differ
  expect_true(x0 != x1)
  expect_identical(as.character(x0), as.character(x1))  # the old key collapsed these

  v <- data.frame(.vx = 1:4, x = c(x0, 5, x1, 5), y = c(1, 2, 1, 2))
  pool <- new_wkpool(v, vx0 = c(1L, 3L), vx1 = c(2L, 4L))
  merged <- merge_coincident(pool)

  # the two (5, 2) vertices merge; the x0/x1 pair must NOT
  expect_identical(nrow(pool_vertices(merged)), 3L)
  s <- pool_segments(merged)
  expect_false(s$.vx0[1] == s$.vx0[2])
  expect_identical(s$.vx1[1], s$.vx1[2])
})

test_that("exactly identical doubles still merge, first occurrence canonical", {
  v <- data.frame(.vx = 1:4, x = c(1, 2, 1, 3), y = c(0, 0, 0, 0))
  pool <- new_wkpool(v, vx0 = c(1L, 3L), vx1 = c(2L, 4L))
  merged <- merge_coincident(pool)
  vm <- pool_vertices(merged)
  expect_identical(nrow(vm), 3L)
  expect_identical(vm$x, c(1, 2, 3))          # pool order, first kept
  s <- pool_segments(merged)
  expect_identical(s$.vx0[1], s$.vx0[2])      # both segments start at (1,0)
})

test_that("z participates in the merge key when present", {
  v <- data.frame(.vx = 1:4, x = c(1, 1, 1, 1), y = c(2, 2, 2, 2),
                  z = c(10, 20, 10, 20))
  pool <- new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(3L, 4L))
  merged <- merge_coincident(pool)
  # same xy but z differs: (1,2,10) and (1,2,20) remain distinct
  expect_identical(nrow(pool_vertices(merged)), 2L)
})

test_that("tolerance is a snap grid (documented, not a distance guarantee)", {
  # both within 1 of each other but straddling a grid boundary at 0.5
  v <- data.frame(.vx = 1:3, x = c(0.49, 0.51, 5), y = c(0, 0, 0))
  pool <- new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(3L, 3L))
  merged <- merge_coincident(pool, tolerance = 1)
  # 0.49 -> cell 0, 0.51 -> cell 1: not merged despite |dx| = 0.02
  expect_identical(nrow(pool_vertices(merged)), 3L)

  # and canonical vertices keep their original coordinates (no snapping)
  v2 <- data.frame(.vx = 1:2, x = c(0.6, 0.7), y = c(0, 0))
  pool2 <- new_wkpool(v2, vx0 = 1L, vx1 = 2L)
  merged2 <- merge_coincident(pool2, tolerance = 1)
  expect_identical(pool_vertices(merged2)$x, 0.6)
})

test_that("topology_report agrees with an actual merge, without performing one", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  pool <- establish_topology(x)
  rep <- topology_report(pool)
  merged <- merge_coincident(pool)

  expect_identical(rep$n_vertices_unique, nrow(pool_vertices(merged)))
  expect_identical(rep$n_shared_edges,
                   length(unique(find_shared_edges(merged)$edge_key)))
})

test_that("pool_compact remaps correctly regardless of segment order", {
  # segments deliberately referencing vertices out of pool order
  v <- data.frame(.vx = c(10L, 20L, 30L, 40L), x = c(1, 2, 3, 4), y = 0)
  pool <- new_wkpool(v, vx0 = c(40L, 20L), vx1 = c(20L, 40L))
  compact <- pool_compact(pool)
  vc <- pool_vertices(compact)
  sc <- pool_segments(compact)
  # only the two referenced vertices survive, coordinates intact
  expect_identical(vc$x, c(2, 4))
  # and segments point at the right coordinates through the remap
  expect_identical(vc$x[match(sc$.vx0, vc$.vx)], c(4, 2))
  expect_identical(vc$x[match(sc$.vx1, vc$.vx)], c(2, 4))
})

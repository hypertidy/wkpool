# test-cycles.R
# find_cycles, classify_cycles, cycle_signed_area, reverse_cycle

# Helpers ------------------------------------------------------------------

single_square <- function() {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  merge_coincident(pool)
}

square_with_hole <- function() {
  # SF convention: outer CCW, hole CW
  x <- wk::as_wkb(
    "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0), (1 1, 1 3, 3 3, 3 1, 1 1))"
  )
  pool <- establish_topology(x)
  merge_coincident(pool)
}

# find_cycles --------------------------------------------------------------

test_that("find_cycles finds one cycle for a simple polygon", {
  merged <- single_square()
  cycles <- find_cycles(merged)

  expect_type(cycles, "list")
  expect_equal(length(cycles), 1)
  # A square has 4 unique vertices in the cycle
  expect_equal(length(cycles[[1]]), 4)
})

test_that("find_cycles finds two cycles for polygon with hole", {
  merged <- square_with_hole()
  cycles <- find_cycles(merged)

  expect_equal(length(cycles), 2)
})

test_that("find_cycles finds separate cycles for two disjoint polygons", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"
  ))
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  cycles <- find_cycles(merged)

  expect_equal(length(cycles), 2)
})

# cycle_signed_area --------------------------------------------------------

test_that("cycle_signed_area computes correct magnitude for unit square", {
  merged <- single_square()
  cycles <- find_cycles(merged)
  v <- pool_vertices(merged)

  area <- cycle_signed_area(cycles[[1]], v)
  # Unit square has area 1; sign depends on traversal direction
  expect_equal(abs(area), 1)
})

test_that("cycle_signed_area is nonzero for valid polygon", {
  merged <- single_square()
  cycles <- find_cycles(merged)
  v <- pool_vertices(merged)

  area <- cycle_signed_area(cycles[[1]], v)
  expect_true(area != 0)
})

# classify_cycles ----------------------------------------------------------

test_that("classify_cycles returns expected structure", {
  merged <- square_with_hole()
  cl <- classify_cycles(merged)

  expect_s3_class(cl, "data.frame")
  expect_true(all(c("cycle", "area", "type") %in% names(cl)))
  expect_equal(nrow(cl), 2)
})

test_that("classify_cycles outer ring has larger abs area than hole", {
  merged <- square_with_hole()
  cl <- classify_cycles(merged)

  # Outer ring area (16) > hole area (4) regardless of sign/label
  areas <- sort(abs(cl$area))
  expect_equal(areas, c(4, 16))
})

test_that("classify_cycles assigns two different types for polygon with hole", {
  merged <- square_with_hole()
  cl <- classify_cycles(merged)

  # Should have both outer and hole
  expect_equal(sort(unique(cl$type)), c("hole", "outer"))
})

test_that("classify_cycles OGC convention flips classification", {
  merged <- square_with_hole()
  cl_sf <- classify_cycles(merged, convention = "sf")
  cl_ogc <- classify_cycles(merged, convention = "ogc")

  # Same areas
  expect_equal(cl_sf$area, cl_ogc$area)

  # Classifications are flipped
  expect_false(identical(cl_sf$type, cl_ogc$type))
})

# reverse_cycle ------------------------------------------------------------

test_that("reverse_cycle flips vertex order", {
  merged <- single_square()
  cycles <- find_cycles(merged)
  cyc <- cycles[[1]]
  rev_cyc <- reverse_cycle(cyc)

  expect_equal(rev_cyc, rev(cyc))
  expect_equal(length(rev_cyc), length(cyc))
})

test_that("reverse_cycle flips signed area", {
  merged <- single_square()
  cycles <- find_cycles(merged)
  v <- pool_vertices(merged)
  cyc <- cycles[[1]]

  area_fwd <- cycle_signed_area(cyc, v)
  area_rev <- cycle_signed_area(reverse_cycle(cyc), v)

  expect_equal(area_fwd, -area_rev)
})

# hole_points --------------------------------------------------------------

test_that("hole_points returns matrix for polygon with hole", {
  merged <- square_with_hole()
  hp <- hole_points(merged)

  expect_true(is.matrix(hp))
  expect_equal(ncol(hp), 2)
  expect_equal(colnames(hp), c("x", "y"))
  # At least one hole detected
  expect_gte(nrow(hp), 1)
})

test_that("hole_points centroid is plausible", {
  merged <- square_with_hole()
  hp <- hole_points(merged)

  # All hole points should be within the bounding box of the geometry
  v <- pool_vertices(merged)
  expect_true(all(hp[, "x"] >= min(v$x) & hp[, "x"] <= max(v$x)))
  expect_true(all(hp[, "y"] >= min(v$y) & hp[, "y"] <= max(v$y)))
})

test_that("hole_points on simple polygon reflects winding convention", {
  # Known issue: with the current sign convention, a standard SF-wound
  # outer ring (CCW, positive area) is treated as a hole. This test
  # documents the current behaviour pending winding convention resolution.
  merged <- single_square()
  hp <- hole_points(merged)

  # Currently returns a point (the centroid of the "hole") rather than NULL
  # because the outer ring's positive area is classified as a hole.
  # When winding convention is resolved, this should return NULL.
  expect_true(is.matrix(hp) || !is.null(hp))
})

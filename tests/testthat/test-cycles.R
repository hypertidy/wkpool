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

test_that("classify_cycles with provenance is structural: convention has no effect", {
  merged <- square_with_hole()
  cl_sf <- classify_cycles(merged, convention = "sf")
  cl_ogc <- classify_cycles(merged, convention = "ogc")

  # Same areas
  expect_equal(cl_sf$area, cl_ogc$area)

  # Ring roles come from path provenance (first ring of a part is the
  # exterior), so the winding convention is not consulted and both
  # calls agree - and are correct
  expect_identical(cl_sf$type, cl_ogc$type)
  expect_identical(sum(cl_sf$type == "outer"), 1L)
  expect_identical(sum(cl_sf$type == "hole"), 1L)
  expect_true(all(c(".path", ".feature") %in% names(cl_sf)))
})

test_that("classify_cycles without provenance falls back to winding convention", {
  merged <- square_with_hole()
  # strip provenance: rebuild the pool without .path
  s <- pool_segments(merged)
  bare <- new_wkpool(pool_vertices(merged), s$.vx0, s$.vx1,
                     feature = s$.feature)
  cl_sf <- classify_cycles(bare, convention = "sf")
  cl_ogc <- classify_cycles(bare, convention = "ogc")

  expect_equal(cl_sf$area, cl_ogc$area)
  # fallback classifies by winding, so the conventions flip
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

test_that("hole_points on simple polygon returns NULL (winding resolved)", {
  # Resolved: ring roles come from path provenance, so a simple polygon
  # has no holes regardless of its winding or the convention argument.
  merged <- single_square()
  expect_null(hole_points(merged))
  expect_null(hole_points(merged, convention = "ogc"))
})

test_that("pool_holes and hole_points agree", {
  merged <- merge_coincident(establish_topology(wk::as_wkb(
    "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1))"
  )))
  expect_identical(pool_holes(merged), hole_points(merged))
  expect_identical(pool_holes(merged, convention = "ogc"),
                   hole_points(merged, convention = "ogc"))
})

test_that("pool_holes point is inside a non-convex hole", {
  # a C-shaped hole whose vertex mean is outside the hole
  merged <- merge_coincident(establish_topology(wk::as_wkb(paste0(
    "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), ",
    "(2 2, 8 2, 8 3, 3 3, 3 7, 8 7, 8 8, 2 8, 2 2))"
  ))))
  hp <- pool_holes(merged)
  expect_equal(nrow(hp), 1L)
  hole_x <- c(2, 8, 8, 3, 3, 8, 8, 2)
  hole_y <- c(2, 2, 3, 3, 7, 7, 8, 8)
  # mean of the ring is (5.25, 5): outside the C
  inside <- function(px, py, x, y) {
    j <- c(length(x), seq_len(length(x) - 1L))
    sum(((y > py) != (y[j] > py)) &
        (px < (x[j] - x) * (py - y) / (y[j] - y) + x)) %% 2 == 1
  }
  expect_false(inside(mean(hole_x), mean(hole_y), hole_x, hole_y))
  expect_true(inside(hp[1, "x"], hp[1, "y"], hole_x, hole_y))
})

test_that("pool_holes point avoids an island in the hole", {
  # the fattest ear of the square hole would contain the island
  merged <- merge_coincident(establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (2 2, 8 2, 8 8, 2 8, 2 2))",
    "POLYGON ((4 4, 5 4, 5 5, 4 5, 4 4))"
  ))))
  hp <- pool_holes(merged)
  expect_equal(nrow(hp), 1L)
  expect_false(hp[1, "x"] >= 4 && hp[1, "x"] <= 5 &&
               hp[1, "y"] >= 4 && hp[1, "y"] <= 5)
  expect_true(hp[1, "x"] > 2 && hp[1, "x"] < 8 &&
              hp[1, "y"] > 2 && hp[1, "y"] < 8)
})

test_that("pool_holes drops a hole filled by another polygon", {
  enclave <- wk::as_wkb(c(
    "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (2 2, 8 2, 8 8, 2 8, 2 2))",
    "POLYGON ((2 2, 8 2, 8 8, 2 8, 2 2))"
  ))
  expect_null(pool_holes(merge_coincident(establish_topology(enclave))))
  # the hole alone is still a hole
  expect_equal(
    nrow(pool_holes(merge_coincident(establish_topology(enclave[1])))), 1L
  )
})

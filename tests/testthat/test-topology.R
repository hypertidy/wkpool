# test-topology.R
# find_shared_edges, find_neighbours, find_internal_boundaries, topology_report

two_squares <- function() {
  wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
}

# find_shared_edges --------------------------------------------------------

test_that("find_shared_edges detects shared boundary", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  shared <- find_shared_edges(merged)

  expect_s3_class(shared, "data.frame")
  expect_gt(nrow(shared), 0)

  # The shared edge is between vertices at (1,0)-(1,1)
  # Should involve exactly features 1 and 2
  all_feats <- unique(shared$.feature)
  expect_true(all(c(1L, 2L) %in% all_feats))
})

test_that("find_shared_edges returns empty for disjoint polygons", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"
  ))
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  shared <- find_shared_edges(merged)

  expect_equal(nrow(shared), 0)
})

# find_internal_boundaries -------------------------------------------------

test_that("find_internal_boundaries finds opposite-direction edges", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  internal <- find_internal_boundaries(merged)

  expect_s3_class(internal, "wkpool")
  # The shared boundary has 1 edge traversed in opposite directions = 2 segments
  expect_gt(length(internal), 0)
})

test_that("find_internal_boundaries empty for single polygon", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  internal <- find_internal_boundaries(merged)

  expect_equal(length(internal), 0)
})

# find_neighbours ----------------------------------------------------------

test_that("find_neighbours edge-based adjacency", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  nb <- find_neighbours(merged, type = "edge")

  expect_s3_class(nb, "data.frame")
  expect_true(all(c("feature_a", "feature_b") %in% names(nb)))
  expect_equal(nrow(nb), 1) # one pair: 1-2
  expect_equal(nb$feature_a, 1L)
  expect_equal(nb$feature_b, 2L)
})

test_that("find_neighbours vertex-based adjacency", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  nb <- find_neighbours(merged, type = "vertex")

  expect_s3_class(nb, "data.frame")
  expect_equal(nrow(nb), 1)
})

test_that("find_neighbours returns empty for disjoint polygons", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"
  ))
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  nb <- find_neighbours(merged, type = "edge")

  expect_equal(nrow(nb), 0)
})

test_that("find_neighbours errors without feature info", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  # Remove feature by reconstructing without it
  v <- pool_vertices(pool)
  s <- pool_segments(pool)

  pool_nofeat <- new_wkpool(v, vx0 = s$.vx0, vx1 = s$.vx1)
  expect_error(find_neighbours(pool_nofeat))
})

# topology_report ----------------------------------------------------------

test_that("topology_report returns expected structure", {
  pool <- establish_topology(two_squares())
  report <- topology_report(pool)

  expect_type(report, "list")
  expected_names <- c(
    "n_vertices_raw", "n_vertices_unique", "n_duplicate_vertices",
    "n_near_miss_vertices", "n_segments", "n_shared_edges", "n_features"
  )
  expect_true(all(expected_names %in% names(report)))
})

test_that("topology_report detects duplicates before merge", {
  pool <- establish_topology(two_squares())
  report <- topology_report(pool)

  # 10 raw, 6 unique, 4 duplicates
  expect_equal(report$n_vertices_raw, 10)
  expect_equal(report$n_vertices_unique, 6)
  expect_equal(report$n_duplicate_vertices, 4)
  expect_equal(report$n_features, 2)
})

test_that("topology_report shows zero duplicates after merge", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  report <- topology_report(merged)

  expect_equal(report$n_duplicate_vertices, 0)
})

# Three polygons -----------------------------------------------------------

test_that("three adjacent polygons have correct neighbours", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
    "POLYGON ((2 0, 3 0, 3 1, 2 1, 2 0))"
  ))
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  nb <- find_neighbours(merged, type = "edge")

  # 1-2 and 2-3 are neighbours; 1-3 are not
  expect_equal(nrow(nb), 2)
  expect_true(all(nb$feature_a < nb$feature_b))
})

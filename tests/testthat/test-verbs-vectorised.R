# Vectorised verbs: find_cycles, cycles_signed_area, hole_points,
# find_neighbours, find_shared_edges must match their per-element
# definitions

grid_polys <- function() {
  wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
    "POLYGON ((0 1, 1 1, 1 2, 0 2, 0 1))",
    "POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))",
    # stacked on feature 2: three features share edge (1 0)-(2 0)
    "POLYGON ((1 0, 2 0, 2 -1, 1 -1, 1 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
}

holes <- function() {
  wk::as_wkb(c(
    paste0("MULTIPOLYGON (((0 0, 0 10, 10 10, 10 0, 0 0), ",
           "(1 1, 2 1, 2 2, 1 2, 1 1), (5 5, 7 5, 7 7, 5 7, 5 5)), ",
           "((20 0, 20 4, 24 4, 24 0, 20 0), (21 1, 22 1, 22 2, 21 1)))"),
    "POLYGON ((10 0, 10 10, 15 10, 15 0, 10 0))"
  ))
}

test_that("find_cycles is unaffected by interleaving segments across paths", {
  m <- merge_coincident(establish_topology(holes()))
  cyc <- find_cycles(m)
  shuffled <- find_cycles(m[order(seq_along(m) %% 3)])
  # same rings on the same paths; a reordered closed ring may start at a
  # different vertex
  expect_identical(attr(shuffled, "path"), attr(cyc, "path"))
  expect_identical(lapply(shuffled, sort), lapply(cyc, sort))
})

test_that("classify_cycles areas match cycle_signed_area per cycle", {
  m <- merge_coincident(establish_topology(holes()))
  cyc <- find_cycles(m)
  each <- vapply(cyc, cycle_signed_area, numeric(1), pool = pool_vertices(m))
  expect_identical(classify_cycles(m)$area, each)
  expect_equal(sum(classify_cycles(m)$type == "hole"), 3)
})

test_that("hole_points are per-hole vertex means", {
  m <- merge_coincident(establish_topology(holes()))
  hp <- hole_points(m)
  expect_equal(nrow(hp), 3)
  expect_equal(unname(hp[1, ]), c(1.5, 1.5))
  expect_equal(unname(hp[2, ]), c(6, 6))
  expect_equal(colnames(hp), c("x", "y"))
})

test_that("find_neighbours handles an edge shared by three features", {
  m <- merge_coincident(establish_topology(grid_polys()))
  nb <- find_neighbours(m, "edge")
  key <- paste(nb$feature_a, nb$feature_b)
  expect_true(all(c("2 5", "2 6", "5 6") %in% key))
  expect_true(all(nb$feature_a < nb$feature_b))
  expect_false(anyDuplicated(key) > 0)
  # vertex neighbours are a superset of edge neighbours
  nbv <- find_neighbours(m, "vertex")
  expect_true(all(key %in% paste(nbv$feature_a, nbv$feature_b)))
  # 1 and 4 touch only at the corner (1 1)
  expect_false("1 4" %in% key)
  expect_true("1 4" %in% paste(nbv$feature_a, nbv$feature_b))
})

test_that("find_shared_edges lists each shared edge's distinct features", {
  m <- merge_coincident(establish_topology(grid_polys()))
  se <- find_shared_edges(m)
  stacked <- se$features[se$.feature == 5]
  expect_true(any(vapply(stacked, function(f) identical(sort(f), c(2L, 5L, 6L)), logical(1))))
  expect_identical(names(se$features), se$edge_key)
})

test_that("cycles_to_wkb rebuilds mixed POLYGON and MULTIPOLYGON features in order", {
  x <- wk::as_wkt(c(
    "MULTIPOLYGON (((0 0, 0 10, 10 10, 10 0, 0 0), (1 1, 2 1, 2 2, 1 2, 1 1)), ((20 0, 20 4, 24 4, 24 0, 20 0)))",
    "POLYGON ((10 0, 10 10, 15 10, 15 0, 10 0))",
    "MULTIPOLYGON (((30 0, 30 1, 31 1, 30 0)), ((40 0, 40 1, 41 1, 40 0)), ((50 0, 50 1, 51 1, 50 0)))",
    "POLYGON ((60 0, 60 1, 61 1, 60 0))"
  ))
  m <- merge_coincident(establish_topology(x))
  out <- cycles_to_wkt(m)
  expect_equal(length(out), 4)
  expect_identical(wk::wk_meta(out)$geometry_type, c(6L, 3L, 6L, 3L))
  expect_identical(wk::wk_meta(wk::as_wkb(out))$size, c(2L, 1L, 3L, 1L))
  expect_identical(unclass(out), unclass(x))
  expect_true(all(wk::wk_coords(out)$x == wk::wk_coords(x)$x))
})

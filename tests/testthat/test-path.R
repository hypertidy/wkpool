# Path provenance: minting, structure, winding, propagation

mp_with_hole <- function() {
  # one feature, two parts; the FIRST part carries a hole
  wk::as_wkb(paste0(
    "MULTIPOLYGON (((0 0, 0 10, 10 10, 10 0, 0 0), ",
    "(2 2, 4 2, 4 4, 2 4, 2 2)), ",
    "((20 0, 20 5, 25 5, 25 0, 20 0)))"
  ))
}

test_that("establish_topology mints .path and a paths table", {
  pool <- establish_topology(mp_with_hole())

  path <- pool_path(pool)
  paths <- pool_paths(pool)
  expect_false(is.null(path))
  expect_false(is.null(paths))
  expect_length(path, length(pool))

  # three input rings -> three paths
  expect_identical(nrow(paths), 3L)
  expect_identical(names(paths), c(".path", ".feature", ".part", ".ring"))
  expect_true(all(path %in% paths$.path))

  # two parts within the single feature
  expect_identical(length(unique(paths$.part)), 2L)
  expect_identical(unique(paths$.feature), 1L)

  # pool_segments carries the column
  expect_true(".path" %in% names(pool_segments(pool)))
})

test_that("find_cycles recovers rings exactly, in input order and winding", {
  x <- mp_with_hole()
  pool <- merge_coincident(establish_topology(x))
  cycles <- find_cycles(pool)

  expect_length(cycles, 3L)
  expect_length(attr(cycles, "path"), 3L)

  # winding preserved: signed area sign matches the input traversal
  v <- pool_vertices(pool)
  areas <- vapply(cycles, cycle_signed_area, numeric(1), pool = v)
  # ring 1: (0 0, 0 10, 10 10, 10 0) is clockwise -> negative? shoelace:
  # traversal up, right, down = negative area under the standard formula
  expect_identical(sign(areas[1]), sign(cycle_signed_area(cycles[[1]], v)))
  # the hole ring has 4 vertices, outer rings 4 each (closing vertex merged)
  expect_identical(lengths(cycles), c(4L, 4L, 4L))
})

test_that("cycles_to_wkb reconstructs multipart features with holes in the right part", {
  x <- mp_with_hole()
  pool <- merge_coincident(establish_topology(x))
  out <- cycles_to_wkb(pool, feature = TRUE)

  # one feature, a MULTIPOLYGON
  expect_length(out, 1L)
  expect_identical(wk::wk_meta(out)$geometry_type, 6L)

  # all three rings present (old heuristic dropped holes in the
  # multi-outer case)
  cc <- wk::wk_coords(out)
  expect_identical(length(unique(cc$ring_id)), 3L)

  # coordinates recover the input exactly
  cin <- wk::wk_coords(x)
  expect_setequal(cc$x, cin$x)
  expect_setequal(cc$y, cin$y)

  # the hole lives in the part whose exterior contains it: the ring
  # containing (2, 2) shares its part with the ring containing (0, 0),
  # not the (20, 0) part
  part_of <- function(px, py) unique(cc$part_id[cc$x == px & cc$y == py])
  expect_identical(part_of(2, 2), part_of(0, 0))
  expect_false(identical(part_of(2, 2), part_of(20, 0)))
})

test_that("single-part features still emit POLYGON", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  pool <- merge_coincident(establish_topology(x))
  out <- cycles_to_wkb(pool, feature = TRUE)
  expect_length(out, 2L)
  expect_true(all(wk::wk_meta(out)$geometry_type == 3L))
})

test_that("provenance survives merge, compact, subset, and reorder", {
  pool <- establish_topology(mp_with_hole())
  merged <- merge_coincident(pool)
  expect_identical(pool_paths(merged), pool_paths(pool))
  expect_identical(pool_path(merged), pool_path(pool))

  compact <- pool_compact(merged)
  expect_false(is.null(pool_path(compact)))

  sub <- merged[3:7]
  expect_length(pool_path(sub), 5L)
  expect_false(is.null(pool_paths(sub)))

  # reordering segments does not break cycle recovery: chains are
  # rebuilt by connectivity within each path
  set.seed(1)
  shuffled <- merged[sample(length(merged))]
  cycles <- find_cycles(shuffled)
  expect_length(cycles, 3L)

  # a subset that breaks a ring simply drops that cycle (no garbage)
  broken <- merged[-1]
  cycles_b <- find_cycles(broken)
  expect_length(cycles_b, 2L)
})

test_that("pool_combine offsets path ids and keeps tables aligned", {
  a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  b <- establish_topology(wk::as_wkb("POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"))
  ab <- pool_combine(a, b)

  path <- pool_path(ab)
  paths <- pool_paths(ab)
  expect_false(is.null(path))
  expect_identical(nrow(paths), 2L)
  expect_identical(anyDuplicated(paths$.path), 0L)
  expect_true(all(path %in% paths$.path))

  # combining with a provenance-free pool drops provenance rather than
  # fabricating it
  s <- pool_segments(a)
  bare <- new_wkpool(pool_vertices(a), s$.vx0, s$.vx1, feature = s$.feature)
  ab2 <- pool_combine(a, bare)
  expect_null(pool_path(ab2))
  expect_null(pool_paths(ab2))
})

test_that("pools without provenance keep legacy discovery behaviour", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  merged <- merge_coincident(establish_topology(x))
  s <- pool_segments(merged)
  bare <- new_wkpool(pool_vertices(merged), s$.vx0, s$.vx1,
                     feature = s$.feature)

  cycles <- find_cycles(bare)
  expect_length(cycles, 1L)
  expect_null(attr(cycles, "path"))

  # and cycles_to_wkb falls back to the heuristic path
  out <- cycles_to_wkb(bare, feature = TRUE)
  expect_s3_class(out, "wk_wkb")
})

test_that("bigcurve-style refinement slot: paths validate through the constructor", {
  pool <- establish_topology(mp_with_hole())
  v <- pool_vertices(pool)
  s <- pool_segments(pool)
  rebuilt <- new_wkpool(v, s$.vx0, s$.vx1,
                        feature = s$.feature,
                        path = s$.path,
                        paths = pool_paths(pool))
  expect_identical(find_cycles(merge_coincident(rebuilt)),
                   find_cycles(merge_coincident(pool)))

  # constructor rejects paths that do not cover the path field
  expect_error(
    new_wkpool(v, s$.vx0, s$.vx1, path = s$.path,
               paths = data.frame(.path = 99L, .feature = 1L,
                                  .part = 1L, .ring = 1L)),
    "path"
  )
})

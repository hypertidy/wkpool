## an n x n grid of unit squares, two counter-clockwise triangles per cell,
## cells numbered along x first
grid_tri <- function(n = 3L) {
  g <- expand.grid(i = 0:n, j = 0:n)
  v <- data.frame(.vx = seq_len(nrow(g)), x = g$i, y = g$j)
  id <- function(i, j) j * (n + 1L) + i + 1L
  cell <- expand.grid(i = seq_len(n) - 1L, j = seq_len(n) - 1L)
  a <- id(cell$i, cell$j)
  b <- id(cell$i + 1L, cell$j)
  c <- id(cell$i + 1L, cell$j + 1L)
  d <- id(cell$i, cell$j + 1L)
  new_wkpool_tri(v, vx0 = c(a, a), vx1 = c(b, c), vx2 = c(c, d),
                 feature = rep(seq_len(nrow(cell)), 2L))
}

## group per triangle from a group per cell
cell_group <- function(g) c(g, g)

geom_area <- function(x) {
  co <- wk::wk_coords(x)
  ring <- paste(co$feature_id, co$part_id, co$ring_id)
  a <- vapply(split(co, factor(ring, levels = unique(ring))), function(r) {
    j <- c(seq_len(nrow(r))[-1L], 1L)
    sum(r$x * r$y[j] - r$x[j] * r$y) / 2
  }, numeric(1))
  f <- co$feature_id[!duplicated(ring)]
  out <- numeric(length(x))
  s <- tapply(a, f, sum)
  out[as.integer(names(s))] <- s
  out
}

n_rings <- function(x) {
  co <- wk::wk_coords(x)
  nrow(unique(co[c("feature_id", "part_id", "ring_id")]))
}

test_that("tri_dissolve() gives one MULTIPOLYGON per group id", {
  tri <- grid_tri(3L)
  out <- tri_dissolve(tri, cell_group(c(1, 1, 2, 1, 1, 2, 3, 3, 3)))
  expect_s3_class(out, "wk_wkb")
  expect_length(out, 3L)
  expect_equal(geom_area(out), c(4, 2, 3))
  expect_equal(unique(wk::wk_meta(out)$geometry_type), 6L)
  ## one ring per group, outer and counter-clockwise
  expect_equal(n_rings(out), 3L)
})

test_that("the default group is the triangle feature", {
  out <- tri_dissolve(grid_tri(2L))
  expect_length(out, 4L)
  expect_equal(geom_area(out), rep(1, 4))
  expect_equal(n_rings(out), 4L)
})

test_that("NA triangles are left out and unused ids are empty", {
  tri <- grid_tri(3L)
  g <- cell_group(c(1, 1, 1, 1, NA, 1, 1, 1, 3))
  out <- tri_dissolve(tri, g)
  expect_length(out, 3L)
  expect_equal(geom_area(out), c(7, 0, 1))
  expect_equal(wk::as_wkt(out[2]), wk::wkt("MULTIPOLYGON EMPTY"))
  ## the NA cell is a hole: outer ring and hole, one polygon
  meta <- wk::wk_coords(out[1])
  expect_equal(length(unique(meta$ring_id)), 2L)
  expect_equal(length(unique(meta$part_id)), 1L)
  expect_length(tri_dissolve(tri, rep(NA_integer_, length(tri))), 0L)
})

test_that("a hole touching its outer ring at a vertex is split there", {
  ## the centre cell is a hole and the top right cell is cut away: they
  ## meet at (2, 2), which the boundary walk passes through twice
  tri <- grid_tri(3L)
  out <- tri_dissolve(tri, cell_group(c(1, 1, 1, 1, NA, 1, 1, 1, NA)))
  expect_equal(geom_area(out), 7)
  co <- wk::wk_coords(out)
  expect_equal(length(unique(co$part_id)), 1L)
  expect_equal(length(unique(co$ring_id)), 2L)
  ## every ring is simple: no vertex repeated within a ring
  ring <- split(paste(co$x, co$y), co$ring_id)
  expect_true(all(vapply(ring, function(r) !anyDuplicated(r[-length(r)]),
                         logical(1))))
})

test_that("parts that touch at a corner are separate polygons", {
  tri <- grid_tri(2L)
  out <- tri_dissolve(tri, cell_group(c(1, NA, NA, 1)))
  expect_equal(geom_area(out), 2)
  co <- wk::wk_coords(out)
  expect_equal(length(unique(co$part_id)), 2L)
})

test_that("tri_dissolve() round-trips ear-cut polygons and keeps the crs", {
  skip_if_not_installed("decido")
  x <- wk::wkb(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.3, 0.2 0.5, 0.4 0.5, 0.2 0.3))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
    "MULTIPOLYGON (((3 0, 4 0, 4 1, 3 0)), ((5 0, 6 0, 6 1, 5 0)))"
  )), crs = "EPSG:3857")
  tri <- merge_coincident(pool_triangulate(establish_topology(x)))
  out <- tri_dissolve(tri)
  expect_equal(wk::wk_crs(out), "EPSG:3857")
  expect_equal(geom_area(out), c(1 - 0.02, 1, 1))
  expect_equal(n_rings(out), 5L)
  ## the two squares share an edge: dissolved together they are one ring
  ## plus the hole
  one <- tri_dissolve(tri, ifelse(pool_triangles(tri)$.feature < 3L, 1L, 2L))
  expect_equal(geom_area(one), c(2 - 0.02, 1))
  expect_equal(n_rings(one[1]), 2L)
})

test_that("tri_dissolve() checks its input", {
  tri <- grid_tri(2L)
  expect_error(tri_dissolve(list()), "wkpool_tri")
  expect_error(tri_dissolve(tri, 1L), "one value per triangle")
  expect_error(tri_dissolve(tri, rep(0L, length(tri))), "positive")
  expect_error(tri_dissolve(tri, rep(1.5, length(tri))), "positive")
})

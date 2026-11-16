test_that("a prepared pool gives the same answers", {
  tri <- grid_tri(4L)
  set.seed(1)
  g <- cell_group(sample(c(1:3, NA), 16, replace = TRUE))
  p <- tri_prepare(tri, g)
  expect_true(tri_is_prepared(p))
  expect_false(tri_is_prepared(tri))
  expect_identical(tri_measures(p, g), tri_measures(tri, g))
  expect_identical(tri_relate(p, g), tri_relate(tri, g))
  expect_identical(tri_dissolve(p, g), tri_dissolve(tri, g))
  expect_identical(tri_structure(p, g), tri_structure(tri, g))
  expect_identical(tri_neighbours(p), tri_neighbours(tri))
  ## another group is computed as usual
  h <- rep(1L, length(tri))
  expect_identical(tri_measures(p, h), tri_measures(tri, h))
  expect_identical(tri_dissolve(p, h), tri_dissolve(tri, h))
})

test_that("the cache is not used once the pool changes", {
  tri <- tri_prepare(grid_tri(2L))
  expect_false(tri_is_prepared(tri[1:4]))
  changed <- tri
  v <- attr(changed, "pool")
  v$x[2] <- 0.5
  attr(changed, "pool") <- v
  expect_false(tri_is_prepared(changed))
  plain <- changed
  attr(plain, "tri_cache") <- NULL
  expect_identical(tri_measures(changed, rep(1L, 8)), tri_measures(plain, rep(1L, 8)))
})

test_that("tri_structure() counts parts and holes", {
  tri <- grid_tri(3L)
  ## group 1 surrounds the middle cell, group 2 is the corner cell it leaves
  g <- cell_group(c(1, 1, 1, 1, NA, 1, 1, 1, 2))
  s <- tri_structure(tri, g)
  expect_equal(s$n_parts, c(1L, 1L))
  expect_equal(s$n_holes, c(1L, 0L))
  expect_equal(s$n_rings, c(2L, 1L))
  ## two cells that meet at a corner are two parts
  s1 <- tri_structure(tri, cell_group(c(1, NA, NA, NA, 1, NA, NA, NA, NA)))
  expect_equal(s1$n_parts, 2L)
  s2 <- tri_structure(tri_prepare(tri, g), g)
  expect_identical(s2, s)
  ## an unused id
  s3 <- tri_structure(tri, cell_group(c(3, 3, 3, 3, 3, 3, 3, 3, 3)))
  expect_equal(s3$n_parts, c(0L, 0L, 1L))
})

test_that("tri_structure() matches geos", {
  skip_if_not_installed("geos")
  tri <- grid_tri(6L)
  set.seed(3)
  g <- cell_group(sample(c(1:4, NA), 36, replace = TRUE))
  s <- tri_structure(tri, g)
  geom <- geos::as_geos_geometry(tri_dissolve(tri, g))
  expect_equal(s$n_parts, geos::geos_num_geometries(geom))
  co <- wk::wk_coords(tri_dissolve(tri, g))
  rings <- unique(co[c("feature_id", "part_id", "ring_id")])
  expect_equal(s$n_rings, tabulate(rings$feature_id, 4))
})

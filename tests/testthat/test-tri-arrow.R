test_that("the feature layout writes one row per group and reads back", {
  skip_if_not_installed("nanoarrow")
  tri <- grid_tri(3L)
  set.seed(1)
  g <- cell_group(sample(c(1:3, NA), 9, replace = TRUE))
  a <- tri_as_arrow(tri, g)
  expect_s3_class(a, "nanoarrow_array")
  expect_equal(a$length, 3L)
  sch <- nanoarrow::infer_nanoarrow_schema(a)
  expect_equal(sch$metadata[["ARROW:extension:name"]], "geoarrow.triangles")
  expect_equal(sch$format, "+l")
  b <- tri_from_arrow(a)
  expect_s3_class(b$mesh, "wkpool_tri")
  expect_equal(length(b$mesh), sum(!is.na(g)))
  expect_equal(tri_measures(b$mesh, b$group), tri_measures(tri, g))
  expect_equal(tri_relate(b$mesh, b$group), tri_relate(tri, g))
  ## a coverage keeps the group as the feature
  expect_equal(tri_measures(b$mesh), tri_measures(tri, g))
  ## vertices: only those used, by first appearance
  v <- pool_vertices(b$mesh)
  expect_equal(nrow(v), length(unique(unlist(pool_triangles(tri)[!is.na(g), 1:3]))))
  t <- pool_triangles(b$mesh)
  expect_equal(unique(as.vector(t(as.matrix(t[1:3])))), seq_len(nrow(v)))
})

test_that("a triangle in several groups is one triangle when read back", {
  skip_if_not_installed("nanoarrow")
  tri <- grid_tri(3L)
  og <- data.frame(triangle = c(1:18, 1:4), group = c(rep(1, 18), rep(2, 4)))
  b <- tri_from_arrow(tri_as_arrow(tri, og))
  expect_equal(length(b$mesh), 18L)
  expect_equal(as.vector(table(b$group$group)), c(18L, 4L))
  expect_equal(tri_measures(b$mesh, b$group), tri_measures(tri, og))
  expect_null(pool_feature(b$mesh))
})

test_that("the triangle layout writes one row per triangle", {
  skip_if_not_installed("nanoarrow")
  tri <- grid_tri(2L)
  a <- tri_as_arrow(tri, layout = "triangle")
  expect_equal(a$length, length(tri))
  expect_true(startsWith(nanoarrow::infer_nanoarrow_schema(a)$format, "+w:3"))
  b <- tri_from_arrow(a)
  expect_null(b$group)
  expect_equal(tri_measures(b$mesh, rep(1L, 8))$area, 4)
  expect_equal(tri_neighbours(b$mesh), tri_neighbours(tri))
})

test_that("a slice of the feature layout reads only its rows", {
  skip_if_not_installed("nanoarrow")
  tri <- grid_tri(3L)
  g <- cell_group(c(1, 1, 2, 2, 2, 3, 3, 3, 3))
  a <- tri_as_arrow(tri, g)
  s <- nanoarrow::nanoarrow_array_modify(a, list(offset = 1L, length = 2L))
  b <- tri_from_arrow(s)
  expect_equal(tri_measures(b$mesh, b$group)$area, c(3, 4))
})

test_that("the crs goes into the metadata and comes back", {
  skip_if_not_installed("nanoarrow")
  tri <- grid_tri(2L)
  attr(tri, "crs") <- "EPSG:3857"
  a <- tri_as_arrow(tri)
  meta <- nanoarrow::infer_nanoarrow_schema(a)$metadata[["ARROW:extension:metadata"]]
  expect_match(meta, "EPSG:3857", fixed = TRUE)
  expect_match(meta, "planar", fixed = TRUE)
  expect_equal(attr(tri_from_arrow(a)$mesh, "crs"), "EPSG:3857")
})

test_that("tri_from_arrow() refuses other arrays", {
  skip_if_not_installed("nanoarrow")
  expect_error(tri_from_arrow(nanoarrow::as_nanoarrow_array(1:3)), "fixed-size list")
})

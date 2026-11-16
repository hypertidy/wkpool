## a grid mesh scaled, moved, rotated and with its inner vertices jittered
moved_tri <- function(n, seed = NULL, scale = 1, shift = c(0, 0), rot = 0) {
  tri <- grid_tri(n)
  v <- pool_vertices(tri)
  if (!is.null(seed)) {
    set.seed(seed)
    inner <- v$x > 0 & v$x < n & v$y > 0 & v$y < n
    v$x[inner] <- v$x[inner] + stats::runif(sum(inner), -0.3, 0.3)
    v$y[inner] <- v$y[inner] + stats::runif(sum(inner), -0.3, 0.3)
  }
  x <- scale * (v$x * cos(rot) - v$y * sin(rot)) + shift[1]
  y <- scale * (v$x * sin(rot) + v$y * cos(rot)) + shift[2]
  v$x <- x
  v$y <- y
  t <- pool_triangles(tri)
  new_wkpool_tri(v, t$.vx0, t$.vx1, t$.vx2, feature = pool_feature(tri))
}

random_cells <- function(n, k, seed) {
  set.seed(seed)
  cell_group(sample(c(seq_len(k), NA), n * n, replace = TRUE))
}

## every pair of features against geos_relate()
expect_relate_geos <- function(ov, gx, gy) {
  pairs <- expand.grid(x = seq_along(gx), y = seq_along(gy))
  r <- tri_overlay_relate(ov, pairs)
  expect_equal(r$relate, geos::geos_relate(gx[r$x], gy[r$y]))
  invisible(r)
}

skip_overlay <- function() {
  skip_if_not_installed("cdtr")
  skip_if_not_installed("geos")
}

test_that("polygons of two crossing meshes relate as geos says", {
  skip_overlay()
  a <- moved_tri(6L, seed = 1)
  ga <- random_cells(6L, 5L, 2)
  b <- moved_tri(5L, seed = 3, shift = c(1.3, -0.7), rot = 0.4)
  gb <- random_cells(5L, 4L, 4)
  ov <- tri_overlay(a, b, ga, gb)
  expect_s3_class(ov, "wkpool_overlay")
  expect_s3_class(ov$mesh, "wkpool_tri")
  expect_equal(ov$groups$layer, rep(c("x", "y"), c(5, 4)))
  expect_true("crossing" %in% pool_vertices(ov$mesh)$origin)
  pa <- geos::as_geos_geometry(tri_dissolve(a, ga))
  pb <- geos::as_geos_geometry(tri_dissolve(b, gb))
  r <- expect_relate_geos(ov, pa, pb)
  ## the default pairs are exactly those that are not disjoint
  d <- tri_overlay_relate(ov)
  nd <- r[r$relate != "FF2FF1212", ]
  expect_equal(sort(paste(d$x, d$y)), sort(paste(nd$x, nd$y)))
  ## overlay areas add up to the inputs
  m <- tri_measures(ov$mesh, data.frame(triangle = ov$members$item,
                                        group = ov$members$group))
  expect_equal(m$area, c(geos::geos_area(pa), geos::geos_area(pb)))
})

test_that("shared and overlapping boundaries relate as geos says", {
  skip_overlay()
  a <- grid_tri(6L)
  ga <- random_cells(6L, 6L, 7)
  pa <- geos::as_geos_geometry(tri_dissolve(a, ga))
  ## cells of 1.5 (edges overlap in part) and of 2 (edges on a's grid lines)
  for (s in c(1.5, 2)) {
    n <- 6 / s
    b <- moved_tri(n, scale = s)
    gb <- random_cells(n, 4L, 8)
    ov <- tri_overlay(a, b, ga, gb)
    expect_relate_geos(ov, pa, geos::as_geos_geometry(tri_dissolve(b, gb)))
  }
})

test_that("lines and points relate to polygons and to each other as geos says", {
  skip_overlay()
  a <- grid_tri(6L)
  ga <- random_cells(6L, 6L, 7)
  pa <- geos::as_geos_geometry(tri_dissolve(a, ga))
  lines <- wk::wkt(c(
    "LINESTRING (0 2, 6 2)", "LINESTRING (1 1, 1 5, 3 5)",
    "LINESTRING (2 2, 4 2, 4 4, 2 4, 2 2)",
    "MULTILINESTRING ((0 0, 3 3), (3 3, 6 0))", "LINESTRING (0.5 0.5, 0.5 3)",
    "LINESTRING (-1 3, 0 3)", "LINESTRING (3 0, 3 6)",
    "MULTILINESTRING ((1 1, 2 2), (1 2, 2 1))"
  ))
  points <- wk::wkt(c(
    "POINT (1 1)", "POINT (0 2)", "MULTIPOINT ((3 3), (4 2))", "POINT (0.5 1)",
    "POINT (2 3)", "POINT (-1 3)"
  ))
  gl <- geos::as_geos_geometry(lines)
  gp <- geos::as_geos_geometry(points)
  expect_relate_geos(tri_overlay(a, lines, ga), pa, gl)
  expect_relate_geos(tri_overlay(a, points, ga), pa, gp)
  expect_relate_geos(tri_overlay(lines, points), gl, gp)
  ## one layer alone
  expect_relate_geos(tri_overlay(lines), gl, gl)
  expect_relate_geos(tri_overlay(points), gp, gp)
  ov <- tri_overlay(lines)
  d <- tri_overlay_relate(ov)
  expect_true(all(d$x < d$y))
})

test_that("random lines and points on a jittered mesh relate as geos says", {
  skip_overlay()
  a <- moved_tri(6L, seed = 1)
  ga <- random_cells(6L, 5L, 2)
  pa <- geos::as_geos_geometry(tri_dissolve(a, ga))
  set.seed(5)
  r <- function() stats::runif(8, -1, 7)
  lines <- wk::wkt(sprintf("LINESTRING (%f %f, %f %f, %f %f)", r(), r(), r(), r(), r(), r()))
  expect_relate_geos(tri_overlay(a, lines, ga), pa, geos::as_geos_geometry(lines))
  v <- pool_vertices(a)
  points <- wk::xy(c(stats::runif(20, -1, 7), v$x[c(8, 9, 20)]),
                   c(stats::runif(20, -1, 7), v$y[c(8, 9, 20)]))
  expect_relate_geos(tri_overlay(a, points, ga), pa, geos::as_geos_geometry(points))
})

test_that("a segment pool is a line layer, one line per feature", {
  skip_overlay()
  v <- data.frame(.vx = 1:4, x = c(0, 2, 2, 0), y = c(0, 0, 2, 2))
  pool <- new_wkpool(v, vx0 = c(1L, 2L, 3L), vx1 = c(2L, 3L, 4L),
                     feature = c(1L, 1L, 2L))
  sq <- moved_tri(1L, shift = c(0.5, 0.5))
  ov <- tri_overlay(sq, pool)
  expect_equal(ov$groups$dim, c(2L, 1L, 1L))
  r <- tri_overlay_relate(ov, cbind(1, 1:2))
  want <- geos::geos_relate(
    geos::geos_read_wkt("POLYGON ((0.5 0.5, 1.5 0.5, 1.5 1.5, 0.5 1.5, 0.5 0.5))"),
    geos::geos_read_wkt(c("LINESTRING (0 0, 2 0, 2 2)", "LINESTRING (2 2, 0 2)"))
  )
  expect_equal(r$relate, want)
})

test_that("tri_overlay() checks its inputs", {
  skip_if_not_installed("cdtr")
  sq <- grid_tri(1L)
  expect_error(tri_overlay(sq, wk::wkt("POLYGON ((0 0, 1 0, 1 1, 0 0))")), "wkpool_tri")
  expect_error(tri_overlay_relate(list()), "wkpool_overlay")
  ov <- tri_overlay(sq, wk::wkt("POINT (0.5 0.2)"))
  expect_error(tri_overlay_relate(ov, cbind(1, 2)), "feature ids")
  a <- wk::wk_set_crs(wk::wkt("POINT (0 0)"), "EPSG:4326")
  b <- wk::wk_set_crs(wk::wkt("POINT (1 1)"), "EPSG:3857")
  expect_error(tri_overlay(a, b), "crs")
})

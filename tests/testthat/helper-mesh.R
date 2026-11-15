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

## a grid mesh with the inner vertices moved, so that nothing is axis aligned
jitter_tri <- function(n = 6L, seed = 1L) {
  tri <- grid_tri(n)
  v <- pool_vertices(tri)
  set.seed(seed)
  inner <- v$x > 0 & v$x < n & v$y > 0 & v$y < n
  v$x[inner] <- v$x[inner] + stats::runif(sum(inner), -0.3, 0.3)
  v$y[inner] <- v$y[inner] + stats::runif(sum(inner), -0.3, 0.3)
  t <- pool_triangles(tri)
  new_wkpool_tri(v, t$.vx0, t$.vx1, t$.vx2, feature = pool_feature(tri))
}

## random groups over cells, with some cells left out
random_groups <- function(n, k, seed = 2L) {
  set.seed(seed)
  g <- sample(c(seq_len(k), NA), n * n, replace = TRUE)
  cell_group(g)
}

## polygons of each group, as geos geometries
group_geos <- function(tri, group) {
  geos::as_geos_geometry(tri_dissolve(tri, group))
}

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

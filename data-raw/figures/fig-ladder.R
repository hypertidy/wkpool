## The vertex identity ladder: how two shapes come to share a vertex.
## Each rung is computed with the real package that implements it.
source("R/common.R")

eps <- 1e-12   # B's left edge was written by another program: 1 + 1e-12
A <- "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"
B <- sprintf("POLYGON ((%1$.15f 0, 2 0, 2 0.5, %1$.15f 0.5, %1$.15f 0))", 1 + eps)
x <- wk::wkt(c(A, B))

count <- function(pool) {
  tr <- topology_report(pool)
  c(V = tr$n_vertices_unique, shared = tr$n_shared_edges)
}
## rung: exact float (wkpool, tolerance 0)
p_exact <- merge_coincident(establish_topology(x), tolerance = 0)
## rung: snapped (wkpool, tolerance)
p_snap  <- merge_coincident(establish_topology(x), tolerance = 1e-9)
## rung: noded (GEOS through sf): snap, node the linework, rebuild faces
## (snap to a 1e-9 grid first: noding works on the snapped coordinates)
g <- st_as_sfc(as.character(x))
g <- st_sfc(lapply(g, function(p) st_polygon(lapply(p, round, 9))))
faces <- st_collection_extract(st_polygonize(st_node(st_union(st_boundary(g)))), "POLYGON")
p_noded <- merge_coincident(establish_topology(wk::as_wkb(faces)), tolerance = 0)
## rung: lattice (meshcore): identity by construction from cell ids
G <- GRID(dim = c(4, 2), extent = c(0, 2, 0, 1))

draw_pool <- function(pool, main, sub) {
  v <- pool_vertices(pool); s <- pool_segments(pool)
  blank(c(-0.15, 2.15), c(-0.1, 1.15), main = main)
  sh <- find_shared_edges(pool)
  key <- paste(pmin(s$.vx0, s$.vx1), pmax(s$.vx0, s$.vx1))
  shk <- if (NROW(sh)) paste(pmin(sh$.vx0, sh$.vx1), pmax(sh$.vx0, sh$.vx1)) else character()
  is_sh <- key %in% shk
  x0 <- v$x[match(s$.vx0, v$.vx)]; y0 <- v$y[match(s$.vx0, v$.vx)]
  x1 <- v$x[match(s$.vx1, v$.vx)]; y1 <- v$y[match(s$.vx1, v$.vx)]
  ## nudge B's copy sideways so unshared duplicates are visible
  off <- ifelse(s$.feature == 2 & !is_sh, 0.04, 0)
  segments(x0 + off, y0, x1 + off, y1, col = ifelse(is_sh, pal$s2, ifelse(s$.feature == 1, pal$s1, pal$s3)),
           lwd = ifelse(is_sh, 4, 2))
  deg <- table(c(s$.vx0, s$.vx1))
  nfeat <- tapply(c(s$.feature, s$.feature), c(s$.vx0, s$.vx1), function(f) length(unique(f)))
  shared_v <- as.integer(names(nfeat)[nfeat > 1])
  points(v$x, v$y, pch = 21, cex = ifelse(v$.vx %in% shared_v, 1.6, 1),
         bg = ifelse(v$.vx %in% shared_v, pal$s2, pal$surface), col = pal$ink)
  note(sub, line = -0.6)
}

fig_open("ladder.png", width = 2000, height = 560)
par(oma = c(0, 0, 1.6, 0))
layout(matrix(1:4, 1))
## lattice
b <- boundaries(G); vv <- vertices(G)
blank(c(-0.15, 2.15), c(-0.1, 1.15), main = "lattice (meshcore GRID)")
draw_cells(G, border = pal$s1, lwd = 2)
points(vv$x, vv$y, pch = 21, bg = pal$surface, col = pal$ink, cex = 1)
text(vv$x + 0.03, vv$y + 0.03, vv$.vx, adj = c(0, 0), cex = 0.75, col = pal$ink2)
mc <- mesh_counts(G)
note(sprintf("keys from cell ids, never compared: V = %d, E = %d", mc[["vertices"]], mc[["edges"]]), line = -0.6)

ce <- count(p_exact); cs <- count(p_snap); cn <- count(p_noded)
draw_pool(p_exact, "exact float (wkpool, tolerance 0)",
          sprintf("1 != 1 + 1e-12: V = %d, shared edges = %d", ce[["V"]], ce[["shared"]]))
draw_pool(p_snap, "snapped (wkpool, tolerance 1e-9)",
          sprintf("corner joins, T-junction does not: V = %d, shared = %d", cs[["V"]], cs[["shared"]]))
draw_pool(p_noded, "noded (GEOS via sf)",
          sprintf("vertex inserted on A's edge: V = %d, shared = %d", cn[["V"]], cn[["shared"]]))
mtext("<-  identity by construction (cheap, needs a lattice)                                        identity by computation (more work, any input; exact predicates are the next rung)  ->",
      outer = TRUE, side = 3, line = 0.2, cex = 0.8, col = pal$muted)
dev.off()
cat("lattice", mc, "\nexact", ce, "\nsnapped", cs, "\nnoded", cn, "\n")

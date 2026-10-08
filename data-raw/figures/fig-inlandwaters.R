## inlandwaters (silicate's example data: 6 features, 189 rings, 32 holes)
## through wkpool: the shared-boundary arcs and nodes, and the rings
## classified by winding into outers and holes.
source("R/common.R")
data(inlandwaters, package = "silicate")
x <- wk::as_wkb(st_geometry(inlandwaters))
pool <- merge_coincident(establish_topology(x))

arcs  <- wk::wk_coords(arcs_to_wkb(pool, quotient = TRUE))
nodes <- find_nodes(pool, quotient = TRUE)
v     <- pool_vertices(pool)
cyc   <- classify_cycles(pool)
rings <- wk::wk_coords(cycles_to_wkb(pool, feature = FALSE))  # one polygon per cycle
tr    <- topology_report(pool)
n_arcs <- length(unique(arcs$feature_id))

fig_open("inlandwaters.png", width = 2000, height = 860)
layout(matrix(1:2, 1))
## zoom to the mainland and Tasmania (a few far offshore islands stretch the extent)
big <- rings[rings$feature_id %in% which(abs(cyc$area) > 1e10), ]
bb <- list(x = range(big$x), y = range(big$y))
## 1. arcs and nodes
blank(bb$x, bb$y, main = sprintf("find_arcs(quotient = TRUE): %s arcs meeting at %s nodes",
                                  format(n_arcs, big.mark = ","), format(length(nodes), big.mark = ",")))
arc_col <- rep(c(pal$s1, pal$s2, pal$s3, pal$s4), length.out = n_arcs)
sp <- split(arcs[c("x", "y")], arcs$feature_id)
for (i in seq_along(sp)) lines(sp[[i]]$x, sp[[i]]$y, col = arc_col[i], lwd = 0.9)
nv <- v[match(nodes, v$.vx), ]
points(nv$x, nv$y, pch = 21, cex = 1.3, bg = pal$ink, col = pal$surface)
note(sprintf("%s vertices, %s shared edges between neighbouring features (exact identity)",
             format(tr$n_vertices_unique, big.mark = ","), format(tr$n_shared_edges, big.mark = ",")), line = -0.5)
## 2. rings by winding
blank(bb$x, bb$y, main = sprintf("classify_cycles(): %d rings, %d outer, %d holes",
                                  nrow(cyc), sum(cyc$type == "outer"), sum(cyc$type == "hole")))
rs <- split(rings[c("x", "y")], rings$feature_id)
hole <- cyc$type == "hole"
for (i in which(!hole)) polygon(rs[[i]]$x, rs[[i]]$y, col = pal$seq[2], border = pal$s1, lwd = 0.5)
for (i in which(hole))  polygon(rs[[i]]$x, rs[[i]]$y, col = pal$surface, border = pal$s2, lwd = 1.4)
legend("bottomleft", bty = "n", fill = c(pal$seq[2], pal$surface), border = c(pal$s1, pal$s2),
       legend = c("outer ring", "hole"), text.col = pal$ink2, cex = 0.9)
note("holes are what a UGRID face cannot hold: as_UGRID() drops them and counts them", line = -0.5)
dev.off()
cat(n_arcs, length(nodes), nrow(cyc), sum(hole), "\n")

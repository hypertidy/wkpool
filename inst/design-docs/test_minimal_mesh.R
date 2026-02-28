# test_minimal_mesh.R
# Run this after devtools::load_all()

library(wk)
library(vctrs)

# The minimal_mesh from silicate - two adjacent multipolygons
x <- wk::as_wkb(c(
  "MULTIPOLYGON (((0 0, 0 1, 0.75 1, 1 0.8, 0.5 0.7, 0.8 0.6, 0.6899999999999999 0, 0 0),
  (0.2 0.2, 0.5 0.2, 0.5 0.4, 0.3 0.6, 0.2 0.4, 0.2 0.2)))",
  "MULTIPOLYGON (((0.6899999999999999 0, 0.8 0.6, 1.1 0.63, 1.23 0.3, 0.6899999999999999 0)))"
))

cat("=== Input geometry ===\n")
print(x)

cat("\n=== Raw coordinates from wk ===\n")
coords <- wk_coords(x)
print(coords)

cat("\n=== Establish topology (raw, no merge) ===\n")
pool <- establish_topology(x)
print(pool)

cat("\nVertices:\n")
print(pool_vertices(pool))

cat("\nSegments (with feature):\n")
print(pool_segments(pool))

cat("\n=== Topology report (before merge) ===\n")
report <- topology_report(pool)
print(report)

cat("\n=== Merge coincident vertices (exact) ===\n")
merged <- merge_coincident(pool, tolerance = 0)
print(merged)

cat("\nVertices after merge:\n")
print(pool_vertices(merged))

cat("\nSegments after merge (with feature):\n")
print(pool_segments(merged))

cat("\n=== Shared edges ===\n")
shared <- find_shared_edges(merged)
print(shared)

cat("\n=== Internal boundaries ===\n")
internal <- find_internal_boundaries(merged)
print(internal)
cat("Internal boundary segments:", length(internal), "\n")

cat("\n=== Neighbours (edge-based) ===\n")
neighbours <- find_neighbours(merged, type = "edge")
print(neighbours)

cat("\n=== Neighbours (vertex-based) ===\n")
neighbours_v <- find_neighbours(merged, type = "vertex")
print(neighbours_v)

cat("\n=== Test subsetting ===\n")
sub <- pool[1:5]
print(sub)
cat("Pool vertices preserved:", nrow(pool_vertices(sub)), "\n")
cat("Features preserved:", unique(pool_feature(sub)), "\n")

cat("\n=== Test combining ===\n")
combined <- pool_combine(pool, pool)
cat("Combined segments:", length(combined), "\n")
cat("Combined vertices:", nrow(pool_vertices(combined)), "\n")

cat("\n=== Test vec_c ===\n")
tryCatch({
  combined2 <- vec_c(pool, pool)
  cat("vec_c worked! Segments:", length(combined2), "\n")
}, error = function(e) {
  cat("vec_c still fails:", conditionMessage(e), "\n")
  cat("Use pool_combine() instead\n")
})

# Visual check: plot the segments
cat("\n=== Plotting (if interactive) ===\n")
if (interactive()) {
  vertices <- pool_vertices(merged)
  segs <- pool_segments(merged)

  # Color by feature
  feature_cols <- c("steelblue", "coral", "forestgreen", "purple")[segs$.feature]

  plot(vertices$x, vertices$y, pch = 19, cex = 0.5,
       xlab = "x", ylab = "y", main = "minimal_mesh as wkpool (colored by feature)")

  # Lookup vertex coords and draw all segments at once
  idx0 <- match(segs$.vx0, vertices$.vx)
  idx1 <- match(segs$.vx1, vertices$.vx)
  segments(vertices$x[idx0], vertices$y[idx0],
           vertices$x[idx1], vertices$y[idx1],
           col = feature_cols, lwd = 2)

  text(vertices$x, vertices$y, vertices$.vx, cex = 0.7, pos = 3)
  legend("topright", legend = paste("Feature", unique(segs$.feature)),
         col = c("steelblue", "coral")[unique(segs$.feature)], lwd = 2)
}

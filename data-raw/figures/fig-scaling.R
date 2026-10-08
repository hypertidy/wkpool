## Benchmark scaling: silicate models against the wkpool pipelines on the
## same hexagon coverages (250 to 8000 cells). Data from the
## silicate-vs-wkpool benchmark (hypertidy/wkpool bench/, results/scaling.csv).
source("R/common.R")
d <- read.csv(Sys.getenv("SCALING_CSV", "../benchmarks/silicate-vs-wkpool/results/scaling.csv"))
d <- d[d$task == "pipelines", ]
slope <- function(e) {
  s <- d[d$expression == e, ]
  unname(coef(lm(log(median_s) ~ log(n_coords), s))[2])
}
groups <- list(
  "vertices (exact identity)" = list(sil = "silicate_PATH0", wkp = "wkpool_vertices"),
  "unique edges"              = list(sil = c("silicate_SC0", "silicate_SC"), wkp = "wkpool_edges"),
  "arcs (shared boundaries)"  = list(sil = "silicate_ARC", wkp = "wkpool_arcs")
)
yl <- range(d$median_s); xl <- range(d$n_coords)
fig_open("scaling.png", width = 2000, height = 720)
layout(matrix(1:3, 1))
par(mar = c(4.2, 4.5, 3, 1))
for (g in names(groups)) {
  plot(NA, xlim = xl, ylim = yl, log = "xy", xlab = "coordinates", ylab = "median seconds",
       main = g, font.main = 1, cex.main = 1.1, axes = FALSE)
  axis(1, col = pal$axis, col.ticks = pal$axis); axis(2, at = 10^(-3:2), labels = c("0.001", "0.01", "0.1", "1", "10", "100"), las = 1, col = pal$axis, col.ticks = pal$axis)
  abline(h = 10^(-3:2), v = c(2e3, 5e3, 1e4, 2e4, 5e4), col = pal$grid, lwd = 0.8)
  ex <- c(groups[[g]]$sil, groups[[g]]$wkp)
  cols <- c(rep(pal$s2, length(groups[[g]]$sil)), pal$s1)
  ltys <- c(1, 2)[seq_along(groups[[g]]$sil)]; ltys <- c(ltys, 1)
  for (i in seq_along(ex)) {
    s <- d[d$expression == ex[i], ]; s <- s[order(s$n_coords), ]
    lines(s$n_coords, s$median_s, col = cols[i], lwd = 2, lty = ltys[i])
    points(s$n_coords, s$median_s, pch = 21, bg = cols[i], col = pal$surface, cex = 1.3)
    lab <- sprintf("%s  (slope %.2f)", sub("silicate_", "silicate ", sub("wkpool_.*", "wkpool", ex[i])), slope(ex[i]))
    text(max(s$n_coords), tail(s$median_s, 1), lab, pos = 2, offset = 0.8, cex = 0.85,
         col = pal$ink2, xpd = NA)
  }
}
dev.off()

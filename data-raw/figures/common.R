## Shared palette and helpers for the illustration scripts.
## Run each fig-*.R from the illustrations/ folder: Rscript R/fig-ladder.R
suppressPackageStartupMessages({
  library(sf)
  library(wk)
  library(wkpool)
  library(meshcore)
  library(silicate)   # the silicate2 branch: UGRID model, zoo helpers
})

pal <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", muted = "#8a8984",
  grid = "#e1e0d9", axis = "#c3c2b7",
  s1 = "#2a78d6", s2 = "#eb6834", s3 = "#1baf7a", s4 = "#4a3aa7",
  seq = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
)
seq_col <- function(v, n = 64) {
  r <- range(v, na.rm = TRUE)
  ramp <- grDevices::colorRampPalette(pal$seq)(n)
  ramp[pmax(1, pmin(n, 1 + floor((v - r[1]) / diff(r) * (n - 1))))]
}

fig_dir <- "figures"
fig_open <- function(name, width = 1600, height = 900) {
  dir.create(fig_dir, showWarnings = FALSE)
  png(file.path(fig_dir, name), width = width, height = height, res = 150,
      type = "cairo", bg = pal$surface)
  par(fg = pal$ink, col.axis = pal$ink2, col.lab = pal$ink2, col.main = pal$ink,
      family = "sans", mar = c(1, 1, 3, 1))
}
blank <- function(xlim, ylim, main = "", asp = 1) {
  plot(NA, xlim = xlim, ylim = ylim, asp = asp, axes = FALSE, xlab = "", ylab = "",
       main = main, font.main = 1, cex.main = 1.05)
}
note <- function(txt, line = 0.2, cex = 0.8, col = pal$ink2) {
  mtext(txt, side = 1, line = line, cex = cex, col = col)
}

## Cell polygons of any meshcore model via as_wk() (one polygon per cell,
## in cells() order; HEALPix cells are unwrapped around their centres).
cell_xy <- function(x) {
  xy <- wk::wk_coords(as_wk(x))
  list(xy = xy, ids = cells(x)$.cell)
}
cell_range <- function(x) {
  xy <- wk::wk_coords(as_wk(x)); list(x = range(xy$x), y = range(xy$y))
}
## col: one colour per cell, in cells() order (or a single colour)
draw_cells <- function(x, col = NA, border = pal$axis, lwd = 0.6, cells = NULL) {
  cx <- cell_xy(x); xy <- cx$xy
  if (length(col) > 1) names(col) <- NULL
  keep <- if (is.null(cells)) rep(TRUE, length(cx$ids)) else cx$ids %in% cells
  fid <- which(keep)
  xy <- xy[xy$feature_id %in% fid, ]
  sp <- split(xy[c("x", "y")], factor(xy$feature_id, levels = fid))
  px <- unlist(lapply(sp, function(d) c(d$x, NA)))
  py <- unlist(lapply(sp, function(d) c(d$y, NA)))
  cc <- if (length(col) > 1) col[fid] else col
  polygon(px, py, col = cc, border = border, lwd = lwd)
  invisible(cx$ids[fid])
}

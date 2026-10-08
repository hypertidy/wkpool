# Benchmark: silicate vs wkpool for decomposition and vertex/edge normalization
#
# Run from the package root with wkpool installed:
#   Rscript bench/silicate-vs-wkpool.R [max_size]
# max_size is "small", "medium" or "large" (default "large").
# Results are written to bench/results/ as CSV (one row per expression x input)
# plus sessionInfo, so runs on different machines/commits can be compared.
#
# Pairings (same input, comparable output):
#   coords     silicate::sc_coord()          vs wk::wk_coords()  (floor)
#   decompose  (no silicate equivalent)          wkpool::establish_topology()
#   vertices   silicate::PATH0(), PATH()     vs establish_topology() + merge_coincident()
#   edges      silicate::SC0(), SC()         vs ... + unique undirected edges
#   arcs       silicate::ARC()               vs ... + find_arcs(quotient = TRUE)
# Plus wkpool-only verbs on the merged pool, to find anything that scales badly.
#
# Vertex identity is exact (bit pattern) in both packages; counts are checked
# for agreement before timing.

suppressPackageStartupMessages({
  library(sf)
  library(wk)
  library(wkpool)
  library(silicate)
  library(bench)
})

args <- commandArgs(trailingOnly = TRUE)
max_size <- if (length(args)) args[1] else "large"
size_rank <- c(small = 1, medium = 2, large = 3)
out_dir <- file.path("bench", "results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- inputs ---------------------------------------------------------------

# polygon coverage of n hexagons: every interior edge shared by two features
hex_grid <- function(n) {
  side <- ceiling(sqrt(n))
  bb <- st_bbox(c(xmin = 0, ymin = 0, xmax = side, ymax = side))
  g <- st_make_grid(st_as_sfc(bb), cellsize = 1, square = FALSE)
  st_sf(id = seq_along(g), geometry = g)[seq_len(min(n, length(g))), ]
}

# same coverage with every edge densified: vertex-heavy, few nodes
dense_hex <- function(n, k = 10) {
  x <- hex_grid(n)
  st_geometry(x) <- st_segmentize(st_geometry(x), dfMaxLength = 1 / (2 * k))
  x
}

# n random-walk linestrings of m vertices on an integer lattice, so there are
# genuine coincident vertices between and within lines
walk_lines <- function(n, m) {
  set.seed(1)
  geoms <- lapply(seq_len(n), function(i) {
    xy <- apply(matrix(sample(c(-1, 0, 1), 2 * m, replace = TRUE), ncol = 2), 2, cumsum)
    xy <- xy + matrix(sample(0:200, 2), m, 2, byrow = TRUE)
    st_linestring(xy)
  })
  st_sf(id = seq_len(n), geometry = st_sfc(geoms))
}

nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
inlandwaters <- silicate::inlandwaters

inputs <- list(
  small = list(
    nc = nc,
    # 6 multipolygons, 189 rings of which 32 are holes, shared state borders
    inlandwaters = inlandwaters,
    hex_1e3 = hex_grid(1e3)
  ),
  medium = list(
    hex_1e4 = hex_grid(1e4),
    dense_hex_1e3 = dense_hex(1e3),
    lines_100x1000 = walk_lines(100, 1000)
  ),
  large = list(
    hex_1e5 = hex_grid(1e5),
    dense_hex_1e4 = dense_hex(1e4),
    lines_1000x1000 = walk_lines(1000, 1000)
  )
)
inputs <- unlist(inputs[names(size_rank)[size_rank <= size_rank[[max_size]]]],
                 recursive = FALSE)
names(inputs) <- sub("^[a-z]+\\.", "", names(inputs))

# ---- wkpool pipelines -----------------------------------------------------

wkp_vertices <- function(x) merge_coincident(establish_topology(x))
wkp_edges <- function(x) {
  m <- wkp_vertices(x)
  wkpool:::quotient_edges(vctrs::field(m, ".vx0"), vctrs::field(m, ".vx1"))
}
wkp_arcs <- function(x) find_arcs(wkp_vertices(x), quotient = TRUE)

# ---- agreement check ------------------------------------------------------

# silicate unique undirected edges taken from SC0's per-object segments
# (cheap; SC() itself is superlinear), so the check runs at every size
check_counts <- function(x) {
  m <- wkp_vertices(x)
  sc0 <- SC0(x)
  seg <- do.call(rbind, sc0$object$topology_)
  sil_edges <- nrow(unique(data.frame(lo = pmin(seg$.vx0, seg$.vx1),
                                      hi = pmax(seg$.vx0, seg$.vx1))))
  e <- wkp_edges(x)
  data.frame(
    n_coords = nrow(wk_coords(x)),
    silicate_vertices = nrow(sc0$vertex),
    wkpool_vertices = nrow(pool_vertices(m)),
    silicate_edges = sil_edges,
    wkpool_edges = length(e$vx0)
  )
}

# ---- run ------------------------------------------------------------------

as_rows <- function(b, task) {
  data.frame(
    task = task, expression = names(b$expression),
    median_s = as.numeric(b$median), min_s = as.numeric(b$min),
    mem_alloc_mb = as.numeric(b$mem_alloc) / 2^20, n_itr = b$n_itr
  )
}

mark <- function(..., iter = c(3, 10), env = parent.frame()) {
  suppressWarnings(bench::mark(..., check = FALSE, min_iterations = iter[1],
                               max_iterations = iter[2], filter_gc = FALSE,
                               env = env))
}

slow_n <- 5e4
silicate_cap <- 2e5

wkpool_verbs <- function(x, m, skip = character()) {
  fns <- list(
    merge_coincident = function() merge_coincident(establish_topology(x)),
    pool_compact = function() pool_compact(m),
    vertex_degree = function() vertex_degree(m),
    find_nodes = function() find_nodes(m),
    find_arcs = function() find_arcs(m),
    find_arcs_quotient = function() find_arcs(m, quotient = TRUE),
    find_shared_edges = function() find_shared_edges(m),
    find_internal_boundaries = function() find_internal_boundaries(m),
    topology_report = function() topology_report(m),
    find_cycles = function() find_cycles(m),
    classify_cycles = function() classify_cycles(m),
    find_neighbours_edge = function() find_neighbours(m, "edge"),
    find_neighbours_vertex = function() find_neighbours(m, "vertex"),
    hole_points = function() hole_points(m)
  )
  fns <- fns[setdiff(names(fns), skip)]
  do.call(rbind, lapply(names(fns), function(f) {
    fn <- fns[[f]]
    r <- as_rows(mark(fn(), iter = c(3, 10)), "wkpool_verbs")
    r$expression <- f
    r
  }))
}

flatten <- function(results) do.call(rbind, results)

results <- list()
counts <- list()
for (nm in names(inputs)) {
  x <- inputs[[nm]]
  n_coords <- nrow(wk_coords(x))
  message(sprintf("[%s] %s: %d features, %d coords", format(Sys.time(), "%H:%M:%S"),
                  nm, nrow(x), n_coords))
  counts[[nm]] <- cbind(input = nm, n_features = nrow(x), check_counts(x))

  # silicate's SC() and ARC() are superlinear (see the scaling series below):
  # single iterations above slow_n coords, skipped above silicate_cap coords
  slow <- n_coords > slow_n
  big <- n_coords > silicate_cap
  it <- if (slow) c(1, 1) else c(3, 10)
  step <- function(task) message(sprintf("  [%s] %s", format(Sys.time(), "%H:%M:%S"), task))

  step("coords")
  b <- as_rows(mark(silicate_sc_coord = sc_coord(x),
                    wk_coords = wk_coords(x)), "coords")
  step("decompose")
  b <- rbind(b, as_rows(mark(wkpool_establish = establish_topology(x)), "decompose"))
  # wkpool pipelines always get repeated iterations (they are fast); a
  # single GC-affected iteration can otherwise misreport them several-fold
  wk_it <- c(3, 10)
  step("vertices")
  b <- rbind(b, as_rows(mark(silicate_PATH0 = PATH0(x),
                             silicate_PATH = PATH(x), iter = it), "vertices"),
             as_rows(mark(wkpool = wkp_vertices(x), iter = wk_it), "vertices"))
  step("edges")
  b <- rbind(b, if (big) {
    as_rows(mark(silicate_SC0 = SC0(x), iter = it), "edges")
  } else {
    as_rows(mark(silicate_SC0 = SC0(x), silicate_SC = SC(x), iter = it), "edges")
  }, as_rows(mark(wkpool = wkp_edges(x), iter = wk_it), "edges"))
  step("arcs")
  if (!big) {
    b <- rbind(b, as_rows(mark(silicate_ARC = ARC(x),
                               iter = if (slow) c(1, 1) else c(3, 3)), "arcs"))
  }
  b <- rbind(b, as_rows(mark(wkpool = wkp_arcs(x), iter = wk_it), "arcs"))
  step("wkpool verbs")
  b <- rbind(b, wkpool_verbs(x, wkp_vertices(x)))
  b$input <- nm
  b$n_coords <- n_coords
  results[[nm]] <- b
  # write as we go so a long run leaves partial results
  write.csv(flatten(results), file.path(out_dir, "timings.csv"), row.names = FALSE)
  write.csv(do.call(rbind, counts), file.path(out_dir, "counts.csv"), row.names = FALSE)
}

# ---- scaling series -----------------------------------------------------------------------------------------
# hex coverages of increasing size; the log-log slope of time against coords
# is the empirical complexity exponent (1 = linear, 2 = quadratic)

scaling <- list()
scale_n <- c(250, 500, 1000, 2000, 4000)
if (size_rank[[max_size]] >= 2) scale_n <- c(scale_n, 8000)
for (n in scale_n) {
  x <- hex_grid(n)
  message(sprintf("[%s] scaling hex %d", format(Sys.time(), "%H:%M:%S"), n))
  s <- rbind(
    as_rows(mark(silicate_PATH0 = PATH0(x), silicate_SC0 = SC0(x),
                 silicate_SC = SC(x), silicate_ARC = ARC(x),
                 wkpool_vertices = wkp_vertices(x), wkpool_edges = wkp_edges(x),
                 wkpool_arcs = wkp_arcs(x), iter = c(1, 3)), "pipelines"),
    wkpool_verbs(x, wkp_vertices(x)))
  s$n_coords <- nrow(wk_coords(x))
  scaling[[length(scaling) + 1]] <- s
  write.csv(do.call(rbind, scaling), file.path(out_dir, "scaling.csv"), row.names = FALSE)
}
scaling <- do.call(rbind, scaling)
slopes <- do.call(rbind, lapply(split(scaling, scaling$expression), function(d) {
  data.frame(expression = d$expression[1],
             exponent = unname(round(coef(lm(log(median_s) ~ log(n_coords), d))[2], 2)),
             median_s_at_max = max(d$median_s))
}))
write.csv(slopes, file.path(out_dir, "scaling-exponents.csv"), row.names = FALSE)

writeLines(c(
  sprintf("date: %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  sprintf("wkpool: %s", as.character(packageVersion("wkpool"))),
  sprintf("silicate: %s", as.character(packageVersion("silicate"))),
  sprintf("wk: %s", as.character(packageVersion("wk"))),
  sprintf("vctrs: %s", as.character(packageVersion("vctrs"))),
  sprintf("dplyr: %s", as.character(packageVersion("dplyr"))),
  sprintf("cpu: %s", tryCatch(trimws(sub(".*:", "", grep("model name",
    readLines("/proc/cpuinfo"), value = TRUE)[1])), error = function(e) NA)),
  "", capture.output(sessionInfo())
), file.path(out_dir, "session.txt"))

message("done")

# wkpool_tri: a pool of triangles
#
# The sibling of wkpool: elements are triangles (2-simplices) rather than
# segments (1-simplices), and they reference the same kind of vertex pool
# by .vx id. A triangulation that creates no vertices (ear cutting) reuses
# the segment pool's vertex table unchanged.

#' Construct a triangle pool
#'
#' A low-level constructor for `wkpool_tri` objects: a vector of triangles
#' whose corners are vertex ids into a shared vertex pool, the same pool
#' structure a [wkpool][new_wkpool] uses for segments.
#'
#' The `wkpool_tri` class is experimental: how a triangle records the
#' polygon part it came from (currently a `.part` field) may change in a
#' later release.
#'
#' @param vertices A data.frame with columns `.vx` (integer vertex ids),
#'   `x`, `y`, and optionally `z`.
#' @param vx0,vx1,vx2 Integer vectors of triangle corner vertex ids, each
#'   value present in `vertices$.vx`.
#' @param feature Optional integer vector of feature ids, one per triangle.
#' @param part Optional integer vector of part ids, one per triangle (the
#'   polygon within a multipolygon feature, as identified by
#'   [wk::wk_coords()]).
#' @param paths Optional data.frame describing the input paths (see
#'   [pool_paths()]), carried from the segment pool.
#' @inheritParams new_wkpool
#'
#' @returns A `wkpool_tri` object.
#'
#' @examples
#' v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#' new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
#'
#' @export
new_wkpool_tri <- function(vertices, vx0, vx1, vx2, feature = NULL,
                           part = NULL, paths = NULL,
                           crs = NULL, geodesic = FALSE) {
  stopifnot(is.data.frame(vertices))
  stopifnot(".vx" %in% names(vertices))
  stopifnot(length(vx0) == length(vx1), length(vx0) == length(vx2))
  stopifnot(!anyNA(vx_match(vx0, vertices$.vx)))
  stopifnot(!anyNA(vx_match(vx1, vertices$.vx)))
  stopifnot(!anyNA(vx_match(vx2, vertices$.vx)))

  fields <- list(.vx0 = as.integer(vx0), .vx1 = as.integer(vx1),
                 .vx2 = as.integer(vx2))
  if (!is.null(feature)) {
    stopifnot(length(feature) == length(vx0))
    fields$.feature <- as.integer(feature)
  }
  if (!is.null(part)) {
    stopifnot(length(part) == length(vx0))
    fields$.part <- as.integer(part)
  }

  vctrs::new_rcrd(
    fields,
    pool = vertices,
    paths = paths,
    crs = crs,
    geodesic = if (is.null(geodesic)) NULL else geodesic_attr(geodesic),
    class = "wkpool_tri"
  )
}

wkpool_tri_empty <- function(crs = wk::wk_crs_inherit(), geodesic = NA) {
  new_wkpool_tri(
    vertices = data.frame(.vx = integer(), x = numeric(), y = numeric()),
    vx0 = integer(), vx1 = integer(), vx2 = integer(),
    feature = integer(), part = integer(),
    crs = crs, geodesic = geodesic
  )
}

#' Triangulate the polygons in a pool
#'
#' Triangulate each polygon of a [wkpool()] (built by [establish_topology()])
#' and return the triangles as a `wkpool_tri` on the same vertex pool.
#'
#' The `wkpool_tri` class is experimental: how a triangle records the
#' polygon part it came from (currently a `.part` field) may change in a
#' later release.
#'
#' The `"decido"` engine is ear cutting ([decido::earcut()]), which never
#' creates vertices: every triangle corner is an existing vertex of the
#' pool, so the vertex table is passed through unchanged. Each polygon (each
#' part of a multipolygon) is triangulated separately, with its first ring
#' as the boundary and later rings as holes, using the path provenance
#' recorded by [establish_topology()]. Paths that are not closed rings
#' (linestrings) are ignored.
#'
#' The pool can be triangulated before or after [merge_coincident()]. On a
#' raw pool each ring's closing coordinate is its own vertex; it is dropped
#' from the ring and never used by a triangle. Merging the triangle pool
#' later with [merge_coincident()] gives adjacent features shared triangle
#' corners.
#'
#' Triangles are counter-clockwise.
#'
#' @param x A wkpool with path provenance (from [establish_topology()]).
#' @param engine Triangulation engine, currently only `"decido"`.
#' @param ... Unused.
#'
#' @returns A `wkpool_tri`.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.2 0.4, 0.4 0.4, 0.2 0.2))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' if (requireNamespace("decido", quietly = TRUE)) {
#'   tri <- pool_triangulate(pool)
#'   tri
#'   pool_triangles(tri)
#'   wk::as_wkt(tri)
#' }
#'
#' @export
pool_triangulate <- function(x, engine = c("decido"), ...) {
  check_wkpool(x)
  engine <- match.arg(engine)
  if (!requireNamespace("decido", quietly = TRUE)) {
    stop("pool_triangulate(engine = \"decido\") requires the 'decido' package")
  }
  crs <- attr(x, "crs", exact = TRUE)
  geodesic <- attr(x, "geodesic", exact = TRUE)
  path <- pool_path(x)
  paths <- pool_paths(x)
  if (is.null(path) || is.null(paths)) {
    stop("pool_triangulate() needs path provenance: build the pool with establish_topology()")
  }

  v <- pool_vertices(x)
  segs <- pool_segments(x)

  # one vertex chain per closed ring, closing coordinate dropped
  by_path <- split(seq_along(path), factor(path, levels = paths$.path))
  rings <- lapply(by_path, function(i) ring_chain(segs$.vx0[i], segs$.vx1[i], v))
  keep <- !vapply(rings, is.null, logical(1))
  rings <- rings[keep]
  rpaths <- paths[keep, , drop = FALSE]
  if (length(rings) == 0L) {
    return(new_wkpool_tri(v, integer(), integer(), integer(),
                          feature = integer(), part = integer(),
                          paths = paths, crs = crs, geodesic = geodesic))
  }

  # polygons are (feature, part); wk emits the outer ring first
  key <- paste(rpaths$.feature, rpaths$.part, sep = "/")
  groups <- split(seq_along(rings), factor(key, levels = unique(key)))
  tri <- lapply(groups, function(g) {
    vx <- unlist(rings[g], use.names = FALSE)
    len <- lengths(rings[g])
    holes <- if (length(g) > 1L) cumsum(len)[-length(g)] + 1L else 0L
    idx <- vx_match(vx, v$.vx)
    i <- decido::earcut(cbind(v$x[idx], v$y[idx]), holes = holes)
    vx[i]
  })
  ntri <- lengths(tri) %/% 3L
  first <- vapply(groups, `[`, integer(1), 1L)
  index <- matrix(unlist(tri, use.names = FALSE), nrow = 3L)

  new_wkpool_tri(v, index[1L, ], index[2L, ], index[3L, ],
                 feature = rep(rpaths$.feature[first], ntri),
                 part = rep(rpaths$.part[first], ntri),
                 paths = paths, crs = crs, geodesic = geodesic)
}

# Vertex chain of one path if it is a closed ring of at least 3 distinct
# corners, else NULL. Closure is by vertex id (merged pool) or by equal
# coordinates of first and last vertex (raw pool); the closing vertex is
# dropped either way.
ring_chain <- function(vx0, vx1, v) {
  ch <- chain_path(vx0, vx1)
  if (is.null(ch)) return(NULL)
  vx <- ch$vx
  if (!ch$closed) {
    n <- length(vx)
    i <- vx_match(vx[c(1L, n)], v$.vx)
    cols <- intersect(c("x", "y", "z"), names(v))
    same <- all(vapply(cols, function(cc) identical(v[[cc]][i[1L]], v[[cc]][i[2L]]),
                       logical(1)))
    if (!same) return(NULL)
    vx <- vx[-n]
  }
  if (length(vx) < 3L) return(NULL)
  vx
}

#' Access the triangles of a triangle pool
#'
#' @param x A `wkpool_tri`.
#'
#' @returns
#' - `pool_triangles()`: a data frame with columns `.vx0`, `.vx1`, `.vx2`
#'   and, when present, `.feature` and `.part`.
#' - `tri_segments()`: a [wkpool()] of the triangle edges, three per triangle
#'   in triangle order and counter-clockwise, on the same vertex pool, with
#'   the triangle's feature. Edges used by one triangle are on the boundary
#'   of the triangulated region, edges used by two are internal (after
#'   [merge_coincident()], so that adjacent triangles share vertex ids).
#'
#' @examples
#' v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
#' pool_triangles(tri)
#' tri_segments(tri)
#'
#' @export
pool_triangles <- function(x) {
  check_wkpool_tri(x)
  out <- data.frame(
    .vx0 = vctrs::field(x, ".vx0"),
    .vx1 = vctrs::field(x, ".vx1"),
    .vx2 = vctrs::field(x, ".vx2")
  )
  for (f in intersect(c(".feature", ".part"), vctrs::fields(x))) {
    out[[f]] <- vctrs::field(x, f)
  }
  out
}

#' @rdname pool_triangles
#' @export
tri_segments <- function(x) {
  t <- pool_triangles(x)
  n <- nrow(t)
  vx0 <- as.vector(rbind(t$.vx0, t$.vx1, t$.vx2))
  vx1 <- as.vector(rbind(t$.vx1, t$.vx2, t$.vx0))
  feature <- if (is.null(t$.feature)) NULL else rep(t$.feature, each = 3L)
  new_wkpool(pool_vertices(x), vx0, vx1, feature = feature,
             crs = attr(x, "crs", exact = TRUE),
             geodesic = attr(x, "geodesic", exact = TRUE))
}

merge_coincident_tri <- function(x, tolerance = 0, period_x = NULL) {
  pool <- pool_vertices(x)
  g <- vertex_groups(pool, tolerance, period_x)
  first <- g > c(0L, cummax(g)[-length(g)])
  new_pool <- pool[first, , drop = FALSE]
  new_pool$.vx <- seq_len(nrow(new_pool))
  remap <- function(f) g[vx_match(vctrs::field(x, f), pool$.vx)]
  t <- pool_triangles(x)
  new_wkpool_tri(new_pool, remap(".vx0"), remap(".vx1"), remap(".vx2"),
                 feature = t$.feature, part = t$.part,
                 paths = pool_paths(x),
                 crs = attr(x, "crs", exact = TRUE),
                 geodesic = attr(x, "geodesic", exact = TRUE))
}

check_wkpool_tri <- function(x, arg = "x") {
  if (!inherits(x, "wkpool_tri")) {
    stop(sprintf("`%s` must be a wkpool_tri object (see `pool_triangulate()`).", arg),
         call. = FALSE)
  }
  invisible(x)
}

# wk handling: one POLYGON per triangle -----------------------------------

#' @rdname pool_triangles
#' @param handleable A `wkpool_tri`.
#' @param handler A [wk handler][wk::wk_handle].
#' @param ... Passed on.
#' @details
#' A `wkpool_tri` is wk-handleable: it presents as one POLYGON per triangle
#' (the handled vector has the same length as the triangle vector), with
#' the pool's crs and geodesic flag attached.
#' @exportS3Method wk::wk_handle
wk_handle.wkpool_tri <- function(handleable, handler, ...) {
  wk::wk_handle(pool_triangles_geometry(handleable), handler, ...)
}

pool_triangles_geometry <- function(x) {
  v <- pool_vertices(x)
  crs <- wk::wk_crs(x)
  geodesic <- wk::wk_is_geodesic(x)
  n <- length(x)
  if (n == 0L) {
    return(wk::wkb(list(), crs = crs, geodesic = geodesic))
  }
  idx <- vx_match(as.vector(rbind(vctrs::field(x, ".vx0"),
                                  vctrs::field(x, ".vx1"),
                                  vctrs::field(x, ".vx2"))), v$.vx)
  coords <- if ("z" %in% names(v)) {
    wk::xyz(v$x[idx], v$y[idx], v$z[idx], crs = crs)
  } else {
    wk::xy(v$x[idx], v$y[idx], crs = crs)
  }
  wk::wk_polygon(coords, feature_id = rep(seq_len(n), each = 3L),
                 geodesic = geodesic)
}

#' @exportS3Method wk::wk_crs
wk_crs.wkpool_tri <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @exportS3Method wk::wk_set_crs
wk_set_crs.wkpool_tri <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @exportS3Method wk::wk_is_geodesic
wk_is_geodesic.wkpool_tri <- function(x) {
  attr(x, "geodesic", exact = TRUE) %||% FALSE
}

#' @exportS3Method wk::wk_set_geodesic
wk_set_geodesic.wkpool_tri <- function(x, geodesic) {
  attr(x, "geodesic") <- geodesic_attr(geodesic)
  x
}

# Format/print and vctrs ---------------------------------------------------

#' @export
format.wkpool_tri <- function(x, ...) {
  sprintf("<triangle: %d,%d,%d>", vctrs::field(x, ".vx0"),
          vctrs::field(x, ".vx1"), vctrs::field(x, ".vx2"))
}

#' @export
vec_ptype_abbr.wkpool_tri <- function(x, ...) "wktri"

#' @export
obj_print_header.wkpool_tri <- function(x, ...) {
  crs <- wk::wk_crs(x)
  crs_label <- if (is.null(crs) || inherits(crs, "wk_crs_inherit")) {
    ""
  } else {
    sprintf(" CRS=%s", format(crs)[1])
  }
  geo_label <- if (isTRUE(wk::wk_is_geodesic(x))) " geodesic" else ""
  cat(sprintf("<wkpool_tri[%d triangles, %d vertices]%s%s>\n",
              length(x), nrow(pool_vertices(x)), crs_label, geo_label))
}

#' @export
vec_ptype2.wkpool_tri.wkpool_tri <- function(x, y, ...) {
  wkpool_tri_empty()
}

#' @export
vec_cast.wkpool_tri.wkpool_tri <- function(x, to, ...) {
  x
}

#' @export
vec_restore.wkpool_tri <- function(x, to, ...) {
  # On subset: keep the full pool, subset triangles
  fld <- function(f) tryCatch(vctrs::field(x, f), error = function(e) NULL)
  new_wkpool_tri(pool_vertices(to), fld(".vx0"), fld(".vx1"), fld(".vx2"),
                 feature = fld(".feature"), part = fld(".part"),
                 paths = attr(to, "paths", exact = TRUE),
                 crs = attr(to, "crs", exact = TRUE),
                 geodesic = attr(to, "geodesic", exact = TRUE))
}

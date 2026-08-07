# wk_handle for wkpool: a pool is wk-handleable
#
# The vctr is a vector of segments, so the handled presentation is the
# honest one: one LINESTRING per segment, length preserved. Richer
# presentations (arcs as linestrings, cycles as polygons, reconstructed
# features) remain explicit conversions - wk_handle() does not guess.
#
# The segments are assembled with wk's C-level linestring filter from
# an xy vertex vector indexed straight out of the pool: no text round
# trip, full double precision, crs and geodesic carried.

#' Handle wkpool objects with wk handlers
#'
#' A wkpool is wk-handleable: it presents as one LINESTRING per segment
#' (so the handled vector has the same length as the pool vector), with
#' the pool's crs and geodesic flag attached. This means the wk
#' ecosystem's generic conversions work directly on a pool:
#' `wk::as_wkb()`, `wk::as_wkt()`, `wk::wk_coords()`,
#' `geos::as_geos_geometry()`, plotting, and anything else built on
#' [wk::wk_handle()].
#'
#' Vertices are emitted at full double precision, indexed directly from
#' the pool (no text round trip). If the pool carries `z`, coordinates
#' are emitted as XYZ.
#'
#' Arcs and cycles remain explicit conversions ([arcs_to_wkt()],
#' [cycles_to_wkt()] and friends): the handled form is deliberately the
#' segment vector itself.
#'
#' @param handleable A wkpool object.
#' @param handler A [wk handler][wk::wk_handle].
#' @param ... Passed on.
#'
#' @returns The result of the handler.
#'
#' @examples
#' x <- wk::wkt(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   crs = "EPSG:4326"
#' )
#' pool <- establish_topology(x)
#' wk::as_wkt(pool)
#' wk::wk_coords(pool)
#' wk::wk_crs(wk::as_wkb(pool))
#'
#' @exportS3Method wk::wk_handle
wk_handle.wkpool <- function(handleable, handler, ...) {
  wk::wk_handle(pool_segments_geometry(handleable), handler, ...)
}

# Build the per-segment LINESTRING wkb natively: pool -> xy -> C-level
# linestring filter. Used by wk_handle.wkpool() and available as the
# non-text emit path.
pool_segments_geometry <- function(x) {
  v <- pool_vertices(x)
  s <- pool_segments(x)
  n <- nrow(s)
  crs <- wk::wk_crs(x)
  geodesic <- wk::wk_is_geodesic(x)

  if (n == 0L) {
    return(wk::wkb(list(), crs = crs, geodesic = geodesic))
  }

  # interleave endpoint rows: v0, v1 per segment
  i0 <- match(s$.vx0, v$.vx)
  i1 <- match(s$.vx1, v$.vx)
  idx <- as.vector(rbind(i0, i1))

  coords <- if ("z" %in% names(v)) {
    wk::xyz(v$x[idx], v$y[idx], v$z[idx], crs = crs)
  } else {
    wk::xy(v$x[idx], v$y[idx], crs = crs)
  }

  wk::wk_linestring(
    coords,
    feature_id = rep(seq_len(n), each = 2L),
    geodesic = geodesic
  )
}

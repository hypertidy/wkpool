# Round-trip conversion: geometry out of the pool
#
# All emitters are built natively on wk: pool vertices are indexed into
# a wk::xy()/wk::xyz() vector and assembled by wk's C-level linestring,
# polygon and collection filters. WKB is the primary product and WKT is
# derived from it. No coordinate text is assembled anywhere, so
# coordinates emit at full double precision, z is carried when the pool
# has it, and crs/geodesic ride along by construction.

# indexed vertex vector for a sequence of pool row positions
pool_coords <- function(x, idx) {
  v <- pool_vertices(x)
  crs <- wk::wk_crs(x)
  if ("z" %in% names(v)) {
    wk::xyz(v$x[idx], v$y[idx], v$z[idx], crs = crs)
  } else {
    wk::xy(v$x[idx], v$y[idx], crs = crs)
  }
}

wkb_empty <- function(x) {
  wk::wkb(list(), crs = wk::wk_crs(x), geodesic = wk::wk_is_geodesic(x))
}

#' @param ... Passed to [wk::as_wkb()]
#' @rdname arcs_to_wkt
#' @export
arcs_to_wkb <- function(x, quotient = FALSE, ...) {
  check_wkpool(x)
  if (length(x) < 1) {
    return(wk::as_wkb(wkb_empty(x), ...))
  }
  arcs <- find_arcs(x, quotient = quotient)
  if (length(arcs) == 0) return(wk::as_wkb(wkb_empty(x), ...))
  v <- pool_vertices(x)

  idx <- match(unlist(arcs), v$.vx)
  result <- wk::wk_linestring(
    pool_coords(x, idx),
    feature_id = rep(seq_along(arcs), lengths(arcs)),
    geodesic = wk::wk_is_geodesic(x)
  )
  wk::as_wkb(result, ...)
}

#' Convert arcs to WKT linestrings
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @inheritParams find_arcs
#' @return A wk_wkt vector of LINESTRING geometries
#'
#' @details
#' Each arc (maximal segment sequence between nodes) becomes a linestring.
#'
#' Geometry is assembled natively via [wk::wk_linestring()] (WKB first,
#' WKT derived from it): coordinates are emitted at full double
#' precision, z is carried when the pool has it, and the pool's crs and
#' geodesic flag are attached to the output.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' arcs_to_wkt(merged)
#'
#' @export
arcs_to_wkt <- function(x, quotient = FALSE) {
  check_wkpool(x)
  wk::as_wkt(arcs_to_wkb(x, quotient = quotient))
}


#' Convert cycles to WKT polygons
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param feature Logical: attempt to reconstruct original features?
#'   If TRUE, groups cycles by .feature and nests holes in outers.
#'   If FALSE, each cycle becomes a separate polygon.
#' @param convention Winding convention: "sf" (default) or "ogc"; only
#'   consulted for pools without path provenance
#' @return A wk_wkt vector of POLYGON geometries
#'
#' @details
#' Converts cycles back to polygons. When feature = TRUE and the pool
#' carries path provenance (minted by [establish_topology()]), the
#' original feature structure is reconstructed exactly: rings are
#' grouped into their original part (exterior first, then that part's
#' holes) and parts into their original feature (POLYGON or
#' MULTIPOLYGON), with winding emitted as stored and `convention` not
#' consulted. Pools without provenance fall back to a heuristic that
#' groups rings by feature and nests all holes with the first outer.
#'
#' Geometry is assembled natively via [wk::wk_polygon()] and
#' [wk::wk_collection()] (WKB first, WKT derived from it): coordinates
#' are emitted at full double precision, rings are closed by wk, z is
#' carried when the pool has it, and the pool's crs and geodesic flag
#' are attached to the output.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' cycles_to_wkt(merged)
#'
#' @export
cycles_to_wkt <- function(x, feature = TRUE, convention = c("sf", "ogc")) {
  check_wkpool(x)
  wk::as_wkt(cycles_to_wkb(x, feature = feature, convention = convention))
}


#' @param ... Passed to [wk::as_wkb()]
#' @rdname cycles_to_wkt
#' @export
cycles_to_wkb <- function(x, feature = TRUE, convention = c("sf", "ogc"), ...) {
  check_wkpool(x)
  convention <- match.arg(convention)
  cycles <- find_cycles(x)
  if (length(cycles) == 0) return(wk::as_wkb(wkb_empty(x), ...))
  v <- pool_vertices(x)
  geodesic <- wk::wk_is_geodesic(x)

  # Get signed areas
  areas <- vapply(cycles, cycle_signed_area, numeric(1), pool = v)

  if (convention == "sf") {
    is_outer <- areas < 0
  } else {
    is_outer <- areas > 0
  }

  # One POLYGON per block of rings: feature_id changes delimit polygons,
  # ring_id changes delimit rings, wk closes each ring
  build_polygons <- function(ring_list, poly_id) {
    idx <- match(unlist(ring_list), v$.vx)
    n <- lengths(ring_list)
    wk::wk_polygon(
      pool_coords(x, idx),
      feature_id = rep(poly_id, n),
      ring_id = rep(seq_along(ring_list), n),
      geodesic = geodesic
    )
  }

  if (!feature || is.null(pool_feature(x))) {
    # Simple: each cycle becomes its own polygon
    return(wk::as_wkb(build_polygons(cycles, seq_along(cycles)), ...))
  }

  # Provenance path: cycles carry their minted path ids, so feature and
  # ring structure is reconstructed exactly - rings grouped into their
  # original part (exterior first, then its own holes), parts grouped
  # into their original feature (POLYGON or MULTIPOLYGON). `convention`
  # is not consulted; winding is emitted as stored.
  cycle_paths <- attr(cycles, "path")
  paths_tab <- pool_paths(x)
  if (!is.null(cycle_paths) && !is.null(paths_tab) &&
      length(cycle_paths) == length(cycles)) {
    idx <- match(cycle_paths, paths_tab$.path)
    feat <- paths_tab$.feature[idx]
    part <- paths_tab$.part[idx]
    ring <- paths_tab$.ring[idx]

    out <- list()
    for (f in unique(feat)) {
      fparts <- unique(part[feat == f])
      polys <- vector("list", length(fparts))
      for (k in seq_along(fparts)) {
        sel <- which(feat == f & part == fparts[k])
        sel <- sel[order(ring[sel])]  # exterior ring first
        polys[[k]] <- build_polygons(cycles[sel], rep(1L, length(sel)))
      }
      poly_vec <- do.call(c, polys)
      out[[length(out) + 1L]] <- if (length(fparts) == 1L) {
        poly_vec
      } else {
        wk::wk_collection(
          poly_vec,
          wk::wk_geometry_type("multipolygon"),
          feature_id = 1L
        )
      }
    }
    return(wk::as_wkb(do.call(c, out), ...))
  }

  # Fallback for pools without provenance: associate each cycle with a
  # feature based on segment membership
  segs <- pool_segments(x)
  cycle_features <- vapply(seq_along(cycles), function(i) {
    cyc <- cycles[[i]]
    # Find segments that match this cycle's edges
    for (j in seq_len(length(cyc))) {
      v0 <- cyc[j]
      v1 <- cyc[if (j == length(cyc)) 1 else j + 1]

      # Find matching segment
      match_idx <- which(
        (segs$.vx0 == v0 & segs$.vx1 == v1) |
          (segs$.vx0 == v1 & segs$.vx1 == v0)
      )
      if (length(match_idx) > 0 && !is.null(segs$.feature)) {
        return(segs$.feature[match_idx[1]])
      }
    }
    NA_integer_
  }, integer(1))

  # Group by feature
  unique_features <- unique(cycle_features[!is.na(cycle_features)])

  out <- vector("list", length(unique_features))
  keep <- logical(length(unique_features))

  for (i in seq_along(unique_features)) {
    feat_cycles <- which(cycle_features == unique_features[i])
    feat_outers <- feat_cycles[is_outer[feat_cycles]]
    feat_holes <- feat_cycles[!is_outer[feat_cycles]]

    if (length(feat_outers) == 0) next
    keep[i] <- TRUE

    if (length(feat_outers) == 1) {
      # Simple case: one outer, associate all holes
      # (Proper implementation would check containment)
      out[[i]] <- build_polygons(cycles[c(feat_outers, feat_holes)], 1L)
    } else {
      # Multiple outers = MULTIPOLYGON (simplified - all holes dropped)
      # Proper implementation would match holes to containing outers
      polys <- build_polygons(cycles[feat_outers], seq_along(feat_outers))
      out[[i]] <- wk::wk_collection(
        polys,
        wk::wk_geometry_type("multipolygon"),
        feature_id = 1L
      )
    }
  }

  out <- out[keep]
  if (length(out) == 0) return(wk::as_wkb(wkb_empty(x), ...))

  wk::as_wkb(do.call(c, out), ...)
}


#' Convert wkpool segments to WKT
#'
#' @param x A wkpool
#' @param type Output type: "linestring" (segments as paths), "multilinestring" (all segments),
#'   or "point" (vertices only)
#' @return A wk_wkt vector
#'
#' @details
#' Geometry is assembled natively via [wk::wk_linestring()] and
#' [wk::wk_collection()] (WKB first, WKT derived from it): coordinates
#' are emitted at full double precision, z is carried when the pool has
#' it, and the pool's crs and geodesic flag are attached to the output.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' segments_to_wkt(pool)
#' segments_to_wkt(pool, type = "linestring")
#'
#' @export
segments_to_wkt <- function(x, type = c("multilinestring", "linestring", "point")) {
  check_wkpool(x)
  wk::as_wkt(segments_to_wkb(x, type = type))
}


#' @param ... Passed to [wk::as_wkb()]
#' @rdname segments_to_wkt
#' @export
segments_to_wkb <- function(x, type = c("multilinestring", "linestring", "point"), ...) {
  check_wkpool(x)
  type <- match.arg(type)

  if (length(x) < 1) {
    return(wk::as_wkb(wkb_empty(x), ...))
  }

  if (type == "point") {
    # vertices only: every pool vertex, referenced or not
    v <- pool_vertices(x)
    return(wk::as_wkb(pool_coords(x, seq_len(nrow(v))), ...))
  }

  # one LINESTRING per segment (the wk_handle presentation)
  lines <- pool_segments_geometry(x)

  if (type == "linestring") {
    return(wk::as_wkb(lines, ...))
  }

  # multilinestring: all segments in one geometry
  result <- wk::wk_collection(
    lines,
    wk::wk_geometry_type("multilinestring"),
    feature_id = 1L
  )
  wk::as_wkb(result, ...)
}

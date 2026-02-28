#' @param ... Passed to [wk::as_wkb()]
#' @rdname arcs_to_wkt
#' @export
arcs_to_wkb <- function(x, ...) {
  check_wkpool(x)
  if (length(x) < 1) {
    return(wk::as_wkb(wk::wkt(character(0))))
  }
  wk::as_wkb(arcs_to_wkt(x), ...)
}

# Round-trip conversion: WKT/WKB from arcs, polygons from cycles

#' Convert arcs to WKT linestrings
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return A wk_wkt vector of LINESTRING geometries
#'
#' @details
#' Each arc (maximal segment sequence between nodes) becomes a linestring.
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
arcs_to_wkt <- function(x) {
  check_wkpool(x)
  if (length(x) < 1) {
    return(wk::wkt(character(0)))
  }
  arcs <- find_arcs(x)
  if (length(arcs) == 0) return(wk::wkt(character(0)))
  pool <- pool_vertices(x)

  wkts <- vapply(arcs, function(arc) {
    idx <- match(arc, pool$.vx)
    coords <- paste(pool$x[idx], pool$y[idx], sep = " ")
    paste0("LINESTRING (", paste(coords, collapse = ", "), ")")
  }, character(1))

  wk::wkt(wkts)
}


#' Convert cycles to WKT polygons
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param feature Logical: attempt to reconstruct original features?
#'   If TRUE, groups cycles by .feature and nests holes in outers.
#'   If FALSE, each cycle becomes a separate polygon.
#' @param convention Winding convention: "sf" (default) or "ogc"
#' @return A wk_wkt vector of POLYGON geometries
#'
#' @details
#' Converts cycles back to polygons. When feature = TRUE, attempts to
#' reconstruct original polygon structure by grouping rings by feature
#' and nesting holes within their containing outer ring.
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
  convention <- match.arg(convention)
  cycles <- find_cycles(x)
  if (length(cycles) == 0) return(wk::wkt(character(0)))
  pool <- pool_vertices(x)
  segs <- pool_segments(x)

  # Get signed areas
  areas <- vapply(cycles, cycle_signed_area, numeric(1), pool = pool)

  if (convention == "sf") {
    is_outer <- areas < 0
  } else {
    is_outer <- areas > 0
  }

  # Convert cycle to WKT ring string
  cycle_to_ring <- function(cyc) {
    idx <- match(cyc, pool$.vx)
    # Close the ring
    coords <- paste(pool$x[idx], pool$y[idx], sep = " ")
    coords <- c(coords, coords[1])
    paste0("(", paste(coords, collapse = ", "), ")")
  }

  if (!feature || is.null(pool_feature(x))) {
    # Simple: each outer cycle becomes a polygon (ignore holes for now)
    # Or: each cycle becomes its own polygon
    wkts <- vapply(seq_along(cycles), function(i) {
      ring <- cycle_to_ring(cycles[[i]])
      paste0("POLYGON (", ring, ")")
    }, character(1))

    return(wk::wkt(wkts))
  }

  # Try to reconstruct features with holes
  # Associate each cycle with a feature based on segment membership
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

  wkts <- vapply(unique_features, function(feat) {
    feat_cycles <- which(cycle_features == feat)
    feat_outers <- feat_cycles[is_outer[feat_cycles]]
    feat_holes <- feat_cycles[!is_outer[feat_cycles]]

    if (length(feat_outers) == 0) {
      return(NA_character_)
    }

    # Simple case: one outer, associate all holes
    # (Proper implementation would check containment)
    rings <- c(
      vapply(feat_outers, function(i) cycle_to_ring(cycles[[i]]), character(1)),
      vapply(feat_holes, function(i) cycle_to_ring(cycles[[i]]), character(1))
    )

    if (length(feat_outers) == 1) {
      paste0("POLYGON (", paste(rings, collapse = ", "), ")")
    } else {
      # Multiple outers = MULTIPOLYGON (simplified - all holes go with first outer)
      # Proper implementation would match holes to containing outers
      parts <- vapply(feat_outers, function(i) {
        paste0("(", cycle_to_ring(cycles[[i]]), ")")
      }, character(1))
      paste0("MULTIPOLYGON (", paste(parts, collapse = ", "), ")")
    }
  }, character(1))

  wk::wkt(wkts[!is.na(wkts)])
}


#' Convert wkpool segments to WKT
#'
#' @param x A wkpool
#' @param type Output type: "linestring" (segments as paths), "multilinestring" (all segments),
#'   or "point" (vertices only)
#' @return A wk_wkt vector
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

  if (length(x) < 1) {
    return(wk::wkt(character(0)))
  }
  type <- match.arg(type)
  pool <- pool_vertices(x)
  segs <- pool_segments(x)
  if (length(segs) == 0) return(wk::wkt(character(0)))
  if (type == "point") {
    wkts <- paste0("POINT (", pool$x, " ", pool$y, ")")
    return(wk::wkt(wkts))
  }

  if (type == "linestring") {
    # Each segment as separate linestring
    idx0 <- match(segs$.vx0, pool$.vx)
    idx1 <- match(segs$.vx1, pool$.vx)

    wkts <- paste0(
      "LINESTRING (",
      pool$x[idx0], " ", pool$y[idx0], ", ",
      pool$x[idx1], " ", pool$y[idx1], ")"
    )
    return(wk::wkt(wkts))
  }

  # multilinestring: all segments in one geometry
  idx0 <- match(segs$.vx0, pool$.vx)
  idx1 <- match(segs$.vx1, pool$.vx)

  lines <- paste0(
    "(", pool$x[idx0], " ", pool$y[idx0], ", ",
    pool$x[idx1], " ", pool$y[idx1], ")"
  )

  wkt <- paste0("MULTILINESTRING (", paste(lines, collapse = ", "), ")")
  wk::wkt(wkt)
}



#' @param ... Passed to [wk::as_wkb()]
#' @rdname cycles_to_wkt
#' @export
cycles_to_wkb <- function(x, feature = TRUE, convention = c("sf", "ogc"), ...) {
  check_wkpool(x)
  wk::as_wkb(cycles_to_wkt(x, feature = feature, convention = convention), ...)
}


#' @param ... Passed to [wk::as_wkb()]
#' @rdname segments_to_wkt
#' @export
segments_to_wkb <- function(x, type = c("multilinestring", "linestring", "point"), ...) {
  check_wkpool(x)
  if (length(x) < 1) {
    return(wk::as_wkb(wk::wkt(character(0))))
  }
  wk::as_wkb(segments_to_wkt(x, type = type), ...)
}

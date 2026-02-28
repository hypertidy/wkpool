# merge_coincident: discover shared vertices and remap
#
# This is where topology emerges from geometry

#' Merge coincident vertices in a pool
#'
#' @param x A wkpool
#' @param tolerance Numeric tolerance for coordinate matching.
#'   Default 0 means exact match only.
#' @return A wkpool with shared vertices merged (fewer unique vertices)
#'
#' @details
#' With tolerance = 0, only exactly identical coordinates are merged.
#' The first occurrence becomes the canonical vertex.
#'
#' This is where you discover shared boundaries between polygons,
#' network connectivity, mesh topology, etc.
#'
#' Feature provenance (.feature) is preserved - segments keep their
#' original feature assignment even after vertex merging.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' topology_report(pool)
#' merged <- merge_coincident(pool)
#' topology_report(merged)
#'
#' @export
merge_coincident <- function(x, tolerance = 0) {
  check_wkpool(x)
  pool <- pool_vertices(x)
  vx0 <- vctrs::field(x, ".vx0")
  vx1 <- vctrs::field(x, ".vx1")
  feature <- pool_feature(x)

  if (tolerance == 0) {
    # Exact match: group by coordinate values
    coord_key <- paste(pool$x, pool$y, sep = "_")

    unique_keys <- unique(coord_key)
    first_vx <- pool$.vx[match(unique_keys, coord_key)]
    names(first_vx) <- unique_keys

    canonical <- first_vx[coord_key]
    names(canonical) <- pool$.vx

    new_vx0 <- unname(canonical[as.character(vx0)])
    new_vx1 <- unname(canonical[as.character(vx1)])

    new_pool <- pool[pool$.vx %in% first_vx, , drop = FALSE]

  } else {
    # Tolerance-based: grid snap then exact match
    grid_x <- round(pool$x / tolerance) * tolerance
    grid_y <- round(pool$y / tolerance) * tolerance
    coord_key <- paste(grid_x, grid_y, sep = "_")

    unique_keys <- unique(coord_key)
    first_vx <- pool$.vx[match(unique_keys, coord_key)]
    names(first_vx) <- unique_keys

    canonical <- first_vx[coord_key]
    names(canonical) <- pool$.vx

    new_vx0 <- unname(canonical[as.character(vx0)])
    new_vx1 <- unname(canonical[as.character(vx1)])

    new_pool <- pool[pool$.vx %in% first_vx, , drop = FALSE]
  }

  # Renumber .vx to be contiguous
  old_vx <- new_pool$.vx
  new_pool$.vx <- seq_len(nrow(new_pool))

  final_remap <- new_pool$.vx
  names(final_remap) <- old_vx

  final_vx0 <- unname(final_remap[as.character(new_vx0)])
  final_vx1 <- unname(final_remap[as.character(new_vx1)])

  new_wkpool(new_pool, final_vx0, final_vx1, feature = feature)
}


#' Find shared edges between features
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return Data frame of edges shared by multiple features
#'
#' @details
#' After merging coincident vertices, edges that were duplicated
#' across features (e.g., shared polygon boundaries) will reference
#' the same vertex pair. This function finds them and reports which
#' features share each edge.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' find_shared_edges(merged)
#'
#' @export
find_shared_edges <- function(x) {
  check_wkpool(x)
  vx0 <- vctrs::field(x, ".vx0")
  vx1 <- vctrs::field(x, ".vx1")
  feature <- pool_feature(x)

  # Normalize edge direction for comparison (lower .vx first)
  edge_lo <- pmin(vx0, vx1)
  edge_hi <- pmax(vx0, vx1)
  edge_key <- paste(edge_lo, edge_hi, sep = "-")

  # Build edge-feature table
  edge_df <- data.frame(
    edge_key = edge_key,
    .vx0 = edge_lo,
    .vx1 = edge_hi,
    .feature = if (!is.null(feature)) feature else NA_integer_,
    segment_idx = seq_along(vx0)
  )

  # Find edges that appear in multiple features
  if (!is.null(feature)) {
    # Group by edge, count distinct features
    edge_features <- tapply(edge_df$.feature, edge_df$edge_key,
                            function(f) unique(f[!is.na(f)]))
    shared_keys <- names(edge_features)[lengths(edge_features) > 1]

    out <- edge_df[edge_df$edge_key %in% shared_keys, ]
    out$features <- edge_features[out$edge_key]
    out
  } else {
    # No feature info - just find duplicate edges
    dup_keys <- unique(edge_key[duplicated(edge_key)])
    edge_df[edge_df$edge_key %in% dup_keys, ]
  }
}


#' Find internal boundaries (edges shared by exactly 2 features, opposite direction)
#'
#' @param x A wkpool after merge_coincident
#' @return A wkpool containing only internal boundary segments
#'
#' @details
#' Internal boundaries are edges where one feature has segment (v0→v1) and
#' another feature has (v1→v0) — i.e., they share the edge but traverse it
#' in opposite directions. This is the defining characteristic of a shared
#' polygon boundary.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' find_internal_boundaries(merged)
#'
#' @export
find_internal_boundaries <- function(x) {
  check_wkpool(x)
  s <- pool_segments(x)

  # Create directed edge key
  edge_directed <- paste(s$.vx0, s$.vx1, sep = "-")

  # Create reversed key
  edge_reversed <- paste(s$.vx1, s$.vx0, sep = "-")

  # Internal = my edge exists as someone else's reverse
  is_internal <- edge_directed %in% edge_reversed

  x[is_internal]
}


#' Convert wkpool to RTriangle pslg format
#'
#' @param x A wkpool (will be passed through merge_coincident if not already)
#' @param ... passed to merge_coincident
#' @return A list with P (vertex matrix) and S (segment matrix) for RTriangle::pslg()
#'
#' @details
#' Produces a Planar Straight Line Graph suitable for constrained triangulation
#' with RTriangle. The vertex pool maps directly to P, and segment indices
#' map directly to S (both 1-indexed).
#'
#' Note: Hole detection is not currently supported. The returned pslg
#' does not include hole points (H). For polygons with holes, you may
#' need to identify hole points manually using `find_cycles()` and
#' `cycle_signed_area()`.
#'
#' @examples
#' x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' pool <- establish_topology(x)
#' as_pslg(pool)
#'
#' @export
as_pslg <- function(x, ...) {
  x <- merge_coincident(x, ...)
  v <- pool_vertices(x)
  s <- pool_segments(x)
  list(
    P = cbind(v$x, v$y),
    S = cbind(s$.vx0, s$.vx1)
  )
}


#' Convert wkpool to decido-ready format
#'
#' @param x A wkpool after merge_coincident
#' @return A list with xy (vertex matrix) and segments (segment matrix, 0-indexed)
#'
#' @details
#' Produces format suitable for decido::earcut() constrained triangulation.
#' Note: decido uses 0-indexed segments.
#'
#' @examples
#' x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' pool <- establish_topology(x)
#' as_decido(pool)
#'
#' @noRd
#' @keywords internal
as_decido <- function(x) {
 x <- merge_coincident(x)
  v <- pool_vertices(x)
  s <- pool_segments(x)

  list(
    xy = cbind(v$x, v$y),
    segments = cbind(s$.vx0 - 1L, s$.vx1 - 1L)  # 0-indexed for decido
  )
}


#' Find closed cycles (rings) in segment graph
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return A list of integer vectors, each containing .vx IDs forming a closed cycle
#'
#' @details
#' Walks the segment graph to discover closed loops. Relies on segments being
#' in consecutive order within each ring (as produced by establish_topology).
#'
#' Each cycle is a vector of vertex IDs in traversal order. The cycle is closed
#' (first vertex connects back to last via a segment).
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' find_cycles(merged)
#'
#' @export
find_cycles <- function(x) {
  check_wkpool(x)
  segs <- pool_segments(x)
  n <- nrow(segs)

  if (n == 0) return(list())

  cycles <- list()
  i <- 1

  while (i <= n) {
    # Start a new cycle
    start_vx <- segs$.vx0[i]
    vertices <- start_vx
    current_vx <- segs$.vx1[i]
    i <- i + 1

    # Follow segments until we close the loop or run out
    while (current_vx != start_vx && i <= n) {
      vertices <- c(vertices, current_vx)
      current_vx <- segs$.vx1[i]
      i <- i + 1
    }

    if (current_vx == start_vx) {
      cycles <- c(cycles, list(vertices))
    }
  }

  cycles
}


#' Calculate signed area of a cycle
#'
#' @param cycle Integer vector of .vx IDs forming a closed cycle
#' @param pool Vertex pool data frame (from pool_vertices)
#' @return Numeric signed area.
#'
#' @details
#' Uses the shoelace formula to compute signed area. The sign indicates
#' winding direction. For typical geographic data (Simple Features convention):
#' - Negative area = outer ring
#' - Positive area = hole
#'
#' This is intrinsic to the geometry — we observe winding, not declare it.
#'
#' @examples
#' x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' cycles <- find_cycles(merged)
#' cycle_signed_area(cycles[[1]], pool_vertices(merged))
#'
#' @export
cycle_signed_area <- function(cycle, pool) {
  idx <- match(cycle, pool$.vx)
  x <- pool$x[idx]
  y <- pool$y[idx]

  n <- length(x)
  # Close the ring for calculation
  x <- c(x, x[1])
  y <- c(y, y[1])

  # Shoelace formula
  sum(x[-(n+1)] * y[-1] - x[-1] * y[-(n+1)]) / 2
}


#' Classify cycles as outer rings or holes based on winding
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param convention Which winding convention to use: "sf" (default) or "ogc"
#'   - sf: negative area = outer, positive = hole
#'   - ogc: positive area = outer, negative = hole
#' @return A data frame with cycle index, signed area, and type (outer/hole)
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   paste0(
#'     "MULTIPOLYGON (((0 0, 0 1, 0.75 1, 1 0.8, 0.5 0.7, ",
#'     "0.8 0.6, 0.69 0, 0 0), (0.2 0.2, 0.5 0.2, ",
#'     "0.5 0.4, 0.3 0.6, 0.2 0.4, 0.2 0.2)))"),
#'   "MULTIPOLYGON (((0.69 0, 0.8 0.6, 1.1 0.63, 1.23 0.3, 0.69 0)))"
#' ))
#'
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' classify_cycles(merged)
#'
#' @export
classify_cycles <- function(x, convention = c("sf", "ogc")) {
  check_wkpool(x)
  convention <- match.arg(convention)
  cycles <- find_cycles(x)
  pool <- pool_vertices(x)

  areas <- vapply(cycles, cycle_signed_area, numeric(1), pool = pool)

  if (convention == "sf") {
    type <- ifelse(areas < 0, "outer", "hole")
  } else {
    type <- ifelse(areas > 0, "outer", "hole")
  }

  data.frame(
    cycle = seq_along(cycles),
    area = areas,
    type = type
  )
}


#' Reverse a cycle's winding direction
#'
#' @param cycle Integer vector of .vx IDs forming a closed cycle
#' @return The same vertices in reversed order (opposite winding)
#'
#' @details
#' Flips the traversal direction of a cycle without changing coordinates.
#' Useful for converting between winding conventions.
#'
#' @examples
#' x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' cycles <- find_cycles(merged)
#' reverse_cycle(cycles[[1]])
#'
#' @export
reverse_cycle <- function(cycle) {
  rev(cycle)
}


#' Get hole points for constrained triangulation
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param convention Which winding convention to use: "sf" (default) or "ogc"
#' @return A matrix of hole points (centroids of hole cycles), or NULL if no holes
#'
#' @details
#' For use with RTriangle::triangulate(). Each row is the centroid of a hole,
#' which tells the triangulator to exclude that region.
#'
#' @examples
#' polygons_with_holes <- wk::as_wkb(c(
#'   paste0(
#'     "MULTIPOLYGON (((0 0, 0 1, 0.75 1, 1 0.8, 0.5 0.7, ",
#'     "0.8 0.6, 0.69 0, 0 0), (0.2 0.2, 0.5 0.2, ",
#'     "0.5 0.4, 0.3 0.6, 0.2 0.4, 0.2 0.2)))"),
#'   "MULTIPOLYGON (((0.69 0, 0.8 0.6, 1.1 0.63, 1.23 0.3, 0.69 0)))"
#' ))
#'
#' x <- establish_topology(polygons_with_holes)
#' merged <- merge_coincident(x)
#' hole_points(merged)
#' if (requireNamespace("RTriangle", quietly = TRUE)) {
#'   pslg <- as_pslg(merged)
#'   holes <- hole_points(merged)
#'   tri <- RTriangle::triangulate(
#'     RTriangle::pslg(P = pslg$P, S = pslg$S, H = holes)
#'   )
#' }
#'
#' @export
hole_points <- function(x, convention = c("sf", "ogc")) {
  check_wkpool(x)
  convention <- match.arg(convention)
  cycles <- find_cycles(x)
  pool <- pool_vertices(x)

  areas <- vapply(cycles, cycle_signed_area, numeric(1), pool = pool)

  if (convention == "sf") {
    hole_idx <- which(areas > 0)
  } else {
    hole_idx <- which(areas < 0)
  }

  if (length(hole_idx) == 0) return(NULL)

  hole_cycles <- cycles[hole_idx]

  # Centroid of each hole
  pts <- t(vapply(hole_cycles, function(cyc) {
    idx <- match(cyc, pool$.vx)
    c(mean(pool$x[idx]), mean(pool$y[idx]))
  }, numeric(2)))

  colnames(pts) <- c("x", "y")
  pts
}




#' Build adjacency from shared edges
#'
#' @param x A wkpool after merge_coincident
#' @param type "edge" for features sharing an edge, "vertex" for features sharing any vertex
#' @return Data frame of feature pairs that are neighbours
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' find_neighbours(merged)
#'
#' @export
find_neighbours <- function(x, type = c("edge", "vertex")) {
  check_wkpool(x)
  type <- match.arg(type)
  feature <- pool_feature(x)

  if (is.null(feature)) {
    stop("wkpool has no feature information")
  }

  if (type == "edge") {
    shared <- find_shared_edges(x)

    if (nrow(shared) == 0) {
      return(data.frame(feature_a = integer(), feature_b = integer()))
    }

    # For each shared edge, create pairs of features
    pairs <- lapply(unique(shared$edge_key), function(key) {
      feats <- unique(shared$.feature[shared$edge_key == key])
      if (length(feats) >= 2) {
        expand.grid(feature_a = feats, feature_b = feats,
                    stringsAsFactors = FALSE)
      }
    })

    pairs <- do.call(rbind, pairs)
    pairs <- pairs[pairs$feature_a < pairs$feature_b, ]  # unique pairs
    unique(pairs)

  } else {
    # Vertex-based: features sharing any vertex
    vx0 <- vctrs::field(x, ".vx0")
    vx1 <- vctrs::field(x, ".vx1")

    # All vertex-feature associations
    vf <- data.frame(
      .vx = c(vx0, vx1),
      .feature = c(feature, feature)
    )
    vf <- unique(vf)

    # For each vertex, find features
    vertex_features <- split(vf$.feature, vf$.vx)
    shared_vertices <- vertex_features[lengths(vertex_features) > 1]

    pairs <- lapply(shared_vertices, function(feats) {
      expand.grid(feature_a = feats, feature_b = feats,
                  stringsAsFactors = FALSE)
    })

    pairs <- do.call(rbind, pairs)
    if (is.null(pairs) || nrow(pairs) == 0) {
      return(data.frame(feature_a = integer(), feature_b = integer()))
    }

    pairs <- pairs[pairs$feature_a < pairs$feature_b, ]
    unique(pairs)
  }
}


#' Report topology diagnostics
#'
#' @param x A wkpool
#' @param tolerance Tolerance for "near miss" detection
#' @return List of diagnostic information
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' topology_report(pool)
#'
#' @export
topology_report <- function(x, tolerance = 1e-8) {
  check_wkpool(x)
  pool <- pool_vertices(x)
  vx0 <- vctrs::field(x, ".vx0")
  vx1 <- vctrs::field(x, ".vx1")
  feature <- pool_feature(x)

  # Exact duplicates
  coord_key <- paste(pool$x, pool$y, sep = "_")
  n_unique_exact <- length(unique(coord_key))
  n_dup_exact <- nrow(pool) - n_unique_exact

  # Near misses (would merge with tolerance)
  grid_x <- round(pool$x / tolerance) * tolerance
  grid_y <- round(pool$y / tolerance) * tolerance
  grid_key <- paste(grid_x, grid_y, sep = "_")
  n_unique_tol <- length(unique(grid_key))
  n_near_miss <- n_unique_exact - n_unique_tol

  # Shared edges (after notional merge)
  merged <- merge_coincident(x, tolerance = 0)
  shared <- find_shared_edges(merged)
  n_shared_edges <- length(unique(shared$edge_key))

  # Feature count
  n_features <- if (!is.null(feature)) length(unique(feature)) else NA

  list(
    n_vertices_raw = nrow(pool),
    n_vertices_unique = n_unique_exact,
    n_duplicate_vertices = n_dup_exact,
    n_near_miss_vertices = n_near_miss,
    n_segments = length(vx0),
    n_shared_edges = n_shared_edges,
    n_features = n_features
  )
}

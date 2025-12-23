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
#' @export
merge_coincident <- function(x, tolerance = 0) {
  
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
#' @export
find_shared_edges <- function(x) {
  
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
#' @export
find_internal_boundaries <- function(x) {
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
#' @examples
#' \dontrun{
#' x <- establish_topology(my_polygons)
#' pslg <- as_pslg(x)
#' tri <- RTriangle::triangulate(RTriangle::pslg(P = pslg$P, S = pslg$S))
#' }
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
#' @export
as_decido <- function(x) {
  x <- merge_coincident(x)
  v <- pool_vertices(x)
  s <- pool_segments(x)
  
  list(
    xy = cbind(v$x, v$y),
    segments = cbind(s$.vx0 - 1L, s$.vx1 - 1L)  # 0-indexed for decido
  )
}


#' Build adjacency from shared edges
#'
#' @param x A wkpool after merge_coincident
#' @param type "edge" for features sharing an edge, "vertex" for features sharing any vertex
#' @return Data frame of feature pairs that are neighbours
#'
#' @export
find_neighbours <- function(x, type = c("edge", "vertex")) {
  
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
#' @export
topology_report <- function(x, tolerance = 1e-8) {
  
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

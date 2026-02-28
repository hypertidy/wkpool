# establish_topology: convert wk-handleable geometry to wkpool
#
# This is the entry point: anything wk can read becomes a wkpool

#' Establish topology from any wk-handleable geometry
#'
#' @param x Any geometry wk can handle (sf, sfc, wkb, wkt, geos, s2, xy, ...)
#' @param ... reserved for future use
#' @return A wkpool object (segments with vertex pool)
#'
#' @details
#' Vertices are minted as-is from the input coordinates.
#' No snapping or deduplication is performed - the pool represents
#' the exact truth of the input geometry. Use merge_coincident()
#' afterward to discover/establish shared topology.
#'
#' Segments track their feature origin via `.feature` attribute,
#' enabling discovery of shared boundaries and neighbour relations.
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
#'
#' @export
establish_topology <- function(x, ...) {

 # wk_coords works on anything handleable
  coords <- wk::wk_coords(x)

  # coords gives us:
  #   feature_id | part_id | ring_id | x | y | (z) | (m)

  n <- nrow(coords)

  # Mint vertices - every coordinate row gets a .vx
  vertices <- data.frame(
    .vx = seq_len(n),
    x = coords$x,
    y = coords$y
  )

  # Add z if present
  if ("z" %in% names(coords) && !all(is.na(coords$z))) {
    vertices$z <- coords$z
  }

  # Build segments from consecutive coordinates within each path
  # A path is defined by (feature_id, part_id, ring_id)
  #
  # We need segment from row i to row i+1 ONLY if they're in the same path

  if (n < 2) {
    return(wkpool_empty())
  }

  # Check which consecutive rows share the same path
  same_feature <- coords$feature_id[-1] == coords$feature_id[-n]
  same_part <- coords$part_id[-1] == coords$part_id[-n]
  same_ring <- coords$ring_id[-1] == coords$ring_id[-n]

  same_path <- same_feature & same_part & same_ring

  # Segment from row i to row i+1 where same_path[i] is TRUE
  seg_idx <- which(same_path)
  vx0 <- seg_idx
  vx1 <- seg_idx + 1L

  # Track feature provenance
  feature_id <- coords$feature_id[seg_idx]

  new_wkpool(vertices, vx0, vx1, feature = feature_id)
}


#' Compact a pool by removing unreferenced vertices
#'
#' @param x A wkpool
#' @return A wkpool with only referenced vertices, .vx remapped
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' compact <- pool_compact(merged)
#' nrow(pool_vertices(compact))
#'
#' @export
pool_compact <- function(x) {

  vx0 <- vctrs::field(x, ".vx0")
  vx1 <- vctrs::field(x, ".vx1")
  pool <- pool_vertices(x)
  feature <- pool_feature(x)

  # Which vertices are actually used?
  active <- unique(c(vx0, vx1))

  # Subset pool to active vertices
  new_pool <- pool[pool$.vx %in% active, , drop = FALSE]

  # Build remap: old .vx -> new .vx (1:n)
  new_pool$.vx <- seq_len(nrow(new_pool))
  remap <- match(active, pool$.vx)
  names(remap) <- active

  # Remap segment indices
  new_vx0 <- new_pool$.vx[match(vx0, active)]
  new_vx1 <- new_pool$.vx[match(vx1, active)]

  new_wkpool(new_pool, new_vx0, new_vx1, feature = feature)
}

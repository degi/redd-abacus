
## NOTE: map operations in this file are stars-only. terra is deliberately NOT
## used here, so a map is always read, warped and tabulated with stars/sf and
## never converted to a SpatRaster (see server.R for the same rule).
require(stars)


#' Raster Cell Distances
#'
#' Cell-to-cell distance with the same eight-neighbour metric as
#' gridDistance()/gridDist() {terra}, computed on a stars raster map.
#'
#' Distances are returned in CELL UNITS (1 for the next cell up, down, left or
#' right; diagonal neighbours are sqrt(2) away), not in map units, and travel
#' only through cells that are not NA - paths crossing an NA cell are blocked,
#' exactly like the terra grid distance.
#'
#' @param map stars raster map
#' @param target value(s) of the cells from which the distance is calculated.
#'   If NULL all cells that are not NA are origins
#'
#' @return The distance map of stars object
#' @export
#'
#' @examples
#'
grid_distance <- function(map, target = NULL) {
  if (is.na(st_crs(map))) st_crs(map) <- "+proj=robin"
  values <- map[[1]]
  if (!is.null(target) && !any(target %in% values)) return(NULL)

  tryCatch({
    m <- if (is.matrix(values)) values else as.matrix(values)
    if (is.null(dim(m)) || length(dim(m)) != 2) return(NULL)

    nr <- nrow(m)
    nc <- ncol(m)
    ## Non-NA cells are traversable terrain; NA cells act as impassable barriers,
    ## matching terra::gridDist.
    traversable <- !is.na(m)
    if (!any(traversable)) return(NULL)

    ## Origins are the target cells (or every non-NA cell when no target is given).
    origins <- if (is.null(target)) traversable else traversable & (m %in% target)
    if (!any(origins)) return(NULL)

    ## Fast Sweeping 8-neighbor raster distance transform (O(N) linear time).
    ## Replaces naive Dijkstra with raster passes; orthogonal step = 1, diagonal = sqrt(2).
    d <- matrix(Inf, nrow = nr, ncol = nc)
    d[origins] <- 0
    d_diag <- sqrt(2)
    max_iter <- 4L

    for (iter in seq_len(max_iter)) {
      changed <- FALSE

      # Forward pass: top-to-bottom, left-to-right
      for (c in seq_len(nc)) {
        for (r in seq_len(nr)) {
          if (!traversable[r, c]) next
          cur <- d[r, c]
          orig_cur <- cur
          if (r > 1L) {
            cur <- min(cur, d[r - 1L, c] + 1)
            if (c > 1L) cur <- min(cur, d[r - 1L, c - 1L] + d_diag)
          }
          if (c > 1L) {
            cur <- min(cur, d[r, c - 1L] + 1)
            if (r < nr) cur <- min(cur, d[r + 1L, c - 1L] + d_diag)
          }
          if (cur < orig_cur) {
            d[r, c] <- cur
            changed <- TRUE
          }
        }
      }

      # Backward pass: bottom-to-top, right-to-left
      for (c in seq(nc, 1L, by = -1L)) {
        for (r in seq(nr, 1L, by = -1L)) {
          if (!traversable[r, c]) next
          cur <- d[r, c]
          orig_cur <- cur
          if (r < nr) {
            cur <- min(cur, d[r + 1L, c] + 1)
            if (c < nc) cur <- min(cur, d[r + 1L, c + 1L] + d_diag)
          }
          if (c < nc) {
            cur <- min(cur, d[r, c + 1L] + 1)
            if (r > 1L) cur <- min(cur, d[r - 1L, c + 1L] + d_diag)
          }
          if (cur < orig_cur) {
            d[r, c] <- cur
            changed <- TRUE
          }
        }
      }

      if (!changed) break
    }

    d[!is.finite(d)] <- NA_real_
    out <- map
    out[[1]] <- d
    return(out)
  }, error = function(e) {
    NULL
  })
}
STARS_WARP_RESAMPLE_METHOD <- "near"

## TRUE when both rasters carry a CRS and it is exactly the same one.
## A CRS that is missing (NA) counts as a mismatch: without a shared CRS there is
## no defensible way to place the two grids relative to each other, so nothing is
## aligned on a guess.
same_crs <- function(x, y) {
  if (is.null(x) || is.null(y)) return(FALSE)
  cx <- tryCatch(sf::st_crs(x), error = function(e) NA)
  cy <- tryCatch(sf::st_crs(y), error = function(e) NA)
  if (is.na(cx) || is.na(cy)) return(FALSE)
  isTRUE(cx == cy)
}

## Why `map` cannot be aligned onto `reference`, or NULL when it can.
## Two rasters must share the exact same projection/coordinate system before any
## alignment is attempted. Reprojecting one onto the other's CRS would change
## what its cell values mean, so a differing CRS is reported, never silently
## transformed or warped.
crs_mismatch_reason <- function(map, reference) {
  if (is.null(map) || is.null(reference)) return(NULL)
  cmap <- tryCatch(sf::st_crs(map), error = function(e) NA)
  cref <- tryCatch(sf::st_crs(reference), error = function(e) NA)
  if (is.na(cmap)) return("no coordinate reference system")
  if (is.na(cref)) return("the first map has no coordinate reference system to match")
  if (!isTRUE(cmap == cref)) {
    return(sprintf(
      "a different coordinate reference system (%s instead of %s)",
      cmap$input %||% cmap$wkt %||% "unknown",
      cref$input %||% cref$wkt %||% "unknown"
    ))
  }
  NULL
}

align_stars_to_reference <- function(map, reference) {
  if (is.null(map) || is.null(reference))
    return(map)
  if (!inherits(map, "stars") || !inherits(reference, "stars"))
    return(map)
  ## Refuse to align across projections: same CRS first, resample second.
  if (!same_crs(map, reference)) {
    warning("Cannot align raster: the maps do not share the same CRS.")
    return(map)
  }
  if (identical(st_dimensions(map), st_dimensions(reference)))
    return(map)

  ## The classes present in the map being warped. A stars_proxy must be
  ## materialised first: on a proxy, [[1]] returns the FILE PATH, not values.
  map_vals <- map
  if (inherits(map_vals, "stars_proxy"))
    map_vals <- tryCatch(stars::st_as_stars(map_vals), error = function(e) NULL)
  src_classes <- if (is.null(map_vals)) NULL else {
    u <- sort(unique(as.vector(map_vals[[1]])))
    u[!is.na(u)]
  }

  ## Native warper first, GDAL second. With two stars_proxy inputs GDAL's
  ## warper writes the REFERENCE's dataset rather than warping the map, so a
  ## zonation layer came back holding the land cover's classes. The native
  ## warper reads the map's own values, so it is tried first.
  for (use_gdal in c(FALSE, TRUE)) {
    aligned <- tryCatch({
      st_warp(map, reference, use_gdal = use_gdal,
              method = STARS_WARP_RESAMPLE_METHOD)
    }, error = function(e) NULL)
    if (is.null(aligned))
      next

    ## Reject a result whose values are not the map's own classes: matching
    ## dimensions alone cannot detect the wrong dataset being returned.
    if (!is.null(src_classes)) {
      got <- sort(unique(as.vector(aligned[[1]])))
      got <- got[!is.na(got)]
      if (!all(got %in% src_classes)) {
        warning("Warper returned values that are not in the source map; ",
                "discarding that result.")
        next
      }
    }
    return(aligned)
  }

  warning("Unable to align raster grid; leaving original map unchanged.")
  map
}


#' Warp a stars raster onto another raster's grid
#'
#' Makes `map` share the exact extent, resolution and CRS of `reference`.
#' This is what a zonation overlay needs before it can be cross-tabulated with a
#' land cover map: `st_warp()` resamples onto the reference grid, but it only
#' produces a cellwise-aligned result when the two grids are comparable, and it
#' silently treats unset no-data as 0. This wrapper declares the no-data value,
#' tries both the GDAL and the native warper, and verifies the outcome, so a
#' mismatched pair is either aligned or reported instead of silently mis-tabulated.
#'
#' @param map stars raster to warp (the zonation layer)
#' @param reference stars raster whose grid is the target (the land cover map)
#' @param no_data numeric value to treat as missing in `map` (default NA)
#'
#' @return `map` on the reference grid, or NULL when alignment is not possible
#' @export
warp_stars_to_grid <- function(map, reference, no_data = NA) {
  if (is.null(map) || is.null(reference))
    return(NULL)
  if (!inherits(map, "stars") || !inherits(reference, "stars"))
    return(NULL)

  ## A shared CRS is a hard precondition, not a warning: `st_warp()` would
  ## otherwise reproject `map` on the fly, which is exactly the silent
  ## coordinate change this function exists to prevent. Refuse instead.
  if (!same_crs(map, reference)) {
    warning("Could not align zonation map: it does not share the land cover CRS.")
    return(NULL)
  }

  if (identical(st_dimensions(map), st_dimensions(reference)))
    return(map)

  ## st_warp() only sets GDAL's -dstnodata when no_data_value is a real number;
  ## passing NA makes it leave missing cells as *zero*, which would then be
  ## counted as a genuine class 0. So warp with a sentinel that cannot occur as
  ## a class id, then translate it back to NA.
  sentinel <- if (!is.null(no_data) && !is.na(no_data)) no_data else -9999

  ## The classes present in the map being warped. An aligned result may only
  ## contain these (plus NA for cells that fell outside the map's footprint) -
  ## a warper that returns anything else has not resampled this map at all.
  ##
  ## A stars_proxy must be materialised first: on a proxy, [[1]] returns the
  ## FILE PATH as a character, not values, so reading classes off it directly
  ## would compare against a filename instead of the map's classes.
  map_vals <- map
  if (inherits(map_vals, "stars_proxy"))
    map_vals <- tryCatch(stars::st_as_stars(map_vals), error = function(e) NULL)
  if (is.null(map_vals))
    return(NULL)
  src_classes <- sort(unique(as.vector(map_vals[[1]])))
  src_classes <- src_classes[!is.na(src_classes)]

  ## Native warper FIRST, GDAL second.
  ##
  ## GDAL's warper is fed the *file path* of a stars_proxy, and with two proxies
  ## it writes the reference's dataset instead of warping the map: a 7-class
  ## zonation map came back carrying the land cover's 23 classes. That is silent
  ## data corruption, so the native warper - which reads the map's own values -
  ## is tried first, and every candidate is checked against the source classes
  ## below rather than trusted for matching dimensions alone.
  for (use_gdal in c(FALSE, TRUE)) {
    aligned <- tryCatch(
      st_warp(map, reference, use_gdal = use_gdal,
              method = STARS_WARP_RESAMPLE_METHOD,
              no_data_value = sentinel),
      error = function(e) NULL
    )
    if (is.null(aligned) || !identical(st_dimensions(aligned), st_dimensions(reference)))
      next

    ## The aligned layer must still describe the same map. Matching dimensions
    ## are not enough: a warper can produce a correctly shaped raster holding
    ## the wrong dataset's values.
    v <- aligned[[1]]
    got <- sort(unique(as.vector(v)))
    got <- got[!is.na(got)]
    got <- got[got != sentinel]
    if (!all(got %in% src_classes)) {
      warning("Warper returned values that are not in the source map; " ,
              "discarding that result.")
      next
    }

    ## Sentinel (and any NA the warper produced) becomes a real NA, so missing
    ## cells are excluded from the tabulation instead of counted as class 0.
    v[!is.na(v) & v == sentinel] <- NA
    aligned[[1]] <- v
    return(aligned)
  }

  warning("Could not align zonation map to the land cover grid.")
  NULL
}


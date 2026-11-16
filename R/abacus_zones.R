## Stars-only zonation utilities: rasterize vector zonation layers and compose
## multiple zonation maps into a single raster on a reference grid.
##
## All operations use stars/sf and never convert to SpatRaster.

#' Rasterise a vector layer onto a stars grid (stars only)
#'
#' Burns the polygons of `sf_data` into a raster on `reference`'s grid, writing
#' the value of `field` in every cell a polygon touches.
#'
#' @param sf_data sf object of polygons (or a path to one)
#' @param reference stars raster whose grid the result is drawn on
#' @param field name of the column holding the cell value; defaults to "ID"
#'
#' @return a single-layer stars raster of `field` values on `reference`'s grid
rasterize_to_reference <- function(sf_data, reference, field = "ID") {
  if (is.null(sf_data) || is.null(reference)) return(NULL)
  if (!inherits(reference, "stars")) return(NULL)
  if (is.character(sf_data)) {
    sf_data <- tryCatch(sf::st_read(sf_data, quiet = TRUE), error = function(e) NULL)
  }
  if (is.null(sf_data) || !inherits(sf_data, "sf") || nrow(sf_data) == 0) return(NULL)
  if (!field %in% names(sf_data)) return(NULL)

  out <- tryCatch({
    stars::st_rasterize(sf_data, reference, file = tempfile(fileext = ".tif"))
  }, error = function(e) NULL)
  if (is.null(out) || !inherits(out, "stars")) return(NULL)

  ## Keep only the value layer
  if (field %in% names(out)) {
    out <- out[field]
  } else if (length(out) > 1) {
    out <- out[1]
  }
  out
}

#' Compose several zonation maps into one raster with a single id space
#'
#' Each map is assigned a block index (position in `ids`, starting at 1)
#' and a cell is encoded as `zone_id * 1000 + block`.
#'
#' @param ids character vector of record keys
#' @param recs named list of records; `recs[[id]]$stars` is the zonation raster
#' @param reference stars raster whose grid all maps are warped onto
#' @param overlap `"first"` keeps the earliest map, `"strict"` drops overlapping cells
#'
#' @return a single-layer stars object of composed ids on `reference`'s grid, or NULL
compose_zone_raster <- function(ids, recs, reference,
                                overlap = c("first", "strict")) {
  overlap <- match.arg(overlap)
  if (is.null(ids) || length(ids) == 0 || is.null(reference))
    return(NULL)
  if (!inherits(reference, "stars"))
    return(NULL)

  ref <- if (inherits(reference, "stars_proxy")) stars::st_as_stars(reference) else reference
  if (is.null(ref))
    return(NULL)
  n <- length(ref[[1]])

  layers <- list()
  block_index <- integer(0)
  for (k in seq_along(ids)) {
    id <- ids[[k]]
    rec <- if (is.list(recs)) recs[[id]] else NULL
    map <- if (!is.null(rec)) rec$stars else NULL
    if (is.null(map))
      next

    zz <- if (inherits(map, "stars_proxy")) stars::st_as_stars(map) else map
    if (is.null(zz) || !inherits(zz, "stars"))
      next
    aligned <- tryCatch(warp_stars_to_grid(zz, ref), error = function(e) NULL)
    if (is.null(aligned))
      next

    v <- as.numeric(aligned[[1]])
    if (length(v) != n)
      next

    ids_present <- v[!is.na(v)]
    if (length(ids_present) > 0 && any(ids_present >= 1000)) {
      warning("Zonation map '", id,
              "' has zone ids >= 1000; skipping it in the composed raster.")
      next
    }

    layers[[length(layers) + 1]] <- v
    block_index <- c(block_index, k)
  }

  if (length(layers) == 0)
    return(NULL)

  cell_value <- rep(NA_real_, n)
  claims <- integer(n)

  for (j in seq_along(layers)) {
    v <- layers[[j]]
    claimed <- !is.na(v)
    if (!any(claimed))
      next
    claims[claimed] <- claims[claimed] + 1L
    fill <- claimed & is.na(cell_value)
    cell_value[fill] <- v[fill] * 1000 + block_index[[j]]
  }

  if (identical(overlap, "strict")) {
    cell_value[claims > 1L] <- NA
  }

  out <- ref
  out[[1]] <- cell_value
  out
}

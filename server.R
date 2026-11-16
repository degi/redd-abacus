
server <- function(input, output, session) {
  if (!exists("prepare_projection_abacus", mode = "function") && file.exists("R/abacus_projection.R")) {
    source("R/abacus_projection.R")
  }

  vars <-
    c("primary",
      "success",
      "info",
      "danger",
      "warning",
      "secondary",
      "dark")

  color_theme <-
    as.list(bs_get_variables(bs_current_theme(session), varnames = vars))

  chart_color <- c(
    paletteer_d("ggthemes::calc"),
    paletteer_d("ggsci::schwifty_rickandmorty"),
    paletteer_d("ggsci::default_uchicago"),
    paletteer_d("ggthemes::Classic_10"),
    paletteer_d("ggsci::default_jco"),
    paletteer_d("ggsci::springfield_simpsons"),
    paletteer_d("ggsci::light_uchicago"),
    paletteer_d("ggthemes::stata_s2color"),
    paletteer_d("RColorBrewer::Set1"),
    paletteer_d("RColorBrewer::Set2"),
    paletteer_d("RColorBrewer::Set3"),
    paletteer_dynamic("cartography::multi.pal", 20)
  )

  light_color <- c(
    paletteer_d("ggsci::legacy_tron"),
    paletteer_d("ggthemes::Superfishel_Stone"),
    paletteer_d("ggthemes::Classic_10_Light"),
    paletteer_d("ggpomological::pomological_palette"),
    paletteer_d("ggthemes::Tableau_10"),
    paletteer_d("ggthemes::Classic_10_Medium"),
    paletteer_dynamic("cartography::pastel.pal", 20)
  )

  get_color <- function(idx = NULL, is_light = F) {
    if (is_light) {
      cl <- light_color
    } else {
      cl <- chart_color
    }

    max_idx <- length(cl)

    if (is.null(idx))

      idx <- sample.int(max_idx, 1)

    idx <- abs(idx)

    idx <- idx %% max_idx

    idx[idx==0] <- max_idx

    return(cl[idx])
  }

  map_color <-
    colorRampPalette(c("darkgreen", "gold", "red3", "blue3"))

  emission_color <- colorRampPalette(c("darkgreen", "grey90", "red3"))

  emission_color_min <- colorRampPalette(c("darkgreen", "grey90"))

  emission_color_max <- colorRampPalette(c("grey90", "red3"))

  options(reactable.theme = reactableTheme(
    style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em")
  ))

  f_number <- function(v, unit = "", digits = 1) {
    paste(format(
      v,
      digits = digits,
      big.mark = ",",
      scientific = F
    ), unit)
  }

  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }

  ## Sentinel zone id used for "All zones".

  all_id <- -999

  ## The changes tab can overlay SEVERAL zonation maps at once. Each map keeps

  ## its own zone ids and gets its own zone selector, and the selectors combine

  ## as an AND (see zone_filter_selection()), so there is no shared id space and

  ## no relabelling anywhere in the app.

  ##

  ## Normalise the zonation-map multi-select into a character vector of record

  ## ids. Older/echoed values may arrive as a list, so unlist first.

  parse_map_selection <- function(z) {
    if (is.null(z) || length(z) == 0)

      return(character(0))

    z <- unlist(z, use.names = FALSE)

    z <- as.character(z)

    z[!is.na(z) & nzchar(z)]
  }

  v <- reactiveValues(
    title = "",
    description = "",
    abacus_file = NULL,
    abacus_data = NULL,
    lc_list_df = NULL,
    lc_palette_set = "gyr",
    zone_list_df = NULL,
    zone_legends = list(),
    zone_palette_sets = list(),
    zone_catalog_count = NULL,
    lc_changes_df = NULL,
    cstock_list = NULL,
    abacus_baseline = NULL,
    selected_project_id = 1,
    selected_scenario_id = 1,
    n_iteration = 5,
    baseline_period = c(0, 1),
    period_year = NULL,
    map1_year = NULL,
    map2_year = NULL,
    map1_date = NULL,
    map2_date = NULL,
    map1_file = NULL,
    map2_file = NULL,
    mapz_file = NULL,
    map_catalog = list(),
    map_catalog_version = 0L,
    pending_map_upload = NULL,
    pending_map_to_remove = NULL,
    pending_map_to_edit = NULL,
    pending_year_edit = NULL,
    next_map_id = 1,
    map1_stars = NULL,
    map2_stars = NULL,
    mapz_stars = NULL,
    map1_sf = NULL,
    map2_sf = NULL,
    mapz_sf = NULL,
    map1_shp_file = NULL,
    map2_shp_file = NULL,
    mapz_shp_file = NULL,
    mapc_stars = NULL,
    map_ghg_stars = NULL,
    ghg_ready = FALSE,
    map_all_stars = NULL,
    map1_df = NULL,
    map2_df = NULL,
    mapz_df = NULL,
    c_emission_total = 0,
    c_sequestration_total = 0,
    other_emission_df = data.frame(),
    other_emission_display_df = data.frame(),
    other_emission_source = NULL,

    other_emission_last_source = "Source_1",
    other_emission_total = 0,
    other_sequestration_total = 0,
    all_emission_total = 0,
    all_sequestration_total = 0,
    scenario_list = list(),
    lc_trend_data = NULL,
    npv_list = NULL,
    map_pair = NULL,
    study_area_boundary = NULL,
    geo_location = NULL,
    map_boundaries = list()
  )

  lc_legend_pal = NULL

  zone_legend_pal = NULL
  emission_val <- reactiveValues(bl_sum = 0, sc_sum = 0)
  final_area <- reactiveValues(sc_lc_df = NULL, bl_lc_df = NULL)

  scenario_lc_colums <- c("lc_id", "color", "label", "description", "zone_id", "iteration_id", "c")

  generate_scenario_object <- function(id = 1, label = NULL, desc = "") {
    if (is.null(label)) label <- paste("Scenario", id)
    l <- list(label = label, desc = desc, abacus_scenario = list(scenario = list()))
    if (!is.null(v$lc_list_df)) {
      if (is.null(v$cstock_list)) {
        lc_df <- v$lc_list_df
        lc_df$c <- 0
      } else {
        lc_df <- merge(v$lc_list_df, v$cstock_list, by = "lc_id", all.x = TRUE)
      }
      lc_df$zone_id <- 0
      lc_df$iteration_id <- 0
      cols_present <- intersect(scenario_lc_colums, names(lc_df))
      l$abacus_scenario$scenario$landcover <- lc_df[, cols_present, drop = FALSE]
    }
    return(l)
  }

  active_scenario <- function() {
    scs <- isolate(v$scenario_list)
    if (is.null(scs) || length(scs) == 0) {
      return(NULL)
    }
    sc_id <- isolate(v$selected_scenario_id)
    if (is.null(sc_id) || is.na(sc_id) || sc_id < 1 || sc_id > length(scs)) {
      sc_id <- 1L
    }
    scs[[sc_id]]
  }

  update_scenario_select_options <- function() {
    sc_list <- v$scenario_list
    if (is.null(sc_list) || length(sc_list) == 0) {
      ch <- c("(No scenarios)" = "")
      cur_sel <- ""
    } else {
      ch <- setNames(
        as.character(seq_along(sc_list)),
        vapply(sc_list, function(x) x$label %||% paste("Scenario", x$id), character(1))
      )
      cur_sel <- if (!is.null(v$selected_scenario_id) && length(v$selected_scenario_id) > 0 && as.character(v$selected_scenario_id[1]) %in% unname(ch)) {
        as.character(v$selected_scenario_id[1])
      } else {
        unname(ch)[1]
      }
    }
    tryCatch({
      update_toolbar_input_select("sel_scenario", choices = ch, selected = cur_sel)
    }, error = function(e) {
      tryCatch({
        updateSelectInput(session, "sel_scenario", choices = ch, selected = cur_sel)
      }, error = function(e2) NULL)
    })
    tryCatch({
      updateSelectInput(session, "select_scenario", choices = ch, selected = cur_sel)
    }, error = function(e) NULL)
  }

  ### Conditional panel UI logic ###

  conditional_id <-
    c(
      "is_map1",
      "is_map2",
      "is_mapz",
      "is_lc_list",
      "is_matrix",
      "is_zone_list",
      "is_zone_available",
      "is_cstock_list",
      "is_lc_trend",
      "is_map_uploaded"
    )

  conditional_v <-
    c(
      "map1_stars",
      "map2_stars",
      "mapz_stars",
      "lc_list_df",
      "lc_changes_df",
      "zone_list_df",
      "zone_catalog_count",
      "cstock_list",
      "lc_trend_data",
      "map_pair"
    )

  ## Any land cover map is enough to show the emission UI: "other sources"

  ## emission factors are applied per land cover class, so they can be shown

  ## on a single map too (anything uploaded counts, not just Before/After).

  output$is_map_uploaded <- reactive({
    !is.null(v$map1_stars) || !is.null(v$map2_stars)
  })

  outputOptions(output, "is_map_uploaded", suspendWhenHidden = FALSE)

  mapply(function(id, val) {
    output[[id]] <- reactive({
      if (is.null(v[[val]]))

        return(FALSE)

      TRUE
    })

    outputOptions(output, id, suspendWhenHidden = FALSE)
  }, conditional_id, conditional_v)

  other_lc_id <- function(id) {
    oid <- ifelse(id == "map1", "map2", "map1")

    odf <- isolate(v[[paste0(oid, "_df")]])

    if (!is.null(odf))

      return(odf$id)

    return(NULL)
  }

  crs <- NULL

  map_progress <- new.env(parent = emptyenv())

  active_zone_id <- NULL

  previous_map_selection <- list(map1 = NULL, map2 = NULL, mapz = NULL)

  close_map_progress <- function(id) {
    progress <- map_progress[[id]]

    if (!is.null(progress)) {
      progress$close()

      map_progress[[id]] <- NULL
    }
  }

  map_choices <- function(records) {
    if (length(records) == 0)

      return(setNames(character(), character()))

    labels <- vapply(records, function(record) {
      if (identical(record$type, "Land cover map")) {
        lbl <- if (!is.null(record$label) && nzchar(record$label)) record$label else as.character(record$year)
        paste(lbl, record$name, sep = " - ")
      } else {
        paste(record$label, record$name, sep = " - ")
      }
    }, character(1))

    setNames(names(records), labels)
  }

  ## Same as map_choices(), but shows only the year/label for land cover maps.

  map_choices_year <- function(records) {
    if (length(records) == 0)

      return(setNames(character(), character()))

    labels <- vapply(records, function(record) {
      if (identical(record$type, "Land cover map")) {
        if (!is.null(record$label) && nzchar(record$label)) as.character(record$label) else as.character(record$year)
      } else {
        paste(record$label, record$name, sep = " - ")
      }
    }, character(1))

    ## Different maps can share a year/label; disambiguate duplicates.

    if (anyDuplicated(labels) > 0)

      labels <- make.unique(labels)

    setNames(names(records), labels)
  }

  ## Zonation map choices: show only the user-given label, never the file name.

  zone_map_choices <- function(records) {
    if (length(records) == 0)

      return(setNames(character(), character()))

    labels <- vapply(records, function(record) {
      label <- record$label

      if (is.null(label) || !nzchar(label))

        label <- record$name

      as.character(label)
    }, character(1))

    ## Different zonation maps can share a label; disambiguate duplicates.

    if (anyDuplicated(labels) > 0)

      labels <- make.unique(labels)

    setNames(names(records), labels)
  }

  get_map_bbox <- function(m = NULL, sf_data = NULL) {
    if (!is.null(sf_data) && inherits(sf_data, "sf") && nrow(sf_data) > 0) {
      bb <- sf::st_bbox(sf_data)

      return(as.list(c(
        xmin = unname(bb[["xmin"]]),
        ymin = unname(bb[["ymin"]]),
        xmax = unname(bb[["xmax"]]),
        ymax = unname(bb[["ymax"]])
      )))
    }

    if (is.null(m)) return(NULL)

    dimensions <- st_dimensions(m)

    x_dimension <- dimensions[["x"]]

    y_dimension <- dimensions[["y"]]

    x_centers <- x_dimension$offset +
      (c(x_dimension$from, x_dimension$to) - 1) * x_dimension$delta

    y_centers <- y_dimension$offset +
      (c(y_dimension$from, y_dimension$to) - 1) * y_dimension$delta

    x_bounds <- range(x_centers) + c(-1, 1) * abs(x_dimension$delta) / 2

    y_bounds <- range(y_centers) + c(-1, 1) * abs(y_dimension$delta) / 2

    corners <- rbind(
      c(x_bounds[1], y_bounds[1]),
      c(x_bounds[1], y_bounds[2]),
      c(x_bounds[2], y_bounds[1]),
      c(x_bounds[2], y_bounds[2])
    )

    projected <- sf::sf_project(
      from = st_crs(m),
      to = "OGC:CRS84",
      pts = corners,
      keep = TRUE,
      authority_compliant = FALSE
    )

    if (any(!is.finite(projected)))

      stop("Unable to project map bounds to longitude/latitude.")

    as.list(c(
      xmin = min(projected[, 1]),
      ymin = min(projected[, 2]),
      xmax = max(projected[, 1]),
      ymax = max(projected[, 2])
    ))
  }

  ## Raster cell counts per id. stars-only: a proxy is materialised and counted

  ## directly, so a map never has to be converted to another raster class.

  get_raster_freq_df <- function(m = NULL, sf_data = NULL) {
    if (!is.null(m)) {
      if (inherits(m, "stars_proxy")) {
        m <- tryCatch(stars::st_as_stars(m), error = function(e) NULL)
      }

      if (inherits(m, "stars") && !inherits(m, "stars_proxy")) {
        df <- as.data.frame(table(m[[1]]))

        if (nrow(df) > 0) {
          colnames(df) <- c("id", "area")

          df$id <- as.numeric(as.character(df$id))

          return(df[!is.na(df$id) & df$area > 0, , drop = FALSE])
        }
      }
    }

    if (!is.null(sf_data) && "ID" %in% names(sf_data)) {
      u_ids <- sort(unique(sf_data$ID))

      u_ids <- u_ids[!is.na(u_ids)]

      return(data.frame(id = u_ids, area = rep(1, length(u_ids))))
    }

    data.frame(id = numeric(0), area = numeric(0))
  }

  map_preview_palette <- function(map = NULL, sf_data = NULL) {
    ids <- if (!is.null(sf_data) && "ID" %in% names(sf_data)) {
      sort(unique(sf_data$ID))
    } else if (!is.null(map)) {
      freq_df <- get_raster_freq_df(map)

      freq_df$id
    } else {
      numeric(0)
    }

    ids <- ids[!is.na(ids)]

    if (length(ids) == 0)

      return(NULL)

    colorFactor(get_color(seq_along(ids)), ids, na.color = NA)
  }

  ## Describe a raster's grid in the terms the overlay needs to line up:

  ## CRS, extent and cell resolution. Used to tell the user *why* an upload

  ## cannot be overlaid on the reference map, and whether warping can fix it.

  grid_signature <- function(m) {
    if (is.null(m) || !inherits(m, "stars"))

      return(NULL)

    dims <- st_dimensions(m)

    x_dim <- dims[["x"]]

    y_dim <- dims[["y"]]

    if (is.null(x_dim) || is.null(y_dim))

      return(NULL)

    ## Cell centers define the extent; converting to lon/lat lets two maps in

    ## different CRSs still be compared on a common footprint.

    x_centers <- x_dim$offset + (c(x_dim$from, x_dim$to) - 1) * x_dim$delta

    y_centers <- y_dim$offset + (c(y_dim$from, y_dim$to) - 1) * y_dim$delta

    x_range <- range(x_centers)

    y_range <- range(y_centers)

    list(
      crs = st_crs(m)$wkt,
      extent = c(xmin = x_range[1], xmax = x_range[2],
                 ymin = y_range[1], ymax = y_range[2]),
      nx = abs(x_dim$to - x_dim$from) + 1,
      ny = abs(y_dim$to - y_dim$from) + 1,
      dx = abs(x_dim$delta),
      dy = abs(y_dim$delta)
    )
  }

  ## Returns the area of one raster cell in hectares for a stars object whose

  ## CRS is projected (linear units in metres). Returns NA for geographic CRS

  ## (degrees) since a simple delta-product is meaningless there.

  pixel_ha <- function(m) {
    if (is.null(m) || !inherits(m, "stars")) return(NA_real_)

    crs_obj <- tryCatch(sf::st_crs(m), error = function(e) NA)

    if (is.na(crs_obj) || sf::st_is_longlat(crs_obj)) return(NA_real_)

    dims <- tryCatch(stars::st_dimensions(m), error = function(e) NULL)

    if (is.null(dims) || is.null(dims[["x"]]) || is.null(dims[["y"]])) return(NA_real_)

    dx <- abs(dims[["x"]]$delta)

    dy <- abs(dims[["y"]]$delta)

    if (is.na(dx) || is.na(dy) || dx <= 0 || dy <= 0) return(NA_real_)

    (dx * dy) / 10000   # m^2 -> ha
  }

  ## Compare a new map's grid against the reference map's grid. Returns NULL

  ## when they match, or a list of human-readable reasons when they differ.

  grid_mismatch_reasons <- function(map, reference) {
    ## No reference yet (this is the first map) means there is nothing to

    ## match against, so the map is accepted as the new reference grid.

    if (is.null(reference))

      return(NULL)

    sig_map <- grid_signature(map)

    sig_ref <- grid_signature(reference)

    if (is.null(sig_map) || is.null(sig_ref))

      return("the map grid could not be determined")

    reasons <- character(0)

    if (!identical(sig_map$crs, sig_ref$crs))

      reasons <- c(reasons, "a different coordinate reference system (CRS)")

    if (sig_map$nx != sig_ref$nx || sig_map$ny != sig_ref$ny)

      reasons <- c(reasons, sprintf(
        "different raster dimensions (%d x %d vs %d x %d)",
        sig_map$nx, sig_map$ny, sig_ref$nx, sig_ref$ny))

    tol <- 1e-6 * max(1, diff(sig_ref$extent)[c(1, 3)])

    if (max(abs(sig_map$extent - sig_ref$extent)) > tol)

      reasons <- c(reasons, "a different spatial extent")

    if (abs(sig_map$dx - sig_ref$dx) > tol || abs(sig_map$dy - sig_ref$dy) > tol)

      reasons <- c(reasons, "a different cell resolution")

    if (length(reasons) == 0) NULL else reasons
  }

  ## Short, human-readable name for a raster's CRS, for use in messages.

  ## st_crs()$input holds the string the user's own file carried (e.g. an EPSG

  ## code or a proj4 string), which is far more useful in a dialog than the WKT.

  crs_label <- function(m) {
    if (is.null(m) || !inherits(m, "stars")) return("none")

    cr <- tryCatch(st_crs(m), error = function(e) NA)

    if (is.na(cr)) return("none")

    nm <- cr$input

    if (is.null(nm) || !nzchar(nm)) nm <- cr$Name

    if (is.null(nm) || !nzchar(nm)) nm <- "an unnamed CRS"

    nm
  }

  ## Longitude/latitude footprint of a raster, as a bbox in degrees. Handles

  ## any CRS (projected or geographic) by projecting the corner cell centres to

  ## OGC:CRS84. Returns NULL when the footprint cannot be determined.

  map_lonlat_bbox <- function(m) {
    if (is.null(m) || !inherits(m, "stars"))

      return(NULL)

    tryCatch(get_map_bbox(m = m), error = function(e) NULL)
  }

  ## How much two footprints must overlap before we accept them as covering the

  ## same location. A genuine time series re-render shares an almost identical

  ## footprint; a map of somewhere else has none (or a sliver). Overlap is

  ## measured against the *smaller* box so that a small map sitting fully inside

  ## a larger one is not reported as mislocated.

  min_bbox_overlap_fraction <- 0.5

  ## Check that a newly uploaded map covers the same geographic area as the

  ## reference map. Returns NULL when the location is acceptable, or a

  ## human-readable reason when the map is somewhere else entirely.

  ##

  ## This is deliberately independent of grid_mismatch_reasons(): a map can be

  ## warped onto the reference grid technically (same CRS, warp succeeds) while

  ## still describing a totally different place. Warping would then happily

  ## produce a layer with no spatial relationship to the reference, so such an

  ## upload must be refused outright rather than offered for alignment.

  location_mismatch_reason <- function(map, reference) {
    if (is.null(reference))

      return(NULL)

    bb_map <- map_lonlat_bbox(map)

    bb_ref <- map_lonlat_bbox(reference)

    if (is.null(bb_map) || is.null(bb_ref))

      return(NULL)  # Cannot determine: let the grid check decide.

    inter_x <- min(bb_map$xmax, bb_ref$xmax) - max(bb_map$xmin, bb_ref$xmin)

    inter_y <- min(bb_map$ymax, bb_ref$ymax) - max(bb_map$ymin, bb_ref$ymin)

    if (inter_x <= 0 || inter_y <= 0)

      return("it does not overlap the existing map at all")

    area_map <- (bb_map$xmax - bb_map$xmin) * (bb_map$ymax - bb_map$ymin)

    area_ref <- (bb_ref$xmax - bb_ref$xmin) * (bb_ref$ymax - bb_ref$ymin)

    smaller <- min(area_map, area_ref)

    if (!is.finite(smaller) || smaller <= 0)

      return(NULL)

    if ((inter_x * inter_y) / smaller < min_bbox_overlap_fraction) {
      return(sprintf(
        "it only overlaps about %d%% of the existing map's area",
        round(100 * (inter_x * inter_y) / smaller)
      ))
    }

    NULL
  }

  ## How much of the reference map a warped map must actually cover for its

  ## cells to be usable without asking. Anything below this is still allowed,
  ## but the upload dialog reports the shortfall and asks the user to confirm.

  min_covered_fraction <- 1

  ## Cells of `map` that fall outside `reference`'s footprint.

  ##

  ## Only meaningful once the two share a grid, so the dimensions are compared

  ## first; a differing grid returns NA rather than a made-up count. The values

  ## are read as vectors, so a proxy is materialised first - on a proxy, [[1]]

  ## is the file path, not the values.

  uncovered_cells <- function(map, reference) {
    if (is.null(map) || is.null(reference))

      return(NA_integer_)

    if (!inherits(map, "stars") || !inherits(reference, "stars"))

      return(NA_integer_)

    if (!identical(st_dimensions(map), st_dimensions(reference)))

      return(NA_integer_)

    tryCatch({
      m <- map

      if (inherits(m, "stars_proxy"))

        m <- stars::st_as_stars(m)

      r <- reference

      if (inherits(r, "stars_proxy"))

        r <- stars::st_as_stars(r)

      mv <- as.vector(m[[1]])

      rv <- as.vector(r[[1]])

      if (length(mv) != length(rv))

        return(NA_integer_)

      ## A cell counts as uncovered when the reference has a value there but the

      ## map does not. Cells the reference itself leaves blank are not required

      ## to be covered, so a map clipped to the reference's data footprint - and

      ## not merely to its bounding box - is accepted.

      sum(!is.na(rv) & is.na(mv))
    }, error = function(e) NA_integer_)
  }

  ## How much of the reference map's footprint a warped map actually covers.

  ##

  ## Returns a fraction in [0, 1] - the share of the reference's non-NA cells

  ## that also have a value in `map` - or NA when it cannot be measured (the

  ## grids differ, or the values cannot be read). This is reported to the user

  ## rather than enforced: a map covering less than the whole reference is

  ## offered for a decision instead of being refused outright.

  covered_fraction <- function(map, reference) {
    missing_cells <- uncovered_cells(map, reference)

    if (is.na(missing_cells))

      return(NA_real_)

    total <- tryCatch({
      r <- reference

      if (inherits(r, "stars_proxy"))

        r <- stars::st_as_stars(r)

      rv <- as.vector(r[[1]])

      sum(!is.na(rv))
    }, error = function(e) NA_integer_)

    if (is.na(total) || total <= 0)

      return(NA_real_)

    1 - missing_cells / total
  }

  ## A sentence describing how much of the reference `map` covers, for the

  ## upload dialog. NULL when the map covers it completely (or coverage cannot

  ## be measured), so the caller only warns when there is a real shortfall.

  coverage_shortfall_reason <- function(map, reference) {
    fraction <- covered_fraction(map, reference)

    if (is.na(fraction))

      return(NULL)

    if (fraction >= min_covered_fraction - 1e-9)

      return(NULL)

    sprintf(
      "after alignment it covers about %d%% of the first map's cells",
      floor(100 * fraction)
    )
  }

  ## Detects when the aligned map extends BEYOND the reference's non-NA pixel

  ## footprint, i.e. it has data in cells that the reference leaves blank.

  ## Returns a human-readable sentence, or NULL when the map fits within the

  ## reference footprint (no excess). Only called after the grids already match

  ## (post-warp), so an identical-dimensions check is all that is needed.

  excess_coverage_reason <- function(map, reference) {
    if (is.null(map) || is.null(reference)) return(NULL)

    if (!inherits(map, "stars") || !inherits(reference, "stars")) return(NULL)

    if (!identical(st_dimensions(map), st_dimensions(reference))) return(NULL)

    tryCatch({
      m <- if (inherits(map, "stars_proxy")) stars::st_as_stars(map) else map

      r <- if (inherits(reference, "stars_proxy")) stars::st_as_stars(reference) else reference

      mv <- as.vector(m[[1]])

      rv <- as.vector(r[[1]])

      if (length(mv) != length(rv)) return(NULL)

      ## Cells where the uploaded map has a value but the reference does not.

      excess <- sum(!is.na(mv) & is.na(rv))

      if (excess == 0) return(NULL)

      total_ref <- sum(!is.na(rv))

      pct <- round(100 * excess / max(total_ref, 1))

      sprintf(
        "it extends beyond the reference map's data footprint by about %d%% extra cells",
        pct
      )
    }, error = function(e) NULL)
  }

  ## Crop an aligned map to the reference's exact non-NA pixel footprint.

  ## Cells the reference leaves blank are set to NA in the incoming map, then

  ## the bounding box is trimmed to the tight rectangle around the survivors.

  ## Both inputs must already share the same grid (post-warp).

  crop_to_reference_footprint <- function(map, reference) {
    if (is.null(map) || is.null(reference)) return(map)

    tryCatch({
      m <- if (inherits(map, "stars_proxy")) stars::st_as_stars(map) else map

      r <- if (inherits(reference, "stars_proxy")) stars::st_as_stars(reference) else reference

      if (any(dim(m) == 1)) m <- adrop(m)

      if (any(dim(r) == 1)) r <- adrop(r)

      m <- m[1]; r <- r[1]

      ## Blank every cell the reference does not cover

      m[[1]][is.na(r[[1]])] <- NA

      ## Trim to the tight bounding box of remaining data cells

      valid <- !is.na(m[[1]])

      if (!any(valid)) return(map)

      has_x <- which(rowSums(valid) > 0)

      has_y <- which(colSums(valid) > 0)

      m[, min(has_x):max(has_x), min(has_y):max(has_y)]
    }, error = function(e) map)
  }

  ## Overlap of two bounding boxes given as xmin/xmax/ymin/ymax vectors.

  ##

  ## Both maps must already share a CRS, so the overlap of their bounding boxes

  ## is the area they genuinely have in common. Returns the shared bbox or NULL

  ## when the two do not overlap. Used to crop every map down to the area they

  ## all cover, so overlays line up cell for cell.

  intersect_extents <- function(a, b) {
    if (is.null(a) || is.null(b))

      return(NULL)

    xmin <- max(a[["xmin"]], b[["xmin"]])

    xmax <- min(a[["xmax"]], b[["xmax"]])

    ymin <- max(a[["ymin"]], b[["ymin"]])

    ymax <- min(a[["ymax"]], b[["ymax"]])

    if (xmin >= xmax || ymin >= ymax)

      return(NULL)

    c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }

  ## Bounding box of a stars map as an xmin/xmax/ymin/ymax vector, or NULL when

  ## the extent cannot be read. `crs` supplies the CRS to stamp on the bbox when

  ## the map itself carries none.

  stars_extent <- function(map, crs = NULL) {
    if (is.null(map) || !inherits(map, "stars"))

      return(NULL)

    bb <- tryCatch(sf::st_bbox(map), error = function(e) NULL)

    if (is.null(bb))

      return(NULL)

    c(xmin = bb[["xmin"]], xmax = bb[["xmax"]],
      ymin = bb[["ymin"]], ymax = bb[["ymax"]])
  }

  ## Crop a stars map to a bbox in its own CRS, keeping the cells that fall

  ## inside it. Categorical values are preserved as-is: this only drops whole

  ## cells that lie outside the shared area, it never resamples.

  crop_stars_to_extent <- function(map, extent) {
    if (is.null(map) || is.null(extent) || !inherits(map, "stars"))

      return(map)

    box <- sf::st_bbox(
      c(xmin = extent[["xmin"]], ymin = extent[["ymin"]],
        xmax = extent[["xmax"]], ymax = extent[["ymax"]]),
      crs = sf::st_crs(map)
    )

    cropped <- tryCatch(suppressWarnings(st_crop(map, box)), error = function(e) NULL)

    ## st_crop() can return a proxy for a proxy input; materialise it so the

    ## record always carries values, exactly as finalize_map_registration expects.

    if (!is.null(cropped) && inherits(cropped, "stars_proxy"))

      cropped <- tryCatch(stars::st_as_stars(cropped), error = function(e) NULL)

    if (is.null(cropped)) map else cropped
  }

  ## The bbox shared by every map currently in the catalog, i.e. the area all

  ## existing layers have in common. NULL when the catalog is empty or a map's

  ## extent cannot be read. Used to tell the user - before they agree - the

  ## exact area an incoming map will leave after the crop.

  catalog_common_extent <- function() {
    records <- isolate(v$map_catalog)

    extent <- NULL

    for (record in records) {
      if (is.null(record$stars))

        next

      this <- stars_extent(record$stars)

      if (is.null(this))

        next

      extent <- if (is.null(extent)) this else intersect_extents(extent, this)

      if (is.null(extent))

        break
    }

    extent
  }

  ## Crop the pending map together with every already-registered map down to

  ## the area they all share, so every layer is 100% aligned and matches when

  ## overlaid. Each existing record's stars and frequency table are replaced in

  ## place and its preview re-rendered. Returns the cropped pending map, placed

  ## on the cropped reference grid so it lines up cell for cell with the maps

  ## already in the catalog.

  crop_catalog_to_intersection <- function(new_map) {
    records <- isolate(v$map_catalog)

    if (length(records) == 0)

      return(new_map)

    m <- new_map

    if (inherits(m, "stars_proxy"))

      m <- tryCatch(stars::st_as_stars(m), error = function(e) NULL)

    if (is.null(m) || !inherits(m, "stars"))

      return(new_map)

    m <- m[1]

    if (any(dim(m) == 1)) m <- adrop(m)

    ## Compute the intersection mask across the incoming map and all existing maps.

    ## All maps are on the reference grid (aligned cell-by-cell).

    in_intersection <- !is.na(m[[1]])

    for (record_id in names(records)) {
      record <- records[[record_id]]

      if (is.null(record$stars)) next

      s <- if (inherits(record$stars, "stars_proxy")) stars::st_as_stars(record$stars) else record$stars

      if (any(dim(s) == 1)) s <- adrop(s)

      s <- s[1]

      if (!identical(st_dimensions(s), st_dimensions(m))) {
        s <- tryCatch(warp_stars_to_grid(s, m), error = function(e) NULL)
      }

      if (!is.null(s)) {
        in_intersection <- in_intersection & (!is.na(s[[1]]))
      }
    }

    ## If there is no overlap at all, cannot crop down to empty.

    if (!any(in_intersection))

      return(new_map)

    ## Bounding index range of the valid intersection cells

    has_x <- which(rowSums(in_intersection) > 0)

    has_y <- which(colSums(in_intersection) > 0)

    x_range <- min(has_x):max(has_x)

    y_range <- min(has_y):max(has_y)

    changed <- FALSE

    for (record_id in names(records)) {
      record <- records[[record_id]]

      if (is.null(record$stars)) next

      s <- if (inherits(record$stars, "stars_proxy")) stars::st_as_stars(record$stars) else record$stars

      if (any(dim(s) == 1)) s <- adrop(s)

      s <- s[1]

      if (!identical(st_dimensions(s), st_dimensions(m))) {
        s <- tryCatch(warp_stars_to_grid(s, m), error = function(e) NULL)
      }

      if (is.null(s)) next

      ## Cells outside the intersection are removed (set to NA)

      s[[1]][!in_intersection] <- NA

      ## Crop grid dimensions to the bounding box of the intersection

      cropped <- s[, x_range, y_range]

      record$stars <- cropped

      record$freq_df <- get_raster_freq_df(cropped, record$sf)

      if (!is.null(record$zone_df) && nrow(record$zone_df) > 0) {
        z_freq <- record$freq_df

        idx_match <- match(record$zone_df$zone_id, z_freq$id)

        record$zone_df$area <- ifelse(!is.na(idx_match), z_freq$area[idx_match], 0)

        v$zone_legends[[record_id]] <- record$zone_df

        if (identical(isolate(input$analysis_zone_map), record_id) || is.null(isolate(input$analysis_zone_map)) || !nzchar(isolate(input$analysis_zone_map))) {
          v$zone_list_df <- record$zone_df
        }
      }

      if (!is.null(record$sf) && inherits(record$sf, "sf")) {
        box <- tryCatch(sf::st_bbox(cropped), error = function(e) NULL)

        if (!is.null(box)) {
          sf_wgs84 <- tryCatch(suppressWarnings(sf::st_crop(sf::st_transform(record$sf, 4326), box)),
                               error = function(e) NULL)

          if (!is.null(sf_wgs84) && nrow(sf_wgs84) > 0)

            record$sf <- sf_wgs84
        }
      }

      records[[record_id]] <- record

      changed <- TRUE
    }

    v$map_catalog <- records

    if (changed) {
      ## When maps are cropped, update the study area boundary once to reflect the new shared intersection.

      ## Only a land cover map defines the study area; a zonation map does not.

      first_cropped <- Find(
        function(r) !is.null(r$stars) && identical(r$type, "Land cover map"),
        records
      )

      if (!is.null(first_cropped)) {
        v$study_area_boundary <- tryCatch(
          create_map_boundary(
            map     = first_cropped$stars,
            sf_data = first_cropped$sf,
            name    = if (!is.null(first_cropped$name)) first_cropped$name else "Study Area",
            type    = first_cropped$type,
            year    = if (!is.null(first_cropped$year)) first_cropped$year else ""
          ),
          error = function(e) NULL
        )

        update_study_area_location(v$study_area_boundary)
      }

      v$map_catalog_version <- v$map_catalog_version + 1L

      ## Always push the cropped stars back into the analysis-map reactives so

      ## that (a) the analysis tab immediately shows the trimmed maps and (b)

      ## first_map_reference_stars() returns the new grid for future warps.

      ## If no analysis map is explicitly selected, fall back to the first

      ## catalog record so the reference is never left stale after a crop.

      sel1 <- isolate(input$analysis_map1)

      first_rec <- NULL

      if (nzchar(sel1) && !is.null(records[[sel1]])) {
        v$map1_stars <- records[[sel1]]$stars
      } else {
        first_rec <- Find(function(r) !is.null(r$stars) && identical(r$type, "Land cover map"), records)

        if (!is.null(first_rec)) v$map1_stars <- first_rec$stars
      }

      sel2 <- isolate(input$analysis_map2)

      if (nzchar(sel2) && !is.null(records[[sel2]]) && !identical(sel2, sel1)) {
        v$map2_stars <- records[[sel2]]$stars
      } else {
        lc_recs <- Filter(function(r) !is.null(r$stars) && identical(r$type, "Land cover map"), records)

        first_id <- if (nzchar(sel1)) sel1 else if (!is.null(first_rec)) first_rec$id else NULL

        candidates <- Filter(function(r) !identical(r$id, first_id), lc_recs)

        if (length(candidates) > 0) {
          v$map2_stars <- candidates[[length(candidates)]]$stars
        }
      }

      selz <- isolate(input$analysis_zone_map)

      if (nzchar(selz) && !is.null(records[[selz]])) {
        v$mapz_stars <- records[[selz]]$stars
      } else {
        zone_rec <- Find(function(r) !is.null(r$stars) && identical(r$type, "Zonation map"), records)

        if (!is.null(zone_rec)) v$mapz_stars <- zone_rec$stars
      }
    }

    ## Crop the incoming map as well

    m[[1]][!in_intersection] <- NA

    cropped_new <- m[, x_range, y_range]

    cropped_new
  }

  ## A plot showing, cell by cell, how an incoming map's footprint compares to

  ## the maps already registered, and which cells survive the crop. Called from

  ## the confirmation dialog so the user sees the real map shape - not just a

  ## box - before they agree.

  ##

  ## The incoming map is first put on the reference grid (the same st_warp()

  ## path the app aligns with), so the two can be compared one cell at a time.

  ## Every cell is then painted with one of four meanings:

  ##   green  - reference only      (cropped away)

  ##   blue   - uploaded map only   (cropped away)

  ##   red    - both cover it (the intersection, kept)

  ##   grey   - neither has data

  ## This mirrors the earlier cell-based preview that the box plot replaced;

  ## the box plot lost the map's actual shape and read as abstract rectangles.

  plot_intersection_preview <- function(new_map) {
    reference <- first_map_reference_stars()

    if (is.null(reference)) {
      plot.new()

      title("No reference map available")

      return(NULL)
    }

    ## Materialise both sides: [[1]] on a proxy is the FILE PATH, not values.

    r <- reference

    if (inherits(r, "stars_proxy"))

      r <- tryCatch(stars::st_as_stars(r), error = function(e) NULL)

    if (is.null(r) || !inherits(r, "stars")) {
      plot.new()

      title("Reference map unavailable")

      return(NULL)
    }

    ## Put the incoming map on the reference grid so cells line up. Same CRS is

    ## required; when it already matches, no warp is needed.

    m <- new_map

    if (inherits(m, "stars_proxy"))

      m <- tryCatch(stars::st_as_stars(m), error = function(e) NULL)

    if (is.null(m) || !inherits(m, "stars")) {
      plot.new()

      title("Uploaded map unavailable")

      return(NULL)
    }

    r <- r[1]

    m <- m[1]

    if (any(dim(r) == 1)) r <- adrop(r)

    if (any(dim(m) == 1)) m <- adrop(m)

    if (!identical(st_dimensions(m), st_dimensions(r))) {
      m <- tryCatch(warp_stars_to_grid(m, r), error = function(e) NULL)

      if (is.null(m) || !identical(st_dimensions(m), st_dimensions(r))) {
        m <- tryCatch(align_stars_to_reference(new_map, r),
                      error = function(e) NULL)
      }
    }

    if (is.null(m) || !identical(st_dimensions(m), st_dimensions(r))) {
      plot.new()

      title("Map cannot be compared on this grid")

      return(NULL)
    }

    ## Warping can leave a stars still holding a proxy; materialise the values.

    if (inherits(m, "stars_proxy"))

      m <- tryCatch(stars::st_as_stars(m), error = function(e) NULL)

    if (is.null(m)) {
      plot.new()

      title("Uploaded map unavailable")

      return(NULL)
    }

    if (any(dim(m) == 1)) m <- adrop(m)

    ## Map operations using stars library only:

    ##   1 = reference only      (cropped away)

    ##   2 = uploaded map only   (cropped away)

    ##   3 = both cover it       (intersection, kept)

    ##   4 = neither has data

    mc <- r[1]

    r_has <- !is.na(r[1])

    m_has <- !is.na(m[1])

    mc[r_has] <- 1L

    mc[r_has & m_has] <- 3L

    mc[!r_has & m_has] <- 2L

    mc[!r_has & !m_has] <- 4L

    par(mar = c(0, 0, 1.2, 0))

    image(mc,
          col = c("#2e7d32", "#1e64c8", "#c62828", "#e0e0e0"),
          breaks = c(0.5, 1.5, 2.5, 3.5, 4.5),
          axes = FALSE, asp = 1, useRaster = TRUE)

    box(col = "#bdbdbd")

    title("Map intersection", cex.main = 1.1, col.main = "#333333")

    legend("topright",
           legend = c("Existing map only (cropped)",
                      "Uploaded map only (cropped)",
                      "Intersection (kept)",
                      "No data"),
           fill = c("#2e7d32", "#1e64c8", "#c62828", "#e0e0e0"),
           border = "#cccccc", bty = "n", cex = 0.8)

    invisible(TRUE)
  }

  ## The first uploaded map defines the grid every later map must match. Its

  ## raster is the reference used to detect mismatches and to warp onto.

  first_map_reference_stars <- function() {
    records <- isolate(v$map_catalog)

    for (record in records) {
      if (!is.null(record$stars)) return(record$stars)
    }

    NULL
  }

  zone_table_def <- c("zone_id", "color", "label", "description")

  zone_column <- data.frame(
    title = c("ZONE_ID", "Color", "Zone", "Description"),
    type = c("numeric", "color", "text", "text"),
    render = c(NA, "square", NA, NA),
    align = c("right", "center", "left", "left"),
    width = c(80, 70, 150, 150),
    readOnly = c(TRUE, FALSE, FALSE, FALSE)
  )

  registered_zone_handlers <- character(0)

  register_zone_map_handlers <- function(record_id) {
    if (record_id %in% registered_zone_handlers) return()

    registered_zone_handlers <<- c(registered_zone_handlers, record_id)

    output[[paste0("zone_legend_table_", record_id)]] <- renderExcel({
      df <- v$zone_legends[[record_id]]

      if (is.null(df) || nrow(df) == 0) return()

      excelTable(
        data = df[zone_table_def],
        ## The zonation card spans the full grid row, so the sidebar now has

        ## room for readable columns. All four stay because the edit handler

        ## maps them onto zone_table_def positionally (names(df_input) <-
        ## zone_table_def), and the colour column is how a zone colour is

        ## edited. Its header must be a real word: an empty title renders as a

        ## stray one-letter glyph, and "Color" truncated to "Col" read like an

        ## abbreviation of ZONE_ID.

        columns = transform(zone_column,
                            title = c("ID", "Color", "Zone", "Desc"),
                            width = c(40, 52, 90, 110)),
        tableOverflow = TRUE,
        wordWrap = TRUE,
        tableWidth = "100%",
        tableHeight = "100%",
        allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE,
        allowInsertColumn = FALSE,
        allowDeleteRow = FALSE,
        allowInsertRow = FALSE,
        rowDrag = FALSE,
        minDimensions = c(NA, 1),
        autoIncrement = TRUE,
        csvFileName = paste0("zonation_table_", record_id),
        includeHeadersOnDownload = TRUE
      )
    })

    observeEvent(input[[paste0("zone_legend_table_", record_id)]], {
      inp <- input[[paste0("zone_legend_table_", record_id)]]

      req(inp)

      df_old <- isolate(v$zone_legends[[record_id]])

      df_input <- tryCatch(excel_to_R(inp), error = function(e) NULL)

      if (is.null(df_input) || nrow(df_input) == 0) return()

      names(df_input) <- zone_table_def

      df_input <- transform(df_input, zone_id = as.numeric(zone_id))

      ## `area` is not shown in the table but is still needed downstream;

      ## carry it over from the previous data frame by zone_id.

      if (!is.null(df_old) && "area" %in% colnames(df_old)) {
        df_input$area <- df_old$area[match(df_input$zone_id, df_old$zone_id)]
      }

      v$zone_legends[[record_id]] <- df_input

      # Check if color changed

      newcol <- setdiff(df_old$color, df_input$color)

      if (length(newcol) > 0) {
        new_pal <- colorFactor(df_input$color, df_input$zone_id, na.color = NA)

        if (identical(record_id, input$analysis_zone_map)) {
          v$zone_list_df <- df_input

          zone_legend_pal <<- new_pal

          refresh_lc_matrix()
        }
      }

      # Check if label changed

      newlbl <- setdiff(df_old$label, df_input$label)

      if (length(newlbl) > 0) {
        if (identical(record_id, input$analysis_zone_map)) {
          v$zone_list_df <- df_input
        }
      }
    })

    observeEvent(input[[paste0("zone_palette_set_", record_id)]], {
      new_set <- input[[paste0("zone_palette_set_", record_id)]]

      req(new_set)

      current_set <- v$zone_palette_sets[[record_id]]

      if (!identical(new_set, current_set)) {
        v$zone_palette_sets[[record_id]] <- new_set

        df <- v$zone_legends[[record_id]]

        if (!is.null(df) && nrow(df) > 0) {
          new_colors <- get_legend_set_colors(new_set, nrow(df))

          df$color <- new_colors

          v$zone_legends[[record_id]] <- df

          new_pal <- colorFactor(df$color, df$zone_id, na.color = NA)

          if (identical(record_id, input$analysis_zone_map)) {
            v$zone_list_df <- df

            zone_legend_pal <<- new_pal

            refresh_lc_matrix()
          }
        }
      }
    }, ignoreInit = TRUE)

    # CSV import handler for this zone map

    observeEvent(input[[paste0("zone_legend_import_", record_id)]], {
      f <- input[[paste0("zone_legend_import_", record_id)]]

      req(f)

      df_in <- tryCatch(read.csv(f$datapath, stringsAsFactors = FALSE), error = function(e) NULL)

      if (is.null(df_in) || nrow(df_in) == 0 || ncol(df_in) < 2) {
        showNotification("Could not read CSV or file is empty.", type = "error")

        return()
      }

      colnames(df_in) <- tolower(trimws(colnames(df_in)))

      if (!("zone_id" %in% colnames(df_in))) {
        showNotification("CSV must have a 'zone_id' column.", type = "error")

        return()
      }

      df_in$zone_id <- suppressWarnings(as.integer(df_in$zone_id))

      df_in <- df_in[!is.na(df_in$zone_id), , drop = FALSE]

      df <- v$zone_legends[[record_id]]

      if (is.null(df)) return()

      for (col in c("color", "label", "description")) {
        if (col %in% colnames(df_in)) {
          matched <- match(df$zone_id, df_in$zone_id)

          valid <- !is.na(matched)

          if (col == "color") {
            vals <- df_in[[col]][matched[valid]]

            ok <- grepl("^#[0-9A-Fa-f]{6}$", vals)

            df[[col]][valid][ok] <- vals[ok]
          } else {
            df[[col]][valid] <- as.character(df_in[[col]][matched[valid]])
          }
        }
      }

      v$zone_legends[[record_id]] <- df

      new_pal <- colorFactor(df$color, df$zone_id, na.color = NA)

      if (identical(record_id, input$analysis_zone_map)) {
        v$zone_list_df <- df

        zone_legend_pal <<- new_pal

        refresh_lc_matrix()
      }

      showNotification("Legend imported successfully.", type = "message")
    })
  }

  build_map_card_ui <- function(record) {
    output_id <- paste0("uploaded_map_preview_", record$id)

    card_id <- paste0("uploaded_", record$id, "_card")

    full_screen <- TRUE

    is_landcover <- identical(record$type, "Land cover map")

    title <- if (is_landcover) {
      lbl <- if (!is.null(record$label) && nzchar(record$label)) record$label else as.character(record$year)
      paste0("Land cover [", lbl, "]")
    } else {
      paste0("Zone [", record$label, "]")
    }

    output[[paste0("map_card_title_", record$id)]] <- renderUI({
      records <- v$map_catalog
      rec <- records[[record$id]]
      if (is.null(rec)) return(NULL)
      is_lc <- identical(rec$type, "Land cover map")
      lbl <- rec$label
      if (is.null(lbl) || !nzchar(lbl)) {
        lbl <- if (is_lc) as.character(rec$year %||% "") else rec$name
      }
      if (is_lc) {
        paste0("Land cover [", lbl, "]")
      } else {
        paste0("Zone [", lbl, "]")
      }
    })

    delete_ui <- actionButton(
      inputId = paste0("map_remove_btn_", record$id),
      label = NULL,
      icon = icon("trash-can"),
      class = "btn btn-sm btn-link p-1 text-decoration-none btn-map-remove",
      title = "Remove this map"
    )

    edit_ui <- actionButton(
      inputId = paste0("map_edit_btn_", record$id),
      label = NULL,
      icon = icon("pen"),
      class = "btn btn-sm btn-link p-1 text-decoration-none btn-map-edit",
      title = "Edit map label"
    )

    extra_ui <- NULL

    menu_ui  <- NULL

    if (!is_landcover) {
      current_set <- if (!is.null(v$zone_palette_sets[[record$id]])) v$zone_palette_sets[[record$id]] else "default"

      extra_ui <- excelOutput(paste0("zone_legend_table_", record$id), width = "100%", height = "100%")

      menu_ui <- popover(
        trigger = toolbar_input_button(
          id    = paste0("zone_menu_btn_", record$id),
          label = "Options",
          icon  = shiny::icon("ellipsis-vertical"),
          tooltip = FALSE
        ),
        title = "Zone map options",
        actionButton(
          inputId = paste0("zone_menu_edit_", record$id),
          label = "Edit map label",
          icon = icon("pen"),
          class = "btn btn-outline-secondary btn-sm w-100 mb-2"
        ),
        tags$strong("Color set", style = "font-size: 0.85rem;"),
        radioButtons(
          inputId  = paste0("zone_palette_set_", record$id),
          label    = NULL,
          choices  = legend_set_choices,
          selected = current_set
        ),
        tags$hr(style = "margin: 6px 0;"),
        tags$strong("Import legend from CSV", style = "font-size: 0.85rem;"),
        tags$small(class = "text-muted d-block mb-1",
          "Columns: zone_id, color, label, description"),
        fileInput(
          inputId     = paste0("zone_legend_import_", record$id),
          label       = NULL,
          accept      = ".csv",
          buttonLabel = tagList(icon("upload"), " Import")
        ),
        tags$hr(style = "margin: 6px 0;"),
        actionButton(
          inputId = paste0("zone_menu_remove_", record$id),
          label = "Remove map",
          icon = icon("trash-can"),
          class = "btn btn-outline-danger btn-sm w-100"
        ),
        id        = paste0("zone_menu_pop_", record$id),
        placement = "bottom"
      )
    }

    download_ui <- card_download_button(
      output_id = paste0("download_map_", record$id),
      title = paste0("Download ", record$name, " (GeoTIFF)")
    )

    uploaded_map_card(
      title,
      record$name,
      output_id,
      card_id = card_id,
      full_screen = full_screen,
      extra_ui = extra_ui,
      menu_ui  = menu_ui,
      delete_ui = delete_ui,
      download_ui = download_ui,
      edit_ui = edit_ui
    )
  }

  register_map_preview <- function(record_id) {
    output[[paste0("download_map_", record_id)]] <- downloadHandler(
      filename = function() {
        rec <- isolate(v$map_catalog[[record_id]])
        fn <- if (!is.null(rec) && !is.null(rec$name) && nzchar(rec$name)) rec$name else paste0("map_", record_id, ".tif")
        if (!grepl("\\.tif$", fn, ignore.case = TRUE)) fn <- paste0(fn, ".tif")
        fn
      },
      content = function(file) {
        rec <- isolate(v$map_catalog[[record_id]])
        if (!is.null(rec) && !is.null(rec$stars)) {
          stars::write_stars(rec$stars, file)
        }
      }
    )

    output[[paste0("uploaded_map_preview_", record_id)]] <- renderPlot({
      v$map_catalog_version
      current_record <- isolate(v$map_catalog[[record_id]])
      req(current_record)

      is_zone <- identical(current_record$type, "Zonation map")
      stars_map <- current_record$stars
      if (is.null(stars_map)) return(NULL)

      palette <- if (is_zone && !is.null(v$zone_legends[[record_id]])) {
        zdf <- v$zone_legends[[record_id]]
        colorFactor(zdf$color, zdf$zone_id, na.color = NA)
      } else if (!is_zone && !is.null(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
        lcdf <- v$lc_list_df
        colorFactor(lcdf$color, lcdf$lc_id, na.color = NA)
      } else {
        colorFactor(legend_pal_default, 1:length(legend_pal_default), na.color = NA)
      }

      fs_id_bslib <- paste0("uploaded_", record_id, "_card_full_screen")
      fs_id_custom <- paste0("uploaded_", record_id, "_card_fullscreen")
      is_fs <- (!is.null(input[[fs_id_bslib]]) && isTRUE(input[[fs_id_bslib]])) ||
               (!is.null(input[[fs_id_custom]]) && isTRUE(input[[fs_id_custom]]))

      if (inherits(stars_map, "stars_proxy")) {
        stars_map <- tryCatch(stars::st_as_stars(stars_map), error = function(e) stars_map)
      }
      vals <- stars_map[[1]]
      ids <- sort(unique(as.vector(vals)))
      ids <- ids[!is.na(ids)]
      if (length(ids) > 0) {
        cols <- palette(ids)
        breaks <- c(ids - 0.5, max(ids) + 0.5)
        suppressMessages(plot(stars_map, col = cols, breaks = breaks, main = "", axes = FALSE, key.pos = NULL, reset = FALSE))
      } else {
        suppressMessages(plot(stars_map, main = "", axes = FALSE, key.pos = NULL, reset = FALSE))
      }
    })
  }

  ## Helper: identify which scenarios depend on a given map record
  get_scenarios_using_map <- function(record_id) {
    scenarios <- isolate(scenario_catalog())
    if (length(scenarios) == 0) return(integer(0))

    records <- isolate(v$map_catalog)
    rec <- records[[record_id]]
    if (is.null(rec)) return(integer(0))

    is_lc <- identical(rec$type, "Land cover map")

    # If removing this land cover map leaves fewer than 2 land cover maps,
    # all scenarios must be removed because scenarios require at least 2 LC maps.
    if (is_lc) {
      lc_left <- sum(vapply(records, function(r) {
        identical(r$type, "Land cover map") && !identical(r$id, record_id)
      }, logical(1)))
      if (lc_left < 2) {
        return(seq_along(scenarios))
      }
    }

    affected_indices <- integer(0)
    for (i in seq_along(scenarios)) {
      sc <- scenarios[[i]]
      uses_map <- FALSE

      if (is_lc) {
        rec_yr <- suppressWarnings(as.integer(rec$year))
        if (identical(sc$map1_id, record_id) || identical(sc$map2_id, record_id)) {
          uses_map <- TRUE
        } else if (!is.na(rec_yr) && (identical(as.integer(sc$map1_year %||% 0L), rec_yr) ||
                                       identical(as.integer(sc$map2_year %||% 0L), rec_yr))) {
          uses_map <- TRUE
        } else if (!is.na(rec_yr) && nzchar(sc$period %||% "") && grepl(paste0("\\b", rec_yr, "\\b"), sc$period)) {
          uses_map <- TRUE
        } else if (identical(record_id, isolate(input$analysis_map1)) || identical(record_id, isolate(input$analysis_map2))) {
          uses_map <- TRUE
        }
      } else {
        # Zonation map
        zm <- sc$zone_maps %||% sc$zone_map
        if (!is.null(zm) && record_id %in% zm) {
          uses_map <- TRUE
        }
      }

      if (uses_map) {
        affected_indices <- c(affected_indices, i)
      }
    }

    affected_indices
  }

  ## Helper: remove scenarios by catalog index, re-index remaining scenarios and update all state
  remove_scenarios_by_indices <- function(remove_indices) {
    scenarios <- isolate(scenario_catalog())
    if (length(scenarios) == 0 || length(remove_indices) == 0) return()

    remove_indices <- sort(unique(remove_indices))
    keep_indices <- setdiff(seq_along(scenarios), remove_indices)

    if (length(keep_indices) == 0) {
      scenario_catalog(list())
      scenario_overrides(list())
      v$scenario_list <- list()
      selected_scenario_index("")
      tryCatch({
        update_toolbar_input_select("sel_scenario", choices = c("(No scenarios)" = ""), selected = "")
      }, error = function(e) {
        updateSelectInput(session, "sel_scenario", choices = c("(No scenarios)" = ""), selected = "")
      })
      return()
    }

    sc_left <- scenarios[keep_indices]
    for (new_i in seq_along(sc_left)) {
      sc_left[[new_i]]$id <- new_i
    }
    scenario_catalog(sc_left)

    old_ov <- isolate(scenario_overrides())
    new_ov <- list()
    if (length(old_ov) > 0) {
      for (new_i in seq_along(keep_indices)) {
        old_i <- keep_indices[new_i]
        if (!is.null(old_ov[[as.character(old_i)]])) {
          entry <- old_ov[[as.character(old_i)]]
          entry$id <- new_i
          new_ov[[as.character(new_i)]] <- entry
        }
      }
    }
    scenario_overrides(new_ov)

    if (!is.null(v$scenario_list) && length(v$scenario_list) > 0) {
      v_keep <- intersect(keep_indices, seq_along(v$scenario_list))
      v$scenario_list <- if (length(v_keep) > 0) v$scenario_list[v_keep] else list()
    }

    cur_sel <- suppressWarnings(as.integer(isolate(selected_scenario_index())))
    new_sel_idx <- if (!is.na(cur_sel) && cur_sel %in% keep_indices) {
      which(keep_indices == cur_sel)
    } else {
      1L
    }
    new_sel_str <- as.character(new_sel_idx)
    selected_scenario_index(new_sel_str)

    choices <- setNames(
      as.character(seq_along(sc_left)),
      vapply(sc_left, function(s) s$label %||% paste("Scenario", s$id), character(1))
    )
    tryCatch({
      update_toolbar_input_select("sel_scenario", choices = choices, selected = new_sel_str)
    }, error = function(e) {
      updateSelectInput(session, "sel_scenario", choices = choices, selected = new_sel_str)
    })
  }

  trigger_remove_modal <- function(record_id) {
    rec <- isolate(v$map_catalog[[record_id]])

    req(rec)

    v$pending_map_to_remove <- record_id

    map_display_name <- if (identical(rec$type, "Land cover map")) {
      paste0("Land cover [", rec$year, "] - ", rec$name)
    } else {
      paste0("Zone [", rec$label, "] - ", rec$name)
    }

    # Find affected scenarios
    aff_idx <- get_scenarios_using_map(record_id)
    sc_list <- isolate(scenario_catalog())
    aff_names <- if (length(aff_idx) > 0) {
      vapply(aff_idx, function(i) sc_list[[i]]$label %||% paste0("Scenario ", i), character(1))
    } else character(0)

    showModal(modalDialog(
      title = tagList(icon("triangle-exclamation", class = "text-danger me-2"), "Remove map"),
      tags$p(
        "Are you sure you want to remove ",
        tags$strong(map_display_name),
        "?"
      ),
      if (length(aff_names) > 0) {
        tags$div(
          class = "alert alert-warning py-2 px-3 small mt-2 mb-2",
          icon("triangle-exclamation", class = "me-1 text-warning"),
          tags$strong(
            if (length(aff_names) == 1) "The following scenario depends on this map and will also be removed:"
            else "The following scenarios depend on this map and will also be removed:"
          ),
          tags$ul(
            class = "mb-0 ps-3 mt-1",
            lapply(aff_names, function(nm) tags$li(tags$strong(nm)))
          )
        )
      },
      tags$p(
        class = "text-muted small mb-0",
        "This map layer will be permanently removed from the project and any associated calculations will be updated."
      ),
      footer = tagList(
        actionButton("cancel_remove_map", "Cancel", class = "btn btn-secondary"),
        actionButton("confirm_remove_map", "Remove map", icon = icon("trash-can"), class = "btn btn-danger")
      ),
      size = "m",
      easyClose = TRUE
    ))
  }

  show_edit_label_modal <- function(record_id) {
    rec <- isolate(v$map_catalog[[record_id]])
    if (is.null(rec)) return()
    is_lc <- identical(rec$type, "Land cover map")
    v$pending_map_to_edit <- record_id
    showModal(modalDialog(
      title = if (is_lc) "Edit acquisition year" else "Edit map label",
      if (is_lc) {
        numericInput("edit_map_year", "Acquisition year",
          value = as.integer(rec$year %||% 2000L),
          min = 1800, max = as.integer(format(Sys.Date(), "%Y")), step = 1)
      } else {
        current_label <- if (!is.null(rec$label) && nzchar(rec$label)) rec$label else rec$name
        textInput("edit_map_label_text", "Map label", value = current_label)
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_map_label", "Save", icon = icon("check"), class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  registered_remove_handlers <- character(0)

  register_map_remove_handler <- function(record_id) {
    if (record_id %in% registered_remove_handlers) return()

    registered_remove_handlers <<- c(registered_remove_handlers, record_id)


    ## Edit label handlers
    observeEvent(input[[paste0("map_edit_btn_", record_id)]], {
      show_edit_label_modal(record_id)
    })

    observeEvent(input[[paste0("zone_menu_edit_", record_id)]], {
      show_edit_label_modal(record_id)
    })

    observeEvent(input[[paste0("map_remove_btn_", record_id)]], {
      trigger_remove_modal(record_id)
    })

    observeEvent(input[[paste0("zone_menu_remove_", record_id)]], {
      trigger_remove_modal(record_id)
    })
  }

  observeEvent(input$cancel_remove_map, {
    v$pending_map_to_remove <- NULL

    removeModal()
  })

  observeEvent(input$confirm_remove_map, {
    record_id <- isolate(v$pending_map_to_remove)

    req(record_id)

    removeModal()

    v$pending_map_to_remove <- NULL

    # 1. Remove card element from DOM

    removeUI(
      selector = paste0("#wrap_uploaded_", record_id, "_card"),
      immediate = TRUE
    )

    # 2. Reset output bindings

    output[[paste0("uploaded_map_preview_", record_id)]] <- NULL

    output[[paste0("zone_legend_table_", record_id)]] <- NULL

    # 3. Clean up disk files

    rec <- isolate(v$map_catalog[[record_id]])

    if (!is.null(rec)) {
      if (!is.null(rec$shp_file) && file.exists(rec$shp_file)) {
        shp_base <- sub("\\.shp$", "", rec$shp_file)

        for (ext in c(".shp", ".shx", ".dbf", ".prj", ".cpg")) {
          f <- paste0(shp_base, ext)

          if (file.exists(f)) try(unlink(f), silent = TRUE)
        }
      }

      unzip_dir <- file.path(tempdir(), paste0("upload_", record_id))

      if (dir.exists(unzip_dir)) {
        try(unlink(unzip_dir, recursive = TRUE), silent = TRUE)
      }

      raster_dir <- file.path(tempdir(), "redd_abacus_rasters")

      r_files <- list.files(raster_dir, pattern = paste0("^", record_id), full.names = TRUE)

      if (length(r_files) > 0) {
        try(unlink(r_files), silent = TRUE)
      }
    }

    # 4. Clean up zone data

    v$zone_legends[[record_id]] <- NULL

    v$zone_palette_sets[[record_id]] <- NULL

    # 4b. Remove any scenarios that were using this map
    aff_idx <- get_scenarios_using_map(record_id)
    if (length(aff_idx) > 0) {
      remove_scenarios_by_indices(aff_idx)
    }

    # 5. Remove record from catalog

    records <- isolate(v$map_catalog)

    records[[record_id]] <- NULL

    v$map_catalog <- records

    zone_left <- sum(vapply(records, function(record) {
      identical(record$type, "Zonation map")
    }, logical(1)))

    v$zone_catalog_count <- if (zone_left > 0) zone_left else NULL

    # 6b. Drop the removed map from the active selection so a deleted map can

    #     no longer be referenced as the active zonation or land cover map.

    if (identical(isolate(input$analysis_zone_map), record_id)) {
      updateSelectInput(session, "analysis_zone_map", selected = "")

      v$mapz_stars <- NULL

      v$mapz_df <- NULL
    }

    if (record_id %in% parse_map_selection(isolate(input$matrix_zone_map))) {
      remaining <- setdiff(parse_map_selection(isolate(input$matrix_zone_map)), record_id)

      updateSelectizeInput(session, "matrix_zone_map", selected = remaining)

      v$mapz_stars <- NULL

      v$mapz_df <- NULL
    }

    if (identical(isolate(input$analysis_map1), record_id)) {
      updateSelectInput(session, "analysis_map1", selected = "")

      v$map1_stars <- NULL

      v$map1_df <- NULL
    }

    if (identical(isolate(input$analysis_map2), record_id)) {
      updateSelectInput(session, "analysis_map2", selected = "")

      v$map2_stars <- NULL

      v$map2_df <- NULL
    }

    # 7. If all maps are removed, reset global CRS and state

    if (length(records) == 0) {
      crs <<- NULL

      v$map1_stars <- NULL

      v$map2_stars <- NULL

      v$mapz_stars <- NULL

      v$map1_df <- NULL

      v$map2_df <- NULL

      v$mapz_df <- NULL

      v$lc_list_df <- NULL

      v$cstock_list <- NULL
      v$npv_list <- NULL

      lc_legend_pal <<- NULL

      v$lc_palette_set <- "gyr"

      updateSelectInput(session, "lc_palette_set", selected = "gyr")

      v$zone_list_df <- NULL

      zone_legend_pal <<- NULL

      v$lc_changes_df <- NULL

      v$map_boundaries <- list()

      v$study_area_boundary <- NULL

      v$geo_location <- NULL
    } else {
      ## Only a land cover map defines the study area; a zonation map does not.

      first_rem <- Find(
        function(r) !is.null(r$stars) && identical(r$type, "Land cover map"),
        records
      )

      if (!is.null(first_rem)) {
        v$study_area_boundary <- tryCatch(
          create_map_boundary(
            map     = first_rem$stars,
            sf_data = first_rem$sf,
            name    = if (!is.null(first_rem$name)) first_rem$name else "Study Area",
            type    = first_rem$type,
            year    = if (!is.null(first_rem$year)) first_rem$year else ""
          ),
          error = function(e) NULL
        )

        update_study_area_location(v$study_area_boundary)
      } else {
        v$study_area_boundary <- NULL

        v$geo_location <- NULL
      }
    }

    # 7b. If no land-cover maps remain, clear the LC legend and LC state.

    #     The conditionalPanel keyed on output$is_lc_list (which reads v$lc_list_df)

    #     will automatically hide the legend card.

    lc_left <- sum(vapply(records, function(r) identical(r$type, "Land cover map"), logical(1)))

    if (lc_left == 0) {
      v$lc_list_df    <- NULL

      v$cstock_list   <- NULL

      lc_legend_pal <<- NULL

      v$lc_palette_set <- "gyr"

      updateSelectInput(session, "lc_palette_set", selected = "gyr")

      v$lc_changes_df <- NULL

      v$map1_stars    <- NULL

      v$map2_stars    <- NULL

      v$map1_df       <- NULL

      v$map2_df       <- NULL
    }

    # 8. Bump catalog version to update dropdowns and selections

    v$map_catalog_version <- v$map_catalog_version + 1L

    map_label_str <- if (!is.null(rec$name)) rec$name else record_id

    notify_msg <- if (length(aff_idx) > 0) {
      paste0("Map '", map_label_str, "' and ", length(aff_idx), " associated scenario(s) have been removed.")
    } else {
      paste0("Map '", map_label_str, "' has been removed.")
    }

    showNotification(
      notify_msg,
      type = "message",
      duration = 4
    )
  })

  ## Helper: apply a year edit to the catalog, update reactives and all scenario settings/editor
  apply_lc_year_edit <- function(record_id, new_year, old_year = NULL) {
    records <- isolate(v$map_catalog)
    if (is.null(old_year) && !is.null(records[[record_id]])) {
      old_year <- suppressWarnings(as.integer(records[[record_id]]$year))
    }
    old_year <- as.integer(old_year %||% 0L)
    new_year <- as.integer(new_year)

    # 1. Update map catalog
    records[[record_id]]$year <- new_year
    v$map_catalog <- records

    # 2. Update analysis map reactives if this map is part of the reference period
    m1 <- isolate(input$analysis_map1)
    m2 <- isolate(input$analysis_map2)
    mm1 <- isolate(input$matrix_map1)
    mm2 <- isolate(input$matrix_map2)
    is_m1 <- identical(record_id, m1) || identical(record_id, mm1) || (!is.null(v$map1_year) && identical(old_year, as.integer(v$map1_year)))
    is_m2 <- identical(record_id, m2) || identical(record_id, mm2) || (!is.null(v$map2_year) && identical(old_year, as.integer(v$map2_year)))

    if (is_m1) {
      v$map1_year <- new_year
      v$map1_date <- new_year
    }
    if (is_m2) {
      v$map2_year <- new_year
      v$map2_date <- new_year
    }
    y1_val <- isolate(v$map1_year)
    y2_val <- isolate(v$map2_year)
    if (!is.null(y1_val) && !is.null(y2_val)) {
      v$period_year <- as.numeric(y2_val - y1_val)
    }

    # 3. Update v$abacus_data project dates and base years
    ab <- isolate(v$abacus_data)
    if (!is.null(ab)) {
      cur_y1 <- isolate(v$map1_year) %||% 2000L
      cur_y2 <- isolate(v$map2_year) %||% 2010L
      d1 <- as.Date(paste0(cur_y1, "-07-01"))
      d2 <- as.Date(paste0(cur_y2, "-07-01"))
      if (!is.null(ab$project)) {
        ab$project$date1 <- d1
        ab$project$date2 <- d2
        ab$project$baseyear0 <- cur_y1
        ab$project$baseyear1 <- cur_y2
      }
      if (!is.null(ab$project_list)) {
        for (pj_i in seq_along(ab$project_list)) {
          if (!is.null(ab$project_list[[pj_i]]$project)) {
            ab$project_list[[pj_i]]$project$date1 <- d1
            ab$project_list[[pj_i]]$project$date2 <- d2
            ab$project_list[[pj_i]]$project$baseyear0 <- cur_y1
            ab$project_list[[pj_i]]$project$baseyear1 <- cur_y2
          }
        }
      }
      v$abacus_data <- ab
    }

    # 4. Update scenario_catalog entries (period label, map years, abacus_data dates)
    cats <- isolate(scenario_catalog())
    if (length(cats) > 0) {
      cats_updated <- lapply(cats, function(sc) {
        if (identical(sc$map1_id, record_id) || identical(as.integer(sc$map1_year %||% 0L), old_year)) {
          sc$map1_year <- new_year
        } else if (is_m1 && is.null(sc$map1_id) && is.null(sc$map1_year)) {
          sc$map1_year <- new_year
        }
        if (identical(sc$map2_id, record_id) || identical(as.integer(sc$map2_year %||% 0L), old_year)) {
          sc$map2_year <- new_year
        } else if (is_m2 && is.null(sc$map2_id) && is.null(sc$map2_year)) {
          sc$map2_year <- new_year
        }
        sc_y1 <- sc$map1_year %||% isolate(v$map1_year) %||% 2000L
        sc_y2 <- sc$map2_year %||% isolate(v$map2_year) %||% 2010L
        sc$map1_year <- sc_y1
        sc$map2_year <- sc_y2
        sc$period <- paste0(sc_y1, " \u2192 ", sc_y2)

        if (!is.null(sc$abacus_data)) {
          sc_d1 <- as.Date(paste0(sc_y1, "-07-01"))
          sc_d2 <- as.Date(paste0(sc_y2, "-07-01"))
          if (!is.null(sc$abacus_data$project)) {
            sc$abacus_data$project$date1 <- sc_d1
            sc$abacus_data$project$date2 <- sc_d2
            sc$abacus_data$project$baseyear0 <- sc_y1
            sc$abacus_data$project$baseyear1 <- sc_y2
          }
          if (!is.null(sc$abacus_data$project_list)) {
            for (pj_i in seq_along(sc$abacus_data$project_list)) {
              if (!is.null(sc$abacus_data$project_list[[pj_i]]$project)) {
                sc$abacus_data$project_list[[pj_i]]$project$date1 <- sc_d1
                sc$abacus_data$project_list[[pj_i]]$project$date2 <- sc_d2
                sc$abacus_data$project_list[[pj_i]]$project$baseyear0 <- sc_y1
                sc$abacus_data$project_list[[pj_i]]$project$baseyear1 <- sc_y2
              }
            }
          }
        }
        sc
      })
      scenario_catalog(cats_updated)
    }

    # 5. Update scenario_overrides
    ov <- isolate(scenario_overrides())
    if (length(ov) > 0) {
      ov_updated <- lapply(ov, function(sc) {
        if (identical(sc$map1_id, record_id) || identical(as.integer(sc$map1_year %||% 0L), old_year)) {
          sc$map1_year <- new_year
        }
        if (identical(sc$map2_id, record_id) || identical(as.integer(sc$map2_year %||% 0L), old_year)) {
          sc$map2_year <- new_year
        }
        sc_y1 <- sc$map1_year %||% isolate(v$map1_year) %||% 2000L
        sc_y2 <- sc$map2_year %||% isolate(v$map2_year) %||% 2010L
        sc$map1_year <- sc_y1
        sc$map2_year <- sc_y2
        sc$period <- paste0(sc_y1, " \u2192 ", sc_y2)
        if (!is.null(sc$abacus_data)) {
          sc_d1 <- as.Date(paste0(sc_y1, "-07-01"))
          sc_d2 <- as.Date(paste0(sc_y2, "-07-01"))
          if (!is.null(sc$abacus_data$project)) {
            sc$abacus_data$project$date1 <- sc_d1
            sc$abacus_data$project$date2 <- sc_d2
            sc$abacus_data$project$baseyear0 <- sc_y1
            sc$abacus_data$project$baseyear1 <- sc_y2
          }
        }
        sc
      })
      scenario_overrides(ov_updated)
    }

    # 6. Update v$scenario_list if present
    if (!is.null(v$scenario_list) && length(v$scenario_list) > 0) {
      v$scenario_list <- lapply(v$scenario_list, function(item) {
        sc_obj <- item$abacus_scenario$scenario %||% item
        if (identical(as.integer(sc_obj$map1_year %||% 0L), old_year)) sc_obj$map1_year <- new_year
        if (identical(as.integer(sc_obj$map2_year %||% 0L), old_year)) sc_obj$map2_year <- new_year
        sc_y1 <- sc_obj$map1_year %||% isolate(v$map1_year) %||% 2000L
        sc_y2 <- sc_obj$map2_year %||% isolate(v$map2_year) %||% 2010L
        sc_obj$map1_year <- sc_y1
        sc_obj$map2_year <- sc_y2
        sc_obj$period <- paste0(sc_y1, " \u2192 ", sc_y2)
        if (!is.null(item$abacus_scenario$scenario)) {
          item$abacus_scenario$scenario <- sc_obj
        } else {
          item <- sc_obj
        }
        item
      })
    }

    # 7. Recalculate baseline emissions with updated dates
    if (!is.null(v$abacus_data)) {
      v$abacus_baseline <- tryCatch(calculate_abacus_emission(v$abacus_data), error = function(e) NULL)
      if (!is.null(v$abacus_baseline)) {
        baseline_emission_data(v$abacus_baseline$emission)
      }
    }

    v$pending_map_to_edit <- NULL
    v$pending_year_edit   <- NULL
    v$map_catalog_version <- v$map_catalog_version + 1L
    showNotification("Map year and scenario settings updated.", type = "message", duration = 3)
  }

  observeEvent(input$confirm_edit_map_label, {
    record_id <- isolate(v$pending_map_to_edit)
    req(record_id)
    rec <- isolate(v$map_catalog[[record_id]])
    req(rec)
    is_lc <- identical(rec$type, "Land cover map")

    if (is_lc) {
      new_year <- suppressWarnings(as.integer(input$edit_map_year))
      if (is.na(new_year)) {
        showNotification("Enter a valid year.", type = "error")
        return()
      }
      existing_years <- unlist(lapply(
        Filter(function(r) identical(r$type, "Land cover map") && !identical(r$id, record_id),
               isolate(v$map_catalog)),
        function(r) if (!is.null(r$year)) as.integer(r$year) else NULL
      ))
      if (new_year %in% existing_years) {
        showNotification(
          sprintf("A land cover map for %d already exists. Each map must have a unique year.", new_year),
          type = "error", duration = 6
        )
        return()
      }
      # Year unchanged — nothing to do
      if (identical(as.integer(new_year), as.integer(rec$year))) {
        v$pending_map_to_edit <- NULL
        removeModal()
        return()
      }

      # Robust check: is this land cover map already used in scenario development?
      old_year <- suppressWarnings(as.integer(rec$year))
      cats <- isolate(scenario_catalog())
      has_scenarios <- length(cats) > 0 || (!is.null(isolate(v$scenario_list)) && length(isolate(v$scenario_list)) > 0)
      has_proj_data <- !is.null(isolate(v$abacus_data)) || !is.null(isolate(v$abacus_baseline))

      m1 <- isolate(input$analysis_map1)
      m2 <- isolate(input$analysis_map2)
      mm1 <- isolate(input$matrix_map1)
      mm2 <- isolate(input$matrix_map2)
      is_selected_map <- record_id %in% c(m1, m2, mm1, mm2)
      matches_active_years <- (!is.null(v$map1_year) && identical(old_year, as.integer(v$map1_year))) ||
                              (!is.null(v$map2_year) && identical(old_year, as.integer(v$map2_year)))
      sc_years <- unlist(lapply(cats, function(s) c(s$map1_year, s$map2_year)))
      sc_ids <- unlist(lapply(cats, function(s) c(s$map1_id, s$map2_id)))
      matches_scenario <- (old_year %in% sc_years) || (record_id %in% sc_ids)
      period_strs <- unlist(lapply(cats, function(s) s$period))
      matches_period <- any(grepl(paste0("\\b", old_year, "\\b"), period_strs))

      is_used_in_scenario <- (has_scenarios || has_proj_data) &&
                             (is_selected_map || matches_active_years || matches_scenario || matches_period)

      if (is_used_in_scenario) {
        # Store the pending edit and show a warning confirmation
        v$pending_year_edit <- list(record_id = record_id, new_year = new_year, old_year = old_year)
        removeModal()
        showModal(modalDialog(
          title = tagList(icon("triangle-exclamation", class = "text-warning me-2"),
                          "Land cover map in use"),
          tags$p(
            "This land cover map is currently used in scenario development.",
            tags$br(),
            "Changing the acquisition year from ",
            tags$strong(as.character(old_year)),
            " to ",
            tags$strong(as.character(new_year)),
            " will update the reference period and scenario settings."
          ),
          tags$p(class = "text-muted small mb-0",
            "Do you want to continue and update the scenarios with the new year?"),
          footer = tagList(
            actionButton("cancel_year_change", "Cancel", class = "btn btn-secondary"),
            actionButton("confirm_year_change_with_scenario", "Continue",
                         icon = icon("check"), class = "btn btn-warning")
          ),
          size = "m",
          easyClose = FALSE
        ))
        return()
      }

      # Not in use in scenarios — apply directly
      removeModal()
      apply_lc_year_edit(record_id, new_year, old_year)
    } else {
      new_label <- trimws(input$edit_map_label_text %||% "")
      if (!nzchar(new_label)) {
        showNotification("Enter a label for the zonation map.", type = "error")
        return()
      }
      records <- isolate(v$map_catalog)
      records[[record_id]]$label <- new_label
      v$map_catalog <- records
      v$pending_map_to_edit <- NULL
      v$map_catalog_version <- v$map_catalog_version + 1L
      removeModal()
      showNotification("Map label updated.", type = "message", duration = 3)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_year_change_with_scenario, {
    pending <- isolate(v$pending_year_edit)
    req(pending)
    removeModal()
    apply_lc_year_edit(pending$record_id, pending$new_year, pending$old_year)
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_year_change, {
    v$pending_year_edit   <- NULL
    v$pending_map_to_edit <- NULL
    removeModal()
  }, ignoreInit = TRUE)

observeEvent(v$map_catalog_version, {
    records <- isolate(v$map_catalog)

    landcover_records <- records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]

    if (length(landcover_records) > 0) {
      landcover_records <- landcover_records[order(vapply(landcover_records, function(record) {
        record$year
      }, integer(1)))]
    }

    landcover_ids <- names(landcover_records)

    landcover_choices <- map_choices_year(landcover_records)

    # Get current selections (isolated to avoid reactivity)

    baseline_id <- isolate(input$analysis_map1)

    comparison_id <- isolate(input$analysis_map2)

    # Validate baseline_id: must be one of landcover_ids

    if (length(baseline_id) != 1 || is.na(baseline_id) || !(baseline_id %in% landcover_ids))

      baseline_id <- if (length(landcover_ids) > 0) landcover_ids[1] else ""

    # Comparison map must be different from baseline map

    comparison_ids_available <- landcover_ids[landcover_ids != baseline_id]

    if (length(comparison_id) != 1 || is.na(comparison_id) || !(comparison_id %in% comparison_ids_available))

      comparison_id <- if (length(comparison_ids_available) > 0) comparison_ids_available[length(comparison_ids_available)] else ""

    baseline_choices <- landcover_choices[unname(landcover_choices) != comparison_id]

    comparison_choices <- landcover_choices[unname(landcover_choices) != baseline_id]

    updateSelectizeInput(session, "analysis_map1", choices = baseline_choices, selected = baseline_id)

    updateSelectizeInput(session, "analysis_map2", choices = comparison_choices, selected = comparison_id)

    ## Update abatement cost curve period selectors independently
    ab_id1 <- isolate(input$abacost_map1)
    ab_id2 <- isolate(input$abacost_map2)
    if (length(ab_id1) != 1 || is.na(ab_id1) || !(ab_id1 %in% landcover_ids))
      ab_id1 <- baseline_id
    ab_avail <- landcover_ids[landcover_ids != ab_id1]
    if (length(ab_id2) != 1 || is.na(ab_id2) || !(ab_id2 %in% ab_avail))
      ab_id2 <- if (length(ab_avail) > 0) ab_avail[length(ab_avail)] else ""
    ab_choices1 <- landcover_choices[unname(landcover_choices) != ab_id2]
    ab_choices2 <- landcover_choices[unname(landcover_choices) != ab_id1]
    updateSelectizeInput(session, "abacost_map1", choices = ab_choices1, selected = ab_id1)
    updateSelectizeInput(session, "abacost_map2", choices = ab_choices2, selected = ab_id2)
  }, ignoreInit = FALSE)

  observeEvent(v$map_catalog_version, {
    records <- isolate(v$map_catalog)

    zone_records <- records[vapply(records, function(record) {
      identical(record$type, "Zonation map")
    }, logical(1))]

    if (length(zone_records) > 0) {
      zone_records <- zone_records[order(vapply(zone_records, function(record) {
        record$label
      }, character(1)))]
    }

    zone_choices <- c("No zonation map" = "", zone_map_choices(zone_records))

    zone_ids <- names(zone_records)

    v$zone_catalog_count <- if (length(zone_ids) > 0) length(zone_ids) else NULL

    zone_id <- isolate(input$analysis_zone_map)

    if (length(zone_id) != 1 || is.na(zone_id) || !(zone_id %in% zone_ids))

      zone_id <- if (length(zone_ids) > 0) zone_ids[1] else ""

    updateSelectInput(session, "analysis_zone_map", choices = zone_choices, selected = zone_id)

    ## Abatement cost curve zone selector (completely independent, defaults to "No zonation map")
    sel_abacost_zone <- isolate(input$abacost_zone_map)
    if (length(sel_abacost_zone) != 1 || is.na(sel_abacost_zone) || !(sel_abacost_zone %in% zone_ids))
      sel_abacost_zone <- ""
    updateSelectizeInput(session, "abacost_zone_map", choices = zone_choices, selected = sel_abacost_zone)

    baseline_sel <- isolate(input$analysis_map1)

    if (!nzchar(baseline_sel) || !(baseline_sel %in% names(records))) {
      lc_ids <- names(records)[vapply(records, function(r) identical(r$type, "Land cover map"), logical(1))]

      baseline_sel <- if (length(lc_ids) > 0) lc_ids[1] else ""
    }

    comparison_sel <- isolate(input$analysis_map2)

    if (!nzchar(comparison_sel) || !(comparison_sel %in% names(records)) || identical(comparison_sel, baseline_sel)) {
      lc_ids <- names(records)[vapply(records, function(r) identical(r$type, "Land cover map"), logical(1))]

      candidates <- lc_ids[lc_ids != baseline_sel]

      comparison_sel <- if (length(candidates) > 0) candidates[length(candidates)] else ""
    }

    baseline <- if (nzchar(baseline_sel)) records[[baseline_sel]] else NULL

    comparison <- if (nzchar(comparison_sel)) records[[comparison_sel]] else NULL

    zone <- if (nzchar(zone_id)) records[[zone_id]] else NULL

    ## Only overwrite the stars reactive values when an explicit analysis map is

    ## selected. If no selection exists we leave the current value in place so

    ## that a catalog version bump (e.g. from crop_catalog_to_intersection) does

    ## not null out a map that is still valid - the crop function updates those

    ## values itself for the records it touched. However, if no land cover maps

    ## exist at all, clear map1/map2 stars.

    has_lc <- any(vapply(records, function(r) identical(r$type, "Land cover map"), logical(1)))

    if (!has_lc) {
      v$map1_stars <- NULL

      v$map2_stars <- NULL
    } else {
      if (!is.null(baseline)) v$map1_stars <- baseline$stars

      if (!is.null(comparison)) v$map2_stars <- comparison$stars
    }

    if (!is.null(zone)) v$mapz_stars <- zone$stars
  }, ignoreInit = FALSE)

  observeEvent(input$analysis_map1, {
    records <- isolate(v$map_catalog)

    sel1 <- input$analysis_map1

    if (nzchar(sel1) && !is.null(records[[sel1]])) {
      v$map1_stars <- records[[sel1]]$stars
    }
  }, ignoreInit = TRUE)

  observeEvent(input$analysis_map2, {
    records <- isolate(v$map_catalog)

    sel2 <- input$analysis_map2

    if (nzchar(sel2) && !is.null(records[[sel2]])) {
      v$map2_stars <- records[[sel2]]$stars
    }
  }, ignoreInit = TRUE)

  ## The GHG and combined-summary tabs have their own selector ids because the

  ## Carbon tab is rendered alongside them and duplicate Shiny input ids are not

  ## reliably updated.

  observeEvent(list(input$ghg_analysis_map1, input$ghg_analysis_map2,
                    input$summary_analysis_map1, input$summary_analysis_map2,
                    input$emission_subtabs, v$map_catalog_version), {
    tab <- input$emission_subtabs

    if (!isTRUE(tab %in% c("GHG flux emissions", "Emissions summary")))

      return()

    records <- isolate(v$map_catalog)

    landcover_records <- records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]

    landcover_records <- landcover_records[order(vapply(landcover_records, function(record) {
      record$year
    }, integer(1)))]

    lc_ids <- names(landcover_records)

    lc_choices <- map_choices_year(landcover_records)

    ## Prefer the active tab's controls, then the other emission selectors, then

    ## the shared analysis selections, so switching tabs preserves the period.

    tab_sel1 <- if (identical(tab, "Emissions summary")) input$summary_analysis_map1 else input$ghg_analysis_map1

    tab_sel2 <- if (identical(tab, "Emissions summary")) input$summary_analysis_map2 else input$ghg_analysis_map2

    alt_sel1 <- if (identical(tab, "Emissions summary")) input$ghg_analysis_map1 else input$summary_analysis_map1

    alt_sel2 <- if (identical(tab, "Emissions summary")) input$ghg_analysis_map2 else input$summary_analysis_map2

    sel1 <- tab_sel1

    if (length(sel1) != 1 || is.na(sel1) || !nzchar(sel1) || !(sel1 %in% lc_ids))

      sel1 <- alt_sel1

    if (length(sel1) != 1 || is.na(sel1) || !nzchar(sel1) || !(sel1 %in% lc_ids))

      sel1 <- input$analysis_map1

    if (length(sel1) != 1 || is.na(sel1) || !nzchar(sel1) || !(sel1 %in% lc_ids))

      sel1 <- if (length(lc_ids) > 0) lc_ids[1] else ""

    candidates <- lc_ids[lc_ids != sel1]

    sel2 <- tab_sel2

    if (length(sel2) != 1 || is.na(sel2) || !nzchar(sel2) || !(sel2 %in% candidates))

      sel2 <- alt_sel2

    if (length(sel2) != 1 || is.na(sel2) || !nzchar(sel2) || !(sel2 %in% candidates))

      sel2 <- input$analysis_map2

    if (length(sel2) != 1 || is.na(sel2) || !nzchar(sel2) || !(sel2 %in% candidates))

      sel2 <- if (length(candidates) > 0) candidates[length(candidates)] else ""

    if (nzchar(sel1) && nzchar(sel2)) {
      year1 <- suppressWarnings(as.integer(records[[sel1]]$year))

      year2 <- suppressWarnings(as.integer(records[[sel2]]$year))

      if (!is.na(year1) && !is.na(year2) && year1 > year2) {
        swap <- sel1

        sel1 <- sel2

        sel2 <- swap
      }
    }

    choices1 <- lc_choices[unname(lc_choices) != sel2]

    choices2 <- lc_choices[unname(lc_choices) != sel1]

    updateSelectizeInput(session, "ghg_analysis_map1", choices = choices1, selected = sel1)

    updateSelectizeInput(session, "ghg_analysis_map2", choices = choices2, selected = sel2)

    updateSelectizeInput(session, "summary_analysis_map1", choices = choices1, selected = sel1)

    updateSelectizeInput(session, "summary_analysis_map2", choices = choices2, selected = sel2)

    ## Keep the emission calculations and the other analysis views on the

    ## selected pair, even when their controls are currently in hidden tabs.

    if (nzchar(sel1) && nzchar(sel2)) {
      first <- records[[sel1]]

      second <- records[[sel2]]

      v$map1_stars <- first$stars

      v$map2_stars <- second$stars

      v$map1_year <- as.integer(first$year)

      v$map2_year <- as.integer(second$year)

      v$map1_date <- v$map1_year

      v$map2_date <- v$map2_year

      v$period_year <- as.numeric(v$map2_year - v$map1_year)

      if (!identical(isolate(input$analysis_map1), sel1))

        updateSelectizeInput(session, "analysis_map1", choices = choices1, selected = sel1)

      if (!identical(isolate(input$analysis_map2), sel2))

        updateSelectizeInput(session, "analysis_map2", choices = choices2, selected = sel2)

      if (!identical(isolate(input$matrix_map1), sel1))

        updateSelectizeInput(session, "matrix_map1", selected = sel1)

      if (!identical(isolate(input$matrix_map2), sel2))

        updateSelectizeInput(session, "matrix_map2", selected = sel2)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  ## Build the land cover legend from the currently selected analysis maps.

  ## This used to be driven by the map1/map2 preview renderers, which no longer

  ## exist in the UI; without this the legend table never receives data.

  build_lc_legend <- function() {
    ## Prefer the analysis selection, but fall back to the first land cover map

    ## in the catalog so the legend appears as soon as a map is uploaded,
    ## before the user ever visits the Emission tab.

    records <- isolate(v$map_catalog)

    lc_records <- records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]

    if (length(lc_records) == 0) {
      v$lc_list_df <- NULL

      v$cstock_list <- NULL
      v$npv_list <- NULL

      lc_legend_pal <<- NULL

      v$lc_palette_set <- "gyr"

      updateSelectInput(session, "lc_palette_set", selected = "gyr")

      return()
    }

    maps <- list()

    for (nm in c("map1", "map2")) {
      m <- isolate(v[[paste0(nm, "_stars")]])

      if (!is.null(m))

        maps[[length(maps) + 1]] <- m
    }

    if (length(maps) == 0 && length(lc_records) > 0)

      maps <- list(lc_records[[1]]$stars)

    maps <- Filter(Negate(is.null), maps)

    ids <- unlist(lapply(maps, function(m) {
      match_rec <- Find(function(r) !is.null(r$stars) && identical(r$stars, m), lc_records)
      freq <- if (!is.null(match_rec) && !is.null(match_rec$freq_df) && nrow(match_rec$freq_df) > 0) {
        match_rec$freq_df
      } else {
        tryCatch(get_raster_freq_df(m), error = function(e) NULL)
      }

      if (is.null(freq) || nrow(freq) == 0) numeric(0) else freq$id
    }))

    lc_id_list <- sort(unique(ids))

    lc_id_list <- lc_id_list[!is.na(lc_id_list)]

    if (length(lc_id_list) == 0) {
      v$lc_list_df <- NULL

      lc_legend_pal <<- NULL

      v$lc_palette_set <- "gyr"

      updateSelectInput(session, "lc_palette_set", selected = "gyr")

      return()
    }

    update_lc_df(isolate(v$lc_list_df), lc_id_list, NULL)
  }

  observeEvent(list(v$map_catalog_version, v$map1_stars, v$map2_stars), {
    build_lc_legend()
  }, ignoreNULL = FALSE)

  update_study_area_location <- function(boundary) {
    if (is.null(boundary) || nrow(boundary) == 0) {
      v$geo_location <- NULL

      return()
    }

    bb <- tryCatch(sf::st_bbox(boundary), error = function(e) NULL)

    if (is.null(bb)) {
      v$geo_location <- NULL

      return()
    }

    center_lon <- (as.numeric(bb["xmin"]) + as.numeric(bb["xmax"])) / 2

    center_lat <- (as.numeric(bb["ymin"]) + as.numeric(bb["ymax"])) / 2

    v$geo_location <- tryCatch(lookup_geographic_location(center_lat, center_lon), error = function(e) NULL)
  }

  # â”€â”€ Location leaflet: base render â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  output$map_location_leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%          # free OpenStreetMap tiles

      setView(lng = 0, lat = 20, zoom = 2)
  })

  # â”€â”€ Location leaflet: single study area boundary overlay â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  # The study area boundary is established once based on the first uploaded map.

  # If a subsequent upload requires cropping, the boundary is updated to the

  # new cropped intersection.

  #

  # The SAME boundary as the info card is drawn (map_info_boundary()), so a

  # zonation map uploaded first is located too - not only land cover maps.

  observe({
    v$map_catalog_version

    boundary <- map_info_boundary()

    proxy <- leafletProxy("map_location_leaflet") %>% clearShapes()

    if (is.null(boundary) || nrow(boundary) == 0) return()

    proxy %>%
      addPolygons(
        data        = boundary,
        layerId     = "study_area_boundary",
        fillColor   = "#e74c3c",
        fillOpacity = 0.25,
        color       = "#c0392b",
        weight      = 2,
        label       = "Study area boundary",
        labelOptions = labelOptions(sticky = TRUE)
      ) %>%
      fitBounds(
        lng1 = sf::st_bbox(boundary)[["xmin"]],
        lat1 = sf::st_bbox(boundary)[["ymin"]],
        lng2 = sf::st_bbox(boundary)[["xmax"]],
        lat2 = sf::st_bbox(boundary)[["ymax"]]
      )
  })

  ## The study area boundary the info card describes. Unlike v$study_area_boundary

  ## (only ever built from a land cover map) this falls back to the first map of

  ## ANY type, so a zonation map uploaded first still has a boundary, location,
  ## area and coordinates to show.

  map_info_boundary <- function(records = v$map_catalog) {
    boundary <- v$study_area_boundary

    if (!is.null(boundary) && nrow(boundary) > 0) return(boundary)

    if (length(records) == 0) return(NULL)

    ## First record that carries a raster (the first uploaded map).

    rec <- Find(function(r) !is.null(r$stars) || inherits(r$sf, "sf"), records)

    if (is.null(rec)) return(NULL)

    if (is.null(rec$stars) && (is.null(rec$sf) || nrow(rec$sf) == 0)) return(NULL)

    tryCatch(
      create_map_boundary(
        map     = rec$stars,
        sf_data = rec$sf,
        name    = if (!is.null(rec$name)) rec$name else "Study Area",
        type    = rec$type,
        year    = if (!is.null(rec$year)) rec$year else ""
      ),
      error = function(e) NULL
    )
  }

  # â”€â”€ General map info UI â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  output$map_general_info_ui <- renderUI({
    v$map_catalog_version

    records    <- v$map_catalog

    boundary   <- map_info_boundary(records)

    lc_changes <- v$lc_changes_df   # reactive dependency â†’ updates when matrix rebuilds

    if (length(records) == 0 && is.null(boundary)) {
      return(tags$div(
        class = "text-muted text-center py-5",
        icon("map-location-dot", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small", "No maps uploaded yet")
      ))
    }

    ref_stars <- first_map_reference_stars()

    # 1. Total area â€” use the same pixel-count method as the LC changes matrix

    #    so both numbers are always identical.

    #

    #    Priority:

    #      a) LC changes matrix total (ha, already converted) â€” exact match with matrix footer

    #      b) Map1 freq_df Ã— pixel_ha              â€” single-map case / no pair selected yet

    #      c) sf::st_area(boundary)                â€” last resort for vector-only uploads

    area_ha_str  <- "N/A"

    area_km2_str <- "N/A"

    area_ha      <- NA_real_

    ## (a) Prefer the matrix total when it is available â€” guarantees identical values

    if (!is.null(lc_changes) && nrow(lc_changes) > 0 && "area" %in% colnames(lc_changes)) {
      total_ha <- sum(lc_changes$area, na.rm = TRUE)

      if (!is.na(total_ha) && total_ha > 0) area_ha <- total_ha
    }

    ## (b) Fall back to map1 pixel count Ã— pixel area

    if (is.na(area_ha) && !is.null(ref_stars)) {
      cell_ha <- pixel_ha(ref_stars)

      if (!is.na(cell_ha) && cell_ha > 0) {
        rec1 <- Find(function(r) !is.null(r$freq_df) && nrow(r$freq_df) > 0, records)

        if (!is.null(rec1)) {
          valid_cell_count <- sum(rec1$freq_df$area, na.rm = TRUE)

          if (valid_cell_count > 0) area_ha <- as.numeric(valid_cell_count) * cell_ha
        }
      }
    }

    ## (c) Last resort: geodetic area of the boundary polygon (vector-only uploads,
    ##     or any map when no land cover map exists yet)

    if (is.na(area_ha) && !is.null(boundary) && nrow(boundary) > 0) {
      area_m2 <- tryCatch(as.numeric(sf::st_area(boundary)), error = function(e) NA_real_)

      if (is.na(area_m2) || area_m2 <= 0)

        area_m2 <- tryCatch(as.numeric(sf::st_area(sf::st_make_valid(boundary))), error = function(e) NA_real_)

      if (!is.na(area_m2) && area_m2 > 0) area_ha <- area_m2 / 10000
    }

    if (!is.na(area_ha) && area_ha > 0) {
      area_ha_str  <- paste0(format(round(area_ha), big.mark = ","), " ha")

      area_km2_str <- paste0(format(round(area_ha / 100, 2), big.mark = ","), " km\u00b2")
    }

    # 2. Location (Country, Province, etc.)

    loc <- v$geo_location

    if (is.null(loc) && !is.null(boundary) && nrow(boundary) > 0) {
      bb <- tryCatch(sf::st_bbox(boundary), error = function(e) NULL)

      if (!is.null(bb)) {
        center_lon <- (as.numeric(bb["xmin"]) + as.numeric(bb["xmax"])) / 2

        center_lat <- (as.numeric(bb["ymin"]) + as.numeric(bb["ymax"])) / 2

        loc <- tryCatch(lookup_geographic_location(center_lat, center_lon), error = function(e) NULL)

        if (!is.null(loc)) v$geo_location <- loc
      }
    }

    loc_ui <- if (!is.null(loc)) {
      parts <- c()

      if (!is.null(loc$province) && nzchar(loc$province)) parts <- c(parts, loc$province)

      if (!is.null(loc$country) && nzchar(loc$country)) parts <- c(parts, loc$country)

      main_text <- if (length(parts) > 0) paste(parts, collapse = ", ") else loc$country

      sub_text <- if (!is.null(loc$district) && nzchar(loc$district) && !identical(loc$district, loc$province)) loc$district else NULL

      tags$div(
        tags$div(class = "fw-semibold text-primary", style = "font-size: 0.82rem;",
                 icon("location-dot", class = "me-1 text-danger"), main_text),
        if (!is.null(sub_text)) tags$div(class = "text-muted small", style = "font-size: 0.74rem; margin-left: 1.15rem;", sub_text) else NULL
      )
    } else {
      tags$span(class = "text-muted small", "Not available")
    }

    # 3. CRS

    crs_obj <- if (!is.null(ref_stars)) {
      tryCatch(sf::st_crs(ref_stars), error = function(e) NA)
    } else if (length(records) > 0 && !is.null(records[[1]]$sf)) {
      tryCatch(sf::st_crs(records[[1]]$sf), error = function(e) NA)
    } else NA

    crs_name <- "Unknown"

    crs_code <- "N/A"

    crs_type <- "N/A"

    if (!is.na(crs_obj)) {
      crs_name <- crs_obj$Name

      if (is.null(crs_name) || is.na(crs_name) || !nzchar(crs_name)) {
        crs_name <- crs_obj$input

        if (is.null(crs_name) || is.na(crs_name)) crs_name <- "Custom / Local"
      }

      crs_code <- if (!is.na(crs_obj$epsg)) paste0("EPSG:", crs_obj$epsg) else "Local"

      crs_type <- if (sf::st_is_longlat(crs_obj)) "Geographic" else "Projected"
    }

    # 4. Resolution & Grid

    res_str         <- "N/A"

    grid_str        <- "N/A"

    total_cells_str <- "N/A"

    if (!is.null(ref_stars)) {
      dims <- tryCatch(stars::st_dimensions(ref_stars), error = function(e) NULL)

      if (!is.null(dims)) {
        dx <- abs(dims$x$delta)

        dy <- abs(dims$y$delta)

        is_geo <- !is.na(crs_obj) && sf::st_is_longlat(crs_obj)

        unit_str <- if (is_geo) "\u00b0" else "m"

        res_str <- paste0(round(dx, if (is_geo) 6 else 1), " ", unit_str, " \u00d7 ",
                          round(dy, if (is_geo) 6 else 1), " ", unit_str)

        if (!is_geo) {
          cell_ha <- (dx * dy) / 10000

          if (cell_ha >= 0.01) {
            res_str <- paste0(res_str, " (", round(cell_ha, 2), " ha/cell)")
          }
        }

        nx <- dims$x$to

        ny <- dims$y$to

        if (!is.null(nx) && !is.null(ny)) {
          grid_str <- paste0(format(nx, big.mark = ","), " \u00d7 ", format(ny, big.mark = ","), " cells")

          total_cells_str <- format(as.numeric(nx) * as.numeric(ny), big.mark = ",")
        }
      }
    }

    # 5. Coordinates / Bounding Box (WGS84)

    wgs_coord_ui <- "N/A"

    if (!is.null(boundary) && nrow(boundary) > 0) {
      bb <- sf::st_bbox(boundary)

      wgs_coord_ui <- tagList(
        tags$div(sprintf("Lon: %.4f\u00b0 to %.4f\u00b0", bb["xmin"], bb["xmax"])),
        tags$div(sprintf("Lat: %.4f\u00b0 to %.4f\u00b0", bb["ymin"], bb["ymax"]))
      )
    } else if (!is.null(ref_stars)) {
      bb_wgs <- tryCatch(sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(ref_stars)), 4326)), error = function(e) NULL)

      if (!is.null(bb_wgs)) {
        wgs_coord_ui <- tagList(
          tags$div(sprintf("Lon: %.4f\u00b0 to %.4f\u00b0", bb_wgs["xmin"], bb_wgs["xmax"])),
          tags$div(sprintf("Lat: %.4f\u00b0 to %.4f\u00b0", bb_wgs["ymin"], bb_wgs["ymax"]))
        )
      }
    }

    # 6. Maps count

    n_lc   <- sum(vapply(records, function(r) identical(r$type, "Land cover map"), logical(1)))

    n_zone <- sum(vapply(records, function(r) identical(r$type, "Zonation map"), logical(1)))

    tags$div(
      class = "map-info-container",
      style = "font-size: 0.82rem;",
      # Total Area Banner

      tags$div(
        class = "p-2 mb-2 rounded bg-light border text-center",
        tags$div(class = "text-muted small text-uppercase fw-bold", style = "font-size: 0.72rem;", "Total Area"),
        tags$div(
          style = "font-size: 1.25rem; font-weight: 700; color: #1e8449;",
          area_ha_str
        ),
        if (area_km2_str != "N/A") tags$div(class = "text-muted small", area_km2_str) else NULL
      ),
      # Detail Table

      tags$table(
        class = "table table-sm table-borderless mb-1",
        tags$tbody(
          tags$tr(
            tags$td(class = "text-muted text-nowrap align-top", style = "width: 32%;", tags$b("Location")),
            tags$td(loc_ui)
          ),
          tags$tr(
            tags$td(class = "text-muted text-nowrap align-top", tags$b("CRS")),
            tags$td(
              tags$div(class = "fw-semibold", style = "font-size: 0.8rem; line-height: 1.2; word-break: break-word;", crs_name),
              tags$div(
                class = "mt-1",
                if (crs_code != "N/A") tags$span(class = "badge bg-secondary me-1", crs_code) else NULL,
                if (crs_type != "N/A") tags$span(class = "badge bg-info text-dark", crs_type) else NULL
              )
            )
          ),
          tags$tr(
            tags$td(class = "text-muted text-nowrap align-top", tags$b("Resolution")),
            tags$td(style = "font-size: 0.8rem;", res_str)
          ),
          tags$tr(
            tags$td(class = "text-muted text-nowrap align-top", tags$b("Grid Size")),
            tags$td(
              style = "font-size: 0.8rem;",
              grid_str,
              if (total_cells_str != "N/A") tags$div(class = "text-muted small", paste0(total_cells_str, " total cells")) else NULL
            )
          ),
          tags$tr(
            tags$td(class = "text-muted text-nowrap align-top", tags$b("Coordinates")),
            tags$td(style = "font-family: monospace; font-size: 0.76rem;", wgs_coord_ui)
          ),
          tags$tr(
            tags$td(class = "text-muted text-nowrap align-top", tags$b("Maps")),
            tags$td(
              if (n_lc > 0) tags$span(class = "badge bg-danger me-1", paste0(n_lc, " Land Cover")) else NULL,
              if (n_zone > 0) tags$span(class = "badge bg-primary", paste0(n_zone, " Zonation")) else NULL
            )
          )
        )
      )
    )
  })

  build_lc_trend <- function() {
    records <- isolate(v$map_catalog)

    lc_records <- records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]

    if (length(lc_records) < 2) {
      v$lc_trend_data <- NULL

      return()
    }

    lc_records <- lc_records[order(vapply(lc_records, function(record) {
      record$year
    }, integer(1)))]

    trend_data <- lapply(lc_records, function(rec) {
      freq <- if (!is.null(rec$freq_df) && nrow(rec$freq_df) > 0) {
        rec$freq_df
      } else {
        tryCatch(get_raster_freq_df(rec$stars), error = function(e) NULL)
      }

      if (is.null(freq) || nrow(freq) == 0) return(NULL)

      freq$year <- rec$year

      freq$map_id <- rec$id

      freq
    })

    trend_data <- Filter(Negate(is.null), trend_data)

    if (length(trend_data) == 0) {
      v$lc_trend_data <- NULL

      return()
    }

    df <- do.call(rbind, trend_data)

    df$id <- as.numeric(as.character(df$id))

    df$area <- as.numeric(as.character(df$area))

    df$year <- as.numeric(as.character(df$year))

    lc_legend <- isolate(v$lc_list_df)

    if (!is.null(lc_legend) && nrow(lc_legend) > 0) {
      lc_legend$lc_id <- as.numeric(as.character(lc_legend$lc_id))

      df <- merge(df, lc_legend[c("lc_id", "label")], by.x = "id", by.y = "lc_id", all.x = TRUE)

      df$label[is.na(df$label)] <- paste0("LC_", df$id[is.na(df$label)])
    } else {
      df$label <- paste0("LC_", df$id)
    }

    v$lc_trend_data <- df
  }

  observeEvent(v$map_catalog_version, {
    build_lc_trend()
  }, ignoreNULL = FALSE)

  lc_trend_chart_widget <- reactive({
    df <- v$lc_trend_data

    if (is.null(df) || nrow(df) == 0) return(NULL)

    years <- sort(unique(df$year))

    if (length(years) < 2) return(NULL)

    ab <- trend_to_abacus(df, v$lc_list_df, lc_changes = v$lc_changes_df, n_iteration = v$n_iteration)

    if (is.null(ab)) return(NULL)

    lc_area_chart(
      data                  = ab,
      scenario              = NULL,
      selected_iteration    = 1L,
      selected_zone         = 0L,
      show_period_selection = FALSE
    )
  })

  output$lc_trend_chart <- render_lc_area_chart({
    lc_trend_chart_widget()
  })

  observeEvent(input$map_upload_file, {
    req(input$map_upload_file)

    v$pending_map_upload <- input$map_upload_file

    current_year <- as.integer(format(Sys.Date(), "%Y"))

    showModal(modalDialog(
      title = "Map details",
      radioButtons(
        "map_upload_type",
        "Map type",
        choices = c("Land cover map", "Zonation map"),
        selected = "Land cover map",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.map_upload_type == 'Land cover map'",
        numericInput(
          "map_acquisition_year",
          "Map acquisition year",
          value = guess_acquisition_year(input$map_upload_file$name, current_year),
          min = 1800,
          max = current_year,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.map_upload_type == 'Zonation map'",
        textInput(
          "map_upload_label",
          "Map label",
          value = tools::file_path_sans_ext(basename(input$map_upload_file$name))
        )
      ),
      footer = tagList(
        actionButton("cancel_map_upload", "Cancel"),
        actionButton("confirm_map_upload", "Add map", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$cancel_map_upload, {
    v$pending_map_upload <- NULL

    removeModal()

    session$sendCustomMessage("resetFileInput", "map_upload_file")
  })

  observeEvent(input$confirm_map_upload, {
    file <- isolate(v$pending_map_upload)

    req(file, input$map_upload_type)

    is_landcover <- identical(input$map_upload_type, "Land cover map")

    map_year <- NULL

    map_label <- NULL

    if (is_landcover) {
      req(input$map_acquisition_year)

      map_year <- as.integer(input$map_acquisition_year)

      ## Each acquisition year must be unique across all land-cover maps in

      ## the catalog. Two maps from the same year cannot be compared

      ## meaningfully and would break the before/after analysis.

      existing_years <- unlist(lapply(
        Filter(function(r) identical(r$type, "Land cover map"), isolate(v$map_catalog)),
        function(r) if (!is.null(r$year)) as.integer(r$year) else NULL
      ))

      if (!is.na(map_year) && map_year %in% existing_years) {
        showNotification(
          sprintf("A land cover map for %d is already in the catalog. Each map must have a unique acquisition year.", map_year),
          type = "error",
          duration = 6
        )

        return()
      }
    } else {
      map_label <- trimws(input$map_upload_label)

      if (!nzchar(map_label)) {
        showNotification("Enter a label for the zonation map.", type = "error")

        return()
      }
    }

    record_id <- paste0("map_", v$next_map_id)

    progress <- Progress$new(session, min = 0, max = 1)

    map_progress[[record_id]] <- progress

    progress_complete <- FALSE

    on.exit(if (!progress_complete) close_map_progress(record_id), add = TRUE)

    file_ext <- tolower(tools::file_ext(file$name))

    is_shp_upload <- file_ext %in% c("shp", "zip")

    ## Vector layers carry an sf object and a saved shapefile path; raster

    ## uploads carry neither. Initialise both so the finalize call below works

    ## for either branch.

    sf_data <- NULL

    shp_saved_path <- NULL

    if (is_shp_upload) {
      progress$set(0.2, message = "Processing map", detail = "Reading vector shapefile")

      upload_res <- tryCatch({
        if (file_ext == "zip") {
          unzip_dir <- file.path(tempdir(), paste0("upload_", record_id))

          dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

          utils::unzip(file$datapath, exdir = unzip_dir)

          shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)

          if (length(shp_files) == 0) stop("No .shp file found inside the zip archive.")

          sf::st_read(shp_files[1], quiet = TRUE)
        } else {
          sf::st_read(file$datapath, quiet = TRUE)
        }
      }, error = function(e) e)

      if (inherits(upload_res, "error")) {
        removeModal()

        showModal(modalDialog(paste("Failed to read shapefile:", upload_res$message), title = "Map File Error"))

        v$pending_map_upload <- NULL

        session$sendCustomMessage("resetFileInput", "map_upload_file")

        return()
      }

      sf_in <- upload_res

      map_crs <- sf::st_crs(sf_in)

      if (is.na(map_crs)) {
        removeModal()

        showModal(modalDialog(
          HTML("Please upload the shapefile with <b>Coordinate Reference System (.prj)</b> included"),
          title = "Map File Error"
        ))

        v$pending_map_upload <- NULL

        session$sendCustomMessage("resetFileInput", "map_upload_file")

        return()
      }

      col_names <- colnames(sf_in)

      geom_col <- attr(sf_in, "sf_column")

      attr_cols <- setdiff(col_names, geom_col)

      id_candidate <- grep("^(id|zone_id|lc_id|value|gridcode|code|class)$", attr_cols, ignore.case = TRUE, value = TRUE)

      if (length(id_candidate) > 0) {
        sf_in$ID <- suppressWarnings(as.numeric(as.character(sf_in[[id_candidate[1]]])))
      } else if (length(attr_cols) > 0) {
        sf_in$ID <- suppressWarnings(as.numeric(as.character(sf_in[[attr_cols[1]]])))
      } else {
        sf_in$ID <- seq_len(nrow(sf_in))
      }

      if (any(is.na(sf_in$ID))) sf_in$ID[is.na(sf_in$ID)] <- seq_len(sum(is.na(sf_in$ID)))

      sf_wgs84 <- sf::st_transform(sf_in, 4326)

      shp_dir <- file.path(tempdir(), "redd_abacus_shp")

      if (!dir.exists(shp_dir)) dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)

      shp_path <- file.path(shp_dir, paste0(record_id, ".shp"))

      sf::st_write(sf_wgs84, shp_path, delete_layer = TRUE, quiet = TRUE)

      progress$set(0.4, detail = "Generating raster grid for analysis")

      ## Burn the polygons into a raster on the first uploaded map's grid. The

      ## reference can be a stars_proxy, so it is materialised first: the grid

      ## has to be read, not just pointed at.

      ref_stars <- if (!is.null(v$map1_stars)) v$map1_stars else if (!is.null(v$map2_stars)) v$map2_stars else if (!is.null(v$mapz_stars)) v$mapz_stars else NULL

      if (inherits(ref_stars, "stars_proxy"))

        ref_stars <- tryCatch(stars::st_as_stars(ref_stars), error = function(e) NULL)

      raster_dir <- file.path(tempdir(), "redd_abacus_rasters")

      if (!dir.exists(raster_dir)) dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)

      tif_path <- file.path(raster_dir, paste0(record_id, ".tif"))

      if (!is.null(ref_stars)) {
        r_grid <- rasterize_to_reference(sf_in, ref_stars, field = "ID")
      } else {
        ## No reference map yet, so the grid has to be made from the polygons

        ## themselves. st_rasterize() into a directory writes the layer and hands

        ## back a stars object on a grid derived from the input geometry.

        tmp_dir <- file.path(raster_dir, paste0(record_id, "_grid"))

        if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

        r_grid <- tryCatch(
          stars::st_rasterize(sf_in, file = tmp_dir),
          error = function(e) NULL
        )

        if (inherits(r_grid, "stars_proxy"))

          r_grid <- tryCatch(stars::st_as_stars(r_grid), error = function(e) NULL)

        if (!is.null(r_grid) && "ID" %in% names(r_grid)) r_grid <- r_grid["ID"]
      }

      if (inherits(r_grid, "stars_proxy"))

        r_grid <- tryCatch(stars::st_as_stars(r_grid), error = function(e) NULL)

      if (is.null(r_grid) || !inherits(r_grid, "stars"))

        stop("The uploaded vector layer could not be turned into a raster grid.")

      map <- r_grid

      sf_data <- sf_wgs84

      shp_saved_path <- shp_path

      ## Same location rule as for rasters: a vector layer describing a

      ## different place must be refused, not warped onto the reference grid.

      reference_stars <- first_map_reference_stars()

      location_issue <- location_mismatch_reason(map, reference_stars)

      if (!is.null(location_issue)) {
        removeModal()

        close_map_progress(record_id)

        progress_complete <- TRUE

        v$pending_map_upload <- NULL

        session$sendCustomMessage("resetFileInput", "map_upload_file")

        showModal(modalDialog(
          title = "Map location does not match",
          HTML(sprintf(
            paste0(
              "This map was not added because <b>%s</b>.<br/><br/>",
              "Only maps covering the same geographic area as the first ",
              "uploaded map can be analysed together. Please upload a map of ",
              "the same location."
            ),
            location_issue
          )),
          easyClose = FALSE
        ))

        return()
      }
    } else {
      progress$set(0.1, message = "Processing map", detail = "Reading uploaded file")

      raster_dir <- file.path(tempdir(), "redd_abacus_rasters")

      if (!dir.exists(raster_dir)) dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)

      persistent_path <- file.path(raster_dir, paste0(record_id, "_", basename(file$name)))

      file.copy(file$datapath, persistent_path, overwrite = TRUE)

      map <- tryCatch(read_stars(persistent_path, proxy = TRUE), error = function(error) error)

      if (inherits(map, "error")) {
        map <- tryCatch(read_stars(persistent_path), error = function(error) error)
      }

      if (inherits(map, "error")) {
        removeModal()

        showModal(modalDialog("The selected file could not be read as a raster map.", title = "Map File Error"))

        v$pending_map_upload <- NULL

        session$sendCustomMessage("resetFileInput", "map_upload_file")

        return()
      }

      progress$set(0.3, detail = "Validating coordinate reference system")

      map_crs <- st_crs(map)

      if (is.na(map_crs)) {
        removeModal()

        showModal(modalDialog(
          HTML("Please upload the map with <b>Coordinate Reference System</b> included"),
          title = "Map File Error"
        ))

        v$pending_map_upload <- NULL

        session$sendCustomMessage("resetFileInput", "map_upload_file")

        return()
      }

      ## A map of a different place cannot be reconciled by warping: it must be

      ## refused, because aligning it would only line its grid up with the

      ## reference while its cells still describe another location.

      reference_stars <- first_map_reference_stars()

      location_issue <- location_mismatch_reason(map, reference_stars)

      if (!is.null(location_issue)) {
        removeModal()

        close_map_progress(record_id)

        progress_complete <- TRUE

        v$pending_map_upload <- NULL

        session$sendCustomMessage("resetFileInput", "map_upload_file")

        showModal(modalDialog(
          title = "Map location does not match",
          HTML(sprintf(
            paste0(
              "This map was not added because <b>%s</b>.<br/><br/>",
              "Only maps covering the same geographic area as the first ",
              "uploaded map can be analysed together. Please upload a map of ",
              "the same location."
            ),
            location_issue
          )),
          easyClose = FALSE
        ))

        return()
      }

      ## A raster whose grid differs from the first uploaded map cannot be

      ## overlaid on it. Rather than hard-failing, offer to align (warp) it onto

      ## the reference grid, or cancel the upload.

      mismatch <- grid_mismatch_reasons(map, reference_stars)

      if (!is.null(mismatch)) {
        ## A differing CRS cannot be repaired by alignment: warping would

        ## reproject the map, changing what its cell values mean. Say so in the

        ## dialog and offer only Cancel, so the user is not invited to click a

        ## button that would produce a mislabelled layer.

        crs_issue <- crs_mismatch_reason(map, reference_stars)

        if (!is.null(crs_issue)) {
          removeModal()

          close_map_progress(record_id)

          progress_complete <- TRUE

          v$pending_map_upload <- NULL

          session$sendCustomMessage("resetFileInput", "map_upload_file")

          showModal(modalDialog(
            title = "Map coordinate system does not match",
            HTML(sprintf(
              paste0(
                "This map cannot be added or aligned because it has <b>%s</b>.",
                "<br/><br/>",
                "This map is in <b>%s</b> while the first uploaded map is in ",
                "<b>%s</b>. Maps must share the exact same CRS to be analysed ",
                "together, and a CRS difference cannot be fixed by alignment ",
                "(resampling cannot change a projection).<br/><br/>",
                "Please upload a map already in the same coordinate system as ",
                "the first map."
              ),
              crs_issue,
              crs_label(map),
              crs_label(reference_stars)
            )),
            easyClose = FALSE
          ))

          return()
        }

        ## Warp the map onto the reference grid. The result is shown to the

        ## user together with how much of the reference it actually covers, so

        ## they can decide whether a partial overlay is acceptable rather than

        ## having the upload refused on their behalf.

        aligned <- tryCatch(
          warp_stars_to_grid(map, reference_stars),
          error = function(error) NULL
        )

        if (is.null(aligned)) {
          removeModal()

          close_map_progress(record_id)

          progress_complete <- TRUE

          v$pending_map_upload <- NULL

          session$sendCustomMessage("resetFileInput", "map_upload_file")

          showModal(modalDialog(
            title = "Map cannot be aligned",
            HTML(sprintf(
              paste0(
                "This map differs from the first uploaded map by ",
                "<b>%s</b>, and it could not be resampled onto the existing ",
                "grid, so its cell values cannot be placed on the analysis ",
                "grid.<br/><br/>",
                "Please upload a map that already matches the reference grid."
              ),
              paste(mismatch, collapse = ", ")
            )),
            easyClose = FALSE
          ))

          return()
        }

        ## Report the coverage. A map that covers the whole reference without

        ## excess is registered straight away. A map that is larger than the

        ## reference (excess cells) is held for a crop-to-footprint warning.

        ## A map that covers less than the whole reference triggers the

        ## crop-all-maps dialog.

        coverage_issue <- coverage_shortfall_reason(aligned, reference_stars)

        excess_issue   <- if (is.null(coverage_issue)) excess_coverage_reason(aligned, reference_stars) else NULL

        if (is.null(coverage_issue) && is.null(excess_issue)) {
          ## Perfect fit: map covers exactly the reference footprint.

          map <- aligned

          map_crs <- tryCatch(st_crs(map), error = function(e) NULL)
        } else if (!is.null(excess_issue)) {
          ## Uploaded map is wider than the reference: warn and offer to crop

          ## it down to the reference's exact non-NA pixel footprint. Only the

          ## incoming map is trimmed; the catalog maps are not changed.

          v$pending_map_upload <- list(
            record_id      = record_id,
            map            = aligned,
            sf_data        = sf_data,
            shp_saved_path = shp_saved_path,
            year           = map_year,
            label          = map_label,
            type           = input$map_upload_type,
            file           = file,
            coverage       = excess_issue,
            needs_ref_crop = TRUE
          )

          close_map_progress(record_id)

          showModal(modalDialog(
            title = "Uploaded map extends beyond the reference map",
            HTML(sprintf(
              paste0(
                "This map differs from the first uploaded map by ",
                "<b>%s</b>.<br/><br/>",
                "After aligning it to the reference grid, <b>%s</b>.",
                "<br/><br/>",
                "<b>The cells outside the reference map's data footprint will ",
                "be removed from this map.</b> The maps already in the catalog ",
                "will <u>not</u> be changed. The preview below shows the ",
                "overlap (red = kept, blue = trimmed away)."
              ),
              paste(mismatch, collapse = ", "),
              excess_issue
            )),
            plotOutput("coverage_preview", height = "260px"),
            easyClose = FALSE,
            footer = tagList(
              actionButton("cancel_map_coverage", "Cancel"),
              actionButton("confirm_map_coverage", "Crop and upload", class = "btn-primary")
            )
          ))

          return()
        } else {
          ## Partial coverage: the incoming map does not reach the full reference

          ## footprint, so every catalog map must also be cropped to the area

          ## all maps have in common.

          shared <- intersect_extents(
            stars_extent(aligned),
            catalog_common_extent()
          )

          v$pending_map_upload <- list(
            record_id      = record_id,
            map            = aligned,
            sf_data        = sf_data,
            shp_saved_path = shp_saved_path,
            year           = map_year,
            label          = map_label,
            type           = input$map_upload_type,
            file           = file,
            coverage       = coverage_issue,
            shared_extent  = shared
          )

          close_map_progress(record_id)

          showModal(modalDialog(
            title = "Map is not aligned: upload will crop all maps",
            HTML(sprintf(
              paste0(
                "This map differs from the first uploaded map by ",
                "<b>%s</b>.<br/><br/>",
                "After aligning it onto the existing grid, <b>%s</b>.",
                "<br/><br/>",
                "<b>If you continue, this map and <u>every previously ",
                "uploaded map</u> will be cropped to the intersection area ",
                "shown below</b> - the shared (overlapping) region. Cells ",
                "outside that intersection will be removed from all maps and ",
                "left out of the analysis."
              ),
              paste(mismatch, collapse = ", "),
              coverage_issue
            )),
            plotOutput("coverage_preview", height = "260px"),
            easyClose = FALSE,
            footer = tagList(
              actionButton("cancel_map_coverage", "Cancel"),
              actionButton("confirm_map_coverage", "Crop and upload", class = "btn-primary")
            )
          ))

          return()
        }
      }

      crs <<- map_crs
    }

    finalize_map_registration(
      record_id = record_id,
      map = map,
      sf_data = sf_data,
      shp_saved_path = shp_saved_path,
      map_year = map_year,
      map_label = map_label,
      is_landcover = is_landcover,
      file = file,
      progress = progress
    )

    progress_complete <- TRUE
  })

  ## Register a warped map the user accepted after a coverage warning.

  ## Two cases are handled:

  ##   needs_ref_crop = TRUE  â†’ incoming map is wider than the reference;

  ##                           only the new map is trimmed to the reference's

  ##                           exact non-NA pixel footprint. Catalog unchanged.

  ##   needs_ref_crop = FALSE/NULL â†’ incoming map covers less than the whole

  ##                           reference; every catalog map AND the new map

  ##                           are cropped to their common intersection.

  observeEvent(input$confirm_map_coverage, {
    pending <- isolate(v$pending_map_upload)

    req(pending)

    removeModal()

    v$pending_map_upload <- NULL

    if (isTRUE(pending$needs_ref_crop)) {
      ## The uploaded map extends beyond the reference's data footprint.

      ## Clip it to the reference's exact non-NA pixel mask; the catalog

      ## maps are not affected.

      reference <- first_map_reference_stars()

      cropped_map <- if (!is.null(reference)) {
        crop_to_reference_footprint(pending$map, reference)
      } else {
        pending$map
      }
    } else {
      ## The uploaded map does not cover the full reference footprint.

      ## Crop every registered map AND the incoming one to the area they all

      ## share, so the catalog is left fully aligned.

      cropped_map <- crop_catalog_to_intersection(pending$map)
    }

    ## Geometry may have changed (the map was shrunk), so the stored sf layer

    ## that accompanies a shapefile upload is cropped to the same area to keep

    ## the vector and raster footprints consistent.

    sf_data <- pending$sf_data

    if (!is.null(sf_data) && inherits(sf_data, "sf")) {
      box <- tryCatch(sf::st_bbox(cropped_map), error = function(e) NULL)

      if (!is.null(box)) {
        sf_wgs84 <- tryCatch(suppressWarnings(sf::st_crop(sf::st_transform(sf_data, 4326), box)),
                             error = function(e) NULL)

        if (!is.null(sf_wgs84) && nrow(sf_wgs84) > 0)

          sf_data <- sf_wgs84
      }
    }

    finalize_map_registration(
      record_id = pending$record_id,
      map = cropped_map,
      sf_data = sf_data,
      shp_saved_path = pending$shp_saved_path,
      map_year = pending$year,
      map_label = pending$label,
      type = pending$type,
      file = pending$file,
      progress = NULL
    )
  })

  ## Drop a warped map whose partial coverage the user declined.

  observeEvent(input$cancel_map_coverage, {
    v$pending_map_upload <- NULL

    removeModal()

    session$sendCustomMessage("resetFileInput", "map_upload_file")
  })

  ## The intersection preview shown in the confirmation dialog. Reads the parked

  ## pending upload reactively (no isolate) so the plot re-renders whenever

  ## v$pending_map_upload is updated - e.g. when the overflow-crop dialog

  ## replaces the initial 'Map details' modal or when the align flow sets a

  ## new pending upload after the previous one was cleared.

  output$coverage_preview <- renderPlot({
    pending <- v$pending_map_upload

    req(pending)

    drawn <- plot_intersection_preview(pending$map)

    if (is.null(drawn)) {
      plot.new()

      title("Map intersection unavailable")
    }
  }, bg = "transparent")

  ## ===========================================================================
  ## build_scenario_zone_sf()
  ## Build the zone polygon sf for a scenario's set of zonation maps.
  ## Called at scenario generation time (result cached in scenario$zone_sf) and
  ## as a fallback inside show_zonation_map when the cache is missing.
  ##
  ## Args:
  ##   zone_map_ids : character vector of map catalog record IDs
  ##   map_catalog  : v$map_catalog
  ##   zone_legends : v$zone_legends (named list of per-map legend data frames)
  ##   ab_zones     : v$abacus_data$zone (composite zone table, may be NULL)
  ##
  ## Returns an sf with columns zone_id (integer) and label (character),
  ## projected to WGS-84 (EPSG:4326).  NULL on failure.
  ## ===========================================================================
  build_scenario_zone_sf <- function(zone_map_ids, map_catalog, zone_legends, ab_zones) {
    ## ---- inner helpers --------------------------------------------------------
    normalise_zone_sf_inner <- function(sf_obj) {
      if (is.null(sf_obj) || !inherits(sf_obj, "sf") || nrow(sf_obj) == 0) return(NULL)
      if (!"zone_id" %in% names(sf_obj)) {
        geom_col <- attr(sf_obj, "sf_column")
        non_geom <- setdiff(names(sf_obj), geom_col)
        id_col   <- intersect(c("ID", "id", "zone_id", "zone", "Zone", "ZONE", "GRIDCODE"),
                              non_geom)[1]
        if (!is.na(id_col)) {
          names(sf_obj)[names(sf_obj) == id_col] <- "zone_id"
        } else if (length(non_geom) > 0) {
          ## Raster-derived sf: first non-geometry column = cell values
          names(sf_obj)[names(sf_obj) == non_geom[1]] <- "zone_id"
        } else {
          sf_obj$zone_id <- seq_len(nrow(sf_obj))
        }
      }
      sf_obj$zone_id <- suppressWarnings(as.integer(as.character(sf_obj$zone_id)))
      sf_obj[!is.na(sf_obj$zone_id), ]
    }

    get_one_zone_sf <- function(rid) {
      rec <- map_catalog[[rid]]
      ## (a) pre-built at upload time (validated against raster if stars present)
      if (!is.null(rec$zone_sf) && inherits(rec$zone_sf, "sf") && nrow(rec$zone_sf) > 0) {
        if (!is.null(rec$stars)) {
          m_check <- if (inherits(rec$stars, "stars_proxy")) stars::st_as_stars(rec$stars) else rec$stars
          r_vals <- unique(as.vector(m_check[[1]]))
          r_vals <- r_vals[!is.na(r_vals)]
          if (all(rec$zone_sf$zone_id %in% r_vals)) return(rec$zone_sf)
        } else {
          return(rec$zone_sf)
        }
      }
      ## (b) shp_file on disk
      if (!is.null(rec$shp_file) && file.exists(rec$shp_file)) {
        out <- tryCatch(sf::st_read(rec$shp_file, quiet = TRUE), error = function(e) NULL)
        if (!is.null(out) && nrow(out) > 0) return(normalise_zone_sf_inner(out))
      }
      ## (c) in-memory sf
      if (!is.null(rec$sf) && inherits(rec$sf, "sf") && nrow(rec$sf) > 0)
        return(normalise_zone_sf_inner(rec$sf))
      ## (d) vectorise raster
      raster_src <- rec$stars
      if (!is.null(raster_src)) {
        if (inherits(raster_src, "sf")) return(normalise_zone_sf_inner(raster_src))
        out <- tryCatch({
          m    <- if (inherits(raster_src, "stars_proxy")) stars::st_as_stars(raster_src) else raster_src
          poly <- sf::st_as_sf(m, as_points = FALSE, merge = TRUE)
          normalise_zone_sf_inner(poly)
        }, error = function(e) NULL)
        if (!is.null(out) && nrow(out) > 0) return(out)
      }
      NULL
    }

    label_from_legend <- function(leg, zid) {
      if (!is.null(leg) && is.data.frame(leg) &&
          "zone_id" %in% names(leg) && "label" %in% names(leg)) {
        m <- leg$label[leg$zone_id == zid]
        if (length(m) > 0 && !is.na(m[1]) && nzchar(m[1])) return(as.character(m[1]))
      }
      paste0("Zone ", zid)
    }

    color_from_legend <- function(leg, zid, default = NULL) {
      if (!is.null(leg) && is.data.frame(leg) &&
          "zone_id" %in% names(leg) && "color" %in% names(leg)) {
        m <- leg$color[leg$zone_id == zid]
        if (length(m) > 0 && !is.na(m[1]) && nzchar(as.character(m[1])))
          return(as.character(m[1]))
      }
      default
    }

    blend_hex_colors <- function(hex_vec) {
      hex_vec <- hex_vec[!is.na(hex_vec) & nzchar(hex_vec)]
      if (length(hex_vec) == 0) return("#808080")
      if (length(hex_vec) == 1) return(hex_vec[1])
      rgb_mat <- tryCatch(grDevices::col2rgb(hex_vec), error = function(e) NULL)
      if (is.null(rgb_mat) || ncol(rgb_mat) == 0) return(hex_vec[1])
      avg <- rowMeans(rgb_mat)
      grDevices::rgb(avg[1], avg[2], avg[3], maxColorValue = 255)
    }

    ## ---- resolve per-map sf --------------------------------------------------
    zone_map_ids <- setdiff(zone_map_ids, "none")
    if (length(zone_map_ids) == 0 || is.null(map_catalog)) return(NULL)

    per_sf <- lapply(zone_map_ids, get_one_zone_sf)
    names(per_sf) <- zone_map_ids
    per_sf <- Filter(function(x) !is.null(x) && nrow(x) > 0, per_sf)

    if (length(per_sf) == 0) return(NULL)

    ## ---- single map ----------------------------------------------------------
    if (length(per_sf) == 1L) {
      rid <- names(per_sf)[1]
      zs  <- per_sf[[1L]]
      ## Validate against raster if stars is available
      rec <- map_catalog[[rid]]
      if (!is.null(rec) && !is.null(rec$stars)) {
        m_chk <- if (inherits(rec$stars, "stars_proxy")) stars::st_as_stars(rec$stars) else rec$stars
        r_u <- unique(as.vector(m_chk[[1]]))
        r_u <- r_u[!is.na(r_u)]
        if (!all(zs$zone_id %in% r_u)) {
          poly <- sf::st_as_sf(m_chk, as_points = FALSE, merge = TRUE)
          zs <- normalise_zone_sf_inner(poly)
        }
      }
      ## Aggregate by zone_id so each zone is exactly 1 multipolygon feature
      if (!is.null(zs) && nrow(zs) > 0) {
        zs <- aggregate(zs["zone_id"], by = list(zone_id = zs$zone_id), FUN = head, 1)[, "zone_id", drop = FALSE]
        zs$zone_id <- as.integer(zs$zone_id)
      }
      leg <- if (!is.null(zone_legends)) zone_legends[[rid]] else NULL
      zs$label <- vapply(zs$zone_id, function(zid) label_from_legend(leg, zid), character(1))
      zs$color <- vapply(zs$zone_id, function(zid) color_from_legend(leg, zid, default = "#440154"), character(1))
      if (!sf::st_is_longlat(zs) && !is.na(sf::st_crs(zs))) {
        zs <- sf::st_transform(zs, 4326)
      }
      return(zs)
    }
    ## ---- multiple maps: fast raster overlay or vector intersection ----------
    tryCatch({
      has_all_stars <- all(vapply(zone_map_ids, function(rid) {
        rec <- map_catalog[[rid]]
        !is.null(rec) && !is.null(rec$stars)
      }, logical(1)))

      if (has_all_stars) {
        ## FAST PATH: Combine on raster grid in memory (seconds vs minutes), then vectorise once!
        m1 <- map_catalog[[zone_map_ids[1]]]$stars
        if (inherits(m1, "stars_proxy")) m1 <- stars::st_as_stars(m1)

        z_vectors <- list(as.integer(m1[[1]]))
        for (k in 2:length(zone_map_ids)) {
          mk <- map_catalog[[zone_map_ids[k]]]$stars
          if (inherits(mk, "stars_proxy")) mk <- stars::st_as_stars(mk)
          mk_aligned <- align_stars_to_reference(mk, m1)
          z_vectors[[k]] <- as.integer(mk_aligned[[1]])
        }

        z_df <- as.data.frame(z_vectors)
        names(z_df) <- paste0("zone_", zone_map_ids)

        valid <- apply(z_df, 1, function(r) any(!is.na(r) & r != 0L))
        combo_unique <- unique(z_df[valid, , drop = FALSE])
        ord_args <- lapply(names(combo_unique), function(col) combo_unique[[col]])
        combo_unique <- combo_unique[do.call(order, ord_args), , drop = FALSE]
        rownames(combo_unique) <- NULL
        combo_unique$composite_id <- seq_len(nrow(combo_unique))

        if (!is.null(ab_zones) && is.data.frame(ab_zones) && "zone_id" %in% names(ab_zones)) {
          valid_ids <- ab_zones$zone_id[ab_zones$zone_id != 0L]
          if (length(valid_ids) >= nrow(combo_unique))
            combo_unique$composite_id <- valid_ids[seq_len(nrow(combo_unique))]
        }

        map_rids <- zone_map_ids
        combo_unique$composite_label <- apply(
          combo_unique[, paste0("zone_", map_rids), drop = FALSE], 1,
          function(row) {
            parts <- mapply(
              function(rid, zid) label_from_legend(
                if (!is.null(zone_legends)) zone_legends[[rid]] else NULL, zid),
              map_rids, as.integer(row))
            paste(parts, collapse = " - ")
          })

        key_all <- do.call(paste, c(lapply(names(z_df), function(col) z_df[[col]][valid]), sep = "\001"))
        key_u   <- do.call(paste, c(lapply(paste0("zone_", map_rids), function(col) combo_unique[[col]]), sep = "\001"))
        m_idx   <- match(key_all, key_u)

        comb_v <- rep(NA_integer_, nrow(z_df))
        comb_v[valid] <- combo_unique$composite_id[m_idx]

        comb_stars <- m1
        comb_stars[[1]] <- matrix(comb_v, nrow = nrow(m1[[1]]), ncol = ncol(m1[[1]]))

        ## If large raster, downsample slightly for responsive vectorisation & rendering
        n_pix <- prod(dim(comb_stars)[1:2])
        down_factor <- if (n_pix > 2000000) 2L else 1L
        comb_stars_disp <- if (down_factor > 1L) stars::st_downsample(comb_stars, n = down_factor) else comb_stars

        comb_sf <- sf::st_as_sf(comb_stars_disp, as_points = FALSE, merge = TRUE)
        names(comb_sf)[1] <- "zone_id"
        comb_sf$zone_id <- suppressWarnings(as.integer(as.character(comb_sf$zone_id)))
        comb_sf <- comb_sf[!is.na(comb_sf$zone_id), ]

        ## Aggregate into 1 MULTIPOLYGON per zone_id for responsive leaflet rendering
        comb_sf <- aggregate(comb_sf["zone_id"], by = list(zone_id = comb_sf$zone_id), FUN = head, 1)[, "zone_id", drop = FALSE]
        comb_sf$zone_id <- as.integer(comb_sf$zone_id)
        combo_unique$composite_color <- apply(
          combo_unique[, paste0("zone_", map_rids), drop = FALSE], 1,
          function(row) {
            c_parts <- mapply(
              function(rid, zid) color_from_legend(
                if (!is.null(zone_legends)) zone_legends[[rid]] else NULL, zid),
              map_rids, as.integer(row), SIMPLIFY = FALSE)
            blend_hex_colors(unlist(c_parts))
          })

        comb_sf$label <- combo_unique$composite_label[match(comb_sf$zone_id, combo_unique$composite_id)]
        comb_sf$color <- combo_unique$composite_color[match(comb_sf$zone_id, combo_unique$composite_id)]

        if (!sf::st_is_longlat(comb_sf) && !is.na(sf::st_crs(comb_sf))) {
          comb_sf <- sf::st_transform(comb_sf, 4326)
        }
        return(comb_sf)
      }

      ## SLOW FALLBACK (vector shapefiles): Project each layer to WGS-84 and intersect
      layers <- lapply(seq_along(per_sf), function(k) {
        poly     <- per_sf[[k]]
        colname  <- paste0("zone_id_", k)
        geom_col <- attr(poly, "sf_column")
        poly     <- poly[, c("zone_id", geom_col)]
        names(poly)[names(poly) == "zone_id"] <- colname
        poly <- sf::st_make_valid(poly)
        if (!sf::st_is_longlat(poly) && !is.na(sf::st_crs(poly)))
          poly <- sf::st_transform(poly, 4326)
        poly
      })

      ## Pairwise intersection
      combined <- layers[[1L]]
      for (k in 2:length(layers)) {
        combined <- suppressWarnings(sf::st_intersection(combined, layers[[k]]))
        combined <- combined[!sf::st_is_empty(combined), ]
      }

      ## Identify zone_id columns (sf::st_intersection may add suffixes on clashes)
      zcols <- grep("^zone_id_", names(combined), value = TRUE)
      if (length(zcols) == 0) stop("No zone_id columns after intersection")

      sf_df <- sf::st_drop_geometry(combined)[, zcols, drop = FALSE]

      combo_unique <- unique(sf_df)
      has_valid    <- apply(combo_unique, 1, function(r) any(!is.na(r) & r != 0L))
      combo_unique <- combo_unique[has_valid, , drop = FALSE]
      if (nrow(combo_unique) == 0) return(per_sf[[1L]])

      ord_args     <- lapply(zcols, function(col) combo_unique[[col]])
      combo_unique <- combo_unique[do.call(order, ord_args), , drop = FALSE]
      rownames(combo_unique) <- NULL
      combo_unique$composite_id <- seq_len(nrow(combo_unique))

      if (!is.null(ab_zones) && is.data.frame(ab_zones) && "zone_id" %in% names(ab_zones)) {
        valid_ids <- ab_zones$zone_id[ab_zones$zone_id != 0L]
        if (length(valid_ids) >= nrow(combo_unique))
          combo_unique$composite_id <- valid_ids[seq_len(nrow(combo_unique))]
      }

      ## Build composite label from per-map legends for each combo row
      map_rids <- names(per_sf)
      combo_unique$composite_label <- apply(
        combo_unique[, zcols, drop = FALSE], 1,
        function(row) {
          parts <- mapply(
            function(rid, zid) label_from_legend(
              if (!is.null(zone_legends)) zone_legends[[rid]] else NULL, zid),
            map_rids, as.integer(row))
          paste(parts, collapse = " - ")
        })

      ## Match polygons -> composite_id and composite_label
      key_poly  <- do.call(paste, c(lapply(zcols, function(col) sf_df[[col]]),        sep = "\001"))
      key_combo <- do.call(paste, c(lapply(zcols, function(col) combo_unique[[col]]), sep = "\001"))
      m_idx     <- match(key_poly, key_combo)

      combo_unique$composite_color <- apply(
        combo_unique[, zcols, drop = FALSE], 1,
        function(row) {
          c_parts <- mapply(
            function(rid, zid) color_from_legend(
              if (!is.null(zone_legends)) zone_legends[[rid]] else NULL, zid),
            map_rids, as.integer(row), SIMPLIFY = FALSE)
          blend_hex_colors(unlist(c_parts))
        })

      combined$zone_id <- combo_unique$composite_id[m_idx]
      combined$label   <- combo_unique$composite_label[m_idx]
      combined$color   <- combo_unique$composite_color[m_idx]
      combined[!is.na(combined$zone_id), ]
    }, error = function(e) {
      message("build_scenario_zone_sf intersection failed: ", e$message,
              " - falling back to first map.")
      per_sf[[1L]]
    })
  }

    finalize_map_registration <- function(record_id, map, sf_data = NULL,
                                         shp_saved_path = NULL, map_year = NULL,
                                         map_label = NULL, is_landcover = TRUE,
                                         file = NULL, progress = NULL,
                                         type = NULL) {
    map_type <- if (!is.null(type)) type else if (is_landcover) "Land cover map" else "Zonation map"

    is_landcover <- identical(map_type, "Land cover map")

    if (!is.null(progress))

      progress$set(0.65, message = "Rendering map", detail = "Preparing map layers")

    freq_df <- get_raster_freq_df(map, NULL)

    record <- list(
      id = record_id,
      type = map_type,
      year = map_year,
      label = map_label,
      name = if (is.null(file)) NULL else basename(file$name),
      file = file,
      stars = map,
      sf = sf_data,
      shp_file = shp_saved_path,
      freq_df = freq_df
    )

    if (!is_landcover) {
      zm <- freq_df

      z_ids <- zm$id

      z_areas <- zm$area

      init_pal <- "default"

      init_cols <- get_legend_set_colors(init_pal, length(z_ids))

      init_df <- data.frame(
        zone_id = z_ids,
        color = init_cols,
        label = paste0("Zone_", z_ids),
        description = "",
        area = z_areas,
        stringsAsFactors = FALSE
      )

      v$zone_legends[[record_id]] <- init_df

      v$zone_palette_sets[[record_id]] <- init_pal

      record$zone_df <- init_df

      record$palette_set <- init_pal

      ## Pre-build individual zone polygon sf for use in the zone selector map.
      ## Done here (upload time) so the selector can show it instantly without
      ## re-vectorising on every button click.
      record$zone_sf <- tryCatch({
        src <- if (!is.null(sf_data) && inherits(sf_data, "sf") && nrow(sf_data) > 0) {
          sf_data
        } else if (!is.null(shp_saved_path) && file.exists(shp_saved_path)) {
          sf::st_read(shp_saved_path, quiet = TRUE)
        } else if (!is.null(map)) {
          m <- if (inherits(map, "stars_proxy")) stars::st_as_stars(map) else map
          sf::st_as_sf(m, as_points = FALSE, merge = TRUE)
        } else NULL
        if (!is.null(src) && inherits(src, "sf") && nrow(src) > 0) {
          ## Normalise zone_id column
          if (!"zone_id" %in% names(src)) {
            geom_col <- attr(src, "sf_column")
            non_geom <- setdiff(names(src), geom_col)
            id_col   <- intersect(c("ID", "id", "zone_id", "zone", "Zone", "ZONE", "GRIDCODE"), non_geom)[1]
            if (!is.na(id_col)) {
              names(src)[names(src) == id_col] <- "zone_id"
            } else if (length(non_geom) > 0) {
              ## Raster-derived sf: first non-geometry column contains the cell values
              names(src)[names(src) == non_geom[1]] <- "zone_id"
            } else {
              src$zone_id <- seq_len(nrow(src))
            }
          }
          src$zone_id <- suppressWarnings(as.integer(as.character(src$zone_id)))
          src <- src[!is.na(src$zone_id), ]
          if (!sf::st_is_longlat(src) && !is.na(sf::st_crs(src)))
            src <- tryCatch(sf::st_transform(src, 4326), error = function(e) src)
          src
        } else NULL
      }, error = function(e) { message("zone_sf pre-build failed: ", e$message); NULL })

      register_zone_map_handlers(record_id)

      if (is.null(v$zone_list_df)) {
        v$zone_list_df <- init_df

        zone_legend_pal <<- colorFactor(init_df$color, init_df$zone_id, na.color = NA)
      }
    }

    records <- isolate(v$map_catalog)

    # Build and store the study area boundary once, based on the first uploaded

    # LAND COVER map. A zonation map covers (part of) the study area but does not

    # define it, so uploading one first must not populate the Relative location

    # and General map info cards.

    if (is_landcover && (length(records) == 0 || is.null(v$study_area_boundary))) {
      boundary_sf <- tryCatch(
        create_map_boundary(
          map      = map,
          sf_data  = sf_data,
          name     = if (!is.null(file)) basename(file$name) else record_id,
          type     = map_type,
          year     = if (!is.null(map_year)) map_year else ""
        ),
        error = function(e) {
          message("create_map_boundary failed: ", conditionMessage(e))

          NULL
        }
      )

      v$study_area_boundary <- boundary_sf

      update_study_area_location(boundary_sf)
    }

    records <- c(records, stats::setNames(list(record), record_id))

    v$map_catalog <- records

    ## The preview for this card is registered by the renderPlot() below, which

    ## reads the record back out of v$map_catalog. There is no separate

    ## register_map_preview() helper: earlier code called one that was never

    ## defined, which raised "could not find function register_map_preview" and

    ## aborted every upload.

    output[[paste0("uploaded_map_preview_", record_id)]] <- renderPlot({
      ## Depend on the catalog version so that edits made to a registered map

      ## (e.g. cropping the catalog to a shared intersection) invalidate and

      ## re-render every preview, not just newly added ones.

      v$map_catalog_version

      current_record <- isolate(v$map_catalog[[record_id]])

      if (is.null(current_record))

        current_record <- record

      current_progress <- map_progress[[record_id]]

      progress_render_scheduled <- FALSE

      if (!is.null(current_progress)) {
        current_progress$set(0.75, message = "Rendering map", detail = "Building map display")

        on.exit(if (!progress_render_scheduled) close_map_progress(record_id), add = TRUE)
      }

      is_zone <- identical(current_record$type, "Zonation map")

      # Get the stars object directly

      stars_map <- current_record$stars

      # Create color palette

      palette <- if (is_zone && !is.null(v$zone_legends[[record_id]])) {
        zdf <- v$zone_legends[[record_id]]

        colorFactor(zdf$color, zdf$zone_id, na.color = NA)
      } else if (!is_zone && !is.null(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
        lcdf <- v$lc_list_df

        colorFactor(lcdf$color, lcdf$lc_id, na.color = NA)
      } else {
        map_preview_palette(current_record$stars, current_record$sf)
      }

      # Plot using stars::plot (S3 method)

      if (!is.null(stars_map)) {
        # Establish reactive dependency on fullscreen state

        # bslib auto-creates <card_id>_full_screen (with underscore), custom JS creates <card_id>_fullscreen (without)

        fs_id_bslib <- paste0("uploaded_", record_id, "_card_full_screen")

        fs_id_custom <- paste0("uploaded_", record_id, "_card_fullscreen")

        is_fs <- (!is.null(input[[fs_id_bslib]]) && isTRUE(input[[fs_id_bslib]])) ||
                 (!is.null(input[[fs_id_custom]]) && isTRUE(input[[fs_id_custom]]))

        if (!is.null(palette)) {
          # Get colors for the raster values

          # Extract values from stars object

          if (inherits(stars_map, "stars_proxy")) {
            # For stars_proxy, read the data first

            stars_map_data <- stars::st_as_stars(stars_map)

            vals <- stars_map_data[[1]]
          } else {
            vals <- stars_map[[1]]
          }

          ids <- if (!is.null(current_record$freq_df) && nrow(current_record$freq_df) > 0) {
            sort(unique(as.numeric(as.character(current_record$freq_df$id))))
          } else {
            sort(unique(as.vector(vals)))
          }

          ids <- ids[!is.na(ids)]

          if (length(ids) > 0) {
            # palette is a colorFactor function - get colors for each ID
            cols <- palette(ids)

            # For discrete plotting, use breaks
            breaks <- c(ids - 0.5, max(ids) + 0.5)

            suppressMessages(plot(stars_map, col = cols, breaks = breaks, main = "", axes = FALSE, key.pos = NULL, reset = FALSE, downsample = if (is_fs) FALSE else TRUE))

            # Add legend if in full-screen mode

            if (is_fs && !is.null(palette)) {
              # Get labels for legend

              labels <- NULL

              if (is_zone && !is.null(v$zone_legends[[record_id]])) {
                zdf <- v$zone_legends[[record_id]]

                labels <- stats::setNames(zdf$label, as.character(zdf$zone_id))
              } else if (!is.null(v$lc_list_df)) {
                labels <- stats::setNames(v$lc_list_df$label, as.character(v$lc_list_df$lc_id))
              }

              if (!is.null(labels)) {
                legend_ids <- ids[ids %in% as.numeric(names(labels))]

                if (length(legend_ids) > 0) {
                  legend_cols <- palette(legend_ids)

                  legend_labels <- labels[as.character(legend_ids)]

                  # Compact legend: multiple columns, smaller text, less spacing

                  n_items <- length(legend_labels)

                  ncol <- if (n_items > 12) 3 else if (n_items > 6) 2 else 1

                  legend("topright", legend = legend_labels, pch = 22, pt.bg = legend_cols, col = legend_cols, pt.cex = 1.5,
                         title = "Land Cover", bty = "n", cex = 1, bg = "white", xpd = NA,
                         ncol = ncol, y.intersp = 0.7, x.intersp = 0.5, inset = c(0.01, 0.01))
                }
              }
            }
          } else {
            suppressMessages(plot(stars_map, main = "", axes = FALSE, key.pos = NULL, reset = FALSE))
          }
        } else {
          suppressMessages(plot(stars_map, main = "", axes = FALSE, key.pos = NULL, reset = FALSE))
        }
      }

      if (!is.null(current_progress)) {
        current_progress$set(0.9, detail = "Sending map to browser")

        session$onFlushed(function() {
          if (identical(map_progress[[record_id]], current_progress)) {
            current_progress$set(1, detail = "Map ready")

            close_map_progress(record_id)
          }
        }, once = TRUE)

        progress_render_scheduled <- TRUE
      }
    })

    new_card_ui <- build_map_card_ui(record)

    insertUI(
      selector = "#uploaded_cards_container",
      where = "beforeEnd",
      ui = new_card_ui,
      immediate = TRUE
    )

    register_map_remove_handler(record_id)

    v$map_catalog <- records

    v$map_catalog_version <- v$map_catalog_version + 1L

    v$next_map_id <- v$next_map_id + 1

    v$pending_map_upload <- NULL

    removeModal()

    session$sendCustomMessage("resetFileInput", "map_upload_file")

    reset_scenario()
  }

  reset_scenario <- function() {
    v$scenario_list <- list()
    final_area$sc_lc_df <- NULL
    emission_val$sc_sum <- 0
    v$selected_scenario_id <- NULL
    tryCatch({
      if (exists("scenario_catalog", mode = "function")) scenario_catalog(list())
      if (exists("selected_scenario_index", mode = "function")) selected_scenario_index("")
      if (exists("scenario_overrides", mode = "function")) scenario_overrides(list())
      if (exists("emission_data", mode = "function")) emission_data(NULL)
      if (exists("baseline_emission_data", mode = "function")) baseline_emission_data(NULL)
      if (exists("scenario_tpm_edits", mode = "function")) scenario_tpm_edits(NULL)
      if (exists("description_store", mode = "function")) description_store(list())
    }, error = function(e) NULL)
    update_scenario_select_options()
  }

  observe({
    if (is.null(v$mapz_stars))

      return()

    ## Normalise the selected zonation record id to a single non-NA string.

    ## input$analysis_zone_map is NULL until the selector is populated, and

    ## nzchar(NULL) returns logical(0), so `if (nzchar(zone_id) && ...)` raised

    ## "attempt to select less than one element in get1index" and aborted the

    ## observer - which broke the first zonation map upload.

    zone_id <- input$analysis_zone_map

    zone_id <- if (is.null(zone_id) || length(zone_id) == 0) "" else as.character(zone_id[[1]])

    if (is.na(zone_id)) zone_id <- ""

    if (nzchar(zone_id) && !is.null(v$zone_legends[[zone_id]])) {
      v$zone_list_df <- v$zone_legends[[zone_id]]
    } else if (is.null(v$zone_list_df)) {
      zm <- get_raster_freq_df(v$mapz_stars, v$mapz_sf)

      znew_id <- zm$id

      df <- data.frame(
        zone_id = znew_id,
        color = get_legend_set_colors("default", length(znew_id)),
        label = paste0("Zone_", znew_id),
        description = "",
        area = zm$area,
        stringsAsFactors = FALSE
      )

      v$zone_list_df <- df

      if (nzchar(zone_id)) {
        v$zone_legends[[zone_id]] <- df

        v$zone_palette_sets[[zone_id]] <- "default"
      }
    }

    df <- isolate(v$zone_list_df)

    zone_legend_pal <<-
      colorFactor(df$color, df$zone_id, na.color = NA)

    refresh_lc_matrix()
  })

  ## Sentinel for "no second map" in the changes tab. Chosen so it can never

  ## collide with a map record id (those are generated by the upload flow).

  matrix_map_none <- "__none__"

  ## Tracks whether the user has explicitly touched the "After" selector, so

  ## the automatic default does not override a deliberate "None" choice.

  ## `matrix_map2_sent` holds the last value pushed by the server, so the echo

  ## of a programmatic update is not mistaken for a user choice.

  matrix_map2_user_set <- FALSE

  matrix_map2_sent <- NULL

  ## Land cover change matrix computed for the maps chosen in the

  ## "Land cover changes" tab (defaults to the analysis maps).

  ## The pair is always ordered from the lower year to the higher year,
  ## so "Before" is never later than "After".

  order_map_pair <- function(id1, id2, records) {
    ## id2 may be the "None" sentinel (single-map mode) - never swap in that

    ## case, so the chosen map stays in the "Before" slot.

    if (identical(id2, matrix_map_none))

      return(list(id1 = id1, id2 = id2))

    r1 <- records[[id1]]

    r2 <- records[[id2]]

    if (!is.null(r1) && !is.null(r2) &&
        identical(r1$type, "Land cover map") &&
        identical(r2$type, "Land cover map") &&
        r2$year < r1$year) {
      tmp <- id1

      id1 <- id2

      id2 <- tmp
    }

    list(id1 = id1, id2 = id2)
  }

  ## Resolve the two matrix selectors. `map2` may be the "None" sentinel,
  ## in which case only `map1` is returned and the UI shows the area chart.

  matrix_selection <- reactive({
    id1 <- input$matrix_map1

    if (is.null(id1) || !nzchar(id1))

      id1 <- input$analysis_map1

    id2 <- input$matrix_map2

    if (is.null(id2) || is.na(id2))

      id2 <- matrix_map_none

    recs <- isolate(v$map_catalog)

    ## Only land cover maps participate; a zonation map in either slot is

    ## ignored so it can never produce a bogus change matrix.

    is_lc <- function(r) !is.null(r) && identical(r$type, "Land cover map")

    rec1 <- if (length(id1) == 1 && !is.na(id1) && nzchar(id1)) recs[[id1]] else NULL

    rec2 <- if (length(id2) == 1 && !is.na(id2) && nzchar(id2) &&
                 !identical(id2, matrix_map_none)) recs[[id2]] else NULL

    if (!is_lc(rec1)) rec1 <- NULL

    if (!is_lc(rec2)) rec2 <- NULL

    ## "Before" is the primary slot: if it is empty but a second map was

    ## chosen, promote that map so one selection always yields the chart.

    if (is.null(rec1) && !is.null(rec2)) {
      rec1 <- rec2

      rec2 <- NULL
    }

    if (is.null(rec1))

      return(NULL)

    ## Two identical maps is not a change matrix; treat as a single map.

    if (!is.null(rec1) && !is.null(rec2) && identical(rec1$id, rec2$id))

      rec2 <- NULL

    list(rec1 = rec1, rec2 = rec2)
  })

  ## The ordered land cover pair for the changes tab. Deliberately carries only

  ## the two maps: the zone layers are NOT bundled in here, because a reactive

  ## that mixes the pair with the zonation selection makes it ambiguous which of

  ## the two a caller is depending on. Callers that need the overlay ask

  ## active_zone_overlay() directly.

  matrix_changes <- reactive({
    sel <- matrix_selection()

    if (is.null(sel) || is.null(sel$rec1) || is.null(sel$rec2))

      return(NULL)

    recs <- isolate(v$map_catalog)

    pair <- order_map_pair(sel$rec1$id, sel$rec2$id, recs)

    list(map1 = recs[[pair$id1]]$stars,
         map2 = recs[[pair$id2]]$stars)
  })
  observe({
    sel <- matrix_selection()

    if (!is.null(sel) && !is.null(sel$rec1) && !is.null(sel$rec2)) {
      recs <- isolate(v$map_catalog)

      pair <- order_map_pair(sel$rec1$id, sel$rec2$id, recs)

      r1 <- recs[[pair$id1]]

      r2 <- recs[[pair$id2]]

      y1 <- if (!is.null(r1$year)) as.integer(r1$year) else NA_integer_

      y2 <- if (!is.null(r2$year)) as.integer(r2$year) else NA_integer_

      if (!is.na(y1) && !is.na(y2) && y2 >= y1) {
        v$map1_year <- y1

        v$map2_year <- y2

        v$map1_date <- y1

        v$map2_date <- y2

        v$period_year <- as.numeric(y2 - y1)

        return()
      }
    }

    if (!is.null(v$map1_year) && !is.null(v$map2_year)) {
      v$period_year <- as.numeric(v$map2_year - v$map1_year)
    } else if (!is.null(v$map1_date) && !is.null(v$map2_date)) {
      y1 <- suppressWarnings(as.integer(v$map1_date))

      y2 <- suppressWarnings(as.integer(v$map2_date))

      if (!is.na(y1) && !is.na(y2)) {
        v$map1_year <- y1

        v$map2_year <- y2

        v$period_year <- as.numeric(y2 - y1)
      } else {
        d1 <- tryCatch(as.Date(v$map1_date), error = function(e) NA)

        d2 <- tryCatch(as.Date(v$map2_date), error = function(e) NA)

        if (!is.na(d1) && !is.na(d2)) {
          v$map1_year <- as.integer(format(d1, "%Y"))

          v$map2_year <- as.integer(format(d2, "%Y"))

          v$period_year <- as.numeric(v$map2_year - v$map1_year)
        }
      }
    }
  })

  get_iteration_year <- function(iteration) {
    y0 <- if (!is.null(v$map1_year)) v$map1_year else if (!is.null(v$map1_date)) as.integer(v$map1_date) else 0

    y <- as.integer(y0) + round(v$period_year) * iteration

    return(y)
  }

  ### LAND COVER LIST ##############################

  update_lc_df <- function(prev_df, lc_id_list, other_id) {
    pal_set <- if (!is.null(isolate(v$lc_palette_set))) isolate(v$lc_palette_set) else "gyr"

    if (is.null(prev_df)) {
      df <- data.frame("lc_id" = lc_id_list)

      df$color <- get_legend_set_colors(pal_set, length(lc_id_list))

      df$label <- paste0("Landcover_", lc_id_list)

      df$description <- ""

      v$lc_list_df <- df

      df$c <- 0

      v$cstock_list <- df[cstock_table_def]
    } else {
      dif <- as.numeric(setdiff(lc_id_list, prev_df$lc_id))

      df <- prev_df

      df_c <- isolate(v$cstock_list)

      if (length(dif) > 0) {
        new_colors <- get_legend_set_colors(pal_set, nrow(prev_df) + length(dif))[(nrow(prev_df) + 1):(nrow(prev_df) + length(dif))]

        a_df <- data.frame(
          lc_id = dif,
          color = new_colors,
          label = paste0("Landcover_", dif),
          description = ""
        )

        df <- rbind(prev_df, a_df)

        a_df$c <- 0

        df_c <- rbind(df_c, a_df[cstock_table_def])
      }

      curr_id <- unique(c(lc_id_list, other_id))

      #remove the previous ids not within map1 and map2

      df <- df[df$lc_id %in% curr_id, ]

      df <- df[order(df$lc_id), ]

      v$lc_list_df <- df

      df_c <- df_c[df_c$lc_id %in% curr_id, ]

      df_c <- df_c[order(df_c$lc_id), ]

      v$cstock_list <- df_c[cstock_table_def]
    }

    lc_legend_pal <<-
      colorFactor(df$color, df$lc_id, na.color = NA)
  }

  observeEvent(input$lc_palette_set, {
    new_set <- input$lc_palette_set

    req(new_set)

    current_set <- if (!is.null(v$lc_palette_set)) v$lc_palette_set else "gyr"

    if (!identical(new_set, current_set)) {
      v$lc_palette_set <- new_set

      df <- v$lc_list_df

      if (!is.null(df) && nrow(df) > 0) {
        df$color <- get_legend_set_colors(new_set, nrow(df))

        v$lc_list_df <- df

        lc_legend_pal <<- colorFactor(df$color, df$lc_id, na.color = NA)
      }
    }
  }, ignoreInit = TRUE)

  #### Land cover legend list table ####

  lc_table_def <- c("lc_id", "color", "label", "description")

  lc_column <- data.frame(
    title = c("LC_ID", "Color", "Land cover", "Description"),
    type = c("numeric", "color", "text", "text"),
    render = c(NA, "square", NA, NA),
    align = c("right", "center", "left", "left"),
    width = c(NA, NA, 150, 150),
    readOnly = c(T, F, F, F)
  )

  output$lc_map_list <- renderExcel({
    if (is.null(v$lc_list_df))

      return()

    excelTable(
      data = v$lc_list_df,
      columns = lc_column,
      tableOverflow = T,
      tableWidth = "100%",
      allowDeleteColumn = F,
      allowRenameColumn = F,
      allowInsertColumn = F,
      allowDeleteRow = F,
      allowInsertRow = F,
      rowDrag = F,
      minDimensions = c(NA, 1),
      tableHeight = "400px",
      autoIncrement = T,
      csvFileName = "landcover_table",
      includeHeadersOnDownload = T
    )
  })

  #### Land cover editing ####

  observeEvent(input$lc_map_list, {
    df_old <- isolate(v$lc_list_df)

    inp <- input$lc_map_list

    df_input <- excel_to_R(inp)

    names(df_input) <- lc_table_def

    df_input <- transform(df_input, lc_id = as.numeric(lc_id))

    v$lc_list_df <- df_input

    newcol <- setdiff(df_old$color, df_input$color)

    if (length(newcol) > 0) {
      lc_legend_pal <<-
        colorFactor(df_input$color, df_input$lc_id, na.color = NA)

      # With renderPlot, maps auto-update when reactives change
    }
  })

  is_color_code <- function(x) {
    res <- try(col2rgb(x), silent = TRUE)

    return(!"try-error" %in% class(res))
  }

  #### Land cover data upload ####

  observeEvent(input$lc_input_upload, {
    #TODO: if(!check_file_csv()) return()

    f <- input$lc_input_upload

    df_inp <- read.csv(f$datapath)

    if (nrow(df_inp) == 0)

      return()

    if (ncol(df_inp) < 2)

      return()

    if (is.na(as.integer(as.character(df_inp[1, 1]))))

      return()

    df <- data.frame("lc_id" = as.integer(as.character(df_inp[, 1])))

    if (is_color_code(df_inp[1, 2])) {
      df$color <- df_inp[, 2]

      if (ncol(df_inp) >= 3) {
        df$label <- df_inp[, 3]
      } else {
        df$label <- paste0("Landcover_", df_inp[, 1])
      }

      df$description <- ""

      if (ncol(df_inp) >= 4) {
        df$description <- df_inp[, 4]
      }
    } else {
      pal_set <- if (!is.null(isolate(v$lc_palette_set))) isolate(v$lc_palette_set) else "gyr"

      df$color <- get_legend_set_colors(pal_set, nrow(df_inp))

      df$label <- df_inp[, 2]

      df$description <- ""

      if (ncol(df_inp) >= 3) {
        df$description <- df_inp[, 3]
      }
    }

    old_df <- isolate(v$lc_list_df)

    valid_id <- intersect(old_df$lc_id, df$lc_id)

    if (length(valid_id) > 0) {
      old_df[old_df$lc_id %in% valid_id, ] <- df[df$lc_id %in% valid_id, ]

      v$lc_list_df <- old_df

      lc_legend_pal <<- colorFactor(old_df$color, old_df$lc_id, na.color = NA)
    }

    toggle_popover("lc_input_pop", F)
  })

  ## Download land cover legend as CSV
  output$download_lc_legend_csv <- downloadHandler(
    filename = function() {
      paste0("landcover_legend_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- v$lc_list_df
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  ## Reset land cover colors to current palette theme
  observeEvent(input$lc_reset_colors_btn, {
    df <- isolate(v$lc_list_df)
    if (is.null(df) || nrow(df) == 0) return()
    pal_set <- isolate(v$lc_palette_set) %||% "gyr"
    df$color <- get_legend_set_colors(pal_set, nrow(df))
    v$lc_list_df <- df
    lc_legend_pal <<- colorFactor(df$color, df$lc_id, na.color = NA)
    toggle_popover("lc_input_pop", FALSE)
  })

  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }

  #### Carbon stock data upload ####

  observeEvent(input$c_input_upload, {
    #TODO: if(!check_file_csv()) return()

    f <- input$c_input_upload

    df_inp <- read.csv(f$datapath)

    if (nrow(df_inp) == 0)

      return()

    if (ncol(df_inp) < 2)

      return()

    if (!is_numeric_str(df_inp[1, 1]))

      return()

    df <- data.frame("lc_id" = as.integer(as.character(df_inp[, 1])))

    for (i in c(2:ncol(df_inp))) {
      if (is_numeric_str(df_inp[1, i])) {
        df$c <- df_inp[, i]

        break
      }
    }

    old_df <- isolate(v$cstock_list)

    valid_id <- intersect(old_df$lc_id, df$lc_id)

    if (length(valid_id) > 0) {
      old_df[old_df$lc_id %in% valid_id, ] <- df[df$lc_id %in% valid_id, ]

      v$cstock_list <- old_df
    }

    toggle_popover("c_input_pop", F)
  })

  ## Download carbon stock data as CSV
  output$download_cstock_csv <- downloadHandler(
    filename = function() {
      paste0("carbon_stocks_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- v$cstock_list
      lc_df <- v$lc_list_df
      if (!is.null(df) && nrow(df) > 0) {
        if (!is.null(lc_df)) {
          df <- merge(df[c("lc_id", "c")], lc_df[c("lc_id", "label")], by = "lc_id", all.x = TRUE)
        }
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  ## Reset all carbon stock values to 0
  observeEvent(input$cstock_reset_btn, {
    df <- isolate(v$cstock_list)
    if (is.null(df) || nrow(df) == 0) return()
    df$c <- 0
    v$cstock_list <- df
    toggle_popover("c_input_pop", FALSE)
  })

  ### Carbon stock list table

  cstock_table_def <- c("lc_id", "c")

  cstock_column <- data.frame(
    title = c(
      "LC_ID",
      " ",
      "Land cover",
      "<div>C-stock (tC ha<sup>-1</sup>)</div>"
    ),
    type = c("numeric", "color", "text", "numeric"),
    render = c(NA, "square", NA, NA),
    align = c("right", "center", "left", "right"),
    width = c(NA, 20, 140, 130),
    readOnly = c(T, T, T, F)
  )

  output$cstock_list <- renderExcel({
    df <- v$cstock_list

    if (is.null(df) || is.null(v$lc_list_df))

      return()

    lc_df <- v$lc_list_df[c("lc_id", "color", "label")]

    df <- merge(df[c("lc_id", "c")], lc_df, by = "lc_id")

    n_rows <- nrow(df)

    t_height <- paste0(40 + n_rows * 28 + 10, "px")

    excelTable(
      data = df[c("lc_id", "color", "label", "c")],
      columns = cstock_column,
      tableOverflow = T,
      tableWidth = "100%",
      allowDeleteColumn = F,
      allowRenameColumn = F,
      allowInsertColumn = F,
      allowDeleteRow = F,
      allowInsertRow = F,
      rowDrag = F,
      minDimensions = c(NA, 1),
      tableHeight = t_height,
      autoIncrement = T,
      csvFileName = "zonation_table",
      includeHeadersOnDownload = T
    )
  })

  observeEvent(input$cstock_list, {
    inp <- input$cstock_list

    df_input <- excel_to_R(inp)

    names(df_input) <- c("lc_id", "color", "lc", "c")

    df_input <- transform(df_input, lc_id = as.numeric(lc_id))

    suppressWarnings(df_input <-
                       transform(df_input, c = as.numeric(c)))

    df_input[is.na(df_input)] <- 0

    v$cstock_list <- df_input[cstock_table_def]
  })

  ### NET PRESENT VALUE (NPV) & ECONOMIC TRADE-OFF ##########

  npv_table_def <- c("lc_id", "npv")

  npv_column <- data.frame(
    title = c(
      "LC_ID",
      " ",
      "Land cover",
      "<div>NPV ($ ha<sup>-1</sup>)</div>"
    ),
    type = c("numeric", "color", "text", "numeric"),
    render = c(NA, "square", NA, NA),
    align = c("right", "center", "left", "right"),
    width = c(NA, 20, 140, 130),
    readOnly = c(TRUE, TRUE, TRUE, FALSE)
  )

  ## Automatically initialize and sync NPV data whenever active land covers change
  observeEvent(v$lc_list_df, {
    lc_df <- v$lc_list_df
    if (is.null(lc_df) || nrow(lc_df) == 0) {
      v$npv_list <- NULL
      return()
    }
    cur_npv <- isolate(v$npv_list)
    if (is.null(cur_npv)) {
      v$npv_list <- data.frame(lc_id = lc_df$lc_id, npv = 0)
    } else {
      missing_ids <- setdiff(lc_df$lc_id, cur_npv$lc_id)
      if (length(missing_ids) > 0) {
        v$npv_list <- rbind(cur_npv, data.frame(lc_id = missing_ids, npv = 0))
      }
      v$npv_list <- v$npv_list[v$npv_list$lc_id %in% lc_df$lc_id, ]
    }
  })

  output$npv_list <- renderExcel({
    if (!is.null(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
      lc_df <- v$lc_list_df[c("lc_id", "color", "label")]
      if (is.null(v$npv_list)) {
        v$npv_list <- data.frame(lc_id = lc_df$lc_id, npv = 0)
      } else {
        # Ensure all current land cover IDs are present
        missing_ids <- setdiff(lc_df$lc_id, v$npv_list$lc_id)
        if (length(missing_ids) > 0) {
          v$npv_list <- rbind(v$npv_list, data.frame(lc_id = missing_ids, npv = 0))
        }
      }
      df <- merge(v$npv_list[c("lc_id", "npv")], lc_df, by = "lc_id")
    } else {
      df <- data.frame(
        lc_id = integer(0),
        color = character(0),
        label = character(0),
        npv   = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    n_rows <- nrow(df)
    t_height <- paste0(40 + n_rows * 28 + 10, "px")
    excelTable(
      data = df[c("lc_id", "color", "label", "npv")],
      columns = npv_column,
          tableOverflow = T,
          tableWidth = "100%",
          allowDeleteColumn = F,
          allowRenameColumn = F,
          allowInsertColumn = F,
          allowDeleteRow = F,
          allowInsertRow = F,
          rowDrag = F,
          minDimensions = c(NA, 1),
          tableHeight = t_height,
          autoIncrement = T,
          csvFileName = "zonation_table",
          includeHeadersOnDownload = T
    )
  })
  
  observeEvent(input$npv_list, {
    inp <- input$npv_list
    df_input <- excel_to_R(inp)
    names(df_input) <- c("lc_id", "color", "lc", "npv")
    df_input <- transform(df_input, lc_id = as.numeric(lc_id))
    suppressWarnings(df_input <- transform(df_input, npv = as.numeric(npv)))
    df_input[is.na(df_input)] <- 0
    v$npv_list <- df_input[npv_table_def]
  })

  observeEvent(input$npv_set_zero_btn, {
    req(v$npv_list)
    df <- v$npv_list
    df$npv <- 0
    v$npv_list <- df
  })

  output$download_npv_csv <- downloadHandler(
    filename = function() {
      paste0("npv_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(v$npv_list)
      write.csv(v$npv_list, file, row.names = FALSE)
    }
  )

  observeEvent(input$upload_npv_csv, {
    req(input$upload_npv_csv)
    tryCatch({
      ext_df <- utils::read.csv(input$upload_npv_csv$datapath, stringsAsFactors = FALSE)
      if (all(c("lc_id", "npv") %in% colnames(ext_df))) {
        v$npv_list <- ext_df[c("lc_id", "npv")]
        showNotification("NPV data successfully imported!", type = "message")
      } else {
        showNotification("CSV must have 'lc_id' and 'npv' columns.", type = "warning")
      }
    }, error = function(e) {
      showNotification("Error reading NPV CSV file.", type = "error")
    })
  })

  ### ABATEMENT COST CURVE WIDGET (DEMO) ###################

  ## Update abacost_map2 choices when abacost_map1 changes (completely self-contained)
  observeEvent(input$abacost_map1, {
    records <- isolate(v$map_catalog)
    if (is.null(records)) return()
    landcover_records <- records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]
    lc_choices <- map_choices_year(landcover_records)
    sel1 <- input$abacost_map1
    sel2 <- isolate(input$abacost_map2)
    choices2 <- lc_choices[unname(lc_choices) != sel1]
    if (identical(sel1, sel2)) {
      sel2 <- if (length(choices2) > 0) unname(choices2)[1] else ""
    }
    updateSelectizeInput(session, "abacost_map2", choices = choices2, selected = sel2)
  }, ignoreInit = TRUE)

  clean_fs <- function(val) {
    if (is.null(val) || length(val) == 0 || is.na(val) || !nzchar(as.character(val))) return(NULL)
    val
  }

  ## ── Independent land cover transition calculator for Abatement Cost Curve ──
  abacost_lc_changes <- reactive({
    records <- v$map_catalog
    if (is.null(records) || length(records) == 0) return(NULL)

    lc_ids <- names(records)[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]

    if (length(lc_ids) == 0) return(NULL)

    m1_id <- input$abacost_map1
    m2_id <- input$abacost_map2

    if (is.null(m1_id) || !nzchar(m1_id) || !(m1_id %in% lc_ids)) {
      m1_id <- lc_ids[1]
    }
    if (is.null(m2_id) || !nzchar(m2_id) || !(m2_id %in% lc_ids) || m2_id == m1_id) {
      avail <- lc_ids[lc_ids != m1_id]
      m2_id <- if (length(avail) > 0) avail[length(avail)] else lc_ids[1]
    }

    if (m1_id == m2_id && length(lc_ids) > 1) {
      m2_id <- lc_ids[lc_ids != m1_id][1]
    }

    rec1 <- records[[m1_id]]
    rec2 <- records[[m2_id]]
    if (is.null(rec1) || is.null(rec2) || is.null(rec1$stars) || is.null(rec2$stars)) {
      return(v$lc_changes_df)
    }

    zone_map_id <- input$abacost_zone_map
    has_zone <- !is.null(zone_map_id) && nzchar(zone_map_id) && (zone_map_id %in% names(records))
    zone_rec <- if (has_zone) records[[zone_map_id]] else NULL
    zone_stars <- if (!is.null(zone_rec) && !is.null(zone_rec$stars)) zone_rec$stars else NULL

    m1_mem <- if (inherits(rec1$stars, "stars_proxy")) tryCatch(stars::st_as_stars(rec1$stars), error = function(e) NULL) else rec1$stars
    m2_mem <- if (inherits(rec2$stars, "stars_proxy")) tryCatch(stars::st_as_stars(rec2$stars), error = function(e) NULL) else rec2$stars
    if (is.null(m1_mem) || is.null(m2_mem)) return(v$lc_changes_df)

    m2_mem <- tryCatch(align_stars_to_reference(m2_mem, m1_mem), error = function(e) NULL)
    if (is.null(m2_mem)) return(v$lc_changes_df)

    cell_ha <- pixel_ha(m1_mem)
    v1 <- as.vector(m1_mem[[1]])
    v2 <- as.vector(m2_mem[[1]])
    valid <- !is.na(v1) & !is.na(v2)

    layers <- list()
    if (!is.null(zone_stars)) {
      zz <- if (inherits(zone_stars, "stars_proxy")) tryCatch(stars::st_as_stars(zone_stars), error = function(e) NULL) else zone_stars
      if (!is.null(zz)) {
        zz <- tryCatch(align_stars_to_reference(zz, m1_mem), error = function(e) NULL)
        if (!is.null(zz)) {
          vals <- zz[[1]]
          vals[is.na(vals)] <- all_id
          zz[[1]] <- vals
          layers[[1]] <- zz
        }
      }
    }

    rm(m1_mem, m2_mem)

    if (!any(valid)) {
      rm(v1, v2, valid)
      invisible(gc(verbose = FALSE))
      return(NULL)
    }

    m <- tryCatch({
      if (length(layers) == 0) {
        tab <- table(lc1_id = v1[valid], lc2_id = v2[valid])
        as.data.frame(tab, responseName = "area", stringsAsFactors = FALSE)
      } else {
        zv <- as.vector(layers[[1]][[1]])[valid]
        tab <- table(lc1_id = v1[valid], lc2_id = v2[valid], zone_id = zv)
        res <- as.data.frame(tab, responseName = "area", stringsAsFactors = FALSE)
        res$zone_id[which(res$zone_id == as.character(all_id))] <- NA
        res
      }
    }, error = function(e) {
      warning("Memory limit in abacost_lc_changes table: ", conditionMessage(e))
      NULL
    })

    rm(v1, v2, valid)
    if (exists("layers", inherits = FALSE)) rm(layers)
    invisible(gc(verbose = FALSE))

    if (is.null(m)) return(NULL)

    m <- m[m$area > 0, , drop = FALSE]
    if (nrow(m) == 0) return(NULL)

    idcol <- setdiff(colnames(m), "area")
    m[idcol] <- lapply(m[idcol], function(x) as.numeric(as.character(x)))
    m$area <- m$area * cell_ha
    m
  })

  ## ── Abatement cost data: ALWAYS use active land covers and NPV (never demo/saved data) ──
  abacost_calculated_data <- reactive({
    lc_df <- v$lc_list_df
    empty_df <- data.frame(
      label  = character(0),
      legend = character(0),
      x      = numeric(0),
      y      = numeric(0),
      stringsAsFactors = FALSE
    )

    if (is.null(lc_df) || nrow(lc_df) == 0) {
      return(empty_df)
    }

    lc_ch   <- abacost_lc_changes()
    c_df    <- if (!is.null(v$cstock_list) && nrow(v$cstock_list) > 0) v$cstock_list else data.frame(lc_id = lc_df$lc_id, c = 0)
    npv_df  <- if (!is.null(v$npv_list) && nrow(v$npv_list) > 0) v$npv_list else data.frame(lc_id = lc_df$lc_id, npv = 0)

    lc_names <- setNames(as.character(lc_df$label), as.character(lc_df$lc_id))
    get_lc_name <- function(id) {
      nm <- lc_names[as.character(id)]
      if (length(nm) == 1 && !is.na(nm) && nzchar(nm)) nm else paste0("LC", id)
    }

    if (is.null(lc_ch) || !is.data.frame(lc_ch) || nrow(lc_ch) == 0) {
      return(data.frame(
        label  = as.character(lc_df$label),
        legend = rep("Active Land Covers", nrow(lc_df)),
        x      = rep(1, nrow(lc_df)),
        y      = rep(0, nrow(lc_df)),
        stringsAsFactors = FALSE
      ))
    }

    df <- lc_ch

    ## Drop same-class transitions and zero-area rows
    df <- df[!is.na(df$lc1_id) & !is.na(df$lc2_id) &
             df$lc1_id != df$lc2_id &
             !is.na(df$area) & df$area > 0, ]

    if (nrow(df) == 0) {
      return(data.frame(
        label  = as.character(lc_df$label),
        legend = rep("Active Land Covers", nrow(lc_df)),
        x      = rep(1, nrow(lc_df)),
        y      = rep(0, nrow(lc_df)),
        stringsAsFactors = FALSE
      ))
    }

    ## Merge carbon stocks
    cdf1 <- c_df[c("lc_id", "c")]; colnames(cdf1) <- c("lc1_id", "c1")
    cdf2 <- c_df[c("lc_id", "c")]; colnames(cdf2) <- c("lc2_id", "c2")
    df <- merge(df, cdf1, by = "lc1_id", all.x = TRUE)
    df <- merge(df, cdf2, by = "lc2_id", all.x = TRUE)

    ## Merge NPV
    npv1 <- npv_df[c("lc_id", "npv")]; colnames(npv1) <- c("lc1_id", "npv1")
    npv2 <- npv_df[c("lc_id", "npv")]; colnames(npv2) <- c("lc2_id", "npv2")
    df <- merge(df, npv1, by = "lc1_id", all.x = TRUE)
    df <- merge(df, npv2, by = "lc2_id", all.x = TRUE)

    ## Fill NAs with 0
    for (col in c("c1", "c2", "npv1", "npv2")) {
      df[[col]] <- ifelse(is.na(df[[col]]), 0, df[[col]])
    }

    ## Compute emission factor and cost
    df$delta_c   <- df$c1 - df$c2
    df$ef        <- df$delta_c * (44 / 12)        # tCO2e / ha
    df$x         <- df$area * df$ef               # total Mg CO2e
    df$delta_npv <- df$npv2 - df$npv1             # $/ha
    df$y         <- ifelse(abs(df$ef) > 0, df$delta_npv / df$ef, 0)

    ## If all carbon stocks are zero (or delta_c is 0 everywhere),
    ## use transition area as x so bars are drawn with active transitions and cost y = 0
    if (all(df$x == 0, na.rm = TRUE)) {
      df$x <- df$area
    } else {
      df <- df[!is.na(df$x) & df$x != 0, ]
    }

    if (nrow(df) == 0) {
      return(data.frame(
        label  = as.character(lc_df$label),
        legend = rep("Active Land Covers", nrow(lc_df)),
        x      = rep(1, nrow(lc_df)),
        y      = rep(0, nrow(lc_df)),
        stringsAsFactors = FALSE
      ))
    }

    df$y[is.na(df$y)] <- 0

    ## Build zone legend labels
    sel_zone_map <- input$abacost_zone_map
    legend_col <- if (!is.null(sel_zone_map) && nzchar(sel_zone_map)) {
      zc <- if ("zone_id" %in% colnames(df)) {
        "zone_id"
      } else if (paste0("zone_", sel_zone_map) %in% colnames(df)) {
        paste0("zone_", sel_zone_map)
      } else {
        grep("^zone_", colnames(df), value = TRUE)[1]
      }

      if (!is.null(zc) && !is.na(zc) && zc %in% colnames(df)) {
        zvals <- as.character(df[[zc]])
        zleg <- v$zone_legends[[sel_zone_map]]
        zlookup <- if (!is.null(zleg) && all(c("zone_id", "label") %in% colnames(zleg))) {
          setNames(as.character(zleg$label), as.character(zleg$zone_id))
        } else {
          character(0)
        }
        vapply(zvals, function(z) {
          if (!is.na(z) && nzchar(z) && z %in% names(zlookup)) zlookup[[z]]
          else if (!is.na(z) && nzchar(z)) paste0("Zone ", z)
          else "No zone"
        }, character(1))
      } else {
        rep("All zones", nrow(df))
      }
    } else {
      rep("All zones", nrow(df))
    }

    ## Final output data frame expected by abacost_curve()
    data.frame(
      label  = mapply(function(a, b) paste(get_lc_name(a), "->", get_lc_name(b)),
                      df$lc1_id, df$lc2_id,
                      SIMPLIFY = TRUE, USE.NAMES = FALSE),
      legend = legend_col,
      x      = df$x,
      y      = df$y,
      stringsAsFactors = FALSE
    )
  })

  theme_first_colors <- c(
    viridis = "#21918c",
    rainbow = "#6e40aa",
    turbo   = "#3082e6",
    cool    = "#3b82f6"
  )

  observeEvent(input$color_theme, {
    th <- input$color_theme %||% "viridis"
    col <- theme_first_colors[[th]] %||% "#2b8cbe"
    session$sendCustomMessage("set_bar_color", col)
  }, ignoreInit = TRUE)

  observeEvent(input$abacost_reset_color_btn, {
    th <- input$color_theme %||% "viridis"
    col <- theme_first_colors[[th]] %||% "#2b8cbe"
    session$sendCustomMessage("set_bar_color", col)
  })

  chart_widget <- reactive({
    has_zone <- !is.null(input$abacost_zone_map) && nzchar(input$abacost_zone_map)
    chosen_color <- if (!has_zone) {
      if (!is.null(input$abacost_bar_color) && nzchar(input$abacost_bar_color)) {
        input$abacost_bar_color
      } else {
        "#2b8cbe"
      }
    } else {
      NULL
    }
    abacost_curve(
      data = abacost_calculated_data(),
      show_negatives = isTRUE(input$show_negatives),
      show_threshold_line = isTRUE(input$show_reference),
      threshold_cost = as.numeric(input$reference_cost %||% 0.005),
      color_theme = input$color_theme %||% "viridis",
      default_color = chosen_color,
      y_scale = input$y_scale %||% "logarithmic",
      show_legend = if (has_zone) isTRUE(input$show_legend) else FALSE,
      font_size = input$font_size %||% 12,
      x_title_font_size = clean_fs(input$x_title_font_size),
      y_title_font_size = clean_fs(input$y_title_font_size),
      x_tick_font_size = clean_fs(input$x_tick_font_size),
      y_tick_font_size = clean_fs(input$y_tick_font_size),
      legend_title_font_size = clean_fs(input$legend_title_font_size),
      legend_text_font_size = clean_fs(input$legend_text_font_size),
      tooltip_font_size = clean_fs(input$tooltip_font_size),
      annotation_font_size = clean_fs(input$annotation_font_size),
      threshold_text_font_size = clean_fs(input$threshold_text_font_size),
      legend_title = if (!is.null(input$abacost_zone_map) && nzchar(input$abacost_zone_map) && !is.null(v$map_catalog[[input$abacost_zone_map]])) (v$map_catalog[[input$abacost_zone_map]]$label %||% "Zone") else "Zone"
    )
  })

  output$curve <- renderAbacostCurve({
    chart_widget()
  })

  download_html_handler <- downloadHandler(
    filename = function() {
      paste0("abatement_cost_curve_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      w <- chart_widget()
      htmlwidgets::saveWidget(w, file = file, selfcontained = TRUE)
    }
  )

  output$download_html <- download_html_handler
  output$download_html_hdr <- download_html_handler

  download_abacost_csv_handler <- downloadHandler(
    filename = function() {
      paste0("abatement_cost_curve_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- abacost_calculated_data()
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  output$download_abacost_csv <- download_abacost_csv_handler
  output$download_abacost_csv_sb <- download_abacost_csv_handler

  output$download_boundary_geojson <- downloadHandler(
    filename = function() {
      paste0("study_area_boundary_", format(Sys.Date(), "%Y%m%d"), ".geojson")
    },
    content = function(file) {
      boundary <- map_info_boundary()
      if (!is.null(boundary) && nrow(boundary) > 0) {
        sf::st_write(boundary, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
      }
    }
  )

  output$download_lc_changes_csv <- downloadHandler(
    filename = function() {
      sel <- matrix_selection()
      if (!is.null(sel) && is.null(sel$rec2)) {
        paste0("landcover_area_", format(Sys.Date(), "%Y%m%d"), ".csv")
      } else {
        paste0("landcover_changes_", format(Sys.Date(), "%Y%m%d"), ".csv")
      }
    },
    content = function(file) {
      sel <- matrix_selection()
      if (!is.null(sel) && is.null(sel$rec2)) {
        rec <- sel$rec1
        ov <- active_zone_overlay()
        df <- NULL
        if (length(ov$layers) > 0) df <- lc_area_within_zone(rec$stars, ov$layers, ov$zones)
        if (is.null(df) || nrow(df) == 0) df <- if (!is.null(rec$freq_df)) rec$freq_df else get_raster_freq_df(rec$stars, rec$sf)
        if (!is.null(df) && nrow(df) > 0) {
          cell_ha <- pixel_ha(rec$stars)
          if (!is.na(cell_ha) && cell_ha > 0) df$area <- df$area * cell_ha
          lc_df <- v$lc_list_df
          if (!is.null(lc_df)) {
            hit <- match(df$id, lc_df$lc_id)
            df$label <- ifelse(is.na(hit), as.character(df$id), as.character(lc_df$label[hit]))
          }
          write.csv(df, file, row.names = FALSE, na = "")
        }
      } else {
        df <- v$lc_changes_df
        if (!is.null(df) && nrow(df) > 0) {
          write.csv(df, file, row.names = FALSE, na = "")
        }
      }
    }
  )

  output$download_lc_changes_html <- downloadHandler(
    filename = function() {
      sel <- matrix_selection()
      if (!is.null(sel) && is.null(sel$rec2)) {
        paste0("landcover_area_", format(Sys.Date(), "%Y%m%d"), ".html")
      } else {
        paste0("landcover_changes_", format(Sys.Date(), "%Y%m%d"), ".html")
      }
    },
    content = function(file) {
      sel <- matrix_selection()
      if (!is.null(sel) && is.null(sel$rec2)) {
        p <- lc_area_bar_plot()
        if (!is.null(p)) {
          htmlwidgets::saveWidget(plotly::as_widget(p), file = file, selfcontained = TRUE)
        }
      } else {
        w <- lc_changes_scenario_widget()
        if (!is.null(w)) {
          htmlwidgets::saveWidget(w, file = file, selfcontained = TRUE)
        }
      }
    }
  )

  output$download_lc_trend_csv <- downloadHandler(
    filename = function() {
      paste0("landcover_area_trend_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- v$lc_trend_data
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  output$download_lc_trend_html <- downloadHandler(
    filename = function() {
      paste0("landcover_area_trend_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      w <- lc_trend_chart_widget()
      if (!is.null(w)) {
        htmlwidgets::saveWidget(w, file = file, selfcontained = TRUE)
      }
    }
  )

  output$download_area_proj_csv <- downloadHandler(
    filename = function() {
      paste0("projected_landcover_area_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      sc <- tryCatch(selected_scenario(), error = function(e) NULL)
      df <- if (!is.null(sc) && !is.null(sc$abacus_scenario$projection$lc_area)) {
        sc$abacus_scenario$projection$lc_area
      } else if (!is.null(v$abacus_baseline$projection$lc_area)) {
        v$abacus_baseline$projection$lc_area
      } else NULL
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  output$download_area_proj_html <- downloadHandler(
    filename = function() {
      paste0("projected_landcover_area_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      w <- area_plot_widget()
      if (!is.null(w)) {
        htmlwidgets::saveWidget(w, file = file, selfcontained = TRUE)
      }
    }
  )

  output$download_emission_proj_csv <- downloadHandler(
    filename = function() {
      paste0("projected_emissions_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      sc <- tryCatch(selected_scenario(), error = function(e) NULL)
      df <- if (!is.null(sc) && !is.null(sc$abacus_scenario$emission$lc_emission)) {
        sc$abacus_scenario$emission$lc_emission
      } else if (!is.null(v$abacus_baseline$emission$lc_emission)) {
        v$abacus_baseline$emission$lc_emission
      } else NULL
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  output$download_emission_proj_html <- downloadHandler(
    filename = function() {
      paste0("projected_emissions_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      p <- emission_plot_widget()
      if (!is.null(p)) {
        htmlwidgets::saveWidget(plotly::as_widget(p), file = file, selfcontained = TRUE)
      }
    }
  )

  ### LAND COVER CHANGE MATRIX ##############################

  ## Rebuild the changes matrix for the analysis maps against every active

  ## zonation map. The analysis tabs act on v$map*_stars, so those are the

  ## inputs; the zone layers are the same set the changes tab overlays.

  refresh_lc_matrix <- function() {
    ov <- active_zone_overlay()

    sel <- matrix_changes()

    if (!is.null(sel) && !is.null(sel$map1) && !is.null(sel$map2)) {
      update_lc_matrix(sel$map1, sel$map2, ov$layers)
    } else if (!is.null(isolate(v$map1_stars)) && !is.null(isolate(v$map2_stars))) {
      update_lc_matrix(isolate(v$map1_stars), isolate(v$map2_stars), ov$layers)
    }
  }

  ## Build the changes matrix, broken down by EVERY active zonation map.

  ##

  ## `zone_layers` is a named list of zonation rasters keyed by map id. Each one

  ## is resampled onto the land cover grid and added to the crosstab as its own

  ## column, so a row records the land cover transition AND which zone it fell in

  ## for each map. The per-map zone selectors then filter these rows with an AND

  ## - "A -> Zone 3" AND "B -> Zone 7" - which is why each map needs its own

  ## column rather than one merged zone id: only separate columns can be filtered

  ## independently and then recombined.

  update_lc_matrix <- function(map1, map2, zone_layers = list()) {
    if (is.null(map1) || is.null(map2)) {
      v$lc_changes_df <- NULL

      return()
    }

    ## Drop layers that carry no raster, so an empty selection behaves exactly

    ## like the no-zonation case.

    zone_layers <- zone_layers[!vapply(zone_layers, is.null, logical(1))]

    ## stars-only: every map is materialised and brought onto the first land

    ## cover map's grid with the same aligner the rest of the app uses, then the

    ## layers are cross-tabulated with table(). No terra at any point.

    m1_mem <- if (inherits(map1, "stars_proxy")) stars::st_as_stars(map1) else map1

    m2_mem <- if (inherits(map2, "stars_proxy")) stars::st_as_stars(map2) else map2

    if (is.null(m1_mem) || is.null(m2_mem))

      return()

    ## The second land cover map is put on the first map's grid, exactly as the

    ## land cover resampling did before: same CRS, extent and resolution.

    m2_mem <- align_stars_to_reference(m2_mem, m1_mem)

    ## One aligned, NA-sentinel-substituted layer per zonation map. The sentinel

    ## keeps out-of-footprint cells in the table() instead of letting it drop

    ## every row that has an NA in any layer.

    layers <- list()

    layer_names <- character(0)

    for (nm in names(zone_layers)) {
      z <- zone_layers[[nm]]

      if (is.null(z))

        next

      zz <- if (inherits(z, "stars_proxy")) stars::st_as_stars(z) else z

      if (is.null(zz))

        next

      zz <- align_stars_to_reference(zz, m1_mem)

      if (is.null(zz))

        next

      vals <- zz[[1]]

      vals[is.na(vals)] <- all_id

      zz[[1]] <- vals

      layers[[length(layers) + 1]] <- zz

      layer_names <- c(layer_names, nm)
    }

    cell_ha <- pixel_ha(m1_mem)
    v1 <- as.vector(m1_mem[[1]])
    v2 <- as.vector(m2_mem[[1]])
    valid <- !is.na(v1) & !is.na(v2)

    rm(m1_mem, m2_mem)

    if (!any(valid)) {
      rm(v1, v2, valid)
      invisible(gc(verbose = FALSE))
      v$lc_changes_df <- NULL
      return()
    }

    m <- tryCatch({
      if (length(layers) == 0) {
        tab <- table(lc1_id = v1[valid], lc2_id = v2[valid])
        as.data.frame(tab, responseName = "area", stringsAsFactors = FALSE)
      } else {
        args_list <- list(lc1_id = v1[valid], lc2_id = v2[valid])
        for (i in seq_along(layers)) {
          nm <- if (!is.null(layer_names) && i <= length(layer_names)) paste0("zone_", layer_names[i]) else paste0("zone_", i)
          args_list[[nm]] <- as.vector(layers[[i]][[1]])[valid]
        }
        tab <- do.call(table, args_list)
        res <- as.data.frame(tab, responseName = "area", stringsAsFactors = FALSE)
        zone_cols <- grep("^zone_", colnames(res), value = TRUE)
        for (zc in zone_cols) {
          res[[zc]][which(res[[zc]] == as.character(all_id))] <- NA
        }
        res
      }
    }, error = function(e) {
      warning("Memory limit in update_lc_matrix table: ", conditionMessage(e))
      NULL
    })

    rm(v1, v2, valid)
    if (exists("layers", inherits = FALSE)) rm(layers)
    if (exists("args_list", inherits = FALSE)) rm(args_list)
    invisible(gc(verbose = FALSE))

    if (is.null(m)) {
      v$lc_changes_df <- NULL
      return()
    }

    m <- m[m$area > 0, , drop = FALSE]
    if (nrow(m) == 0) {
      v$lc_changes_df <- NULL
      return()
    }

    idcol <- setdiff(colnames(m), "area")
    m[idcol] <- lapply(m[idcol], function(x) as.numeric(as.character(x)))

    ## Convert the "All zones" sentinel back to NA in zone columns. The

    ## sentinel only existed to keep out-of-footprint cells past the

    ## crosstab/table (which drop NA); downstream code (render_lc_changes

    ## filtering and the abacus() call) expects NA for those cells.

    zone_cols <- grep("^zone_", colnames(m), value = TRUE)

    for (zc in zone_cols) {
      m[[zc]][which(m[[zc]] == all_id)] <- NA
    }

    ## Convert pixel counts â†’ hectares so the matrix totals match the map info.

    ## Fall back to raw counts only when the pixel area cannot be determined

    ## (e.g. geographic CRS), keeping the downstream abacus() call intact.

    if (!is.na(cell_ha) && cell_ha > 0) {
      m$area <- as.numeric(m$area) * cell_ha
    } else {
      m$area <- as.numeric(m$area)
    }

    m$scenario_id <- 0

    m$iteration_id <- 0

    v$lc_changes_df <- m
  }

  ## Input id of the zone selector belonging to the k-th selected zonation map.

  ## The selectors are built server-side, so their ids are derived rather than

  ## declared in the UI: map 1 -> zone_filter_1, map 2 -> zone_filter_2, ...

  zone_filter_input_id <- function(k) paste0("zone_filter_", k)

  ## Rebuild the changes matrix against the current zonation selection. Called

  ## whenever the map pair, the selected zonation maps, or any per-map zone

  ## selector changes, so the zone columns always match the filter the user

  ## sees. A no-op when the tab is in single-map mode (no pair to tabulate).

  rebuild_matrix_for_zones <- function() {
    map_ids <- active_zone_map_ids()

    needed_cols <- paste0("zone_", map_ids)

    if (!is.null(v$lc_changes_df) && all(needed_cols %in% colnames(v$lc_changes_df))) {
      return()
    }

    refresh_lc_matrix()
  }

  ## The zonation maps currently active in the changes tab, in a stable order

  ## (sorted by record id). The order fixes which selector number belongs to

  ## which map, so it must not follow the order the user happened to click in.

  ##

  ## A deliberately EMPTY selection means "no zonation overlay": the analysis

  ## zone is NOT substituted in, because doing so would keep a map active after

  ## the user cleared the list and the zone selectors would never go away. The

  ## analysis zone is only used until the changes-tab selector has been seen at

  ## all (it is populated on first load), which is what NULL means here.

  active_zone_map_ids <- function() {
    raw <- input$matrix_zone_map

    map_ids <- parse_map_selection(raw)

    if (is.null(raw))

      map_ids <- parse_map_selection(input$analysis_zone_map)

    recs <- isolate(v$map_catalog)

    ## Keep only actual zonation maps. Checking the record merely EXISTS is not

    ## enough: analysis_zone_map shares this path, and overlaying a land cover

    ## map as if it were a zonation would tabulate nonsense.

    map_ids <- map_ids[vapply(map_ids, function(id) {
      rec <- recs[[id]]

      !is.null(rec) && identical(rec$type, "Zonation map")
    }, logical(1))]

    sort(unique(map_ids))
  }

  ## TRUE when at least one zonation map is overlaid on the changes tab. This is

  ## what gates the zone selectors: with no zonation map there are no zones to

  ## choose, so the controls are hidden rather than left showing an idle

  ## "All zones" box.

  zones_are_active <- function() {
    length(active_zone_map_ids()) > 0
  }

  ## Exposed to the UI so the zonation hint text can switch between "each map

  ## gets a selector" and "no map selected", and so the zone controls can be

  ## hidden as a group the moment the selection is cleared.

  output$is_zone_selected <- reactive({
    zones_are_active()
  })

  outputOptions(output, "is_zone_selected", suspendWhenHidden = FALSE)

  ## One zone selector per selected zonation map, rendered as N controls.

  ##

  ## Two maps give two selectors, three give three, and so on - each offering

  ## only its own map's zones. Together they are a combination filter: a cell is

  ## kept when it satisfies EVERY selector, so "Zonation A -> Zone 3" AND

  ## "Zonation B -> Zone 7" narrows to their intersection. "All zones" on a

  ## selector means that map imposes no constraint.

  output$zone_filters <- renderUI({
    v$map_catalog_version

    map_ids <- active_zone_map_ids()

    ## No zonation map: render nothing at all. Returning an empty tagList rather

    ## than NULL keeps the container empty (and its inputs absent from `input`)

    ## until a map is chosen again.

    if (length(map_ids) == 0)

      return(NULL)

    legends <- isolate(v$zone_legends)

    recs <- isolate(v$map_catalog)

    controls <- lapply(seq_along(map_ids), function(k) {
      map_id <- map_ids[[k]]

      input_id <- zone_filter_input_id(k)

      rec <- recs[[map_id]]

      map_name <- if (!is.null(rec) && !is.null(rec$label) && nzchar(rec$label))

        rec$label else map_id

      ## This map's own zones, as a plain value/label pair. No relabelled

      ## composed ids here: the filter matches on each map's zone layer

      ## directly, so the value is the map's raw zone id.

      legend <- legends[[map_id]]

      choices <- list("All zones" = all_id)

      if (!is.null(legend) && nrow(legend) > 0) {
        ids <- legend$zone_id

        names(ids) <- as.character(legend$label)

        choices <- append(choices, as.list(ids))
      }

      ## Keep a still-valid current value; otherwise fall back to "All zones"

      ## rather than leaving the selector on a zone this map no longer has.

      current <- isolate(input[[input_id]])

      if (length(current) != 1 || is.na(current) ||
          !(as.character(current) %in% as.character(unlist(choices))))

        current <- as.character(all_id)

      div(
        class = "d-flex align-items-center",
        span(paste0(map_name, ":"), class = "matrix-header-label"),
        shiny::selectizeInput(
          input_id, NULL,
          choices = choices,
          selected = current,
          width = "170px",
          options = list(placeholder = "All zones")
        )
      )
    })

    tagList(controls)
  })

  outputOptions(output, "zone_filters", suspendWhenHidden = FALSE)

  ## Feed the legend/palette from the active zonation layer. This observer no

  ## longer builds the zone CHOICES - output$zone_filters owns those now - it

  ## only keeps v$zone_list_df in step so the rest of the app (and the zone

  ## legend display) sees the right zones.

  observe({
    input$matrix_zone_map

    input$analysis_zone_map

    v$map_catalog_version

    zdf <- active_zone_legend()

    if (!is.null(zdf) && nrow(zdf) > 0) {
      v$zone_list_df <- zdf

      zone_legend_pal <<- colorFactor(zdf$color, zdf$zone_id, na.color = NA)
    }
  })

  ## Populate the zonation-map selector shown in the changes tab.

  observeEvent(v$map_catalog_version, {
    records <- isolate(v$map_catalog)

    zone_records <- records[vapply(records, function(record) {
      identical(record$type, "Zonation map")
    }, logical(1))]

    if (length(zone_records) > 0) {
      zone_records <- zone_records[order(vapply(zone_records, function(record) {
        record$label
      }, character(1)))]
    }

    zone_choices <- c("No zonation map" = "", zone_map_choices(zone_records))

    zone_ids <- names(zone_records)

    ## matrix_zone_map is a multi-select, so `current` is a vector. Keep every

    ## still-valid selection and only fall back to the analysis zone / first

    ## map when nothing valid is selected at all.

    current <- intersect(parse_map_selection(isolate(input$matrix_zone_map)), zone_ids)

    if (length(current) == 0) {
      fallback <- parse_map_selection(isolate(input$analysis_zone_map))

      current <- intersect(fallback, zone_ids)

      if (length(current) == 0 && length(zone_ids) > 0)

        current <- zone_ids[[1]]
    }

    updateSelectizeInput(session, "matrix_zone_map", choices = zone_choices,
                         selected = current, options = list(placeholder = "No zonation map"))
  }, ignoreInit = FALSE)

  ## Changing the zonation map rebuilds the changes matrix with that zone layer.

  observeEvent(input$matrix_zone_map, {
    zone_id <- parse_map_selection(input$matrix_zone_map)

    ## Keep the hidden analysis selector in sync so the rest of the app follows.

    ## It is single-select, so a multi-map overlay keeps the first map active.

    updateSelectInput(session, "analysis_zone_map",
                      selected = if (length(zone_id) > 0) zone_id[[1]] else "")

    records <- isolate(v$map_catalog)

    ## v$mapz_stars is the single "active zonation" map the analysis tabs use

    ## for their own overlays; the changes tab works from every selected map.

    ## Guard the lookup: a record id that is no longer in the catalog (deleted

    ## map) would make records[[...]] NULL and NULL$stars an error.

    first_rec <- if (length(zone_id) > 0) records[[zone_id[[1]]]] else NULL

    v$mapz_stars <- if (!is.null(first_rec)) first_rec$stars else NULL

    ## Only rebuild when a land cover pair is selected; in single-map mode the

    ## area chart takes over and the matrix state is left untouched.

    sel <- matrix_changes()

    if (is.null(sel))

      return()

    update_lc_matrix(sel$map1, sel$map2, active_zone_overlay()$layers)
  }, ignoreInit = TRUE)


  ## Populate the land cover map selectors shown in the changes tab.

  ## Reacts to the catalog and to either selection, so each selector can drop

  ## the map chosen in the other one.

  observe({
    v$map_catalog_version

    input$matrix_map1

    input$matrix_map2

    records <- isolate(v$map_catalog)

    landcover_records <- records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))]

    if (length(landcover_records) > 0) {
      landcover_records <- landcover_records[order(vapply(landcover_records, function(record) {
        record$year
      }, integer(1)))]
    }

    lc_ids <- names(landcover_records)

    ## Matrix selectors show the year only, not the file name.

    lc_choices <- map_choices_year(landcover_records)

    id1 <- input$matrix_map1

    if (length(id1) != 1 || is.na(id1) || !nzchar(id1) || !(id1 %in% lc_ids))

      id1 <- if (length(lc_ids) > 0) lc_ids[1] else ""

    id2 <- input$matrix_map2

    ## "None" is always a valid choice for the second map.

    if (length(id2) != 1 || is.na(id2) || !nzchar(id2)) {
      id2 <- matrix_map_none
    } else if (id2 != matrix_map_none && !(id2 %in% lc_ids)) {
      id2 <- matrix_map_none
    }

    ## When more than one land cover map is available, default the "After" slot

    ## to a different map so the changes matrix shows straight away. Only do

    ## this while the user has not made an explicit choice, so a deliberate

    ## "None" (single-map area chart) is preserved.

    if (!matrix_map2_user_set && length(lc_ids) > 1 &&
        identical(id2, matrix_map_none)) {
      candidate <- lc_ids[lc_ids != id1]

      if (length(candidate) > 0)

        id2 <- candidate[length(candidate)]
    }

    pair <- order_map_pair(id1, id2, records)

    ## Each selector hides the map already taken by the other one, so the same

    ## land cover map can never be picked on both sides. In these choices the

    ## *values* are the map ids and the *names* are the year labels, so filter

    ## on the value side.

    choices1 <- lc_choices[unname(lc_choices) != pair$id2]

    lc_choices2 <- lc_choices[unname(lc_choices) != pair$id1]

    lc_choices2 <- c(stats::setNames(list(matrix_map_none), "None"), lc_choices2)

    matrix_map2_sent <<- pair$id2

    updateSelectizeInput(session, "matrix_map1", choices = choices1, selected = pair$id1)

    updateSelectizeInput(session, "matrix_map2", choices = lc_choices2, selected = pair$id2)

    updateSelectizeInput(session, "analysis_map1", choices = choices1, selected = pair$id1)

    updateSelectizeInput(session, "analysis_map2", choices = lc_choices[unname(lc_choices) != pair$id1], selected = if (pair$id2 != matrix_map_none) pair$id2 else "")
  })

  observeEvent(input$analysis_map1, {
    sel <- input$analysis_map1

    if (length(sel) == 1 && nzchar(sel) && !identical(input$matrix_map1, sel)) {
      updateSelectizeInput(session, "matrix_map1", selected = sel)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$analysis_map2, {
    sel <- input$analysis_map2

    if (length(sel) == 1 && nzchar(sel) && !identical(input$matrix_map2, sel)) {
      updateSelectizeInput(session, "matrix_map2", selected = sel)
    }
  }, ignoreInit = TRUE)

  ## Remember an explicit "After" selection so the automatic default above

  ## never overrides it on a later catalog refresh. Ignore the echo of our own

  ## programmatic updates (those match the last value we sent).

  observeEvent(input$matrix_map2, {
    if (!identical(input$matrix_map2, matrix_map2_sent))

      matrix_map2_user_set <<- TRUE
  }, ignoreInit = TRUE)

  ## Rebuild the matrix when the map pair OR the zonation selection changes.

  ##

  ## input$matrix_zone_map is an explicit dependency: the zone layers come from

  ## that selection, and relying on v$mapz_stars to carry the change does NOT

  ## work - going from one zonation map to two leaves v$mapz_stars on the same

  ## first map (identical reactive value, so it does not re-fire), and the

  ## overlay would never appear.

  observeEvent(list(input$matrix_map1, input$matrix_map2,
                    input$matrix_zone_map, v$mapz_stars), {
    sel <- matrix_changes()

    if (is.null(sel)) {
      ## No map *pair* means the changes tab is in single-map mode and shows

      ## the area chart instead. Do not wipe the matrix simply because a

      ## zonation map was added: changing v$mapz_stars fires this observer,
      ## and clearing here used to hide the whole changes panel as soon as

      ## any zonation map was uploaded.

      return()
    }

    update_lc_matrix(sel$map1, sel$map2, active_zone_overlay()$layers)
  }, ignoreNULL = FALSE)

  ## Which view the changes tab shows: the matrix for a pair, the area chart

  ## when only a single map is selected.

  output$is_matrix_pair <- reactive({
    sel <- matrix_selection()

    !is.null(sel) && !is.null(sel$rec1) && !is.null(sel$rec2)
  })

  outputOptions(output, "is_matrix_pair", suspendWhenHidden = FALSE)

  ## Emission (carbon + GHG) needs both land cover maps: the "other sources"

  ## emission factors are applied to the "before" and "after" maps and the

  ## maps/table are shown once that pair exists. Single-map mode keeps map1

  ## only, which the is_map_uploaded output uses to show the UI as well.

  observe({
    sel <- matrix_selection()

    v$map_pair <- if (!is.null(sel) && !is.null(sel$rec1) && !is.null(sel$rec2))

      c(sel$rec1$id, sel$rec2$id)
    else

      NULL
  })

  output$is_matrix_single <- reactive({
    sel <- matrix_selection()

    !is.null(sel) && !is.null(sel$rec1) && is.null(sel$rec2)
  })

  outputOptions(output, "is_matrix_single", suspendWhenHidden = FALSE)

  ## Card header adapts to the view: matrix for a pair, area chart for one map.

  ## Describe the active zone filter for display: which zonation maps are in

  ## play and, for each, which zone was picked. Every map's filter is shown

  ## because they combine as an AND, so "A > Zone 3 + B > Zone 7" is the scope

  ## and showing only one of them would misstate it.

  zone_filter_caption <- function() {
    ov <- active_zone_overlay()

    if (length(ov$map_ids) == 0)

      return("")

    recs <- isolate(v$map_catalog)

    legends <- isolate(v$zone_legends)

    parts <- vapply(ov$map_ids, function(id) {
      rec <- recs[[id]]

      map_name <- if (!is.null(rec) && !is.null(rec$label) && nzchar(rec$label))

        rec$label else id

      zones <- ov$zones[[id]]

      if (is.null(zones) || length(zones) == 0)

        return(paste0(map_name, ": all zones"))

      legend <- legends[[id]]

      hit <- if (!is.null(legend)) match(zones, legend$zone_id) else NA

      zone_names <- vapply(seq_along(zones), function(k) {
        if (!is.na(hit[[k]])) as.character(legend$label[hit[[k]]])
        else paste0("Zone ", zones[[k]])
      }, character(1))

      paste0(map_name, ": ", paste(zone_names, collapse = ", "))
    }, character(1))

    paste0(" | zone: ", paste(parts, collapse = " + "))
  }

  output$matrix_card_title <- renderUI({
    sel <- matrix_selection()

    ## Reflect the active zonation map(s) / zone so the matrix scope is explicit.

    zone_txt <- zone_filter_caption()

    if (!is.null(sel) && !is.null(sel$rec1) && is.null(sel$rec2)) {
      paste0("Land cover area (ha) - ", sel$rec1$year, zone_txt)
    } else if (!is.null(sel) && !is.null(sel$rec1) && !is.null(sel$rec2)) {
      paste0(
        "Land cover changes matrix (ha) - ",
        min(sel$rec1$year, sel$rec2$year), " to ",
        max(sel$rec1$year, sel$rec2$year), zone_txt
      )
    } else {
      "Land cover changes"
    }
  })

  ## Land cover frequency clipped to the active zone overlay, or the whole map

  ## when no zonation map / "All zones" is selected. The zonation layer is an

  ## overlay on the land cover map, so a zone simply restricts the area counted.

  ##

  ## `zones` is a named list keyed by zonation map id, each holding that map's

  ## selected zone ids (numeric(0) meaning "no constraint from this map"). Every

  ## map's layer is warped onto the land cover grid separately and the cell must

  ## satisfy ALL of them, so the selectors combine as an AND.

  ##

  ## stars only: warp_stars_to_grid() brings each zone layer onto the land cover

  ## grid (declaring no-data and verifying the result), and the tabulation is a

  ## plain table() over the aligned values.

  lc_area_within_zone <- function(map, map_layers, zones) {
    if (length(map_layers) == 0)

      return(NULL)

    r <- tryCatch({
      ## Materialise proxies; the aligned values are required, not a file pointer.

      lc <- if (inherits(map, "stars_proxy")) stars::st_as_stars(map) else map

      if (is.null(lc))

        return(NULL)

      lc_vals <- as.vector(lc[[1]])

      keep <- !is.na(lc_vals)

      for (map_id in names(map_layers)) {
        wanted <- zones[[map_id]]

        ## No zone picked for this map: it imposes no constraint at all, so its

        ## cells are not required to be covered by it either.

        if (is.null(wanted) || length(wanted) == 0)

          next

        layer <- map_layers[[map_id]]

        zz <- if (inherits(layer, "stars_proxy")) stars::st_as_stars(layer) else layer

        if (is.null(zz))

          next

        ## Bring this zone layer onto the land cover grid. NULL means the grids

        ## could not be reconciled; fall back to the whole-map totals upstream.

        zz <- warp_stars_to_grid(zz, lc)

        if (is.null(zz))

          return(NULL)

        z_vals <- as.vector(zz[[1]])

        ## This map's constraint: the cell must fall in one of the chosen zones.

        keep <- keep & !is.na(z_vals) & z_vals %in% wanted
      }

      if (!any(keep))

        return(NULL)

      ## Cell counts per land cover class. Kept as raw counts so the units

      ## match get_raster_freq_df(), which the whole-map view uses - mixing in

      ## a cell-area conversion here would make the two views disagree.

      counts <- table(lc_vals[keep])

      if (length(counts) == 0)

        return(NULL)

      df <- data.frame(
        id = as.numeric(names(counts)),
        area = as.numeric(counts),
        stringsAsFactors = FALSE
      )

      df[!is.na(df$id) & df$area > 0, , drop = FALSE]
    }, error = function(e) {
      warning("Zone overlay tabulation failed: ", e$message)

      NULL
    })

    r
  }

  ## Read the per-map zone selectors into a named list: one entry per selected

  ## zonation map, keyed by map id, holding that map's chosen zone ids. An empty

  ## vector means "All zones" for that map, i.e. no constraint.

  ##

  ## Read by derived id rather than by value, so this keeps working however many

  ## selectors output$zone_filters rendered.

  zone_filter_selection <- function() {
    map_ids <- active_zone_map_ids()

    if (length(map_ids) == 0)

      return(list())

    zones <- lapply(seq_along(map_ids), function(k) {
      z <- input[[zone_filter_input_id(k)]]

      if (is.null(z) || length(z) != 1 || is.na(z) || !nzchar(z))

        return(numeric(0))

      z <- suppressWarnings(as.numeric(z))

      if (is.na(z) || z == all_id) numeric(0) else z
    })

    names(zones) <- map_ids

    zones
  }

  ## Forget the zone picked for a selector that is going away.

  ##

  ## Deselecting a zonation map removes its selector from the DOM, but the value

  ## lingers in `input` - so if the map is later selected again (or the slot is

  ## reused by a different map), the stale zone would silently re-apply as a

  ## filter. Clearing it makes "no zonation map" a genuinely clean state.

  reset_zone_filter <- function(k) {
    input_id <- zone_filter_input_id(k)

    if (!is.null(input[[input_id]]))

      updateSelectizeInput(session, input_id, selected = as.character(all_id))
  }

  ## When the active zonation maps change, drop the values held by selectors that

  ## no longer correspond to a selected map, and reset every selector's contents

  ## when the last map is deselected.

  observeEvent(list(input$matrix_zone_map, input$analysis_zone_map), {
    map_ids <- active_zone_map_ids()

    ## Selectors are numbered 1..N over the CURRENT selection, so any slot above

    ## N belongs to a map that is gone.

    n <- length(map_ids)

    for (k in seq_len(6)) {
      input_id <- zone_filter_input_id(k)

      if (is.null(input[[input_id]]))

        next

      if (k > n) {
        reset_zone_filter(k)

        next
      }

      ## Slot still in use: make sure the stored zone is valid for the map now

      ## sitting in it, otherwise fall back to "All zones".

      map_id <- map_ids[[k]]

      legend <- isolate(v$zone_legends)[[map_id]]

      valid <- if (!is.null(legend)) c(all_id, legend$zone_id) else all_id

      current <- suppressWarnings(as.numeric(input[[input_id]]))

      if (length(current) != 1 || is.na(current) || !(current %in% valid))

        reset_zone_filter(k)
    }
  }, ignoreInit = TRUE)

  ## A change to any per-map zone selector must rebuild the matrix, because the

  ## zone columns are produced BY that build (update_lc_matrix() crosstabs the

  ## zone rasters). Filtering a stale matrix down to a zone it was never built

  ## with would drop every row, so the rebuild has to happen here rather than at

  ## render time.

  ##

  ## The selector ids are derived (zone_filter_1..N), so the dependency has to

  ## be taken on the whole input list rather than on named inputs: shiny cannot

  ## depend on inputs that do not exist yet.

  observeEvent(input$zone_filter_1, rebuild_matrix_for_zones(), ignoreInit = TRUE)

  observeEvent(input$zone_filter_2, rebuild_matrix_for_zones(), ignoreInit = TRUE)

  observeEvent(input$zone_filter_3, rebuild_matrix_for_zones(), ignoreInit = TRUE)

  observeEvent(input$zone_filter_4, rebuild_matrix_for_zones(), ignoreInit = TRUE)

  observeEvent(input$zone_filter_5, rebuild_matrix_for_zones(), ignoreInit = TRUE)

  observeEvent(input$zone_filter_6, rebuild_matrix_for_zones(), ignoreInit = TRUE)

  ## Resolve the zone selection into the per-map layers and the per-map zone

  ## constraints, so the overlay can be applied. A map with no zone picked (or

  ## no selectors at all) simply drops out of the constraint list.

  active_zone_overlay <- function() {
    map_ids <- active_zone_map_ids()

    empty <- list(layers = list(), zones = list(), map_ids = character(0))

    if (length(map_ids) == 0)

      return(empty)

    recs <- isolate(v$map_catalog)

    layers <- lapply(map_ids, function(id) recs[[id]]$stars)

    names(layers) <- map_ids

    list(layers = layers,
         zones = zone_filter_selection(),
         map_ids = map_ids)
  }

  lc_area_bar_plot <- reactive({
    sel <- matrix_selection()

    if (is.null(sel) || is.null(sel$rec1))

      return(NULL)

    rec <- sel$rec1

    ## Prefer the zone-clipped areas when a zonation overlay is active.

    ov <- active_zone_overlay()

    df <- NULL

    if (length(ov$layers) > 0)

      df <- lc_area_within_zone(rec$stars, ov$layers, ov$zones)

    if (is.null(df) || nrow(df) == 0)

      df <- if (!is.null(rec$freq_df)) rec$freq_df else get_raster_freq_df(rec$stars, rec$sf)

    if (is.null(df) || nrow(df) == 0)

      return(NULL)

    df <- df[!is.na(df$area) & df$area > 0, , drop = FALSE]

    if (nrow(df) == 0)

      return(NULL)

    ## Convert pixel counts â†’ hectares so the bar heights match map info totals.

    cell_ha <- pixel_ha(rec$stars)

    if (!is.na(cell_ha) && cell_ha > 0)

      df$area <- df$area * cell_ha

    df <- df[order(-df$area), , drop = FALSE]

    lc_df <- v$lc_list_df

    labels <- vapply(df$id, function(id) {
      if (is.null(lc_df))

        return(as.character(id))

      hit <- match(id, lc_df$lc_id)

      if (is.na(hit)) as.character(id) else as.character(lc_df$label[hit])
    }, character(1))

    colors <- vapply(df$id, function(id) get_landcover_color(id), character(1))

    ## Note in the title that the areas are restricted to a zone overlay.

    title_txt <- paste0("Land cover area - ", rec$year)

    if (length(ov$map_ids) > 0) {
      title_txt <- paste0(title_txt, zone_filter_caption())
    }

    plot_ly(
      x = labels,
      y = df$area,
      type = "bar",
      marker = list(color = colors),
      text = format(round(df$area), big.mark = ",", scientific = FALSE),
      textposition = "outside",
      hovertemplate = paste0("%{x}<br>", f_number(df$area, " ha"), "<extra></extra>"),
      customdata = df$id
    ) |> layout(
      title = list(text = title_txt, font = list(size = 14)),
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "Area (ha)"),
      margin = list(b = 130, t = 40, l = 70, r = 20),
      bargap = 0.25
    )
  })

  output$lc_area_bar <- renderPlotly({
    lc_area_bar_plot()
  })

  get_lc_colDef <- function(lc_id_list,
                            align = "left",
                            sticky = NULL,
                            footer = NULL) {
    if (length(lc_id_list) == 0) {
      return(colDef(name = ""))
    }

    lc_df <- isolate(v$lc_list_df)

    if (is.null(lc_df) || nrow(lc_df) == 0) {
      return(colDef(name = "Land cover ID", align = align, sticky = sticky, footer = footer))
    }

    lc_df <- lc_df[lc_df$lc_id %in% lc_id_list, ]

    lmax <- if (length(lc_df$label) > 0) max(nchar(lc_df$label)) else 10

    lc_width <- lmax * 6 + 70

    c <- colDef(
      name = "Land cover ID",
      align = align,
      sticky = sticky,
      minWidth = lc_width,
      footer = footer,
      footerStyle = list(backgroundColor = "", fontWeight = "bold"),
      cell = function(value) {
        ## Never index with a NA/zero-length id: `lc_df$lc_id == NA` yields all

        ## NA, which subsets to a zero-row result and makes reactable's

        ## internal lapply call `if (NA)` -> "missing value where TRUE/FALSE

        ## needed". Use a plain lookup and fall back to the raw value.

        id <- suppressWarnings(as.numeric(as.character(value)))

        lc <- character(0)

        if (length(id) == 1 && !is.na(id)) {
          hit <- match(id, lc_df$lc_id)

          if (!is.na(hit))

            lc <- lc_df$label[hit]
        }

        if (length(lc) == 0 || is.na(lc))

          lc <- as.character(value)

        div(lc, span(
          if (is.na(id)) as.character(value) else id,
          style = paste(
            "color:white;padding-top: 2px; border-radius:3px;

              width:30px; margin-left:5px; display:inline-block; text-align: center;

                                     background-color:",
            get_landcover_color(id)
          )
        ))
      }
    )

    return(c)
  }

  ## Legend rows for the ACTIVE selection, used for the palette and the zone

  ## legend display.

  ##

  ## There is no longer any relabelling to undo: each selected map keeps its own

  ## zone ids and is filtered through its own selector, so this is just the

  ## union of the selected maps' legends. Nothing is composed, so nothing can

  ## collide, and no zone id has to be decoded back to a map.

  active_zone_legend <- function() {
    map_ids <- active_zone_map_ids()

    legends <- isolate(v$zone_legends)

    if (length(map_ids) == 0)

      return(isolate(v$zone_list_df))

    if (length(map_ids) == 1)

      return(legends[[map_ids[[1]]]])

    available <- legends[map_ids]

    available <- available[!vapply(available, is.null, logical(1))]

    if (length(available) == 0)

      return(NULL)

    do.call(rbind, available)
  }

  total_style <- list(
    backgroundColor = "#FCE5E6",
    fontWeight = "bold",
    border = "1px solid white",
    borderRadius = "5px"
  )

  ## The changes matrix. `zones` is the per-map selection (see

  ## zone_filter_selection()): each active zonation map contributes a zone_<id>
  ## column to v$lc_changes_df, and the rows kept are those satisfying EVERY

  ## selector - the combination filter, expressed as an AND over the columns.

  render_lc_changes <- function(zones) {
    if (is.null(v$lc_changes_df) || is.null(v$lc_list_df)) {
      return()
    }

    lcz <- v$lc_changes_df

    ## Apply one constraint per zonation map that has a zone picked. A map left

    ## on "All zones" (empty vector) imposes nothing.

    for (map_id in names(zones)) {
      wanted <- zones[[map_id]]

      if (is.null(wanted) || length(wanted) == 0)

        next

      col <- paste0("zone_", map_id)

      if (!(col %in% colnames(lcz)))

        next

      lcz <- lcz[!is.na(lcz[[col]]) & lcz[[col]] %in% wanted, , drop = FALSE]
    }

    ## Only the transition columns matter for the matrix itself.

    keep_cols <- intersect(c("lc1_id", "lc2_id", "area"), colnames(lcz))

    lcz <- lcz[, keep_cols, drop = FALSE]

    if (nrow(lcz) == 0) {
      return()
    }

    m <- dcast(lcz,
               lc1_id ~ lc2_id,
               value.var = "area",
               fun.aggregate = sum)

    c1 <- m[1]

    m[m == 0] <- NA

    m[1] <- c1

    if (nrow(m) >= 1) {
      if (ncol(m) == 2) {
        m$TOTAL <- m[[2]]
      } else {
        mt <- apply(m[, -1, drop = FALSE], 1, function(x) {
          sum(x, na.rm = TRUE)
        })

        m$TOTAL <- mt
      }
    }

    lmax <-
      max(nchar(v$lc_list_df[v$lc_list_df$lc_id %in% m$lc1_id, "label"]))

    lc_width <- lmax * 8

    ## Row ids can be 0, which is falsy in JS and collides with the "no row"

    ## sentinel, so expose them as strings and use the string as the reactable

    ## row key. Also keep only rows/cols that are actually present, so the

    ## `class` callback can compare safely.

    row_ids <- as.character(m[[1]])

    m_out <- m

    m_out[[1]] <- row_ids

    reactable(
      m_out,
      pagination = F,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      wrap = FALSE,
      sortable = F,
      fullWidth = F,
      rowStyle = NULL,
      style = list(maxWidth = "100%"),
      theme = reactableTheme(style = list(fontSize = "0.9em")),
      columns = list(
        lc1_id = get_lc_colDef(
          m$lc1_id,
          align = "right",
          sticky = "left",
          footer = "TOTAL"
        ),
        TOTAL = colDef(
          sticky = "right",
          minWidth = 80,
          style = total_style
        )
      ),
      defaultColDef = colDef(
        format = colFormat(digit = 0, separators = TRUE),
        minWidth = 60,
        header = function(x, y) {
          if (y == "lc1_id") {
            return(x)
          } else if (y == "TOTAL") {
            return(y)
          } else {
            div(
              x,
              style = paste(
                "color:white;padding: 4px 20px; border-radius:3px;

                                    background-color:",
                get_landcover_color(x)
              )
            )
          }
        },
        class = function(value, index, name) {
          ## `index` is the 1-based row index; `name` the column name

          ## (character). Both sides are coerced to character so the

          ## comparison can never yield NA.

          row_id <- row_ids[index]

          if (length(row_id) != 1 || is.na(row_id) || is.na(name))

            return(NULL)

          if (identical(as.character(name), as.character(row_id)))

            return("matrix_diagonal")

          NULL
        },
        footerStyle = total_style,
        footer = function(values) {
          f_number(sum(values, na.rm = T))
        }
      )
    )
  }

  output$lc_changes2 <- renderReactable({
    render_lc_changes(zone_filter_selection())
  })

  lc_changes_scenario_widget <- reactive({
    req(v$lc_changes_df, v$lc_list_df)

    zones <- zone_filter_selection()
    lcz <- v$lc_changes_df

    ## Apply one constraint per zonation map that has a zone picked.
    for (map_id in names(zones)) {
      wanted <- zones[[map_id]]
      if (is.null(wanted) || length(wanted) == 0) next
      col <- paste0("zone_", map_id)
      if (!(col %in% colnames(lcz))) next
      lcz <- lcz[!is.na(lcz[[col]]) & lcz[[col]] %in% wanted, , drop = FALSE]
    }

    keep_cols <- intersect(c("lc1_id", "lc2_id", "area"), colnames(lcz))
    lcz <- lcz[, keep_cols, drop = FALSE]
    if (nrow(lcz) == 0) return(NULL)

    lcz$lc1_id <- as.integer(lcz$lc1_id)
    lcz$lc2_id <- as.integer(lcz$lc2_id)
    lcz$area   <- as.numeric(lcz$area)
    lcz <- lcz[is.finite(lcz$area) & lcz$area > 0, , drop = FALSE]
    if (nrow(lcz) == 0) return(NULL)

    lcz_agg <- stats::aggregate(area ~ lc1_id + lc2_id, data = lcz, FUN = sum)
    lcz_agg$zone_id      <- 0L
    lcz_agg$scenario_id  <- 0L
    lcz_agg$iteration_id <- 0L

    sel <- matrix_selection()
    y1 <- if (!is.null(sel$rec1) && !is.null(sel$rec1$year)) sel$rec1$year else 2000L
    y2 <- if (!is.null(sel$rec2) && !is.null(sel$rec2$year)) sel$rec2$year else (y1 + 10L)
    min_y <- min(y1, y2)
    max_y <- max(y1, y2)
    if (min_y == max_y) max_y <- min_y + 1L
    d1 <- as.Date(paste0(min_y, "-07-01"))
    d2 <- as.Date(paste0(max_y, "-07-01"))

    zn <- data.frame(zone_id = 0L, label = "All zones", description = "", stringsAsFactors = FALSE)

    ab <- prepare_projection_abacus(
      landcover             = v$lc_list_df,
      landcover_change      = lcz_agg,
      zone                  = zn,
      carbonstock           = v$cstock_list,
      date1                 = d1,
      date2                 = d2,
      n_iteration           = 1L
    )
    if (is.null(ab)) return(NULL)

    scenario_editor(
      data               = ab,
      scenario           = NULL,
      selected_iteration = 1L,
      selected_zone      = 0L,
      selected_lc        = 0L,
      editable           = FALSE,
      initial_view_mode  = "matrix"
    )
  })

  output$lc_changes_scenario <- render_scenario_editor({
    lc_changes_scenario_widget()
  })

  ### CARBON EMISSION CALCULATION ######################

  observe({
    if (is.null(v$lc_changes_df))

      return()

    c_df <- v$cstock_list

    if (is.null(v$lc_changes_df) || is.null(v$cstock_list))

      return()

    c_df <- v$cstock_list

    if (!is.data.frame(c_df) || nrow(c_df) == 0 || !("c" %in% colnames(c_df)))

      return()

    c_vals <- suppressWarnings(as.numeric(c_df$c))

    if (all(is.na(c_vals)))

      return()

    ## All-zero carbon stock is a valid input: it yields a zero emission map,
    ## which is still shown rather than leaving the map card blank.

    sel <- matrix_changes()

    m1 <- if (!is.null(sel) && !is.null(sel$map1)) sel$map1 else isolate(v$map1_stars)

    m2 <- if (!is.null(sel) && !is.null(sel$map2)) sel$map2 else isolate(v$map2_stars)

    if (!is.null(m1) && !is.null(m2)) {
      m2 <- align_stars_to_reference(m2, m1)

      m1c <- reclassify_map(m1, c_df)

      m2c <- reclassify_map(m2, c_df)

      ## calculate emission ##

      me <- (m1c - m2c) * 44 / 12

      v$mapc_stars <- me

      cell_ha <- pixel_ha(m1)

      if (is.na(cell_ha) || cell_ha <= 0) cell_ha <- 1

      pos_em <- me[me > 0][[1]]

      neg_em <- me[me < 0][[1]]

      v$c_emission_total <- if (length(pos_em) > 0) sum(pos_em, na.rm = TRUE) * cell_ha else 0

      v$c_sequestration_total <- if (length(neg_em) > 0) abs(sum(neg_em, na.rm = TRUE)) * cell_ha else 0

      if (!is.null(v$map_ghg_stars)) {
        v$map_all_stars <- tryCatch(me + v$map_ghg_stars, error = function(e) me)
      } else {
        v$map_all_stars <- me
      }

      v$all_emission_total <- sum(c(v$c_emission_total, v$other_emission_total), na.rm = TRUE)

      v$all_sequestration_total <- sum(c(v$c_sequestration_total, v$other_sequestration_total), na.rm = TRUE)
    } else {
      df <- v$lc_changes_df

      if (!is.null(df) && is.data.frame(df) && all(c("lc1_id", "lc2_id", "area") %in% colnames(df))) {
        cdf <- v$cstock_list[c("lc_id", "c")]

        colnames(cdf) <- c("lc1_id", "c1")

        df <- merge(df, cdf, by = "lc1_id", all.x = TRUE)

        colnames(cdf) <- c("lc2_id", "c2")

        df <- merge(df, cdf, by = "lc2_id", all.x = TRUE)

        df$c1 <- suppressWarnings(as.numeric(df$c1))

        df$c2 <- suppressWarnings(as.numeric(df$c2))

        df$c1[is.na(df$c1)] <- 0

        df$c2[is.na(df$c2)] <- 0

        df$e <- (df$c1 - df$c2) * df$area * 44 / 12

        v$c_emission_total <- sum(df[df$e > 0, "e"], na.rm = TRUE)

        v$c_sequestration_total <- abs(sum(df[df$e < 0, "e"], na.rm = TRUE))

        v$all_emission_total <- sum(c(v$c_emission_total, v$other_emission_total), na.rm = TRUE)

        v$all_sequestration_total <- sum(c(v$c_sequestration_total, v$other_sequestration_total), na.rm = TRUE)
      }
    }
  })

### MAP PLOT OF EMISSION ###################

## The GHG map never renders as an empty pane: when there is nothing to

## compute yet it draws the same frame with a message inside.

draw_ghg_placeholder <- function(msg) {
  plot.new()

  graphics::title("Emission intensity")

  graphics::text(0.5, 0.5, msg, col = "grey40", cex = 1.2)
}

map_render_ids <- c("mapc", "map_ghg", "map_all")

  lapply(map_render_ids, function(id) {
    output[[paste0(id, "_out")]] <- renderPlot({
      # Establish reactive dependency on fullscreen state

      # bslib auto-creates <card_id>_full_screen (with underscore), custom JS creates <card_id>_fullscreen (without)

      fs_id_bslib <- paste0(id, "_card_full_screen")

      fs_id_custom <- paste0(id, "_card_fullscreen")

      is_fs <- (!is.null(input[[fs_id_bslib]]) && isTRUE(input[[fs_id_bslib]])) ||
               (!is.null(input[[fs_id_custom]]) && isTRUE(input[[fs_id_custom]]))

      m <- v[[paste0(id, "_stars")]]

      ## The GHG map is drawn only once its inputs are complete; the carbon

      ## maps keep drawing whatever is available.

      if (id == "map_ghg") {
        if (!isTRUE(v$ghg_ready) || is.null(m)) {
          draw_ghg_placeholder("Select flux emission sources to show the map")

          return()
        }
      } else if (id == "map_all") {
        if (is.null(m)) {
          draw_ghg_placeholder("Add carbon stocks and/or flux factors to show the combined map")

          return()
        }
      } else if (is.null(m)) {
        return()
      }

      # Get values for color scaling

      if (inherits(m, "stars_proxy")) {
        m_data <- stars::st_as_stars(m)

        vals_vec <- as.vector(m_data[[1]])
      } else {
        vals_vec <- as.vector(m[[1]])
      }

      vals_vec <- vals_vec[!is.na(vals_vec)]

      if (length(vals_vec) == 0) {
        if (id == "map_ghg")

          draw_ghg_placeholder("No map data")
        else {
          plot.new()

          title("No data")
        }

        return()
      }

      vr <- range(vals_vec)

      rm <- max(abs(vr[1]), vr[2])

      if (is.na(rm) || rm <= 0) rm <- 1

      # Create centered diverging color scale with grey for zero emission

      n_half <- 50

      delta <- max(1e-4, rm / (2 * n_half))

      breaks <- c(
        seq(-rm, -delta, length.out = n_half),
        seq(delta, rm, length.out = n_half + 1)
      )

      cols <- c(
        colorRampPalette(c("darkgreen", "#74c476"))(n_half - 1),
        "grey90",
        colorRampPalette(c("#fc9272", "red3"))(n_half)
      )

      # Plot using stars::plot with continuous color scale

      m_plot <- if (inherits(m, "stars_proxy")) stars::st_as_stars(m) else m

      # Show color key legend for all emission intensity maps (carbon, GHG flux, combined)

      show_key <- TRUE

      if (show_key) {
        # Show color key

        suppressMessages(plot(m_plot, col = cols, breaks = breaks, main = "", axes = FALSE, key.pos = 1, reset = FALSE))
      } else {
        # Compact: no color key

        suppressMessages(plot(m_plot, col = cols, breaks = breaks, main = "", axes = FALSE, key.pos = NULL, reset = FALSE))
      }
    })

    # With renderPlot, maps auto-update when reactives change
  })

  ### BOX INFO OF EMISSION ###################

  lapply(c("c", "other", "all"), function(id) {
    output[[paste0("box_", id, "_emission_out")]] <- renderUI({
      e <-
        (v[[paste0(id, "_emission_total")]] - v[[paste0(id, "_sequestration_total")]]) /
        1000

      p(f_number(e, digits = 3), co2_unit("kt"))
    })

    output[[paste0("box_", id, "_emission_info")]] <- renderUI({
      tagList(
        p(
          "Total emission:",
          f_number(v[[paste0(id, "_emission_total")]] / 1000, digits = 3),
          co2_unit("kt")
        ),
        p(
          "Total sequestration:",
          f_number(v[[paste0(id, "_sequestration_total")]] / 1000, digits = 3),
          co2_unit("kt")
        )
      )
    })
  })

  ### CARBON EMISSION SUMMARY UI ###################

  output$c_emission_summary_ui <- renderUI({
    gross_em <- v$c_emission_total

    seq_em   <- v$c_sequestration_total

    c_df     <- v$cstock_list

    m_stars  <- v$mapc_stars

    lc_ch    <- v$lc_changes_df

    has_data <- (!is.null(gross_em) && (gross_em > 0 || seq_em > 0)) ||
                (!is.null(m_stars) && !is.null(c_df) && sum(c_df$c, na.rm = TRUE) > 0)

    if (!has_data) {
      return(tags$div(
        class = "text-muted text-center py-5",
        icon("calculator", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small", "Select baseline and comparison maps and fill in carbon stock values to calculate emissions.")
      ))
    }

    gross_val <- if (!is.null(gross_em) && length(gross_em) == 1 && !is.na(gross_em)) as.numeric(gross_em) else 0

    seq_val   <- if (!is.null(seq_em) && length(seq_em) == 1 && !is.na(seq_em)) abs(as.numeric(seq_em)) else 0

    net_val   <- gross_val - seq_val

    has_emission <- isTRUE(gross_val > 0) || isTRUE(seq_val > 0)

    has_cstock   <- !is.null(c_df) && is.data.frame(c_df) && nrow(c_df) > 0 && "c" %in% colnames(c_df) && isTRUE(sum(as.numeric(c_df$c), na.rm = TRUE) > 0)

    has_map      <- !is.null(m_stars)

    if (!has_emission && !(has_map && has_cstock)) {
      return(tags$div(
        class = "text-muted text-center py-5",
        icon("calculator", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small", "Select baseline and comparison maps and fill in carbon stock values to calculate emissions.")
      ))
    }

    # Status classification

    if (isTRUE(net_val > 0)) {
      status_label  <- "Net Source"

      status_badge  <- "bg-danger"

      banner_border <- "border-danger"

      banner_bg     <- "bg-danger-subtle text-danger-emphasis"
    } else if (isTRUE(net_val < 0)) {
      status_label  <- "Net Sink"

      status_badge  <- "bg-success"

      banner_border <- "border-success"

      banner_bg     <- "bg-success-subtle text-success-emphasis"
    } else {
      status_label  <- "Neutral"

      status_badge  <- "bg-secondary"

      banner_border <- "border-secondary"

      banner_bg     <- "bg-light"
    }

    # Formatting helpers

    fmt_co2 <- function(val_ton) {
      if (is.null(val_ton) || is.na(val_ton)) return("0 t")

      abs_val <- abs(val_ton)

      if (abs_val >= 1e6) {
        paste0(format(round(val_ton / 1e6, 2), big.mark = ","), " Mt")
      } else if (abs_val >= 1e3) {
        paste0(format(round(val_ton / 1e3, 2), big.mark = ","), " kt")
      } else {
        paste0(format(round(val_ton, 1), big.mark = ","), " t")
      }
    }

    fmt_num <- function(x, digits = 0) {
      if (is.null(x) || length(x) != 1 || is.na(x)) "N/A" else format(round(as.numeric(x), digits), big.mark = ",")
    }

    # Period resolution

    sel_rec <- matrix_selection()

    y1_str <- NULL

    y2_str <- NULL

    period_yrs <- if (!is.null(v$period_year) && length(v$period_year) == 1 && !is.na(v$period_year)) as.numeric(v$period_year) else NA_real_

    if (!is.null(sel_rec) && !is.null(sel_rec$rec1) && !is.null(sel_rec$rec2)) {
      recs <- isolate(v$map_catalog)

      pair_ids <- order_map_pair(sel_rec$rec1$id, sel_rec$rec2$id, recs)

      r1 <- recs[[pair_ids$id1]]

      r2 <- recs[[pair_ids$id2]]

      if (!is.null(r1) && !is.null(r2) && !is.null(r1$year) && !is.null(r2$year) && !is.na(r1$year) && !is.na(r2$year)) {
        y1_int <- as.integer(r1$year)

        y2_int <- as.integer(r2$year)

        y1_str <- as.character(y1_int)

        y2_str <- as.character(y2_int)

        if (is.na(period_yrs) || period_yrs <= 0) {
          period_yrs <- as.numeric(abs(y2_int - y1_int))
        }
      }
    }

    if (is.null(y1_str) && !is.null(v$map1_year) && !is.na(v$map1_year)) {
      y1_str <- as.character(v$map1_year)
    }

    if (is.null(y2_str) && !is.null(v$map2_year) && !is.na(v$map2_year)) {
      y2_str <- as.character(v$map2_year)
    }

    if (is.na(period_yrs) || period_yrs <= 0) {
      if (!is.null(y1_str) && !is.null(y2_str)) {
        y1_num <- suppressWarnings(as.numeric(y1_str))

        y2_num <- suppressWarnings(as.numeric(y2_str))

        if (!is.na(y1_num) && !is.na(y2_num) && y2_num >= y1_num) {
          period_yrs <- y2_num - y1_num
        }
      }
    }

    period_str <- if (!is.na(period_yrs) && period_yrs > 0) {
      if (!is.null(y1_str) && !is.null(y2_str) && nzchar(y1_str) && nzchar(y2_str)) {
        paste0(y1_str, " \u2013 ", y2_str, " (", round(period_yrs, 1), " yrs)")
      } else {
        paste0(round(period_yrs, 1), " years")
      }
    } else "N/A"

    # Annual rate

    annual_net_str <- if (!is.na(period_yrs) && period_yrs > 0) {
      paste0(fmt_co2(net_val / period_yrs), " CO₂e yr⁻¹")
    } else "N/A"

    # Total area

    total_ha <- if (!is.null(lc_ch) && is.data.frame(lc_ch) && nrow(lc_ch) > 0 && "area" %in% colnames(lc_ch)) {
      sum(as.numeric(lc_ch$area), na.rm = TRUE)
    } else NA_real_

    area_str <- if (!is.na(total_ha) && total_ha > 0) paste0(fmt_num(total_ha), " ha") else "N/A"

    # Emission intensity

    intensity_str <- if (!is.na(total_ha) && total_ha > 0 && !is.na(period_yrs) && period_yrs > 0) {
      paste0(fmt_num(net_val / (total_ha * period_yrs), 2), " t CO₂e ha⁻¹ yr⁻¹")
    } else "N/A"

    tags$div(
      class = "d-flex flex-column gap-2",
      style = "font-size: 0.85rem;",

      # 1. Net Emission Hero Banner

      tags$div(
        class = paste("p-3 rounded border", banner_border, banner_bg),
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(class = "fw-bold text-uppercase small", style = "letter-spacing: 0.5px;", "Net Carbon Emission"),
          tags$span(class = paste("badge", status_badge), status_label)
        ),
        tags$div(
          class = "d-flex align-items-baseline gap-2",
          tags$span(style = "font-size: 1.5rem; font-weight: 700;", fmt_co2(net_val)),
          tags$span(class = "fw-semibold", "CO₂e")
        ),
        tags$div(
          class = "small text-muted",
          paste0("(", fmt_num(net_val), " t CO₂e)")
        )
      ),

      # 2. Breakdown Cards: Gross Emission & Sequestration

      fluidRow(
        column(
          6,
          tags$div(
            class = "p-2 rounded border bg-light text-start",
            tags$div(class = "small text-muted mb-1", icon("fire", class = "text-danger me-1"), "Gross Emission"),
            tags$div(class = "fw-bold text-danger", style = "font-size: 1.05rem;", fmt_co2(gross_val)),
            tags$div(class = "text-muted", style = "font-size: 0.72rem;", paste0(fmt_num(gross_val), " t"))
          )
        ),
        column(
          6,
          tags$div(
            class = "p-2 rounded border bg-light text-start",
            tags$div(class = "small text-muted mb-1", icon("seedling", class = "text-success me-1"), "Sequestration"),
            tags$div(class = "fw-bold text-success", style = "font-size: 1.05rem;", fmt_co2(seq_val)),
            tags$div(class = "text-muted", style = "font-size: 0.72rem;", paste0(fmt_num(seq_val), " t"))
          )
        )
      ),

      # 3. Key Details List

      tags$div(
        class = "border rounded p-2 bg-light",
        tags$table(
          class = "table table-sm table-borderless mb-0",
          style = "font-size: 0.8rem;",
          tags$tbody(
            tags$tr(
              tags$td(class = "text-muted py-1", icon("calendar", class = "me-1 text-secondary"), "Period:"),
              tags$td(class = "text-end fw-semibold py-1", period_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("gauge-high", class = "me-1 text-secondary"), "Annual Net:"),
              tags$td(class = "text-end fw-semibold py-1", annual_net_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("ruler-combined", class = "me-1 text-secondary"), "Total Area:"),
              tags$td(class = "text-end fw-semibold py-1", area_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("chart-line", class = "me-1 text-secondary"), "Mean Intensity:"),
              tags$td(class = "text-end fw-semibold py-1", intensity_str)
            )
          )
        )
      )
    )
  })

  ### GHG EMISSION SUMMARY UI ###################

  ## Mirrors the carbon emission summary, using the "other sources" totals and

  ## the same period resolution (GHG factors are annual, so the period matters).

  output$ghg_emission_summary_ui <- renderUI({
    gross_em <- v$other_emission_total

    seq_em   <- v$other_sequestration_total

    m_stars  <- v$map_ghg_stars

    has_src  <- length(v$other_emission_source) > 0

    gross_val <- if (!is.null(gross_em) && length(gross_em) == 1 && !is.na(gross_em)) as.numeric(gross_em) else 0

    seq_val   <- if (!is.null(seq_em) && length(seq_em) == 1 && !is.na(seq_em)) abs(as.numeric(seq_em)) else 0

    net_val   <- gross_val - seq_val

    if (!has_src && is.null(m_stars)) {
      return(tags$div(
        class = "text-muted text-center py-5",
        icon("calculator", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small", "Add emission sources and factors to calculate additional emissions.")
      ))
    }

    # Status classification

    if (isTRUE(net_val > 0)) {
      status_label  <- "Net Source"

      status_badge  <- "bg-danger"

      banner_border <- "border-danger"

      banner_bg     <- "bg-danger-subtle text-danger-emphasis"
    } else if (isTRUE(net_val < 0)) {
      status_label  <- "Net Sink"

      status_badge  <- "bg-success"

      banner_border <- "border-success"

      banner_bg     <- "bg-success-subtle text-success-emphasis"
    } else {
      status_label  <- "Neutral"

      status_badge  <- "bg-secondary"

      banner_border <- "border-secondary"

      banner_bg     <- "bg-light"
    }

    fmt_co2 <- function(val_ton) {
      if (is.null(val_ton) || is.na(val_ton)) return("0 t")

      abs_val <- abs(val_ton)

      if (abs_val >= 1e6) {
        paste0(format(round(val_ton / 1e6, 2), big.mark = ","), " Mt")
      } else if (abs_val >= 1e3) {
        paste0(format(round(val_ton / 1e3, 2), big.mark = ","), " kt")
      } else {
        paste0(format(round(val_ton, 1), big.mark = ","), " t")
      }
    }

    fmt_num <- function(x, digits = 0) {
      if (is.null(x) || length(x) != 1 || is.na(x)) "N/A" else format(round(as.numeric(x), digits), big.mark = ",")
    }

    # Period resolution (same fallbacks as the carbon summary)

    sel_rec <- matrix_selection()

    y1_str <- NULL

    y2_str <- NULL

    period_yrs <- if (!is.null(v$period_year) && length(v$period_year) == 1 && !is.na(v$period_year)) as.numeric(v$period_year) else NA_real_

    if (!is.null(sel_rec) && !is.null(sel_rec$rec1) && !is.null(sel_rec$rec2)) {
      recs <- isolate(v$map_catalog)

      pair_ids <- order_map_pair(sel_rec$rec1$id, sel_rec$rec2$id, recs)

      r1 <- recs[[pair_ids$id1]]

      r2 <- recs[[pair_ids$id2]]

      if (!is.null(r1) && !is.null(r2) && !is.null(r1$year) && !is.null(r2$year) && !is.na(r1$year) && !is.na(r2$year)) {
        y1_str <- as.character(as.integer(r1$year))

        y2_str <- as.character(as.integer(r2$year))

        if (is.na(period_yrs) || period_yrs <= 0) {
          period_yrs <- as.numeric(abs(as.integer(r2$year) - as.integer(r1$year)))
        }
      }
    }

    if (is.null(y1_str) && !is.null(v$map1_year) && !is.na(v$map1_year)) {
      y1_str <- as.character(v$map1_year)
    }

    if (is.null(y2_str) && !is.null(v$map2_year) && !is.na(v$map2_year)) {
      y2_str <- as.character(v$map2_year)
    }

    if (is.na(period_yrs) || period_yrs <= 0) {
      if (!is.null(y1_str) && !is.null(y2_str)) {
        y1_num <- suppressWarnings(as.numeric(y1_str))

        y2_num <- suppressWarnings(as.numeric(y2_str))

        if (!is.na(y1_num) && !is.na(y2_num) && y2_num >= y1_num) {
          period_yrs <- y2_num - y1_num
        }
      }
    }

    period_str <- if (!is.na(period_yrs) && period_yrs > 0) {
      if (!is.null(y1_str) && !is.null(y2_str) && nzchar(y1_str) && nzchar(y2_str)) {
        paste0(y1_str, " \u2013 ", y2_str, " (", round(period_yrs, 1), " yrs)")
      } else {
        paste0(round(period_yrs, 1), " years")
      }
    } else "N/A"

    annual_net_str <- if (!is.na(period_yrs) && period_yrs > 0) {
      paste0(fmt_co2(net_val / period_yrs), " CO\u2082e yr\u207b\u00b9")
    } else "N/A"

    # Total area of the selected map pair

    lc_ch <- v$lc_changes_df

    total_ha <- if (!is.null(lc_ch) && is.data.frame(lc_ch) && nrow(lc_ch) > 0 && "area" %in% colnames(lc_ch)) {
      sum(as.numeric(lc_ch$area), na.rm = TRUE)
    } else NA_real_

    area_str <- if (!is.na(total_ha) && total_ha > 0) paste0(fmt_num(total_ha), " ha") else "N/A"

    intensity_str <- if (!is.na(total_ha) && total_ha > 0 && !is.na(period_yrs) && period_yrs > 0) {
      paste0(fmt_num(net_val / (total_ha * period_yrs), 2), " t CO\u2082e ha\u207b\u00b9 yr\u207b\u00b9")
    } else "N/A"

    tags$div(
      class = "d-flex flex-column gap-2",
      style = "font-size: 0.85rem;",

      # 1. Net Emission Hero Banner

      tags$div(
        class = paste("p-3 rounded border", banner_border, banner_bg),
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(class = "fw-bold text-uppercase small", style = "letter-spacing: 0.5px;", "Net GHG Emission"),
          tags$span(class = paste("badge", status_badge), status_label)
        ),
        tags$div(
          class = "d-flex align-items-baseline gap-2",
          tags$span(style = "font-size: 1.5rem; font-weight: 700;", fmt_co2(net_val)),
          tags$span(class = "fw-semibold", "CO\u2082e")
        ),
        tags$div(
          class = "small text-muted",
          paste0("(", fmt_num(net_val), " t CO\u2082e)")
        )
      ),

      # 2. Breakdown Cards: Gross Emission & Sequestration

      fluidRow(
        column(
          6,
          tags$div(
            class = "p-2 rounded border bg-light text-start",
            tags$div(class = "small text-muted mb-1", icon("fire", class = "text-danger me-1"), "Gross Emission"),
            tags$div(class = "fw-bold text-danger", style = "font-size: 1.05rem;", fmt_co2(gross_val)),
            tags$div(class = "text-muted", style = "font-size: 0.72rem;", paste0(fmt_num(gross_val), " t"))
          )
        ),
        column(
          6,
          tags$div(
            class = "p-2 rounded border bg-light text-start",
            tags$div(class = "small text-muted mb-1", icon("seedling", class = "text-success me-1"), "Sequestration"),
            tags$div(class = "fw-bold text-success", style = "font-size: 1.05rem;", fmt_co2(seq_val)),
            tags$div(class = "text-muted", style = "font-size: 0.72rem;", paste0(fmt_num(seq_val), " t"))
          )
        )
      ),

      # 3. Key Details List

      tags$div(
        class = "border rounded p-2 bg-light",
        tags$table(
          class = "table table-sm table-borderless mb-0",
          style = "font-size: 0.8rem;",
          tags$tbody(
            tags$tr(
              tags$td(class = "text-muted py-1", icon("calendar", class = "me-1 text-secondary"), "Period:"),
              tags$td(class = "text-end fw-semibold py-1", period_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("gauge-high", class = "me-1 text-secondary"), "Annual Net:"),
              tags$td(class = "text-end fw-semibold py-1", annual_net_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("ruler-combined", class = "me-1 text-secondary"), "Total Area:"),
              tags$td(class = "text-end fw-semibold py-1", area_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("chart-line", class = "me-1 text-secondary"), "Mean Intensity:"),
              tags$td(class = "text-end fw-semibold py-1", intensity_str)
            )
          )
        )
      )
    )
  })

  ### COMBINED EMISSION SUMMARY UI ###################
  ## Adds carbon stock-change emissions and GHG flux emissions for the same period.
  output$combined_emission_summary_ui <- renderUI({
    safe_tot <- function(x) {
      if (!is.null(x) && length(x) == 1 && is.finite(as.numeric(x)[1])) as.numeric(x) else 0
    }

    c_gross <- safe_tot(v$c_emission_total)
    c_seq   <- abs(safe_tot(v$c_sequestration_total))
    f_gross <- safe_tot(v$other_emission_total)
    f_seq   <- abs(safe_tot(v$other_sequestration_total))
    c_net   <- c_gross - c_seq
    f_net   <- f_gross - f_seq
    gross_val <- c_gross + f_gross
    seq_val   <- c_seq + f_seq
    net_val   <- c_net + f_net

    has_c   <- !is.null(v$mapc_stars) || isTRUE(c_gross > 0) || isTRUE(c_seq > 0)
    has_f   <- isTRUE(v$ghg_ready) || length(v$other_emission_source) > 0 || isTRUE(f_gross > 0) || isTRUE(f_seq > 0)
    has_map <- !is.null(v$map_all_stars)

    if (!has_c && !has_f && !has_map) {
      return(tags$div(
        class = "text-muted text-center py-5",
        icon("calculator", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small", "Fill in carbon stocks and/or flux emission factors to calculate the combined summary.")
      ))
    }

    if (isTRUE(net_val > 0)) {
      status_label  <- "Net Source"
      status_badge  <- "bg-danger"
      banner_border <- "border-danger"
      banner_bg     <- "bg-danger-subtle text-danger-emphasis"
    } else if (isTRUE(net_val < 0)) {
      status_label  <- "Net Sink"
      status_badge  <- "bg-success"
      banner_border <- "border-success"
      banner_bg     <- "bg-success-subtle text-success-emphasis"
    } else {
      status_label  <- "Neutral"
      status_badge  <- "bg-secondary"
      banner_border <- "border-secondary"
      banner_bg     <- "bg-light"
    }

    fmt_co2 <- function(val_ton) {
      if (is.null(val_ton) || is.na(val_ton)) return("0 t")
      abs_val <- abs(val_ton)
      if (abs_val >= 1e6) {
        paste0(format(round(val_ton / 1e6, 2), big.mark = ","), " Mt")
      } else if (abs_val >= 1e3) {
        paste0(format(round(val_ton / 1e3, 2), big.mark = ","), " kt")
      } else {
        paste0(format(round(val_ton, 1), big.mark = ","), " t")
      }
    }

    fmt_num <- function(x, digits = 0) {
      if (is.null(x) || length(x) != 1 || is.na(x)) "N/A" else format(round(as.numeric(x), digits), big.mark = ",")
    }

    sel_rec <- matrix_selection()
    y1_str <- NULL
    y2_str <- NULL
    period_yrs <- if (!is.null(v$period_year) && length(v$period_year) == 1 && !is.na(v$period_year)) as.numeric(v$period_year) else NA_real_

    if (!is.null(sel_rec) && !is.null(sel_rec$rec1) && !is.null(sel_rec$rec2)) {
      recs <- isolate(v$map_catalog)
      pair_ids <- order_map_pair(sel_rec$rec1$id, sel_rec$rec2$id, recs)
      r1 <- recs[[pair_ids$id1]]
      r2 <- recs[[pair_ids$id2]]
      if (!is.null(r1) && !is.null(r2) && !is.null(r1$year) && !is.null(r2$year) && !is.na(r1$year) && !is.na(r2$year)) {
        y1_str <- as.character(as.integer(r1$year))
        y2_str <- as.character(as.integer(r2$year))
        if (is.na(period_yrs) || period_yrs <= 0) {
          period_yrs <- as.numeric(abs(as.integer(r2$year) - as.integer(r1$year)))
        }
      }
    }

    if (is.null(y1_str) && !is.null(v$map1_year) && !is.na(v$map1_year)) {
      y1_str <- as.character(v$map1_year)
    }
    if (is.null(y2_str) && !is.null(v$map2_year) && !is.na(v$map2_year)) {
      y2_str <- as.character(v$map2_year)
    }

    if (is.na(period_yrs) || period_yrs <= 0) {
      if (!is.null(y1_str) && !is.null(y2_str)) {
        y1_num <- suppressWarnings(as.numeric(y1_str))
        y2_num <- suppressWarnings(as.numeric(y2_str))
        if (!is.na(y1_num) && !is.na(y2_num) && y2_num >= y1_num) {
          period_yrs <- y2_num - y1_num
        }
      }
    }

    period_str <- if (!is.na(period_yrs) && period_yrs > 0) {
      if (!is.null(y1_str) && !is.null(y2_str) && nzchar(y1_str) && nzchar(y2_str)) {
        paste0(y1_str, " \u2013 ", y2_str, " (", round(period_yrs, 1), " yrs)")
      } else {
        paste0(round(period_yrs, 1), " years")
      }
    } else "N/A"

    annual_net_str <- if (!is.na(period_yrs) && period_yrs > 0) {
      paste0(fmt_co2(net_val / period_yrs), " CO\u2082e yr\u207b\u00b9")
    } else "N/A"

    lc_ch <- v$lc_changes_df
    total_ha <- if (!is.null(lc_ch) && is.data.frame(lc_ch) && nrow(lc_ch) > 0 && "area" %in% colnames(lc_ch)) {
      sum(as.numeric(lc_ch$area), na.rm = TRUE)
    } else NA_real_
    area_str <- if (!is.na(total_ha) && total_ha > 0) paste0(fmt_num(total_ha), " ha") else "N/A"

    intensity_str <- if (!is.na(total_ha) && total_ha > 0 && !is.na(period_yrs) && period_yrs > 0) {
      paste0(fmt_num(net_val / (total_ha * period_yrs), 2), " t CO\u2082e ha\u207b\u00b9 yr\u207b\u00b9")
    } else "N/A"

    source_card <- function(title, icon_name, net, gross, seq) {
      tags$div(
        class = "p-2 rounded border bg-light",
        tags$div(class = "small text-muted mb-1", icon(icon_name, class = "me-1 text-secondary"), title),
        tags$div(class = "fw-bold", style = "font-size: 1.05rem;", fmt_co2(net),
                  tags$span(class = "fw-semibold text-muted", style = "font-size: 0.8rem; margin-left: 4px;", "net")),
        tags$div(class = "text-muted", style = "font-size: 0.72rem;",
                  paste0("Gross ", fmt_co2(gross), " \u00b7 Seq ", fmt_co2(seq)))
      )
    }

    tags$div(
      class = "d-flex flex-column gap-2",
      style = "font-size: 0.85rem;",
      tags$div(
        class = paste("p-3 rounded border", banner_border, banner_bg),
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(class = "fw-bold text-uppercase small", style = "letter-spacing: 0.5px;", "Net Combined Emission"),
          tags$span(class = paste("badge", status_badge), status_label)
        ),
        tags$div(
          class = "d-flex align-items-baseline gap-2",
          tags$span(style = "font-size: 1.5rem; font-weight: 700;", fmt_co2(net_val)),
          tags$span(class = "fw-semibold", "CO\u2082e")
        ),
        tags$div(
          class = "small text-muted",
          paste0("Carbon ", fmt_co2(c_net), " + Flux ", fmt_co2(f_net),
                 " (", fmt_num(net_val), " t CO\u2082e)")
        )
      ),
      fluidRow(
        column(6, source_card("Carbon stock change", "coins", c_net, c_gross, c_seq)),
        column(6, source_card("GHG flux", "wind", f_net, f_gross, f_seq))
      ),
      fluidRow(
        column(
          6,
          tags$div(
            class = "p-2 rounded border bg-light text-start",
            tags$div(class = "small text-muted mb-1", icon("fire", class = "text-danger me-1"), "Gross Emission"),
            tags$div(class = "fw-bold text-danger", style = "font-size: 1.05rem;", fmt_co2(gross_val)),
            tags$div(class = "text-muted", style = "font-size: 0.72rem;", paste0(fmt_num(gross_val), " t"))
          )
        ),
        column(
          6,
          tags$div(
            class = "p-2 rounded border bg-light text-start",
            tags$div(class = "small text-muted mb-1", icon("seedling", class = "text-success me-1"), "Sequestration"),
            tags$div(class = "fw-bold text-success", style = "font-size: 1.05rem;", fmt_co2(seq_val)),
            tags$div(class = "text-muted", style = "font-size: 0.72rem;", paste0(fmt_num(seq_val), " t"))
          )
        )
      ),
      tags$div(
        class = "border rounded p-2 bg-light",
        tags$table(
          class = "table table-sm table-borderless mb-0",
          style = "font-size: 0.8rem;",
          tags$tbody(
            tags$tr(
              tags$td(class = "text-muted py-1", icon("calendar", class = "me-1 text-secondary"), "Period:"),
              tags$td(class = "text-end fw-semibold py-1", period_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("gauge-high", class = "me-1 text-secondary"), "Annual Net:"),
              tags$td(class = "text-end fw-semibold py-1", annual_net_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("ruler-combined", class = "me-1 text-secondary"), "Total Area:"),
              tags$td(class = "text-end fw-semibold py-1", area_str)
            ),
            tags$tr(
              tags$td(class = "text-muted py-1", icon("chart-line", class = "me-1 text-secondary"), "Mean Intensity:"),
              tags$td(class = "text-end fw-semibold py-1", intensity_str)
            )
          )
        )
      )
    )
  })

  ### EMISSION FROM OTHER SOURCES ###############

  other_table_def <- c("zone_id", "lc_id", "efactor", "source")

  ## Display table uses 3 columns. The first column ("Applies to") is a grouped

  ## multi-select combining land cover classes and all loaded zonation map zones

  ## in one list. IDs are prefixed to disambiguate:

  ##   "lc:<lc_id>"                 - a land cover class

  ##   "z:<zone_record_id>:<zone_id>" - a zone from a specific zonation map

  ## The all-id sentinel (-999) becomes "lc:-999" / "z:<rid>:-999".

  other_col_display_def <- c("applies_to", "efactor", "source")

  update_table <- reactiveVal(T)

  ## Build the grouped choice list for the "Applies to" dropdown.

  ## Returns a data frame with columns id / name / group suitable for jspreadsheet.

  other_applies_choices <- function() {
    records <- v$map_catalog

    legends <- v$zone_legends

    lc      <- v$lc_list_df

    ## --- Land cover group ---
    lc_rows <- if (!is.null(lc) && nrow(lc) > 0) {
      data.frame(
        id    = paste0("lc:", lc$lc_id),
        name  = lc$label,
        group = "Land cover",
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(id = character(), name = character(), group = character(),
                 stringsAsFactors = FALSE)
    }

    all_lc <- data.frame(id = paste0("lc:", all_id), name = "All land covers",
                         group = "Land cover", stringsAsFactors = FALSE)

    lc_choices <- rbind(all_lc, lc_rows)

    ## --- Zone groups (one per loaded zonation map) ---
    zone_choices <- do.call(rbind, lapply(names(legends), function(rid) {
      zdf <- legends[[rid]]

      if (is.null(zdf) || nrow(zdf) == 0) return(NULL)

      ## Use the map label from the catalog as the group heading.

      grp <- tryCatch(records[[rid]]$label, error = function(e) rid)

      if (is.null(grp) || !nzchar(grp)) grp <- rid

      z_names <- if (nzchar(grp)) {
        ifelse(!grepl(paste0("\\(", grp, "\\)"), zdf$label, ignore.case = TRUE),
               paste0(zdf$label, " (", grp, ")"),
               zdf$label)
      } else {
        zdf$label
      }

      z_rows <- data.frame(
        id    = paste0("z:", rid, ":", zdf$zone_id),
        name  = z_names,
        group = grp,
        stringsAsFactors = FALSE
      )

      all_z <- data.frame(id = paste0("z:", rid, ":", all_id),
                          name = paste0("All zones (", grp, ")"),
                          group = grp, stringsAsFactors = FALSE)

      rbind(all_z, z_rows)
    }))

    rbind(lc_choices, zone_choices)
  }

  ## Encode stored (zone_id, lc_id) numeric/sentinel values back to the

  ## prefixed display format used in the "Applies to" column.

  ## zone_id and lc_id are semicolon-joined strings of numeric ids.

  ## zone_record_ids is a character vector of all zone record ids currently loaded.

  other_full_to_display <- function(zone_id_str, lc_id_str, zone_record_ids) {
    parts <- character(0)

    lc_str <- trimws(as.character(lc_id_str))

    z_str  <- trimws(as.character(zone_id_str))

    has_lc <- nzchar(lc_str) && lc_str != as.character(all_id)

    has_z  <- nzchar(z_str)  && z_str  != as.character(all_id)

    if (has_lc) {
      lc_ids <- trimws(unlist(strsplit(lc_str, ";")))

      parts <- c(parts, paste0("lc:", lc_ids))
    } else if (!has_z && lc_str == as.character(all_id)) {
      parts <- c(parts, paste0("lc:", all_id))
    }

    if (has_z && length(zone_record_ids) > 0) {
      z_ids <- trimws(unlist(strsplit(z_str, ";")))

      for (rid in zone_record_ids) {
        parts <- c(parts, paste0("z:", rid, ":", z_ids))
      }
    } else if (!has_lc && z_str == as.character(all_id) && length(zone_record_ids) > 0) {
      for (rid in zone_record_ids) {
        parts <- c(parts, paste0("z:", rid, ":", all_id))
      }
    }

    paste(unique(parts), collapse = ";")
  }

  ## Parse a prefixed "applies_to" cell value back to zone_id / lc_id strings.

  other_parse_applies_to <- function(val) {
    if (is.na(val) || !nzchar(trimws(val))) {
      return(list(zone_id = as.character(all_id), lc_id = as.character(all_id)))
    }

    parts  <- trimws(unlist(strsplit(val, ";")))

    lc_raw <- parts[grepl("^lc:", parts)]

    z_raw  <- parts[grepl("^z:", parts)]

    lc_ids <- if (length(lc_raw) > 0) {
      sub("^lc:", "", lc_raw)
    } else {
      as.character(all_id)
    }

    ## Zone ids: strip the "z:<record_id>:" prefix, keep the numeric part.

    ## If multiple zone maps are selected we union all their zone ids.

    z_ids <- if (length(z_raw) > 0) {
      unique(sub("^z:[^:]+:", "", z_raw))
    } else {
      as.character(all_id)
    }

    list(
      zone_id = paste(z_ids,  collapse = ";"),
      lc_id   = paste(lc_ids, collapse = ";")
    )
  }

  build_grouped_applies_choices <- function() {
    ch_df <- other_applies_choices()
    if (is.null(ch_df) || nrow(ch_df) == 0) return(list())
    groups <- unique(ch_df$group)
    res <- list()
    for (g in groups) {
      sub_df <- ch_df[ch_df$group == g, ]
      res[[g]] <- setNames(sub_df$id, sub_df$name)
    }
    res
  }

  ## Helper: generate the next unique source label when a land cover or zone conflict is detected.
  ## Source labels are case-insensitive.
  get_next_source_label <- function(src, tokens, source_tokens_map) {
    src_clean <- trimws(src)
    if (!nzchar(src_clean)) src_clean <- "Source_1"

    if (grepl("^(.*?)_([0-9]+)$", src_clean)) {
      base <- sub("^(.*?)_([0-9]+)$", "\\1", src_clean)
      num  <- as.integer(sub("^(.*?)_([0-9]+)$", "\\2", src_clean))
    } else {
      base <- src_clean
      num  <- 1L
    }

    repeat {
      num <- num + 1L
      cand <- paste0(base, "_", num)
      cand_lower <- tolower(cand)
      existing_tokens <- source_tokens_map[[cand_lower]]
      if (is.null(existing_tokens) || !any(tokens %in% existing_tokens)) {
        return(cand)
      }
    }
  }

  ## Helper: reconstruct canonical (zone_id, lc_id, efactor, source) data frame from display table
  other_display_to_full <- function(df_display) {
    if (is.null(df_display) || nrow(df_display) == 0) {
      return(data.frame(
        zone_id = character(),
        lc_id   = character(),
        efactor = character(),
        source  = character(),
        stringsAsFactors = FALSE
      ))
    }
    applies_vals <- as.character(df_display$applies_to)
    parsed <- lapply(applies_vals, other_parse_applies_to)
    data.frame(
      zone_id = vapply(parsed, function(p) p[["zone_id"]], character(1)),
      lc_id   = vapply(parsed, function(p) p[["lc_id"]],   character(1)),
      efactor = as.character(df_display$efactor),
      source  = as.character(df_display$source),
      stringsAsFactors = FALSE
    )
  }

  editing_other_emission_idx <- reactiveVal(NULL)
  selected_sources <- reactiveVal(character(0))

  output$other_emission_table <- renderReactable({
    v$lc_list_df
    v$zone_legends

    df_display <- v$other_emission_display_df

    ## Fallback: if other_emission_display_df is not yet populated but other_emission_df
    ## exists (e.g. from an imported session or project):
    if ((is.null(df_display) || nrow(df_display) == 0) &&
        !is.null(isolate(v$other_emission_df)) && nrow(isolate(v$other_emission_df)) > 0) {
      df_full <- isolate(v$other_emission_df)
      zone_record_ids <- names(v$zone_legends)[
        vapply(v$zone_legends, function(z) !is.null(z) && nrow(z) > 0, logical(1))
      ]
      applies_to_vals <- vapply(seq_len(nrow(df_full)), function(i) {
        other_full_to_display(df_full$zone_id[i], df_full$lc_id[i], zone_record_ids)
      }, character(1))
      df_display <- data.frame(
        applies_to = applies_to_vals,
        efactor    = as.character(df_full$efactor),
        source     = as.character(df_full$source),
        stringsAsFactors = FALSE
      )
      v$other_emission_display_df <- df_display
    }

    if (is.null(df_display) || nrow(df_display) == 0) {
      return(reactable(
        data.frame(
          applies_to = character(),
          efactor    = numeric(),
          source     = character(),
          actions    = character(),
          stringsAsFactors = FALSE
        ),
        minRows = 3,
        columns = list(
          applies_to = colDef(name = "Applies to"),
          efactor    = colDef(name = "Factor", align = "right", width = 90),
          source     = colDef(name = "Source", width = 110),
          actions    = colDef(name = "", width = 80)
        ),
        language = reactableLang(
          noData = "No flux emission factors defined. Click 'Add Factor' above to add one."
        ),
        theme = reactableTheme(
          headerStyle = list(background = "#f8f9fa", fontWeight = "600")
        )
      ))
    }

    # Build lookup map for friendly applies_to names
    ch_df <- other_applies_choices()
    id_to_name <- if (!is.null(ch_df) && nrow(ch_df) > 0) setNames(ch_df$name, ch_df$id) else list()

    table_df <- df_display
    table_df$row_id <- seq_len(nrow(table_df))
    table_df$efactor_num <- suppressWarnings(as.numeric(table_df$efactor))
    table_df$efactor_num[is.na(table_df$efactor_num)] <- 0

    reactable(
      table_df[, c("applies_to", "efactor_num", "source", "row_id")],
      compact = TRUE,
      striped = TRUE,
      bordered = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      columns = list(
        applies_to = colDef(
          name = "Applies to",
          cell = function(value) {
            if (is.na(value) || !nzchar(trimws(value))) return("All")
            tokens <- trimws(unlist(strsplit(as.character(value), ";")))
            labels <- vapply(tokens, function(tok) {
              nm <- id_to_name[[tok]]
              if (!is.null(nm) && nzchar(nm)) {
                nm
              } else if (grepl("^z:", tok)) {
                parts <- strsplit(tok, ":")[[1]]
                if (length(parts) == 3) {
                  rid <- parts[2]
                  zid <- parts[3]
                  map_lbl <- tryCatch(v$map_catalog[[rid]]$label, error = function(e) rid)
                  if (is.null(map_lbl) || !nzchar(map_lbl)) map_lbl <- rid
                  if (zid == as.character(all_id)) {
                    paste0("All zones (", map_lbl, ")")
                  } else {
                    paste0("Zone ", zid, " (", map_lbl, ")")
                  }
                } else {
                  tok
                }
              } else {
                tok
              }
            }, character(1))
            badge_tags <- lapply(labels, function(lbl) {
              tags$span(
                class = "badge bg-light text-dark border me-1 mb-1",
                style = "font-weight: 500; font-size: 0.85em; white-space: normal;",
                lbl
              )
            })
            tags$div(style = "display: flex; flex-wrap: wrap;", badge_tags)
          }
        ),
        efactor_num = colDef(
          name = "Factor",
          align = "right",
          format = colFormat(digits = 2),
          width = 90
        ),
        source = colDef(
          name = "Source",
          width = 110,
          cell = function(value) {
            tags$span(
              class = "badge bg-secondary",
              style = "font-size: 0.85em;",
              as.character(value)
            )
          }
        ),
        row_id = colDef(
          name = "",
          sortable = FALSE,
          searchable = FALSE,
          width = 80,
          align = "center",
          cell = function(value) {
            tags$div(
              class = "btn-group btn-group-sm",
              tags$button(
                class = "btn btn-outline-primary btn-sm py-0 px-2",
                title = "Edit factor",
                onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('edit_other_emission_row', %d, {priority: 'event'});", as.integer(value)),
                tags$i(class = "fa fa-pen")
              ),
              tags$button(
                class = "btn btn-outline-danger btn-sm py-0 px-2",
                title = "Delete factor",
                onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('delete_other_emission_row', %d, {priority: 'event'});", as.integer(value)),
                tags$i(class = "fa fa-trash")
              )
            )
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#f8f9fa", fontWeight = "600")
      )
    )
  })

  show_other_emission_modal <- function(edit_idx = NULL) {
    editing_other_emission_idx(edit_idx)

    is_edit <- !is.null(edit_idx)
    df <- v$other_emission_display_df

    default_applies <- if (!is.null(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
      paste0("lc:", all_id)
    } else {
      character(0)
    }
    default_efactor <- 0
    default_source  <- if (!is.null(v$other_emission_last_source) && nzchar(v$other_emission_last_source)) {
      v$other_emission_last_source
    } else {
      "Source_1"
    }

    if (is_edit && !is.null(df) && edit_idx <= nrow(df)) {
      row_data <- df[edit_idx, , drop = FALSE]
      raw_applies <- trimws(as.character(row_data$applies_to))
      default_applies <- if (nzchar(raw_applies)) trimws(unlist(strsplit(raw_applies, ";"))) else character(0)
      val_num <- suppressWarnings(as.numeric(row_data$efactor))
      default_efactor <- if (!is.na(val_num)) val_num else 0
      val_src <- trimws(as.character(row_data$source))
      if (nzchar(val_src)) default_source <- val_src
    }

    applies_choices <- build_grouped_applies_choices()

    existing_srcs <- if (!is.null(df) && nrow(df) > 0) df$source else character(0)
    source_choices <- unique(c(
      c("Source_1", "Peat", "Agriculture"),
      default_source,
      existing_srcs[nzchar(trimws(existing_srcs))],
      v$other_emission_source[nzchar(trimws(v$other_emission_source))]
    ))
    source_choices <- source_choices[!is.na(source_choices) & nzchar(trimws(source_choices))]

    modal_title <- if (is_edit) {
      tagList(icon("pen"), " Edit Flux Emission Factor")
    } else {
      tagList(icon("plus"), " Add Flux Emission Factor")
    }

    showModal(modalDialog(
      title = modal_title,
      size = "m",
      easyClose = TRUE,
      tagList(
        div(
          class = "mb-3",
          tags$label(class = "form-label fw-bold", "Applies to:"),
          selectizeInput(
            "modal_other_applies",
            label = NULL,
            choices = applies_choices,
            selected = default_applies,
            multiple = TRUE,
            width = "100%",
            options = list(
              placeholder = "Select land cover and/or zones...",
              plugins = list("remove_button")
            )
          ),
          tags$small(class = "text-muted", "Select specific land cover classes or zonation maps to apply this emission factor to.")
        ),
        div(
          class = "mb-3",
          tags$label(class = "form-label fw-bold", HTML("Emission factor (tCO<sub>2</sub>e ha<sup>-1</sup> yr<sup>-1</sup>):")),
          numericInput(
            "modal_other_efactor",
            label = NULL,
            value = default_efactor,
            step = 0.01,
            width = "100%"
          )
        ),
        div(
          class = "mb-3",
          tags$label(class = "form-label fw-bold", "Emission source:"),
          selectizeInput(
            "modal_other_source",
            label = NULL,
            choices = source_choices,
            selected = default_source,
            width = "100%",
            options = list(
              create = TRUE,
              placeholder = "Select from suggestions or type custom source name..."
            )
          ),
          tags$small(class = "text-muted", "Type a custom source name or pick an existing one. Emission sources must have unique land cover/zones.")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_other_emission_modal_btn", "Save", class = "btn btn-primary")
      )
    ))
  }

  observeEvent(input$add_other_emission_btn, {
    show_other_emission_modal(edit_idx = NULL)
  })

  ## Clear all flux emission factors
  observeEvent(input$clear_other_emission_btn, {
    v$other_emission_display_df <- data.frame(
      applies_to = character(),
      efactor    = character(),
      source     = character(),
      stringsAsFactors = FALSE
    )
    v$other_emission_df <- NULL
    v$other_emission_source <- character(0)
    selected_sources(character(0))
    updateSelectizeInput(session, "select_source", choices = character(0), selected = character(0))
    updateSelectizeInput(session, "summary_select_source", choices = character(0), selected = character(0))
    toggle_popover("other_emission_menu_pop", FALSE)
    showNotification("All emission factors cleared.", type = "message", duration = 3)
  })

  ## Download flux emission factors as CSV
  output$download_other_emission_csv <- downloadHandler(
    filename = function() {
      paste0("flux_emission_factors_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- v$other_emission_display_df
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df[c("applies_to", "efactor", "source")], file, row.names = FALSE, na = "")
      }
    }
  )

  ## Import flux emission factors from CSV
  observeEvent(input$upload_other_emission_csv, {
    req(input$upload_other_emission_csv)
    tryCatch({
      df_inp <- utils::read.csv(input$upload_other_emission_csv$datapath, stringsAsFactors = FALSE)
      if (nrow(df_inp) == 0) {
        showNotification("Uploaded CSV file is empty.", type = "warning", duration = 4)
        return()
      }

      colnames(df_inp) <- tolower(trimws(colnames(df_inp)))

      df_new <- NULL

      # Case 1: Standard format exported by download: applies_to, efactor, source
      if ("efactor" %in% colnames(df_inp)) {
        applies_col <- if ("applies_to" %in% colnames(df_inp)) "applies_to" else if ("applies" %in% colnames(df_inp)) "applies" else NULL
        source_col  <- if ("source" %in% colnames(df_inp)) "source" else NULL

        if (!is.null(applies_col)) {
          df_new <- data.frame(
            applies_to = as.character(df_inp[[applies_col]]),
            efactor    = as.character(df_inp$efactor),
            source     = if (!is.null(source_col)) as.character(df_inp[[source_col]]) else "Source_1",
            stringsAsFactors = FALSE
          )
        } else if (all(c("zone_id", "lc_id") %in% colnames(df_inp))) {
          # Case 2: Canonical columns: zone_id, lc_id, efactor, source
          zone_record_ids <- names(v$zone_legends)[
            vapply(v$zone_legends, function(z) !is.null(z) && nrow(z) > 0, logical(1))
          ]
          applies_vals <- vapply(seq_len(nrow(df_inp)), function(i) {
            other_full_to_display(df_inp$zone_id[i], df_inp$lc_id[i], zone_record_ids)
          }, character(1))
          df_new <- data.frame(
            applies_to = applies_vals,
            efactor    = as.character(df_inp$efactor),
            source     = if (!is.null(source_col)) as.character(df_inp[[source_col]]) else "Source_1",
            stringsAsFactors = FALSE
          )
        }
      }

      if (is.null(df_new) || nrow(df_new) == 0) {
        showNotification("CSV must contain columns: 'applies_to', 'efactor', 'source'.", type = "error", duration = 5)
        return()
      }

      # Clean and validate
      df_new$applies_to <- trimws(df_new$applies_to)
      df_new$efactor    <- trimws(df_new$efactor)
      df_new$source     <- trimws(df_new$source)
      df_new$source[df_new$source == ""] <- "Source_1"

      v$other_emission_display_df <- df_new
      df_full <- other_display_to_full(df_new)
      v$other_emission_df <- validate_other_df(df_full)

      all_sources <- unique(df_new$source)
      all_sources <- all_sources[!is.na(all_sources) & nzchar(trimws(all_sources))]
      v$other_emission_source <- all_sources

      selected_sources(all_sources)
      updateSelectizeInput(session, "select_source", choices = all_sources, selected = all_sources)
      updateSelectizeInput(session, "summary_select_source", choices = all_sources, selected = all_sources)

      toggle_popover("other_emission_menu_pop", FALSE)
      showNotification(sprintf("Successfully imported %d flux emission factor(s).", nrow(df_new)), type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error importing CSV:", e$message), type = "error", duration = 5)
    })
  })

  observeEvent(input$edit_other_emission_row, {
    idx <- as.integer(input$edit_other_emission_row)
    show_other_emission_modal(edit_idx = idx)
  })

  observeEvent(input$delete_other_emission_row, {
    idx <- as.integer(input$delete_other_emission_row)
    df <- v$other_emission_display_df
    if (!is.null(df) && !is.na(idx) && idx >= 1 && idx <= nrow(df)) {
      df <- df[-idx, , drop = FALSE]
      v$other_emission_display_df <- df
      df_full <- other_display_to_full(df)
      v$other_emission_df <- validate_other_df(df_full)

      all_sources <- if (!is.null(df) && nrow(df) > 0) unique(df$source) else character(0)
      all_sources <- all_sources[!is.na(all_sources) & nzchar(trimws(all_sources))]
      v$other_emission_source <- all_sources

      cur_sel <- selected_sources()
      new_sel <- cur_sel[cur_sel %in% all_sources]
      selected_sources(new_sel)
      updateSelectizeInput(session, "select_source", choices = all_sources, selected = new_sel)
      updateSelectizeInput(session, "summary_select_source", choices = all_sources, selected = new_sel)

      showNotification("Emission factor removed.", type = "message", duration = 3)
    }
  })

  observeEvent(input$save_other_emission_modal_btn, {
    raw_applies <- input$modal_other_applies
    if (is.null(raw_applies) || length(raw_applies) == 0) {
      showNotification("Please select at least one land cover or zone in 'Applies to'.", type = "error", duration = 4)
      return()
    }

    efactor_val <- input$modal_other_efactor
    if (is.null(efactor_val) || is.na(efactor_val)) {
      showNotification("Please enter a valid numeric emission factor.", type = "error", duration = 4)
      return()
    }

    source_val <- trimws(as.character(input$modal_other_source))
    if (!nzchar(source_val)) {
      source_val <- if (!is.null(v$other_emission_last_source) && nzchar(v$other_emission_last_source)) {
        v$other_emission_last_source
      } else {
        "Source_1"
      }
    }

    df <- v$other_emission_display_df
    if (is.null(df)) {
      df <- data.frame(
        applies_to = character(),
        efactor    = character(),
        source     = character(),
        stringsAsFactors = FALSE
      )
    }

    edit_idx <- editing_other_emission_idx()
    other_rows_df <- if (!is.null(edit_idx) && edit_idx <= nrow(df)) {
      df[-edit_idx, , drop = FALSE]
    } else {
      df
    }

    is_sentinel <- function(tok) {
      tok %in% c(paste0("lc:", all_id), paste0("z:", all_id)) || grepl(":-999$", tok)
    }

    source_tokens_map <- list()
    if (nrow(other_rows_df) > 0) {
      for (j in seq_len(nrow(other_rows_df))) {
        s_j <- tolower(trimws(as.character(other_rows_df$source[j])))
        a_j <- trimws(as.character(other_rows_df$applies_to[j]))
        toks <- if (nzchar(a_j)) trimws(unlist(strsplit(a_j, ";"))) else character(0)
        toks <- toks[nzchar(toks) & !vapply(toks, is_sentinel, logical(1))]
        source_tokens_map[[s_j]] <- unique(c(source_tokens_map[[s_j]], toks))
      }
    }

    cur_toks <- raw_applies[nzchar(raw_applies) & !vapply(raw_applies, is_sentinel, logical(1))]
    cur_src_lower <- tolower(source_val)
    conflict <- FALSE
    if (length(cur_toks) > 0 && !is.null(source_tokens_map[[cur_src_lower]])) {
      if (any(cur_toks %in% source_tokens_map[[cur_src_lower]])) {
        conflict <- TRUE
      }
    }

    if (conflict) {
      new_source <- get_next_source_label(source_val, cur_toks, source_tokens_map)
      showNotification(
        sprintf("Duplicate land cover/zone within '%s': automatically renamed to '%s'.", source_val, new_source),
        type = "warning",
        duration = 6
      )
      source_val <- new_source
    }

    new_applies_str <- paste(raw_applies, collapse = ";")
    new_row <- data.frame(
      applies_to = new_applies_str,
      efactor    = as.character(efactor_val),
      source     = source_val,
      stringsAsFactors = FALSE
    )

    if (!is.null(edit_idx) && edit_idx <= nrow(df)) {
      df[edit_idx, ] <- new_row
    } else {
      df <- rbind(df, new_row)
    }

    v$other_emission_last_source <- source_val
    v$other_emission_display_df <- df

    df_full <- other_display_to_full(df)
    v$other_emission_df <- validate_other_df(df_full)

    all_sources <- unique(df$source)
    all_sources <- all_sources[!is.na(all_sources) & nzchar(trimws(all_sources))]
    v$other_emission_source <- all_sources

    cur_sel <- selected_sources()
    new_sel <- unique(c(cur_sel[cur_sel %in% all_sources], source_val))
    selected_sources(new_sel)
    updateSelectizeInput(session, "select_source", choices = all_sources, selected = new_sel)
    updateSelectizeInput(session, "summary_select_source", choices = all_sources, selected = new_sel)

    removeModal()
  })
  validate_other_df <- function(df_input) {
    ## Normalise column names: a CSV round-trip or an unexpected source may

    ## produce a data frame whose columns arrive in a different order or with

    ## names that only partially match.  We require zone_id, lc_id, efactor,
    ## source; if any are missing we return an empty result rather than crash.

    req_cols <- c("zone_id", "lc_id", "efactor", "source")

    if (!all(req_cols %in% colnames(df_input))) {
      warning("validate_other_df: missing columns in df_input; got: ",
              paste(colnames(df_input), collapse = ", "))

      return(data.frame(zone_id = numeric(), lc_id = numeric(),
                        efactor = numeric(), source = character(),
                        stringsAsFactors = FALSE))
    }

    if (nrow(df_input) == 0)

      return(data.frame(zone_id = numeric(), lc_id = numeric(),
                        efactor = numeric(), source = character(),
                        stringsAsFactors = FALSE))

    ## Safety net: any source that is still blank after the display-layer fill
    ## (e.g. data loaded from a project file) gets the last-used default label.
    src_v <- trimws(as.character(df_input$source))
    last_src_v <- if (!is.null(v$other_emission_last_source) && nzchar(v$other_emission_last_source))
                    v$other_emission_last_source else "Source_1"
    needs_default <- is.na(src_v) | src_v == ""
    df_input$source[needs_default] <- last_src_v
    ## checkbox filter never drops rows silently and the source is always visible.

    src_v <- trimws(as.character(df_input$source))

    needs_default <- is.na(src_v) | src_v == ""

    if (any(needs_default)) {
      existing_v <- unique(src_v[!needs_default])

      n_existing_v <- length(existing_v)

      counter_v <- n_existing_v

      for (j in which(needs_default)) {
        counter_v <- counter_v + 1

        df_input$source[j] <- paste0("Source ", counter_v)
      }
    }

    ## Use lapply over row indices so r is a 1-row data frame; this avoids the

    ## apply()-to-matrix coercion that can drop or reorder column names.

    x_df <- lapply(seq_len(nrow(df_input)), function(i) {
      r <- df_input[i, , drop = FALSE]

      zs <- as.character(all_id)

      if (!is.null(v$zone_list_df)) {
        z_val <- as.character(r[["zone_id"]])

        if (is.na(z_val) || z_val == "" || z_val == as.character(all_id)) {
          zs <- v$zone_list_df$zone_id
        } else {
          zs <- trimws(unlist(strsplit(z_val, split = ";")))

          if (as.character(all_id) %in% zs)

            zs <- v$zone_list_df$zone_id
        }
      }

      l_val <- as.character(r[["lc_id"]])

      ls <- as.character(all_id)

      if (!is.null(v$lc_list_df)) {
        if (is.na(l_val) || l_val == "" || l_val == as.character(all_id)) {
          ls <- v$lc_list_df$lc_id
        } else {
          ls <- trimws(unlist(strsplit(l_val, split = ";")))

          if (as.character(all_id) %in% ls)

            ls <- v$lc_list_df$lc_id
        }
      }

      zone_id <- unlist(rep(zs, each = length(ls)))

      lc_id   <- unlist(rep(ls, length(zs)))

      data.frame(zone_id, lc_id,
                 efactor = r[["efactor"]], source = r[["source"]],
                 stringsAsFactors = FALSE)
    })

    x_df <- do.call(rbind, x_df)

    if (is.null(x_df) || nrow(x_df) == 0)

      return(data.frame(zone_id = numeric(), lc_id = numeric(),
                        efactor = numeric(), source = character(),
                        stringsAsFactors = FALSE))

    cn <- c("zone_id", "lc_id", "efactor")

    x_df[, cn] <- lapply(x_df[, cn, drop = FALSE], function(x)

      as.numeric(as.character(x)))

    x_df[is.na(x_df)] <- 0

    return(x_df)
  }

  output$has_other_emission_source <- reactive({
    df <- v$other_emission_display_df
    has_df <- !is.null(df) && nrow(df) > 0
    s <- v$other_emission_source
    has_s <- !is.null(s) && length(s) > 0
    has_df || has_s
  })
  outputOptions(output, "has_other_emission_source", suspendWhenHidden = FALSE)

  observeEvent(input$select_source, {
    new_val <- as.character(input$select_source)
    cur_val <- selected_sources()
    if (!identical(sort(new_val), sort(cur_val))) {
      selected_sources(new_val)
      updateSelectizeInput(session, "summary_select_source", choices = v$other_emission_source, selected = new_val)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$summary_select_source, {
    new_val <- as.character(input$summary_select_source)
    cur_val <- selected_sources()
    if (!identical(sort(new_val), sort(cur_val))) {
      selected_sources(new_val)
      updateSelectizeInput(session, "select_source", choices = v$other_emission_source, selected = new_val)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Observer for initial data load or external session updates
  observeEvent(v$other_emission_display_df, {
    df <- v$other_emission_display_df
    all_sources <- if (!is.null(df) && nrow(df) > 0) unique(df$source) else character(0)
    all_sources <- all_sources[!is.na(all_sources) & nzchar(trimws(all_sources))]

    cur_known <- isolate(v$other_emission_source)
    if (is.null(cur_known)) cur_known <- character(0)

    if (!identical(sort(all_sources), sort(cur_known))) {
      v$other_emission_source <- all_sources
      cur_sel <- isolate(selected_sources())
      new_sources <- setdiff(all_sources, cur_known)
      new_sel <- if (length(cur_known) == 0 || length(cur_sel) == 0) {
        all_sources
      } else {
        unique(c(cur_sel[cur_sel %in% all_sources], new_sources))
      }
      selected_sources(new_sel)
      updateSelectizeInput(session, "select_source", choices = all_sources, selected = new_sel)
      updateSelectizeInput(session, "summary_select_source", choices = all_sources, selected = new_sel)
    }
  }, ignoreNULL = FALSE)

  output$other_source_list <- renderUI({
    s <- v$other_emission_source
    if (is.null(s) || length(s) == 0) return()
    enable <- selected_sources()
    lapply(s, function(x) {
      if (x %in% enable) {
        span(x, class = "header_item")
      } else {
        span(x, class = "header_item", style = "text-decoration: line-through;")
      }
    })
  })

  #### CALCULATE EMISSION FROM OTHER SOURCES ###############
  observe({
    v$map_catalog_version
    df_display <- v$other_emission_display_df

    m1 <- tryCatch({
      if (inherits(v$map1_stars, "stars_proxy")) stars::st_as_stars(v$map1_stars) else v$map1_stars
    }, error = function(e) {
      warning("Could not allocate memory for map1: ", conditionMessage(e))
      showNotification(
        paste("Raster map is too large to load into memory (exceeds available RAM):", conditionMessage(e)),
        type = "error",
        duration = 10
      )
      NULL
    })
    m2 <- tryCatch({
      if (inherits(v$map2_stars, "stars_proxy")) stars::st_as_stars(v$map2_stars) else v$map2_stars
    }, error = function(e) {
      warning("Could not allocate memory for map2: ", conditionMessage(e))
      showNotification(
        paste("Raster map is too large to load into memory (exceeds available RAM):", conditionMessage(e)),
        type = "error",
        duration = 10
      )
      NULL
    })

    if (is.null(m1) || is.null(m2)) {
      v$ghg_ready <- FALSE
      v$map_ghg_stars <- NULL
      return()
    }

    ## Align m2 to m1 if dimensions differ
    if (!identical(stars::st_dimensions(m1), stars::st_dimensions(m2)))
      m2 <- align_stars_to_reference(m2, m1)

    if (!identical(stars::st_dimensions(m1), stars::st_dimensions(m2)))
      return()

    period_year <- v$period_year
    if (length(period_year) != 1 || is.na(period_year) || period_year == 0) {
      y1 <- v$map1_year
      y2 <- v$map2_year
      if (!is.null(y1) && !is.null(y2) && !is.na(y1) && !is.na(y2) && y2 != y1)
        period_year <- as.numeric(y2 - y1)
    }

    if (length(period_year) != 1 || is.na(period_year) || period_year == 0)
      return()

    enable <- selected_sources()
    if (is.null(enable)) enable <- character(0)

    # Filter df_display for valid factors and active sources
    if (is.null(df_display) || nrow(df_display) == 0) {
      df_clean <- data.frame(applies_to = character(), efactor = numeric(), source = character(), stringsAsFactors = FALSE)
    } else {
      df_clean <- df_display
      df_clean$efactor <- suppressWarnings(as.numeric(df_clean$efactor))
      df_clean$source <- trimws(as.character(df_clean$source))
      df_clean <- df_clean[!is.na(df_clean$efactor) & (df_clean$source %in% as.character(enable)), , drop = FALSE]
    }

    if (nrow(df_clean) == 0) {
      mo <- m1
      mo[!is.na(mo)] <- 0
    } else {
      v1 <- as.character(m1[[1]])
      v2 <- as.character(m2[[1]])
      N <- length(v1)
      valid_mask <- !is.na(v1) & !is.na(v2)

      # Load and align ALL zonation maps available in v$map_catalog
      records <- v$map_catalog
      zone_records <- Filter(function(r) identical(r$type, "Zonation map") && !is.null(r$stars), records)

      z_vals <- list()
      for (rid in names(zone_records)) {
        zs <- zone_records[[rid]]$stars
        zs_mem <- if (inherits(zs, "stars_proxy")) tryCatch(stars::st_as_stars(zs), error = function(e) NULL) else zs
        if (is.null(zs_mem)) next
        if (!identical(stars::st_dimensions(m1), stars::st_dimensions(zs_mem))) {
          zs_mem <- align_stars_to_reference(zs_mem, m1)
        }
        if (!is.null(zs_mem) && identical(stars::st_dimensions(m1), stars::st_dimensions(zs_mem))) {
          z_vals[[rid]] <- as.character(zs_mem[[1]])
        }
      }

      # Fallback to single v$mapz_stars if z_vals is empty
      if (!is.null(v$mapz_stars) && length(z_vals) == 0) {
        mzs <- if (inherits(v$mapz_stars, "stars_proxy")) tryCatch(stars::st_as_stars(v$mapz_stars), error = function(e) NULL) else v$mapz_stars
        if (!identical(stars::st_dimensions(m1), stars::st_dimensions(mzs)))
          mzs <- align_stars_to_reference(mzs, m1)
        if (!is.null(mzs) && identical(stars::st_dimensions(m1), stars::st_dimensions(mzs)))
          z_vals[["__active_zone__"]] <- as.character(mzs[[1]])
      }

      # Pre-parse each factor row
      parsed_rows <- lapply(seq_len(nrow(df_clean)), function(r) {
        applies <- trimws(as.character(df_clean$applies_to[r]))
        tokens <- trimws(unlist(strsplit(applies, ";")))
        tokens <- tokens[nzchar(tokens)]

        lc_tokens <- tokens[grepl("^lc:", tokens)]
        lc_ids <- sub("^lc:", "", lc_tokens)
        bare <- tokens[!grepl("^(lc|z):", tokens)]
        lc_ids <- c(lc_ids, bare)
        lc_ids <- lc_ids[nzchar(lc_ids)]

        lc_restricted <- (length(lc_ids) > 0) && !(as.character(all_id) %in% lc_ids)
        target_lc <- if (lc_restricted) unique(lc_ids) else NULL

        z_tokens <- tokens[grepl("^z:", tokens)]
        zone_map_targets <- list()
        if (length(z_tokens) > 0) {
          for (zt in z_tokens) {
            parts <- strsplit(zt, ":")[[1]]
            if (length(parts) >= 3) {
              rid <- parts[2]
              zid <- parts[3]
              zone_map_targets[[rid]] <- c(zone_map_targets[[rid]], zid)
            }
          }
        }

        list(
          efactor = df_clean$efactor[r],
          source  = df_clean$source[r],
          lc_restricted = lc_restricted,
          target_lc = target_lc,
          zone_map_targets = zone_map_targets
        )
      })

      # Function to match a row against a land cover raster state (v1 or v2)
      match_row_pixels <- function(p_row, vt) {
        hit <- valid_mask & !is.na(vt)
        if (p_row$lc_restricted) {
          hit <- hit & (vt %in% p_row$target_lc)
        }
        if (length(p_row$zone_map_targets) > 0) {
          for (rid in names(p_row$zone_map_targets)) {
            target_zids <- unique(p_row$zone_map_targets[[rid]])
            vz <- z_vals[[rid]]
            # If specified zonation map not in z_vals, fallback to first available zonation map
            if (is.null(vz) && length(z_vals) > 0) {
              vz <- z_vals[[1]]
            }
            if (is.null(vz)) {
              hit <- rep(FALSE, N)
              break
            }
            if (as.character(all_id) %in% target_zids) {
              hit <- hit & !is.na(vz) & (vz != "0")
            } else {
              hit <- hit & !is.na(vz) & (vz %in% target_zids)
            }
          }
        }
        hit
      }

      # Compute emission factor maps for time 1 and time 2
      # Multi-zonation logic:
      # - Within the same source: multiple overlapping factors are AVERAGED
      # - Across different sources: factors are ADDITIVE (summed)
      total_ef_1 <- numeric(N)
      total_ef_2 <- numeric(N)

      unique_sources <- unique(df_clean$source)

      for (src in unique_sources) {
        src_row_indices <- which(vapply(parsed_rows, function(pr) pr$source == src, logical(1)))

        sum_ef_1 <- numeric(N)
        cnt_ef_1 <- integer(N)
        sum_ef_2 <- numeric(N)
        cnt_ef_2 <- integer(N)

        for (idx in src_row_indices) {
          pr <- parsed_rows[[idx]]
          ef_val <- pr$efactor

          hit1 <- match_row_pixels(pr, v1)
          if (any(hit1)) {
            sum_ef_1[hit1] <- sum_ef_1[hit1] + ef_val
            cnt_ef_1[hit1] <- cnt_ef_1[hit1] + 1L
          }

          hit2 <- match_row_pixels(pr, v2)
          if (any(hit2)) {
            sum_ef_2[hit2] <- sum_ef_2[hit2] + ef_val
            cnt_ef_2[hit2] <- cnt_ef_2[hit2] + 1L
          }
        }

        # Average where overlapping within the same source
        avg_ef_1 <- numeric(N)
        has1 <- cnt_ef_1 > 0L
        avg_ef_1[has1] <- sum_ef_1[has1] / cnt_ef_1[has1]

        avg_ef_2 <- numeric(N)
        has2 <- cnt_ef_2 > 0L
        avg_ef_2[has2] <- sum_ef_2[has2] / cnt_ef_2[has2]

        # Add to total (additive across different sources)
        total_ef_1 <- total_ef_1 + avg_ef_1
        total_ef_2 <- total_ef_2 + avg_ef_2
      }

      mo1 <- m1
      mo1[[1]] <- total_ef_1
      mo2 <- m2
      mo2[[1]] <- total_ef_2

      mo <- (mo1 + mo2) * period_year / 2
      mo[[1]][!valid_mask] <- NA
    }

    v$map_ghg_stars <- mo
    v$ghg_ready <- TRUE

    if (!is.null(v$mapc_stars)) {
      v$map_all_stars <- tryCatch(mo + v$mapc_stars, error = function(e) mo)
    } else {
      v$map_all_stars <- mo
    }

    v$other_emission_total <- sum(mo[mo > 0][[1]], na.rm = T)
    v$other_sequestration_total <- abs(sum(mo[mo < 0][[1]], na.rm = T))
    v$all_emission_total <- v$c_emission_total + v$other_emission_total
    v$all_sequestration_total <- v$c_sequestration_total + v$other_sequestration_total
  })

  ### UPDATE ABACUS DATA ######################

  observe({
    if (is.null(v$lc_changes_df))

      return()

    ## `include_other` lives in a checkbox that is NOT rendered (its group is

    ## commented out in the UI), so it is NULL until the user touches it - and

    ## `if (NULL)` throws "argument is of length zero" the first time a second

    ## map makes `lc_changes_df` non-NULL. Treat "missing or not set" as included,
    ## matching the checkbox's default of TRUE.

    ot <- NULL

    include_other <- isTRUE(input$include_other)

    if (length(input$include_other) == 0)

      include_other <- TRUE

    if (include_other)

      ot <- v$other_emission_df

    d1 <- if (!is.null(v$map1_date) && nzchar(as.character(v$map1_date))) {
      v$map1_date
    } else if (!is.null(v$map1_year)) {
      v$map1_year
    } else {
      2000
    }
    d2 <- if (!is.null(v$map2_date) && nzchar(as.character(v$map2_date))) {
      v$map2_date
    } else if (!is.null(v$map2_year)) {
      v$map2_year
    } else {
      2010
    }

    zone_src <- v$zone_list_df
    zone_cols <- grep("^zone_", names(v$lc_changes_df), value = TRUE)
    if (length(zone_cols) > 1L) {
      zids <- sub("^zone_", "", zone_cols)
      comp_res <- build_composite_zone_data(v$lc_changes_df, zids, v$zone_legends)
      zone_src <- comp_res$zone_df
    } else if (length(zone_cols) == 1L) {
      map_id <- sub("^zone_", "", zone_cols[[1]])
      if (!is.null(v$zone_legends) && !is.null(v$zone_legends[[map_id]])) {
        zone_src <- v$zone_legends[[map_id]]
      } else {
        zone_src <- v$zone_list_df
      }
    } else {
      zone_src <- v$zone_list_df
    }

    ab <- prepare_projection_abacus(
      landcover = v$lc_list_df,
      landcover_change = v$lc_changes_df,
      zone = zone_src,
      carbonstock = v$cstock_list,
      date1 = d1,
      date2 = d2,
      n_iteration = as.integer(isolate(v$n_iteration) %||% 4L),
      other_emission_factor = ot
    )
    if (!is.null(ab)) {
      v$abacus_data <- ab
    }
  })

  ### ABACUS DATA IMPORT ##############################

  scenario_colums <- c("zone",
                       "period",
                       "lc1",
                       "lc2",
                       "r",
                       "area",
                       "def_area",
                       "def_r")

  observeEvent(input$abacus_file, {
    v$abacus_file <- input$abacus_file

    if (is.null(v$abacus_file)) {
      return(NULL)
    }

    v$map1_file <- NULL

    v$map2_file <- NULL

    v$mapz_file <- NULL

    v$map1_stars <- NULL

    v$map2_stars <- NULL

    v$mapz_stars <- NULL

    v$map1_sf <- NULL

    v$map2_sf <- NULL

    v$mapz_sf <- NULL

    v$map1_shp_file <- NULL

    v$map2_shp_file <- NULL

    v$mapz_shp_file <- NULL

    v$mapc_stars <- NULL

    v$map_ghg_stars <- NULL

    v$ghg_ready <- FALSE

    v$map_all_stars <- NULL

    v$map1_df <- NULL

    v$map2_df <- NULL

    v$mapz_df <- NULL

    v$abacus_data <- read.abacus(v$abacus_file$datapath)

    p <- v$abacus_data$project_list[[v$selected_project_id]]

    p$landcover$color <- map_color(nrow(p$landcover))

    v$lc_list_df <- p$landcover[lc_table_def]

    p$zone$color <- map_color(nrow(p$zone))

    ## `area` is not shown in the legend table, but keep it if the project

    ## provides one so downstream code still has the column.

    if ("area" %in% colnames(p$zone)) {
      v$zone_list_df <- p$zone[c(zone_table_def, "area")]
    } else {
      p$zone$area <- ""

      v$zone_list_df <- p$zone[zone_table_def]
    }

    cdf <- p$carbonstock

    v$cstock_list <- cdf[cdf$scenario_id == 0 &
                           cdf$iteration_id == 0 &
                           cdf$zone_id == 0, c("lc_id", "c")]

    v$lc_changes_df <- p$landcover_change

    v$n_iteration <- p$project$n_iteration

    v$map1_year <- as.integer(p$project$baseyear0)

    v$map2_year <- as.integer(p$project$baseyear1)

    v$map1_date <- v$map1_year

    v$map2_date <- v$map2_year

    removeModal()
  })

  get_period_label <- function(iteration) {
    n <- abs(v$baseline_period[2] - v$baseline_period[1])

    p1 <- v$baseline_period[1] + iteration * n

    p2 <- v$baseline_period[1] + (iteration + 1) * n

    return(paste(p1, "-", p2))
  }

  get_landcover_label <- function(id) {
    if (!is.null(v$lc_list_df)) {
      hit <- match(id, v$lc_list_df$lc_id)

      if (!is.na(hit) && nzchar(v$lc_list_df$label[hit]))

        return(v$lc_list_df$label[hit])
    }

    df <- tryCatch(active_scenario()$abacus_scenario$scenario$landcover, error = function(e) NULL)

    if (!is.null(df)) {
      hit <- match(id, df$lc_id)

      if (!is.na(hit)) return(df$label[hit])
    }

    paste0("Landcover_", id)
  }

  get_landcover_color <- function(id) {
    if (length(id) != 1 || is.na(id))

      return("#808080")

    if (!is.null(v$lc_list_df)) {
      hit <- match(id, v$lc_list_df$lc_id)

      if (!is.na(hit)) {
        col <- v$lc_list_df$color[hit]

        if (length(col) > 0 && !is.na(col) && nzchar(col))

          return(col)
      }
    }

    df <- tryCatch(active_scenario()$abacus_scenario$scenario$landcover, error = function(e) NULL)

    if (!is.null(df)) {
      hit <- match(id, df$lc_id)

      if (!is.na(hit)) {
        col <- df$color[hit]

        if (length(col) > 0 && !is.na(col) && nzchar(col))

          return(col)
      }
    }

    "#808080"
  }

  observe({
    if (is.null(v$n_iteration)) {
      return()
    }

    if (is.null(v$baseline_period)) {
      return()
    }

    i <- c(0:v$n_iteration)

    n <- unlist(lapply(i, get_period_label))

    pl <- as.list(i)

    names(pl) <- n

    updateSelectInput(session, "iteration_select_c", choices = pl)
  })

  ### PROJECTION SCENARIO EDITOR (ABACUSLIB) ######################
  parse_project_date <- function(d, default_year = 2000) {
    as_iso_date(d, default_year)
  }

  projection_project <- function() {
    unwrap_abacus_project(v$abacus_data, v$selected_project_id %||% 1L)
  }

  projection_dates <- function(data = NULL) {
    if (is.null(data)) data <- projection_project()
    req(data)
    if (!is.null(data$project$date1) || !is.null(data$project$date2)) {
      return(list(
        date1 = parse_project_date(data$project$date1, 2000),
        date2 = parse_project_date(data$project$date2, 2010)
      ))
    }
    y1 <- data$project$baseyear0[[1]] %||% 2000
    y2 <- data$project$baseyear1[[1]] %||% 2010
    list(
      date1 = as.Date(paste0(as.integer(y1), "-07-01")),
      date2 = as.Date(paste0(as.integer(y2), "-07-01"))
    )
  }

  projection_zones <- function() {
    data <- projection_project()
    if (is.null(data) || is.null(data$zone) || !is.data.frame(data$zone) || nrow(data$zone) == 0) {
      return(data.frame(zone_id = 0L, label = "All zones", stringsAsFactors = FALSE))
    }
    data$zone
  }

  scenario_catalog <- reactiveVal(list())
  selected_scenario_index <- reactiveVal("1")
  has_scenario_zones <- reactive({
    # Read directly from the catalog entry — it is the authoritative source for zone_map
    # and abacus_data, which the widget update payload never writes back.
    sc_idx <- suppressWarnings(as.integer(selected_scenario_index()))
    if (is.na(sc_idx) || sc_idx < 1L) return(FALSE)
    cats <- scenario_catalog()
    if (sc_idx > length(cats)) return(FALSE)
    cat_entry <- cats[[sc_idx]]
    # Cheap field checks on the catalog entry
    zm <- cat_entry$zone_maps %||% cat_entry$zone_map %||% "none"
    if (length(zm) == 0 || (length(zm) == 1 && (zm == "none" || !nzchar(zm)))) return(FALSE)
    zo <- cat_entry$zonation %||% ""
    if (nzchar(zo) && zo == "All zones (no zonation)") return(FALSE)
    # Check the abacus_data zone table from the catalog entry
    ab <- cat_entry$abacus_data %||% v$abacus_data
    if (is.null(ab) || is.null(ab$zone)) return(FALSE)
    zones <- ab$zone
    if (!is.data.frame(zones) || nrow(zones) == 0) return(FALSE)
    any(zones$zone_id != 0)
  })
  output$has_scenario_zones <- reactive({
    has_scenario_zones()
  })
  outputOptions(output, "has_scenario_zones", suspendWhenHidden = FALSE)

  selected_zone <- reactive({
    if (!has_scenario_zones()) return(0L)
    zone <- suppressWarnings(as.integer(input$sel_zone))
    if (is.na(zone)) 0L else zone
  })
  zonation_map_counter <- reactiveVal(0L)
  active_zonation_map_id <- reactiveVal(NULL)
  scenario_overrides <- reactiveVal(list())
  emission_data <- reactiveVal(NULL)
  baseline_emission_data <- reactiveVal(NULL)
  scenario_tpm_edits <- reactiveVal(NULL)
  description_store <- reactiveVal(list())
  output$has_scenario <- reactive({
    length(scenario_catalog()) > 0
  })
  outputOptions(output, "has_scenario", suspendWhenHidden = FALSE)

  # Sync scenario_catalog when v$abacus_data is ready or v$scenario_list exists
  observe({
    req(v$abacus_data)
    existing_scs <- isolate(scenario_catalog())
    if (length(existing_scs) == 0) {
      n_it <- as.integer(v$abacus_data$project$n_iteration[[1]] %||% v$n_iteration %||% 4L)
      if (!is.null(v$scenario_list) && length(v$scenario_list) > 0) {
        sc_cat <- lapply(seq_along(v$scenario_list), function(i) {
          item <- v$scenario_list[[i]]
          sc_obj <- item$abacus_scenario$scenario %||% item
          list(
            id = as.integer(i),
            label = item$label %||% paste0("Scenario_", i),
            description = item$desc %||% item$description %||% "",
            tpm = sc_obj$tpm,
            baseline_tpm = sc_obj$baseline_tpm,
            baseline_area = sc_obj$baseline_area,
            landcover = sc_obj$landcover %||% projection_project()$landcover,
            new_lc_id = sc_obj$new_lc_id %||% list(),
            n_iteration = as.integer(sc_obj$n_iteration %||% n_it),
            zone_sf = sc_obj$zone_sf,
            zone_maps = sc_obj$zone_maps,
            zone_map = sc_obj$zone_map,
            zone_layers = sc_obj$zone_layers,
            abacus_data = sc_obj$abacus_data,
            zone_src = sc_obj$zone_src
          )
        })
        scenario_catalog(sc_cat)
        selected_scenario_index("1")
      } else {
        scenario_catalog(list())
        selected_scenario_index("")
      }
    }
  })

  ## Keep projection data and scenario catalogs in sync when land cover colors / legend change
  observeEvent(v$lc_list_df, {
    req(v$lc_list_df)
    new_lc <- v$lc_list_df
    if (!is.data.frame(new_lc) || nrow(new_lc) == 0) return()

    # 1. Update v$abacus_data
    if (!is.null(v$abacus_data)) {
      ab <- v$abacus_data
      if (!is.null(ab$landcover)) {
        ab$landcover <- update_landcover_colors(ab$landcover, new_lc)
      }
      if (!is.null(ab$scenario) && is.list(ab$scenario)) {
        ab$scenario <- lapply(ab$scenario, function(sc) {
          if (!is.null(sc$landcover)) {
            sc$landcover <- update_landcover_colors(sc$landcover, new_lc)
          }
          sc
        })
      }
      v$abacus_data <- ab
    }

    # 2. Update scenario_catalog
    sc_cat <- isolate(scenario_catalog())
    if (length(sc_cat) > 0) {
      sc_cat_updated <- lapply(sc_cat, function(sc) {
        if (!is.null(sc$landcover)) {
          sc$landcover <- update_landcover_colors(sc$landcover, new_lc)
        }
        if (!is.null(sc$abacus_data) && !is.null(sc$abacus_data$landcover)) {
          sc$abacus_data$landcover <- update_landcover_colors(sc$abacus_data$landcover, new_lc)
        }
        sc
      })
      scenario_catalog(sc_cat_updated)
    }

    # 3. Update scenario_overrides
    ov <- isolate(scenario_overrides())
    if (length(ov) > 0) {
      ov_updated <- lapply(ov, function(sc) {
        if (!is.null(sc$landcover)) {
          sc$landcover <- update_landcover_colors(sc$landcover, new_lc)
        }
        if (!is.null(sc$abacus_data) && !is.null(sc$abacus_data$landcover)) {
          sc$abacus_data$landcover <- update_landcover_colors(sc$abacus_data$landcover, new_lc)
        }
        sc
      })
      scenario_overrides(ov_updated)
    }

    # 4. Update v$scenario_list
    if (!is.null(v$scenario_list) && length(v$scenario_list) > 0) {
      for (i in seq_along(v$scenario_list)) {
        sc_item <- v$scenario_list[[i]]$abacus_scenario$scenario
        if (!is.null(sc_item) && !is.null(sc_item$landcover)) {
          v$scenario_list[[i]]$abacus_scenario$scenario$landcover <-
            update_landcover_colors(sc_item$landcover, new_lc)
        }
      }
    }
  }, ignoreInit = TRUE)

  selected_scenario <- reactive({
    sc_idx <- as.integer(selected_scenario_index())
    scenarios <- scenario_catalog()
    if (length(scenarios) == 0) return(NULL)
    if (is.na(sc_idx) || sc_idx < 1 || sc_idx > length(scenarios)) {
      sc_idx <- 1L
    }
    cat_sc <- scenarios[[sc_idx]]
    overrides <- scenario_overrides()
    scenario <- overrides[[as.character(sc_idx)]]
    if (is.null(scenario)) {
      scenario <- cat_sc
    } else {
      # Restore metadata fields from catalog that the widget never writes back
      if (is.null(scenario$label) || !nzchar(scenario$label))
        scenario$label <- cat_sc$label
      if (is.null(scenario$description) || !nzchar(scenario$description))
        scenario$description <- cat_sc$description
      if (is.null(scenario$n_iteration))
        scenario$n_iteration <- cat_sc$n_iteration
      # Always carry immutable config fields from the catalog entry.
      # The widget update payload (sc_update) never includes these, so without this
      # they would be lost after the first widget edit, breaking has_scenario_zones().
      if (is.null(scenario$zone_map))    scenario$zone_map    <- cat_sc$zone_map
      if (is.null(scenario$zone_maps))   scenario$zone_maps   <- cat_sc$zone_maps
      if (is.null(scenario$zone_layers)) scenario$zone_layers <- cat_sc$zone_layers
      if (is.null(scenario$zonation))    scenario$zonation    <- cat_sc$zonation
      if (is.null(scenario$flux_sources)) scenario$flux_sources <- cat_sc$flux_sources
      if (is.null(scenario$period))      scenario$period      <- cat_sc$period
      if (is.null(scenario$abacus_data)) scenario$abacus_data <- cat_sc$abacus_data
      if (is.null(scenario$zone_src))    scenario$zone_src    <- cat_sc$zone_src
      if (is.null(scenario$map1_year))   scenario$map1_year   <- cat_sc$map1_year
      if (is.null(scenario$map2_year))   scenario$map2_year   <- cat_sc$map2_year
      if (is.null(scenario$zone_sf))     scenario$zone_sf     <- cat_sc$zone_sf
    }
    if (!is.null(scenario) && !is.null(v$lc_list_df) && is.data.frame(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
      if (!is.null(scenario$landcover)) {
        scenario$landcover <- update_landcover_colors(scenario$landcover, v$lc_list_df)
      }
      if (!is.null(scenario$abacus_data) && !is.null(scenario$abacus_data$landcover)) {
        scenario$abacus_data$landcover <- update_landcover_colors(scenario$abacus_data$landcover, v$lc_list_df)
      }
    }
    scenario
  })

  n_iterations <- reactive({
    sc <- selected_scenario()
    n_it <- if (!is.null(sc) && !is.null(sc$n_iteration)) sc$n_iteration else NULL
    if (is.null(n_it)) {
      proj <- projection_project()
      if (!is.null(proj) && !is.null(proj$project$n_iteration)) {
        n_it <- proj$project$n_iteration[[1]]
      }
    }
    as.integer(n_it %||% v$n_iteration %||% 4L)
  })

  edited_zones_and_periods <- reactive({
    tpm <- scenario_tpm_edits()
    if (is.null(tpm) || length(tpm) == 0) {
      sc <- selected_scenario()
      tpm <- if (!is.null(sc)) sc$tpm else NULL
    }
    if (is.null(tpm) || length(tpm) == 0)
      return(list(zone_ids = integer(0), iter_ids = integer(0)))
    if (!is.data.frame(tpm))
      tpm <- as.data.frame(tpm)
    if ("lock" %in% names(tpm)) {
      locked <- tpm$lock == TRUE | tpm$lock == "TRUE" | tpm$lock == 1
      tpm <- tpm[locked, , drop = FALSE]
    }
    zone_ids <- if ("zone_id" %in% names(tpm))
      unique(as.integer(tpm$zone_id))
    else
      integer(0)
    iter_ids <- if ("iteration" %in% names(tpm))
      unique(as.integer(tpm$iteration))
    else
      integer(0)
    list(zone_ids = zone_ids, iter_ids = iter_ids)
  })

  iteration_choices <- reactive({
    req(v$abacus_data)
    dts <- projection_dates()
    date1 <- dts$date1
    date2 <- dts$date2
    period_years <- round(as.numeric(difftime(date2, date1, units = "days")) / 7 / 52.25)
    if (is.na(period_years) || period_years <= 0) period_years <- 1L
    first_year <- as.integer(format(date1, "%Y"))
    years <- first_year + (0:(n_iterations() + 1L)) * period_years
    labels <- paste(years[-length(years)], years[-1], sep = "-")
    labels[1] <- paste0(labels[1], " [baseline]")
    iter_ids <- edited_zones_and_periods()$iter_ids
    periods <- 0:n_iterations()
    labels <- ifelse(periods %in% iter_ids, paste0("\u25cf ", labels), labels)
    setNames(as.character(periods), labels)
  })

  iteration_year <- reactive({
    req(v$abacus_data)
    dts <- projection_dates()
    date1 <- dts$date1
    date2 <- dts$date2
    period_years <- round(as.numeric(difftime(date2, date1, units = "days")) / 7 / 52.25)
    if (is.na(period_years) || period_years <= 0) period_years <- 1L
    first_year <- as.integer(format(date1, "%Y"))
    first_year + (0:n_iterations()) * period_years
  })

  iteration_year_label <- reactive({
    yr <- iteration_year()
    req(v$abacus_data)
    dts <- projection_dates()
    date1 <- dts$date1
    date2 <- dts$date2
    period_years <- round(as.numeric(difftime(date2, date1, units = "days")) / 7 / 52.25)
    if (is.na(period_years) || period_years <= 0) period_years <- 1L
    paste(yr, yr + period_years, sep = "-")
  })

  render_data <- reactive({
    req(v$abacus_data)
    data <- projection_project()
    req(data)
    data$project$n_iteration <- n_iterations()
    if (!is.null(v$lc_list_df) && is.data.frame(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
      data$landcover <- update_landcover_colors(data$landcover, v$lc_list_df)
    }
    data
  })

  period_year_total <- reactive({
    req(v$abacus_data)
    dts <- projection_dates()
    date1 <- dts$date1
    date2 <- dts$date2
    period_years <- round(as.numeric(difftime(date2, date1, units = "days")) / 7 / 52.25)
    if (is.na(period_years) || period_years <= 0) period_years <- 1L
    first_year <- as.integer(format(date1, "%Y"))
    last_year <- first_year + (n_iterations() + 1) * period_years
    total_proj_yrs <- last_year - first_year
    format_period_with_length(first_year, last_year, total_proj_yrs)
  })

  observe({
    req(v$abacus_data)
    scenarios <- scenario_catalog()
    req(length(scenarios) > 0)
    choices <- setNames(
      as.character(seq_along(scenarios)),
      vapply(scenarios, function(s) s$label %||% paste("Scenario", s$id), character(1))
    )
    cur_sc_idx <- selected_scenario_index()
    selected <- if (length(cur_sc_idx) > 0 && as.character(cur_sc_idx[1]) %in% unname(choices)) {
      as.character(cur_sc_idx[1])
    } else {
      unname(choices)[1]
    }
    update_toolbar_input_select("sel_scenario", choices = choices, selected = selected)

    if (has_scenario_zones()) {
      sc_i2 <- suppressWarnings(as.integer(selected[1]))
      cats2 <- scenario_catalog()
      ab_cur <- if (!is.na(sc_i2) && sc_i2 >= 1L && sc_i2 <= length(cats2))
        cats2[[sc_i2]]$abacus_data %||% v$abacus_data else v$abacus_data
      zone_ids_vec   <- if (!is.null(ab_cur$zone) && nrow(ab_cur$zone) > 0) ab_cur$zone$zone_id else 0L
      zone_labels_vec <- if (!is.null(ab_cur$zone) && nrow(ab_cur$zone) > 0) ab_cur$zone$label else "All Zones"
      zone_choices <- setNames(as.character(zone_ids_vec), zone_labels_vec)
      if (length(zone_choices) > 0) {
        cur_z <- input$sel_zone
        sel_z <- if (length(cur_z) > 0 && as.character(cur_z[1]) %in% unname(zone_choices)) {
          as.character(cur_z[1])
        } else {
          unname(zone_choices)[1]
        }
        updateSelectInput(session, "sel_zone", choices = zone_choices, selected = sel_z)
      } else {
        updateSelectInput(session, "sel_zone", choices = character(0))
      }
    }
  })

  observe({
    req(v$abacus_data)
    edited <- edited_zones_and_periods()
    sc_i3 <- suppressWarnings(as.integer(selected_scenario_index()[1]))
    cats3 <- scenario_catalog()
    ab_cur <- if (!is.na(sc_i3) && sc_i3 >= 1L && sc_i3 <= length(cats3))
      cats3[[sc_i3]]$abacus_data %||% v$abacus_data else v$abacus_data
    zone_ids_vec   <- if (!is.null(ab_cur$zone) && nrow(ab_cur$zone) > 0) ab_cur$zone$zone_id else 0L
    zone_labels_vec <- if (!is.null(ab_cur$zone) && nrow(ab_cur$zone) > 0) ab_cur$zone$label else "All Zones"
    zone_labels_vec <- ifelse(
      zone_ids_vec %in% edited$zone_ids,
      paste0("\u25cf ", zone_labels_vec),
      zone_labels_vec
    )
    zone_choices <- setNames(as.character(zone_ids_vec), zone_labels_vec)
    if (length(zone_choices) > 0) {
      current_zone <- input$sel_zone
      sel_zone_val <- if (length(current_zone) > 0 && as.character(current_zone[1]) %in% unname(zone_choices)) {
        as.character(current_zone[1])
      } else {
        unname(zone_choices)[1]
      }
      updateSelectInput(session, "sel_zone", choices = zone_choices, selected = sel_zone_val)
    } else {
      updateSelectInput(session, "sel_zone", choices = character(0))
    }
  })

  output$scenario_iteration_text <- renderText({
    as.character(n_iterations())
  })

  observeEvent(input$sel_scenario, {
    idx <- input$sel_scenario
    if (!is.null(idx) && nzchar(idx)) {
      selected_scenario_index(as.character(idx))
      # Restore per-scenario abacus_data snapshot when switching scenarios
      sc_i <- suppressWarnings(as.integer(idx))
      if (!is.na(sc_i)) {
        cats <- isolate(scenario_catalog())
        if (sc_i >= 1L && sc_i <= length(cats)) {
          sc_snap <- cats[[sc_i]]
          if (!is.null(sc_snap$abacus_data)) {
            v$abacus_data <- sc_snap$abacus_data
          }
          if (!is.null(sc_snap$map1_stars)) {
            v$map1_stars <- sc_snap$map1_stars
          } else if (!is.null(sc_snap$map1_id) && !is.null(v$map_catalog[[sc_snap$map1_id]])) {
            v$map1_stars <- v$map_catalog[[sc_snap$map1_id]]$stars
          }
          if (!is.null(sc_snap$map2_stars)) {
            v$map2_stars <- sc_snap$map2_stars
          } else if (!is.null(sc_snap$map2_id) && !is.null(v$map_catalog[[sc_snap$map2_id]])) {
            v$map2_stars <- v$map_catalog[[sc_snap$map2_id]]$stars
          }
          if (!is.null(sc_snap$mapz_stars)) {
            v$mapz_stars <- sc_snap$mapz_stars
          } else if (!is.null(sc_snap$zone_map) && sc_snap$zone_map != "none" && !is.null(v$map_catalog[[sc_snap$zone_map]])) {
            v$mapz_stars <- v$map_catalog[[sc_snap$zone_map]]$stars
          }
          z_maps <- sc_snap$zone_maps %||% (if (!is.null(sc_snap$zone_map) && sc_snap$zone_map != "none") sc_snap$zone_map else character(0))
          updateSelectizeInput(session, "matrix_zone_map", selected = z_maps)
          updateSelectInput(session, "analysis_zone_map", selected = if (length(z_maps) > 0) z_maps[[1]] else "")
          if (!is.null(sc_snap$map1_year)) { v$map1_year <- sc_snap$map1_year; v$map1_date <- sc_snap$map1_year }
          if (!is.null(sc_snap$map2_year)) { v$map2_year <- sc_snap$map2_year; v$map2_date <- sc_snap$map2_year }
          # Immediately update zone selector from this scenario's snapshot
          # so the conditionalPanel has correct choices when it becomes visible
          ab_snap <- sc_snap$abacus_data
          if (!is.null(ab_snap) && !is.null(ab_snap$zone) && is.data.frame(ab_snap$zone) && nrow(ab_snap$zone) > 0) {
            zids <- ab_snap$zone$zone_id
            zlabs <- ab_snap$zone$label
            if (any(zids != 0)) {
              zchoices <- setNames(as.character(zids), zlabs)
              updateSelectInput(session, "sel_zone", choices = zchoices, selected = unname(zchoices)[1])
            } else {
              updateSelectInput(session, "sel_zone", choices = character(0))
            }
          } else {
            updateSelectInput(session, "sel_zone", choices = character(0))
          }
        }
      }
    }
    scenario_tpm_edits(NULL)
  }, ignoreInit = TRUE)

  output$scenario_description_text <- renderUI({
    sc <- selected_scenario()
    desc <- if (!is.null(sc)) sc$description %||% "" else ""
    if (!nzchar(trimws(desc))) {
      tags$span(class = "text-muted fst-italic", "No description provided.")
    } else {
      desc
    }
  })

  output$scenario_card_info_ui <- renderUI({
    sc <- selected_scenario()
    if (is.null(sc)) {
      return(tags$div(
        class = "text-muted text-center py-4",
        icon("sliders", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small mb-0", "No scenario selected or created yet.")
      ))
    }

    # Period
    p_str <- sc$period %||% ""
    if (!nzchar(p_str)) {
      if (!is.null(sc$map1_year) && !is.null(sc$map2_year)) {
        p_str <- format_period_with_length(sc$map1_year, sc$map2_year)
      } else {
        p_str <- "N/A"
      }
    } else {
      p_str <- format_period_str_with_length(p_str)
    }

    # Zonation
    z_str <- sc$zonation %||% ""
    if (!nzchar(z_str)) {
      zm <- sc$zone_maps %||% sc$zone_map
      valid_zm <- if (!is.null(zm)) setdiff(zm, "none") else character(0)
      if (length(valid_zm) > 0) {
        zc <- get_scenario_zone_choices()
        lbls <- vapply(valid_zm, function(x) {
          idx_c <- which(unname(zc) == x)
          if (length(idx_c) > 0) names(zc)[idx_c[1]] else x
        }, character(1))
        z_str <- paste(lbls, collapse = ", ")
      } else {
        z_str <- "All zones (no zonation)"
      }
    }

    # Iterations
    iters <- sc$n_iteration %||% n_iterations() %||% 4L
    iter_str <- paste0(iters, " periods")

    # Flux emission sources
    fluxes <- sc$flux_sources
    flux_ui <- if (!is.null(fluxes) && length(fluxes) > 0) {
      tags$div(
        class = "d-flex flex-wrap gap-1 justify-content-end",
        lapply(fluxes, function(f) {
          tags$span(class = "badge bg-info-subtle text-info-emphasis border border-info-subtle", style = "font-size: 0.72rem;", f)
        })
      )
    } else {
      ## Bright text: this sits on the #99a88c scenario card.
      tags$span(class = "text-white fst-italic", style = "font-size: 0.75rem; opacity: 0.9;", "None")
    }

    desc <- trimws(sc$description %||% "")

    ## Inner boxes on the #99a88c card: light text, no table zebra striping, so
    ## the content stays readable against the card surface. The boxes sit on the
    ## darker #66827a surface with a #540b0e border, so they stand out from the
    ## card itself.
    tags$div(
      class = "d-flex flex-column gap-2 text-white",
      style = "font-size: 0.85rem;",
      # Parameters Table
      tags$div(
        class = "border rounded p-2",
        style = "background-color: #66827a; border-color: #540b0e !important;",
        tags$table(
          class = "table table-sm table-borderless table-dark bg-transparent mb-0",
          style = "font-size: 0.8rem; --bs-table-bg: transparent; --bs-table-color: inherit;",
          tags$tbody(
            tags$tr(
              tags$td(class = "py-1 text-white fw-semibold", icon("calendar", class = "me-1 scenario-param-icon"), "Base Period:"),
              tags$td(class = "text-end fw-semibold py-1", p_str)
            ),
            tags$tr(
              tags$td(class = "py-1 text-white fw-semibold", icon("layer-group", class = "me-1 scenario-param-icon"), "Zonation:"),
              tags$td(class = "text-end fw-semibold py-1", z_str)
            ),
            tags$tr(
              tags$td(class = "py-1 text-white fw-semibold", icon("clock-rotate-left", class = "me-1 scenario-param-icon"), "Iterations:"),
              tags$td(class = "text-end fw-semibold py-1", iter_str)
            ),
            tags$tr(
              tags$td(class = "py-1 align-middle text-white fw-semibold", icon("wind", class = "me-1 scenario-param-icon"), "Flux Sources:"),
              tags$td(class = "text-end py-1 align-middle", flux_ui)
            )
          )
        )
      ),
      # Description Box
      if (nzchar(desc)) {
        tags$div(
          class = "border rounded p-2 small",
          style = "background-color: #66827a; border-color: #540b0e !important; max-height: 52px; overflow-y: auto; font-size: 0.78rem; line-height: 1.25;",
          tags$div(class = "fw-bold text-white mb-1", style = "font-size: 0.7rem; letter-spacing: 0.5px;", "DESCRIPTION"),
          tags$div(class = "text-white", style = "white-space: pre-wrap;", desc)
        )
      } else {
        tags$div(
          class = "border rounded p-2 small fst-italic text-white",
          style = "background-color: #66827a; border-color: #540b0e !important; font-size: 0.75rem;",
          "No description provided."
        )
      }
    )
  })

  observeEvent(input$show_zonation_map, {
    req(v$abacus_data)
    previous_map_id <- active_zonation_map_id()
    if (!is.null(previous_map_id)) {
      output[[previous_map_id]] <- NULL
    }
    map_id <- paste0("zonation_map_", zonation_map_counter() + 1L)
    zonation_map_counter(zonation_map_counter() + 1L)
    active_zonation_map_id(map_id)

    ## ------------------------------------------------------------------
    ## Determine which zonation records are active for the current scenario.
    ## ------------------------------------------------------------------
    sc_cur <- isolate(selected_scenario())
    active_zone_ids <- if (!is.null(sc_cur)) {
      zm <- sc_cur$zone_maps %||% (if (!is.null(sc_cur$zone_map) && sc_cur$zone_map != "none") sc_cur$zone_map else character(0))
      setdiff(zm, "none")
    } else {
      character(0)
    }
    ## Fallback: any zonation record in the catalog
    if (length(active_zone_ids) == 0 && !is.null(v$map_catalog)) {
      active_zone_ids <- names(Filter(function(r) identical(r$type, "Zonation map"), v$map_catalog))
    }

    ## ------------------------------------------------------------------
    ## Resolve zone_sf for the current scenario's zonation map(s).
    ## Fast path: use the pre-built sf cached on the scenario entry
    ##            (computed at scenario generation time).
    ## Slow path: call build_scenario_zone_sf() on the fly (first open,
    ##            or for scenarios loaded from a saved session).
    ## ------------------------------------------------------------------
    ab_zids <- if (!is.null(v$abacus_data$zone)) v$abacus_data$zone$zone_id else NULL
    cached_ok <- !is.null(sc_cur$zone_sf) &&
                 inherits(sc_cur$zone_sf, "sf") &&
                 nrow(sc_cur$zone_sf) > 0 &&
                 (is.null(ab_zids) || all(sc_cur$zone_sf$zone_id %in% ab_zids))
    zone_sf <- if (cached_ok) {
      sc_cur$zone_sf
    } else {
      build_scenario_zone_sf(
        zone_map_ids = active_zone_ids,
        map_catalog  = v$map_catalog,
        zone_legends = v$zone_legends,
        ab_zones     = v$abacus_data$zone
      )
    }
    ## Last resort: vectorise v$mapz_stars
    if (is.null(zone_sf) && !is.null(v$mapz_stars)) {
      zone_sf <- tryCatch({
        m    <- if (inherits(v$mapz_stars, "stars_proxy")) stars::st_as_stars(v$mapz_stars) else v$mapz_stars
        poly <- sf::st_as_sf(m, as_points = FALSE, merge = TRUE)
        ## normalise via build_scenario_zone_sf with single entry
        build_scenario_zone_sf(
          zone_map_ids = active_zone_ids,
          map_catalog  = c(v$map_catalog,
            list("_fallback" = list(type = "Zonation map", stars = v$mapz_stars,
                                   zone_sf = NULL, sf = NULL, shp_file = NULL))),
          zone_legends = v$zone_legends,
          ab_zones     = v$abacus_data$zone
        )
      }, error = function(e) NULL)
    }

    ## Cache zone_sf back onto catalog entry and override if it was generated on the fly
    if (!is.null(zone_sf) && nrow(zone_sf) > 0 && (is.null(sc_cur$zone_sf) || !inherits(sc_cur$zone_sf, "sf"))) {
      sc_idx <- suppressWarnings(as.integer(selected_scenario_index()))
      if (!is.na(sc_idx) && sc_idx >= 1L) {
        cats <- isolate(scenario_catalog())
        if (sc_idx <= length(cats)) {
          cats[[sc_idx]]$zone_sf <- zone_sf
          scenario_catalog(cats)
        }
        overrides <- isolate(scenario_overrides())
        if (!is.null(overrides[[as.character(sc_idx)]])) {
          overrides[[as.character(sc_idx)]]$zone_sf <- zone_sf
          scenario_overrides(overrides)
        }
      }
    }

    if (!is.null(zone_sf) && nrow(zone_sf) > 0) {
      output[[map_id]] <- leaflet::renderLeaflet({
        zones <- v$abacus_data$zone
        zone_map <- zone_sf
        ## Use pre-built composite labels if available, otherwise look up
        ## from the scenario zone table, or fall back to "Zone N".
        if ("label" %in% names(zone_map) && !all(is.na(zone_map$label))) {
          ## Labels already attached by build_scenario_zone_sf - use them
        } else if (!is.null(zones) && nrow(zones) > 0 && "label" %in% names(zones)) {
          zone_map$label <- zones$label[match(zone_map$zone_id, zones$zone_id)]
        } else {
          zone_map$label <- paste("Zone", zone_map$zone_id)
        }
        ## Color mapping: strictly 1 color per unique zone_id (never by row index)
        if ("color" %in% names(zone_map) && !all(is.na(zone_map$color))) {
          ## Colors already combined and attached by build_scenario_zone_sf - use them!
        } else if (!is.null(zones) && nrow(zones) > 0 && "color" %in% names(zones)) {
          zone_map$color <- zones$color[match(zone_map$zone_id, zones$zone_id)]
        } else {
          zone_map$color <- NA_character_
        }

        missing_color <- is.na(zone_map$color) | !nzchar(zone_map$color)
        if (any(missing_color)) {
          u_zids <- sort(unique(zone_map$zone_id))
          pal <- grDevices::hcl.colors(max(length(u_zids), 1L), palette = "Viridis")
          pal_map <- setNames(pal, as.character(u_zids))
          zone_map$color[missing_color] <- pal_map[as.character(zone_map$zone_id[missing_color])]
        }

        zone_map$is_selected <- zone_map$zone_id == selected_zone()
        zone_map$is_edited <- zone_map$zone_id %in% edited_zones_and_periods()$zone_ids
        if (!is.na(sf::st_crs(zone_map))) {
          zone_map <- tryCatch(sf::st_transform(zone_map, 4326), error = function(e) zone_map)
        }
        bounds <- sf::st_bbox(zone_map)
        map <- leaflet::leaflet(data = zone_map)
        map <- leaflet::addTiles(map)
        map <- leaflet::addPolygons(
          map,
          layerId = ~ zone_id,
          color = ~ ifelse(
            is_edited,
            "#DC3545",
            ifelse(is_selected, "#111111", "#333333")
          ),
          weight = ~ ifelse(is_edited | is_selected, 3, 1),
          fillColor = ~ ifelse(is_selected, "#4A4A4A", color),
          fillOpacity = ~ ifelse(is_selected, 0.95, 0.8),
          label = ~ label,
          popup = ~ label,
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "#111111",
            bringToFront = TRUE
          )
        )

        ## Add color legend so the number of legend items exactly matches the number of zone IDs
        u_zones <- unique(data.frame(
          zone_id = zone_map$zone_id,
          label = zone_map$label,
          color = zone_map$color,
          stringsAsFactors = FALSE
        ))
        u_zones <- u_zones[order(u_zones$zone_id), ]
        if (nrow(u_zones) > 0 && nrow(u_zones) <= 30) {
          map <- leaflet::addLegend(
            map,
            position = "bottomright",
            colors = u_zones$color,
            labels = u_zones$label,
            title = "Zones",
            opacity = 0.85
          )
        }

        leaflet::fitBounds(
          map,
          lng1 = bounds[["xmin"]],
          lat1 = bounds[["ymin"]],
          lng2 = bounds[["xmax"]],
          lat2 = bounds[["ymax"]]
        )
      })
      showModal(
        modalDialog(
          title = "Zonation map",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          tags$div("Click a zone to select it.", class = "small text-muted mb-2"),
          leaflet::leafletOutput(map_id, height = "70vh")
        )
      )
    } else {
      showModal(
        modalDialog(
          title = "Zonation map",
          size = "m",
          easyClose = TRUE,
          footer = modalButton("Close"),
          tags$p("Spatial polygon data for the zonation map is not currently available.", class = "text-muted"),
          tags$p("Please use the 'Zone' dropdown to select zones.")
        )
      )
    }
  }, ignoreInit = TRUE)

  observeEvent({
    map_id <- active_zonation_map_id()
    if (is.null(map_id)) NULL else input[[paste0(map_id, "_shape_click")]]
  }, {
    zone_id <- as.character(input[[paste0(active_zonation_map_id(), "_shape_click")]]$id)
    zone_ids <- if (!is.null(v$abacus_data$zone)) as.character(v$abacus_data$zone$zone_id) else character(0)
    if (length(zone_ids) > 0) req(zone_id %in% zone_ids)
    if (length(zone_id) == 1 && nzchar(zone_id)) {
      updateSelectInput(session, "sel_zone", selected = zone_id)
    }
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(input$edit_scenario, {
    sc <- selected_scenario()
    req(sc)
    showModal(
      modalDialog(
        title = "Edit scenario",
        textInput(
          "edit_scenario_label",
          "Scenario name",
          value = sc$label %||% ""
        ),
        textAreaInput(
          "edit_scenario_description",
          "Description",
          value = sc$description %||% "",
          rows = 3
        ),
        numericInput(
          "edit_scenario_iteration",
          "Number of iterations",
          value = as.integer(sc$n_iteration %||% n_iterations()),
          min = 1,
          max = 50,
          step = 1
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "confirm_edit_scenario",
            "Save",
            icon = icon("check"),
            class = "btn-primary"
          )
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_edit_scenario, {
    label <- trimws(input$edit_scenario_label %||% "")
    description <- trimws(input$edit_scenario_description %||% "")
    if (!nzchar(label)) {
      showNotification("Enter a scenario name.", type = "error")
      return()
    }
    sc_idx <- as.integer(selected_scenario_index())
    if (is.na(sc_idx) || sc_idx < 1) return()
    scenarios <- scenario_catalog()
    if (sc_idx > length(scenarios)) return()
    scenarios[[sc_idx]]$label <- label
    scenarios[[sc_idx]]$description <- description
    n_it <- as.integer(input$edit_scenario_iteration)
    if (!is.na(n_it) && n_it >= 1) {
      scenarios[[sc_idx]]$n_iteration <- n_it
    }
    scenario_catalog(scenarios)
    overrides <- scenario_overrides()
    if (!is.null(overrides[[as.character(sc_idx)]])) {
      overrides[[as.character(sc_idx)]]$label <- label
      overrides[[as.character(sc_idx)]]$description <- description
      if (!is.na(n_it) && n_it >= 1) {
        overrides[[as.character(sc_idx)]]$n_iteration <- n_it
      }
      scenario_overrides(overrides)
    }
    # Sync with v$scenario_list
    if (!is.null(v$scenario_list) && sc_idx <= length(v$scenario_list)) {
      v$scenario_list[[sc_idx]]$label <- label
      v$scenario_list[[sc_idx]]$desc <- description
    }
    removeModal()
  }, ignoreInit = TRUE)

  get_scenario_map_choices <- function() {
    records <- isolate(v$map_catalog)
    landcover_records <- if (!is.null(records)) records[vapply(records, function(record) {
      identical(record$type, "Land cover map")
    }, logical(1))] else list()
    if (length(landcover_records) > 0) {
      landcover_records <- landcover_records[order(vapply(landcover_records, function(record) {
        suppressWarnings(as.integer(record$year %||% 0L))
      }, integer(1)))]
    }
    lc_choices <- map_choices_year(landcover_records)
    if (length(lc_choices) == 0) {
      y1 <- isolate(v$map1_year %||% v$map1_date %||% 2000)
      y2 <- isolate(v$map2_year %||% v$map2_date %||% 2010)
      lc_choices <- setNames(c("map1_fallback", "map2_fallback"), c(as.character(y1), as.character(y2)))
    }
    lc_choices
  }

  get_scenario_zone_choices <- function() {
    recs <- isolate(v$map_catalog)
    zone_recs <- if (!is.null(recs)) recs[vapply(recs, function(r) identical(r$type, "Zonation map"), logical(1))] else list()
    zone_map_choices(zone_recs)
  }

  get_scenario_flux_choices <- function() {
    srcs <- character(0)
    ot <- isolate(v$other_emission_df)
    if (!is.null(ot) && nrow(ot) > 0 && "source" %in% names(ot)) {
      srcs <- unique(ot$source)
      srcs <- srcs[!is.na(srcs) & nzchar(trimws(srcs))]
    }
    if (length(srcs) == 0 && !is.null(v$other_emission_source)) {
      srcs <- isolate(v$other_emission_source)
    }
    unique(srcs)
  }

  get_projection_context_info <- function() {
    v$map_catalog_version
    records <- isolate(v$map_catalog)
    cur_m1 <- isolate(input$matrix_map1) %||% isolate(input$analysis_map1)
    cur_m2 <- isolate(input$matrix_map2) %||% isolate(input$analysis_map2)
    y1 <- if (!is.null(cur_m1) && !is.null(records) && cur_m1 %in% names(records)) records[[cur_m1]]$year else (isolate(v$map1_year) %||% 2000)
    y2 <- if (!is.null(cur_m2) && !is.null(records) && cur_m2 %in% names(records)) records[[cur_m2]]$year else (isolate(v$map2_year) %||% 2010)
    period_str <- format_period_with_length(y1, y2)

    z_choices <- get_scenario_zone_choices()
    cur_z <- parse_map_selection(isolate(input$matrix_zone_map))
    cur_z <- setdiff(cur_z, "none")
    zone_str <- if (length(cur_z) > 0) {
      z_labels <- vapply(cur_z, function(zid) {
        idx <- which(unname(z_choices) == zid)
        if (length(idx) > 0) names(z_choices)[idx[1]] else zid
      }, character(1))
      paste(z_labels, collapse = ", ")
    } else {
      "All zones (no zonation)"
    }

    flux_srcs <- get_scenario_flux_choices()
    active_flux <- tryCatch(if (exists("selected_sources", mode = "function")) isolate(selected_sources()) else NULL, error = function(e) NULL)
    if (!is.null(active_flux) && length(active_flux) > 0) {
      flux_srcs <- active_flux
    }
    flux_str <- if (length(flux_srcs) > 0) paste(flux_srcs, collapse = ", ") else NULL

    list(
      period = period_str,
      zonation = zone_str,
      flux = flux_str
    )
  }

  output$empty_scenario_inputs_summary <- renderUI({
    v$map_catalog_version
    info <- get_projection_context_info()
    div(
      class = "card bg-light border p-3 text-start mb-3 shadow-none",
      style = "width: 100%; max-width: 560px;",
      div(
        class = "fw-bold text-secondary small mb-2 text-uppercase d-flex align-items-center gap-1",
        icon("sliders", class = "text-primary"),
        "Current Analysis Settings"
      ),
      div(
        class = "d-flex flex-column gap-2 small",
        div(
          class = "d-flex align-items-center justify-content-between",
          span(class = "text-muted", "Period:"),
          span(class = "fw-medium text-dark text-end", info$period)
        ),
        div(
          class = "d-flex align-items-center justify-content-between",
          span(class = "text-muted", "Zonation:"),
          span(class = "fw-medium text-dark text-end", info$zonation)
        ),
        if (!is.null(info$flux)) {
          div(
            class = "d-flex align-items-center justify-content-between",
            span(class = "text-muted", "Flux emission sources:"),
            span(class = "badge bg-info text-white", info$flux)
          )
        }
      )
    )
  })

  show_add_scenario_modal <- function() {
    lc_choices <- get_scenario_map_choices()
    zone_choices <- get_scenario_zone_choices()
    flux_choices <- get_scenario_flux_choices()

    cur_m1 <- isolate(input$matrix_map1) %||% isolate(input$analysis_map1)
    if (is.null(cur_m1) || !(cur_m1 %in% unname(lc_choices))) {
      cur_m1 <- unname(lc_choices)[1]
    }
    cur_m2 <- isolate(input$matrix_map2) %||% isolate(input$analysis_map2)
    if (is.null(cur_m2) || !(cur_m2 %in% unname(lc_choices))) {
      cur_m2 <- if (length(lc_choices) > 1) unname(lc_choices)[2] else unname(lc_choices)[1]
    }

    cur_zones <- parse_map_selection(isolate(input$matrix_zone_map))
    cur_zones <- setdiff(cur_zones, "none")
    default_zones <- intersect(cur_zones, unname(zone_choices))

    default_flux <- if (exists("selected_sources", mode = "function")) isolate(selected_sources()) else flux_choices
    if (length(default_flux) == 0) default_flux <- flux_choices

    sc_count <- length(scenario_catalog())
    default_name <- if (sc_count == 0) "Scenario_1" else paste0("Scenario_", sc_count + 1L)

    showModal(
      modalDialog(
        title = if (sc_count == 0) "Add First Scenario" else "Add scenario",
        textInput("new_scenario_label", "Scenario name", value = default_name),
        textAreaInput(
          "new_scenario_description",
          "Description",
          value = "",
          placeholder = "Optional scenario description...",
          rows = 3
        ),
        div(
          class = "card bg-light border p-3 mt-3 mb-1",
          div(
            class = "fw-bold text-secondary small mb-2 text-uppercase d-flex align-items-center gap-1",
            icon("sliders", class = "text-primary"),
            "Scenario Parameters"
          ),
          div(
            class = "d-flex flex-column gap-3",
            div(
              tags$label("Period (Land cover changes)", class = "form-label fw-semibold small mb-1"),
              div(
                class = "d-flex align-items-center matrix-header-controls gap-2",
                shiny::selectizeInput(
                  "new_scenario_map1",
                  NULL,
                  choices = lc_choices,
                  selected = cur_m1,
                  width = "180px",
                  options = list(placeholder = "Before map")
                ),
                span(icon("arrow-right-long"), class = "matrix-header-label mx-1 text-muted"),
                shiny::selectizeInput(
                  "new_scenario_map2",
                  NULL,
                  choices = lc_choices,
                  selected = cur_m2,
                  width = "180px",
                  options = list(placeholder = "After map")
                )
              )
            ),
            div(
              tags$label("Number of iterations", class = "form-label fw-semibold small mb-1"),
              numericInput(
                "new_scenario_iteration",
                NULL,
                value = as.integer(n_iterations()),
                min = 1,
                max = 50,
                step = 1,
                width = "100%"
              )
            ),
            div(
              tags$label("Zonation maps", class = "form-label fw-semibold small mb-1"),
              selectizeInput(
                "new_scenario_zone_map",
                NULL,
                choices = zone_choices,
                selected = default_zones,
                multiple = TRUE,
                width = "100%",
                options = list(
                  placeholder = "No zonation map (All zones)",
                  plugins = list("remove_button")
                )
              )
            ),
            if (length(flux_choices) > 0) {
              div(
                tags$label("Flux emission sources", class = "form-label fw-semibold small mb-1"),
                selectizeInput(
                  "new_scenario_flux_sources",
                  NULL,
                  choices = flux_choices,
                  selected = default_flux,
                  multiple = TRUE,
                  width = "100%",
                  options = list(
                    plugins = list("remove_button"),
                    placeholder = "Select flux emission sources..."
                  )
                )
              )
            } else {
              div(
                class = "small text-muted fst-italic",
                icon("circle-info", class = "me-1"),
                "No flux emission sources defined in project."
              )
            }
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add_scenario", "Add scenario", icon = icon("plus"), class = "btn-primary")
        )
      )
    )
  }

  observeEvent(input$add_scenario, {
    show_add_scenario_modal()
  }, ignoreInit = TRUE)

  observeEvent(input$btn_add_first_scenario, {
    show_add_scenario_modal()
  }, ignoreInit = TRUE)

  first_scenario_prompted <- reactiveVal(FALSE)
  observe({
    req(v$abacus_data)
    is_proj <- identical(input$main_page, "Projection")
    if (is_proj && length(scenario_catalog()) == 0 && !first_scenario_prompted()) {
      first_scenario_prompted(TRUE)
      show_add_scenario_modal()
    }
  })

  observeEvent(input$confirm_add_scenario, {
    label <- trimws(input$new_scenario_label %||% "")
    description <- trimws(input$new_scenario_description %||% "")
    if (!nzchar(label)) {
      showNotification("Enter a scenario name.", type = "error")
      return()
    }

    # 1. Period selection handling
    id1 <- input$new_scenario_map1
    id2 <- input$new_scenario_map2
    records <- isolate(v$map_catalog)

    if (!is.null(records) && !is.null(id1) && !is.null(id2) &&
        id1 %in% names(records) && id2 %in% names(records)) {
      v$map1_stars <- records[[id1]]$stars
      v$map2_stars <- records[[id2]]$stars
      v$map1_year <- suppressWarnings(as.integer(records[[id1]]$year))
      v$map2_year <- suppressWarnings(as.integer(records[[id2]]$year))
      v$map1_date <- v$map1_year
      v$map2_date <- v$map2_year
      v$period_year <- as.numeric(v$map2_year - v$map1_year)
      updateSelectizeInput(session, "analysis_map1", selected = id1)
      updateSelectizeInput(session, "analysis_map2", selected = id2)
      updateSelectizeInput(session, "matrix_map1", selected = id1)
      updateSelectizeInput(session, "matrix_map2", selected = id2)
    }

    y1 <- if (!is.null(id1) && !is.null(records) && id1 %in% names(records)) records[[id1]]$year else (v$map1_year %||% 2000)
    y2 <- if (!is.null(id2) && !is.null(records) && id2 %in% names(records)) records[[id2]]$year else (v$map2_year %||% 2010)
    p_label <- paste0(y1, " \u2192 ", y2)

    # 2. Zonation selection handling (supports multiple zonation maps)
    zone_vals <- parse_map_selection(input$new_scenario_zone_map)
    zone_vals <- setdiff(zone_vals, "none")
    has_zone_selected <- length(zone_vals) > 0
    z_choices <- get_scenario_zone_choices()

    zone_layers <- list()
    if (has_zone_selected) {
      z_labels <- vapply(zone_vals, function(zv) {
        idx <- which(unname(z_choices) == zv)
        if (length(idx) > 0) names(z_choices)[idx[1]] else zv
      }, character(1))
      z_label <- paste(z_labels, collapse = ", ")
      for (zv in zone_vals) {
        if (!is.null(records) && zv %in% names(records) && !is.null(records[[zv]]$stars)) {
          zone_layers[[zv]] <- records[[zv]]$stars
        }
      }
      updateSelectizeInput(session, "matrix_zone_map", selected = zone_vals)
      updateSelectInput(session, "analysis_zone_map", selected = zone_vals[[1]])
      first_rec <- records[[zone_vals[[1]]]]
      v$mapz_stars <- if (!is.null(first_rec)) first_rec$stars else NULL
    } else {
      z_label <- "All zones (no zonation)"
      v$mapz_stars <- NULL
      updateSelectizeInput(session, "matrix_zone_map", selected = character(0))
      updateSelectInput(session, "analysis_zone_map", selected = "")
    }

    # 3. Flux emission source handling
    flux_val <- input$new_scenario_flux_sources
    if (!is.null(flux_val) && length(flux_val) > 0) {
      if (exists("selected_sources", mode = "function")) selected_sources(flux_val)
      updateSelectizeInput(session, "select_source", selected = flux_val)
      updateSelectizeInput(session, "summary_select_source", selected = flux_val)
    }

    # 4. Rebuild land cover changes matrix with matching zonation
    if (!is.null(v$map1_stars) && !is.null(v$map2_stars)) {
      update_lc_matrix(v$map1_stars, v$map2_stars, zone_layers = zone_layers)
    }

    # 5. Build composite zone data & prepare abacus projection data
    comp_res <- build_composite_zone_data(v$lc_changes_df, zone_vals, v$zone_legends)
    zone_src <- comp_res$zone_df
    lc_ch_for_abacus <- comp_res$lc_ch

    d1 <- if (!is.null(v$map1_date) && nzchar(as.character(v$map1_date))) v$map1_date else (v$map1_year %||% 2000)
    d2 <- if (!is.null(v$map2_date) && nzchar(as.character(v$map2_date))) v$map2_date else (v$map2_year %||% 2010)
    ot <- v$other_emission_df
    if (!is.null(ot) && nrow(ot) > 0 && !is.null(flux_val) && length(flux_val) > 0) {
      ot <- ot[ot$source %in% flux_val, , drop = FALSE]
    }

    ab <- prepare_projection_abacus(
      landcover = v$lc_list_df,
      landcover_change = lc_ch_for_abacus,
      zone = zone_src,
      carbonstock = v$cstock_list,
      date1 = d1,
      date2 = d2,
      n_iteration = as.integer(input$new_scenario_iteration %||% 4L),
      other_emission_factor = ot
    )
    if (!is.null(ab)) {
      v$abacus_data <- ab
    }

    # 6. Add to scenario_catalog
    scenarios <- scenario_catalog()
    current <- if (length(scenarios) > 0) scenarios[[1]] else list()
    new_index <- length(scenarios) + 1L
    current$tpm <- NULL
    current$baseline_tpm <- NULL
    current$baseline_area <- NULL
    current$id <- new_index
    current$label <- label
    current$description <- description
    current$n_iteration <- as.integer(input$new_scenario_iteration %||% 4L)
    current$landcover <- if (!is.null(v$abacus_data)) v$abacus_data$landcover else NULL
    current$new_lc_id <- list()
    current$period <- p_label
    current$zonation <- z_label
    current$zone_maps <- zone_vals
    current$zone_map <- if (has_zone_selected) zone_vals[[1]] else "none"
    current$flux_sources <- flux_val
    current$abacus_data <- ab
    current$map1_stars <- v$map1_stars
    current$map2_stars <- v$map2_stars
    current$mapz_stars <- if (has_zone_selected) v$mapz_stars else NULL
    current$map1_year  <- v$map1_year
    current$map2_year  <- v$map2_year
    current$map1_id    <- id1
    current$map2_id    <- id2
    current$zone_src   <- zone_src
    current$zone_layers <- zone_layers

    ## Pre-build composite zone_sf for the zone selector map and cache it on the
    ## scenario entry.  Intersection happens here (once) so clicking the Map
    ## button later is instant.
    current$zone_sf <- tryCatch(
      build_scenario_zone_sf(
        zone_map_ids = zone_vals,
        map_catalog  = isolate(v$map_catalog),
        zone_legends = isolate(v$zone_legends),
        ab_zones     = if (!is.null(ab)) ab$zone else NULL
      ),
      error = function(e) { message("zone_sf cache build failed: ", e$message); NULL }
    )

    all_scenarios <- c(scenarios, list(current))
    scenario_catalog(all_scenarios)
    overrides <- scenario_overrides()
    overrides[[as.character(new_index)]] <- current
    scenario_overrides(overrides)
    selected_scenario_index(as.character(new_index))

    choices <- setNames(
      as.character(seq_along(all_scenarios)),
      vapply(all_scenarios, function(s) s$label %||% paste("Scenario", s$id), character(1))
    )
    update_toolbar_input_select("sel_scenario", choices = choices, selected = as.character(new_index))

    # Immediately sync sel_zone with the scenario zone table
    if (!is.null(ab) && !is.null(ab$zone) && is.data.frame(ab$zone) && nrow(ab$zone) > 0) {
      zids <- ab$zone$zone_id
      zlabs <- ab$zone$label
      if (any(zids != 0)) {
        zchoices <- setNames(as.character(zids), zlabs)
        updateSelectInput(session, "sel_zone", choices = zchoices, selected = unname(zchoices)[1])
      } else {
        updateSelectInput(session, "sel_zone", choices = character(0))
      }
    } else {
      updateSelectInput(session, "sel_zone", choices = character(0))
    }

    # Sync with v$scenario_list and compute initial emissions independently
    ab_init_calc <- tryCatch(calculate_abacus_emission(ab, n_iteration = current$n_iteration), error = function(e) NULL)
    v$scenario_list[[new_index]] <- list(
      label = label,
      desc = description,
      abacus_scenario = list(
        scenario = current,
        baseline_emission = ab_init_calc,
        emission = ab_init_calc
      )
    )
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(input$remove_scenario, {
    # Guard against firing on mount/init when value is NULL or 0
    if (is.null(input$remove_scenario) || is.na(input$remove_scenario) || input$remove_scenario < 1) {
      return()
    }
    scenarios <- scenario_catalog()
    sc_idx <- suppressWarnings(as.integer(input$sel_scenario))
    if (length(scenarios) == 0 || is.na(sc_idx) || sc_idx < 1L || sc_idx > length(scenarios)) {
      return()
    }
    if (length(scenarios) == 1L) {
      scenario_catalog(list())
      scenario_overrides(list())
      v$scenario_list <- list()
      selected_scenario_index("")
      update_toolbar_input_select("sel_scenario", choices = c("(No scenarios)" = ""), selected = "")
      return()
    }
    sc_left <- scenarios[-sc_idx]
    scenario_catalog(sc_left)
    overrides <- scenario_overrides()
    overrides <- overrides[setdiff(names(overrides), as.character(sc_idx))]
    if (length(overrides) > 0) {
      shifted <- vector("list", length(overrides))
      for (name in names(overrides)) {
        old_index <- suppressWarnings(as.integer(name))
        if (!is.na(old_index)) {
          new_index <- if (old_index > sc_idx) old_index - 1L else old_index
          shifted[[as.character(new_index)]] <- overrides[[name]]
        }
      }
      overrides <- shifted[!vapply(shifted, is.null, logical(1))]
    }
    scenario_overrides(overrides)
    if (length(v$scenario_list) >= sc_idx) {
      v$scenario_list <- v$scenario_list[-sc_idx]
    }
    new_sel_idx <- as.character(min(sc_idx, length(sc_left)))
    selected_scenario_index(new_sel_idx)
    choices <- setNames(
      as.character(seq_along(sc_left)),
      vapply(sc_left, function(s) s$label %||% paste("Scenario", s$id), character(1))
    )
    update_toolbar_input_select("sel_scenario", choices = choices, selected = new_sel_idx)
  }, ignoreInit = TRUE)

  observe({
    req(v$abacus_data)
    choices <- iteration_choices()
    if (is.null(choices) || length(choices) == 0) return()
    current_iteration <- as.character(input$sel_iteration)
    selected <- if (length(current_iteration) > 0 && as.character(current_iteration[1]) %in% unname(choices))
      as.character(current_iteration[1])
    else if ("1" %in% unname(choices))
      "1"
    else
      unname(choices)[1]
    if (!is.null(selected) && !is.na(selected) && length(selected) == 1 && selected %in% unname(choices)) {
      updateSelectInput(session, "sel_iteration", choices = choices, selected = as.character(selected))
    }
  })

  observeEvent(input[["area_plot_period"]], {
    period_data <- input[["area_plot_period"]]
    period <- suppressWarnings(as.integer(period_data$iteration))
    if (!is.na(period) && period >= 0 && period <= n_iterations()) {
      updateSelectInput(session, "sel_iteration", selected = as.character(period))
    }
  }, ignoreInit = TRUE)

  observeEvent(input[["widget_out_baseline"]], {
    bl <- input[["widget_out_baseline"]]
    if (!is.null(bl)) {
      v$abacus_baseline <- bl
    }
  }, ignoreInit = TRUE)

  observeEvent(input[["widget_out_update"]], {
    update_data <- input[["widget_out_update"]]
    if (!is.null(update_data$emission)) {
      emission_data(update_data$emission)
    }
    if (!is.null(update_data$baseline_emission)) {
      baseline_emission_data(update_data$baseline_emission)
    }
    if (!is.null(update_data$scenario)) {
      sc_cat <- scenario_catalog()
      if (length(sc_cat) == 0) return()
      sc_idx <- as.integer(input$sel_scenario)
      if (is.na(sc_idx) || sc_idx < 1 || sc_idx > length(sc_cat)) {
        sc_idx <- 1L
      }
      overrides <- scenario_overrides()
      existing_sc <- overrides[[as.character(sc_idx)]] %||% sc_cat[[sc_idx]]
      sc_update <- update_data$scenario
      if (!is.null(existing_sc)) {
        sc_update$id <- existing_sc$id
        sc_update$label <- existing_sc$label
        sc_update$description <- existing_sc$description
        sc_update$zone_sf <- existing_sc$zone_sf
        sc_update$zone_maps <- existing_sc$zone_maps
        sc_update$zone_map <- existing_sc$zone_map
        sc_update$zone_src <- existing_sc$zone_src
        sc_update$abacus_data <- existing_sc$abacus_data
      }
      overrides[[as.character(sc_idx)]] <- sc_update
      scenario_overrides(overrides)
      if (!is.null(update_data$scenario) && !is.null(update_data$scenario$tpm)) {
        scenario_tpm_edits(update_data$scenario$tpm)
      } else if (!is.null(update_data$scenario)) {
        scenario_tpm_edits(NULL)
      }
      # Sync with v$scenario_list
      if (sc_idx <= length(v$scenario_list)) {
        v$scenario_list[[sc_idx]] <- list(
          label = sc_update$label %||% paste("Scenario", sc_idx),
          desc = sc_update$description %||% "",
          abacus_scenario = list(
            scenario = sc_update,
            projection = update_data$projection,
            emission = update_data$emission,
            baseline_emission = update_data$baseline_emission,
            baseline_projection = update_data$baseline_projection
          )
        )
      }
    }
  }, ignoreInit = TRUE)

  # Render Scenario Editor Widget
  output$widget_out <- render_scenario_editor({
    req(v$abacus_data)
    sc <- selected_scenario()
    iter <- as.integer(input$sel_iteration)
    if (is.na(iter)) iter <- 1L

    scenario_editor(
      data               = render_data(),
      scenario           = sc,
      selected_iteration = iter,
      selected_zone      = selected_zone(),
      selected_lc        = 0
    )
  })

  # Render Edited Items Table
  output$scenario_table <- renderReactable({
    req(v$abacus_data)
    scenario <- selected_scenario()
    req(scenario)
    tpm <- scenario_tpm_edits()
    if (is.null(tpm) || length(tpm) == 0) {
      tpm <- scenario$tpm
    }
    if (is.null(tpm) || length(tpm) == 0) {
      return(reactable(
        data.frame(info = "No edited scenario items"),
        columns = list(info = colDef(name = NULL))
      ))
    }
    if (!is.data.frame(tpm)) {
      tpm <- as.data.frame(tpm)
    }
    # Show only locked items
    if ("lock" %in% names(tpm)) {
      locked <- tpm$lock == TRUE | tpm$lock == "TRUE" | tpm$lock == 1
      tpm <- tpm[locked, , drop = FALSE]
    }
    if (nrow(tpm) == 0) {
      return(reactable(
        data.frame(info = "No edited scenario items"),
        columns = list(info = colDef(name = NULL))
      ))
    }
    display_cols <- list(
      period   = "Period",
      zone     = "Zone",
      lc1      = "From LC",
      lc2      = "To LC",
      description = "Description",
      area      = "Edited Area (ha)",
      def_area  = "Baseline Area (ha)",
      r         = "Edited Ratio",
      def_r     = "Baseline Ratio"
    )
    present <- intersect(names(tpm), names(display_cols))
    if (length(present) == 0) {
      return(reactable(
        data.frame(info = "No edited scenario items"),
        columns = list(info = colDef(name = NULL))
      ))
    }
    group_cols <- intersect(c("period", "zone"), names(tpm))
    if (length(group_cols) == 0) {
      area_cols  <- intersect(c("area", "def_area"), present)
      ratio_cols <- intersect(c("r", "def_r"), present)
      info_cols  <- setdiff(present, c(area_cols, ratio_cols, "description"))
      ordered_cols <- c(info_cols, area_cols, ratio_cols, "description")
      df <- tpm[, intersect(ordered_cols, names(tpm)), drop = FALSE]

      zone_col <- if ("zone" %in% names(df)) df[["zone"]] else rep("", nrow(df))
      lc1_col  <- if ("lc1" %in% names(df)) df[["lc1"]] else rep("", nrow(df))
      lc2_col  <- if ("lc2" %in% names(df)) df[["lc2"]] else rep("", nrow(df))
      df$.desc_key <- paste0(zone_col, "\001", lc1_col, "\001", lc2_col)

      descs <- description_store()
      df$description <- sapply(df$.desc_key, function(k) {
        v <- descs[[k]]
        if (is.null(v)) "" else v
      })
      desc_keys <- df$.desc_key
      df$.desc_key <- NULL
      df <- df[, ordered_cols, drop = FALSE]

      edit_zone_id <- if ("zone_id" %in% names(tpm)) tpm$zone_id else rep(0L, nrow(tpm))
      edit_iter_id <- if ("iteration" %in% names(tpm)) tpm$iteration else rep(1L, nrow(tpm))
      edit_lc1_id  <- if ("lc1_id" %in% names(tpm)) tpm$lc1_id else rep(0L, nrow(tpm))

      df$.edit_key <- paste0(edit_zone_id, "_", edit_iter_id, "_", edit_lc1_id)

      columns <- lapply(ordered_cols, function(col) {
        nm <- display_cols[[col]] %||% col
        fmt <- if (col %in% c("area", "def_area"))
          colFormat(digits = 0, separators = TRUE)
        else if (col %in% c("r", "def_r"))
          colFormat(digits = 2)
        else
          NULL
        align <- if (col %in% c("area", "def_area", "r", "def_r")) "right" else "left"
        if (col == "description") {
          colDef(
            name = nm,
            html = TRUE,
            align = "left",
            minWidth = 200,
            cell = function(value, index, colname) {
              key <- desc_keys[[index]]
              htmltools::tags$input(
                type = "text",
                value = value,
                `data-key` = key,
                class = "desc-input",
                style = "width: 100%; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px;",
                onchange = sprintf(
                  "Shiny.setInputValue('desc_edit', {key: '%s', value: this.value}, {priority: 'event'});",
                  key
                )
              )
            }
          )
        } else {
          args <- list(name = nm, align = align)
          if (col == "area") args$style <- list(color = "#dc3545", fontWeight = "600")
          if (!is.null(fmt)) args$format <- fmt
          do.call(colDef, args)
        }
      })
      names(columns) <- ordered_cols
      columns$.edit_key <- colDef(
        name = "",
        width = 70,
        sortable = FALSE,
        html = TRUE,
        cell = function(value, index) {
          parts <- strsplit(value, "_")[[1]]
          zone_id_val <- parts[[1]]
          iter_val    <- parts[[2]]
          lc1_val     <- parts[[3]]
          htmltools::tags$button(
            "Edit",
            class = "btn btn-sm btn-outline-primary",
            style = "padding: 2px 8px; font-size: 0.75rem;",
            onclick = sprintf(
              paste0(
                "Shiny.setInputValue('sel_zone', '%s', {priority: 'event'});",
                "Shiny.setInputValue('sel_iteration', '%s', {priority: 'event'});",
                "Shiny.setInputValue('tbl_edit_row', {zone_id: %s, iteration: %s, lc1_id: %s}, {priority: 'event'});"
              ),
              zone_id_val, iter_val, zone_id_val, iter_val, lc1_val
            )
          )
        }
      )
      reactable(df, columns = columns, fullWidth = TRUE, defaultPageSize = 10)
    } else {
      parent_df <- unique(tpm[, group_cols, drop = FALSE])
      rownames(parent_df) <- NULL
      parent_df$n_items <- sapply(seq_len(nrow(parent_df)), function(i) {
        match_rows <- rep(TRUE, nrow(tpm))
        for (gc in group_cols) {
          match_rows <- match_rows & (tpm[[gc]] == parent_df[[gc]][i])
        }
        sum(match_rows)
      })
      parent_cols <- list(
        period = colDef(name = "Period", width = 140),
        zone = colDef(name = "Zone"),
        n_items = colDef(name = "Transitions", align = "center", width = 120)
      )
      parent_cols <- parent_cols[names(parent_cols) %in% names(parent_df)]
      reactable(
        parent_df,
        columns = parent_cols,
        defaultExpanded = TRUE,
        highlight = TRUE,
        outlined = TRUE,
        fullWidth = TRUE,
        details = function(index) {
          match_rows <- rep(TRUE, nrow(tpm))
          for (gc in group_cols) {
            match_rows <- match_rows & (tpm[[gc]] == parent_df[[gc]][index])
          }
          sub_df <- tpm[match_rows, , drop = FALSE]
          zone_val <- if ("zone" %in% names(sub_df)) sub_df$zone else rep("", nrow(sub_df))
          sub_desc_keys <- paste0(zone_val, "\001", sub_df$lc1, "\001", sub_df$lc2)
          descs <- description_store()
          sub_df$description <- sapply(sub_desc_keys, function(k) {
            v <- descs[[k]]
            if (is.null(v)) "" else v
          })
          fmt_ar <- function(area_val, ratio_val) {
            mapply(function(a, r) {
              a_str <- if (!is.na(a)) format(round(a), big.mark = ",", scientific = FALSE) else "-"
              r_str <- if (!is.na(r)) paste0(round(r * 100, 1), "%") else "-"
              paste0(a_str, " (", r_str, ")")
            }, area_val, ratio_val, SIMPLIFY = TRUE)
          }
          has_edited   <- "area" %in% names(sub_df) && "r" %in% names(sub_df)
          has_baseline <- "def_area" %in% names(sub_df) && "def_r" %in% names(sub_df)
          if (has_edited) sub_df$edited <- fmt_ar(sub_df$area, sub_df$r)
          if (has_baseline) sub_df$baseline <- fmt_ar(sub_df$def_area, sub_df$def_r)
          child_order <- c("lc1", "lc2", if (has_edited) "edited", if (has_baseline) "baseline", "description")
          sub_cols <- intersect(child_order, names(sub_df))
          sub_edit_zone <- if ("zone_id" %in% names(sub_df)) sub_df$zone_id else rep(0L, nrow(sub_df))
          sub_edit_iter <- if ("iteration" %in% names(sub_df)) sub_df$iteration else rep(1L, nrow(sub_df))
          sub_edit_lc1  <- if ("lc1_id" %in% names(sub_df)) sub_df$lc1_id else rep(0L, nrow(sub_df))
          sub_edit_keys <- paste0(sub_edit_zone, "_", sub_edit_iter, "_", sub_edit_lc1)
          sub_df <- sub_df[, sub_cols, drop = FALSE]
          sub_df$.edit_key <- sub_edit_keys
          sub_cols_with_edit <- c(sub_cols, ".edit_key")
          child_columns <- list(
            lc1 = colDef(name = "From LC"),
            lc2 = colDef(name = "To LC"),
            edited = colDef(name = "Edited Area (ratio)", align = "right", html = FALSE, style = list(color = "#dc3545", fontWeight = "600")),
            baseline = colDef(name = "Baseline Area (ratio)", align = "right", html = FALSE),
            description = colDef(
              name = "Description",
              html = TRUE,
              minWidth = 180,
              cell = function(value, sub_index) {
                key <- sub_desc_keys[[sub_index]]
                htmltools::tags$input(
                  type = "text",
                  value = value,
                  `data-key` = key,
                  class = "desc-input",
                  style = "width: 100%; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px;",
                  onchange = sprintf(
                    "Shiny.setInputValue('desc_edit', {key: '%s', value: this.value}, {priority: 'event'});",
                    key
                  )
                )
              }
            ),
            .edit_key = colDef(
              name = "",
              width = 70,
              sortable = FALSE,
              html = TRUE,
              cell = function(value, sub_index) {
                parts <- strsplit(value, "_")[[1]]
                zone_id_val <- parts[[1]]
                iter_val    <- parts[[2]]
                lc1_val     <- parts[[3]]
                htmltools::tags$button(
                  "Edit",
                  class = "btn btn-sm btn-outline-primary",
                  style = "padding: 2px 8px; font-size: 0.75rem;",
                  onclick = sprintf(
                    paste0(
                      "Shiny.setInputValue('sel_zone', '%s', {priority: 'event'});",
                      "Shiny.setInputValue('sel_iteration', '%s', {priority: 'event'});",
                      "Shiny.setInputValue('tbl_edit_row', {zone_id: %s, iteration: %s, lc1_id: %s}, {priority: 'event'});"
                    ),
                    zone_id_val, iter_val, zone_id_val, iter_val, lc1_val
                  )
                )
              }
            )
          )
          child_columns <- child_columns[names(child_columns) %in% sub_cols_with_edit]
          htmltools::div(
            style = "padding: 10px 18px; background-color: #f8f9fa; border-top: 1px solid #e9ecef; border-bottom: 1px solid #e9ecef;",
            reactable(
              sub_df,
              columns = child_columns,
              outlined = TRUE,
              compact = TRUE,
              striped = TRUE,
              bordered = TRUE,
              fullWidth = TRUE
            )
          )
        }
      )
    }
  })

  ## Download scenario edited items as CSV
  output$download_scenario_edits_csv <- downloadHandler(
    filename = function() {
      paste0("scenario_edited_items_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(v$abacus_data)
      scenario <- selected_scenario()
      req(scenario)
      tpm <- scenario_tpm_edits()
      if (is.null(tpm) || length(tpm) == 0) {
        tpm <- scenario$tpm
      }
      if (!is.null(tpm) && is.data.frame(tpm)) {
        if ("lock" %in% names(tpm)) {
          locked <- tpm$lock == TRUE | tpm$lock == "TRUE" | tpm$lock == 1
          tpm <- tpm[locked, , drop = FALSE]
        }
        write.csv(tpm, file, row.names = FALSE, na = "")
      }
    }
  )

  ## Reset scenario edits
  observeEvent(input$reset_scenario_edits_btn, {
    req(v$abacus_data)
    scenario_tpm_edits(NULL)
    sc_idx <- as.integer(selected_scenario_index() %||% 1)
    if (!is.na(sc_idx)) {
      overrides <- scenario_overrides()
      overrides[[as.character(sc_idx)]] <- NULL
      scenario_overrides(overrides)
      if (sc_idx <= length(v$scenario_list) && !is.null(v$scenario_list[[sc_idx]])) {
        v$scenario_list[[sc_idx]]$abacus_scenario <- NULL
      }
    }
    description_store(list())
    showNotification("Scenario edits have been reset.", type = "message", duration = 3)
  })

  observeEvent(input$desc_edit, {
    req(input$desc_edit)
    store <- description_store()
    store[[input$desc_edit$key]] <- input$desc_edit$value
    description_store(store)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$tbl_edit_row, {
    req(input$tbl_edit_row)
    row <- input$tbl_edit_row
    updateSelectInput(session, "sel_zone", selected = as.character(row$zone_id))
    updateSelectInput(session, "sel_iteration", selected = as.character(row$iteration))
    nav_select("workspace_tabs", selected = "Scenario Editor", session = session)
    lc1_id <- row$lc1_id
    session$sendCustomMessage(
      type = "open_lc_edit_widget_out",
      message = list(lc1_id = lc1_id)
    )
  }, ignoreInit = TRUE)

  selected_zone_name <- reactive({
    req(v$abacus_data)
    zid <- selected_zone()
    zones <- v$abacus_data$zone
    if (!is.null(zones) && nrow(zones) > 0) {
      match_row <- zones[zones$zone_id == zid, ]
      if (nrow(match_row) > 0 && nzchar(match_row$label[1])) {
        return(match_row$label[1])
      }
    }
    if (zid == 0) "All Zones" else paste("Zone", zid)
  })

  output$area_plot_zone <- renderText({
    if (!has_scenario_zones()) return("")
    paste0("Zone: ", selected_zone_name())
  })

  area_plot_widget <- reactive({
    req(v$abacus_data)
    sc <- selected_scenario()
    iter <- as.integer(input$sel_iteration)
    if (is.na(iter)) iter <- 1L

    lc_area_chart(
      data               = render_data(),
      scenario           = sc,
      selected_iteration = iter,
      selected_zone      = selected_zone(),
      show_period_selection = TRUE
    )
  })

  output$area_plot <- render_lc_area_chart({
    area_plot_widget()
  })

  emission_plot_widget <- reactive({
    emissions <- emission_data()
    baseline_emissions <- baseline_emission_data()
    iteration_emission <- if (!is.null(emissions)) emissions$iteration_emission else NULL
    baseline_iteration_emission <- if (!is.null(baseline_emissions)) baseline_emissions$iteration_emission else NULL
    if (is.null(iteration_emission) ||
        is.null(baseline_iteration_emission) ||
        nrow(iteration_emission) == 0 ||
        nrow(baseline_iteration_emission) == 0) {
      return(
        plotly::plot_ly(type = "scatter", mode = "text") %>%
          plotly::add_annotations(
            text = "Emission data is loading...",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(color = "#666666")
          ) %>%
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }

    years <- iteration_year()
    iteration_emission$year <- years[iteration_emission$iteration + 1]
    baseline_iteration_emission$year <- years[baseline_iteration_emission$iteration + 1]

    # Normalize iteration 0: scenario emission at the reference period must equal
    # baseline (interventions cannot change history; JS may differ due to custom
    # carbon stocks in scenario.landcover).
    if (0 %in% iteration_emission$iteration && 0 %in% baseline_iteration_emission$iteration) {
      bl_em0 <- baseline_iteration_emission$emission[baseline_iteration_emission$iteration == 0]
      iteration_emission$emission[iteration_emission$iteration == 0] <- bl_em0[1]
    }

    ## The widget reports per-period emission. The deviation card and this plot
    ## work with the cumulative (running-total) value over the projection, so
    ## accumulate the periods here, ordered by iteration.
    cumulate <- function(df) {
      df <- df[order(df$iteration), , drop = FALSE]
      df$emission <- cumsum(df$emission)
      df
    }
    iteration_emission <- cumulate(iteration_emission)
    baseline_iteration_emission <- cumulate(baseline_iteration_emission)

    emission_val$bl_sum <- tail(baseline_iteration_emission$emission, 1) / 1e3
    emission_val$sc_sum <- tail(iteration_emission$emission, 1) / 1e3

    ## Line colours follow the Bootstrap theme variables ($danger, $warning),
    ## so the plot stays in sync with the app palette.
    col_baseline <- "#335C67"   
    col_scenario <- "#E09F3E"   
    fill_scenario <- grDevices::adjustcolor(col_scenario, alpha.f = 0.2)

    plotly::plot_ly() %>%
      plotly::add_trace(
        data = baseline_iteration_emission,
        x = ~ year,
        y = ~ emission / 1e3,
        name = "Baseline",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = col_baseline, width = 1),
        marker = list(color = col_baseline)
      ) %>%
      plotly::add_trace(
        data = iteration_emission,
        x = ~ year,
        y = ~ emission / 1e3,
        name = "Scenario",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = col_scenario, width = 1),
        fill = "tonexty",
        fillcolor = fill_scenario
      ) %>%
      plotly::layout(
        showlegend = FALSE,
        xaxis = list(
          visible = T,
          showgrid = F,
          title = "",
          tickmode = "array",
          tickvals = iteration_emission$year,
          ticktext = iteration_year_label(),
          showticklabels = FALSE,
          ticks = ""
        ),
        yaxis = list(
          visible = T,
          showgrid = TRUE,
          gridwidth = 1,
          zeroline = T,
          title = "",
          showticklabels = FALSE,
          ticks = ""
        ),
        hoverlabel = list(align = "left"),
        hovermode = "x unified",
        margin = list(t = 0, r = 0, l = 0, b = 0)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$emission_plot <- renderPlotly({
    emission_plot_widget()
  })

  ### CUMULATIVE PROJECTED EMISSIONS (ALL SCENARIOS) ###########
  scenarios_emission_summary_widget <- reactive({
    em_type <- input$projection_summary_emission_type %||% "net"
    if (!(em_type %in% c("net", "positive", "negative"))) em_type <- "net"

    baseline_emissions <- baseline_emission_data() %||% v$abacus_baseline$emission
    if (is.null(baseline_emissions) && !is.null(v$abacus_data)) {
      v$abacus_baseline <- tryCatch(calculate_abacus_emission(v$abacus_data), error = function(e) NULL)
      baseline_emissions <- if (!is.null(v$abacus_baseline)) v$abacus_baseline$emission else NULL
    }
    baseline_iter_df <- extract_iteration_emission(baseline_emissions, em_type)
    if (is.null(baseline_iter_df) || nrow(baseline_iter_df) == 0) {
      return(
        plotly::plot_ly(type = "scatter", mode = "text") %>%
          plotly::add_annotations(
            text = "Baseline emission data is loading...",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(color = "#666666", size = 14)
          ) %>%
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }

    # Robust baseline year and period step computation using as_iso_date
    d1_bl <- as_iso_date(if (!is.null(v$abacus_data) && !is.null(v$abacus_data$project)) v$abacus_data$project$date1 else NULL, v$map1_year %||% 2000L)
    d2_bl <- as_iso_date(if (!is.null(v$abacus_data) && !is.null(v$abacus_data$project)) v$abacus_data$project$date2 else NULL, v$map2_year %||% 2010L)
    bl_y_start <- as.integer(format(d1_bl, "%Y"))
    if (is.na(bl_y_start) || bl_y_start < 1000L) bl_y_start <- (v$map1_year %||% 2000L)
    bl_p_yr <- round(as.numeric(difftime(d2_bl, d1_bl, units = "days")) / 7 / 52.25)
    if (is.na(bl_p_yr) || bl_p_yr <= 0) bl_p_yr <- (v$period_year %||% 10L)
    if (is.na(bl_p_yr) || bl_p_yr <= 0) bl_p_yr <- 10L

    get_sc_years <- function(sc_item, sc_ab, n_it) {
      d1_raw <- if (!is.null(sc_ab) && !is.null(sc_ab$project)) sc_ab$project$date1 else (sc_item$map1_year %||% bl_y_start)
      d2_raw <- if (!is.null(sc_ab) && !is.null(sc_ab$project)) sc_ab$project$date2 else (sc_item$map2_year %||% (bl_y_start + bl_p_yr))
      d1 <- as_iso_date(d1_raw, bl_y_start)
      d2 <- as_iso_date(d2_raw, bl_y_start + bl_p_yr)
      y1 <- as.integer(format(d1, "%Y"))
      if (is.na(y1) || y1 < 1000L) y1 <- bl_y_start
      p_yr <- round(as.numeric(difftime(d2, d1, units = "days")) / 7 / 52.25)
      if (is.na(p_yr) || p_yr <= 0) p_yr <- bl_p_yr
      if (is.na(p_yr) || p_yr <= 0) p_yr <- 10L
      n_count <- suppressWarnings(as.integer(n_it))
      if (is.na(n_count) || n_count < 1L) n_count <- 4L
      y1 + (0:n_count) * p_yr
    }

    cumulate <- function(df, sc_years = NULL) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df <- df[order(df$iteration), , drop = FALSE]
      df$cum_emission_kt <- cumsum(df$emission) / 1e3
      max_it <- max(df$iteration)
      if (!is.null(sc_years) && length(sc_years) >= max_it + 1) {
        df$year <- sc_years[df$iteration + 1]
      } else {
        base_y <- if (!is.null(sc_years) && length(sc_years) > 0) sc_years[1] else bl_y_start
        p_step <- if (!is.null(sc_years) && length(sc_years) >= 2) sc_years[2] - sc_years[1] else bl_p_yr
        if (is.na(p_step) || p_step <= 0) p_step <- bl_p_yr
        df$year <- base_y + df$iteration * p_step
      }
      df
    }

    bl_years <- bl_y_start + (0:max(baseline_iter_df$iteration, 4L)) * bl_p_yr
    bl_cum <- cumulate(baseline_iter_df, bl_years)
    bl_end_years <- bl_cum$year + bl_p_yr
    bl_period_lbl <- paste0(bl_cum$year, "\u2013", bl_end_years)   # e.g. "2000–2010"

    sc_cat <- scenario_catalog()
    if (length(sc_cat) == 0 && !is.null(v$scenario_list) && length(v$scenario_list) > 0) {
      sc_cat <- v$scenario_list
    }

    p <- plotly::plot_ly()

    if (length(sc_cat) == 0) {
      col_baseline <- "#335C67"
      p <- p %>% plotly::add_trace(
        x = bl_cum$year,
        y = bl_cum$cum_emission_kt,
        name = "Baseline",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = col_baseline, width = 2.5, dash = "dash"),
        marker = list(color = col_baseline, size = 7),
        customdata = bl_period_lbl,
        hovertemplate = "<b>Baseline</b> [%{customdata}]: %{y:,.1f} kt CO\u2082e<extra></extra>"
      )
    } else {
      palette <- c("#E09F3E", "#2A9D8F", "#E76F51", "#264653", "#457B9D",
                   "#F4A261", "#9B5DE5", "#00BBF9", "#00F5D4", "#F15BB5",
                   "#6A994E", "#DDA15E")
      if (length(sc_cat) > length(palette)) {
        palette <- grDevices::colorRampPalette(palette)(length(sc_cat))
      }

      cur_sc_idx <- suppressWarnings(as.integer(selected_scenario_index()))

      all_sc_years_list <- list()
      all_sc_p_yr_list <- list()

      for (i in seq_along(sc_cat)) {
        sc_item <- (if (exists("scenario_overrides", mode = "function")) scenario_overrides()[[as.character(i)]] else NULL) %||% sc_cat[[i]]
        sc_label <- sc_item$label %||% paste("Scenario", sc_item$id %||% i)
        sc_col <- palette[((i - 1) %% length(palette)) + 1]

        # Resolve scenario abacus_data and iteration count
        sc_ab <- sc_item$abacus_data %||%
          (if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) v$scenario_list[[i]]$abacus_scenario$scenario$abacus_data else NULL) %||%
          v$abacus_data
        sc_n <- sc_item$n_iteration %||% 4L
        sc_years <- get_sc_years(sc_item, sc_ab, sc_n)

        # 1. Baseline emissions for this scenario (independent computation fallback)
        sc_base_obj <- NULL
        if (!is.na(cur_sc_idx) && cur_sc_idx == i && !is.null(baseline_emission_data())) {
          sc_base_obj <- baseline_emission_data()
        } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) &&
                   !is.null(v$scenario_list[[i]]$abacus_scenario$baseline_emission)) {
          sc_base_obj <- v$scenario_list[[i]]$abacus_scenario$baseline_emission
        } else if (!is.null(sc_item$baseline_emission)) {
          sc_base_obj <- sc_item$baseline_emission
        } else if (!is.null(sc_ab)) {
          sc_base_calc <- tryCatch(calculate_abacus_emission(sc_ab, n_iteration = sc_n), error = function(e) NULL)
          if (!is.null(sc_base_calc)) {
            sc_base_obj <- sc_base_calc
            if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) {
              if (is.null(v$scenario_list[[i]]$abacus_scenario)) {
                v$scenario_list[[i]]$abacus_scenario <- list(scenario = sc_item)
              }
              v$scenario_list[[i]]$abacus_scenario$baseline_emission <- sc_base_calc
            }
          }
        }
        sc_base_iter_df <- extract_iteration_emission(sc_base_obj, em_type)
        if (is.null(sc_base_iter_df)) {
          sc_base_iter_df <- baseline_iter_df
        }

        # 2. Projected emissions for this scenario (independent computation fallback)
        sc_em_obj <- NULL
        if (!is.na(cur_sc_idx) && cur_sc_idx == i && !is.null(emission_data())) {
          sc_em_obj <- emission_data()
        } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) &&
                   !is.null(v$scenario_list[[i]]$abacus_scenario$emission)) {
          sc_em_obj <- v$scenario_list[[i]]$abacus_scenario$emission
        } else if (!is.null(sc_item$emission)) {
          sc_em_obj <- sc_item$emission
        } else {
          sc_tpm <- sc_item$tpm %||%
            (if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) v$scenario_list[[i]]$abacus_scenario$scenario$tpm else NULL)
          if (!is.null(sc_tpm) && is.data.frame(sc_tpm) && nrow(sc_tpm) > 0 && !is.null(sc_ab)) {
            sc_proj_calc <- tryCatch(calculate_abacus_emission(sc_ab, scenario_tpm = sc_tpm, n_iteration = sc_n), error = function(e) NULL)
            if (!is.null(sc_proj_calc)) {
              sc_em_obj <- sc_proj_calc
              if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) {
                if (is.null(v$scenario_list[[i]]$abacus_scenario)) {
                  v$scenario_list[[i]]$abacus_scenario <- list(scenario = sc_item)
                }
                v$scenario_list[[i]]$abacus_scenario$emission <- sc_proj_calc
              }
            }
          }
        }
        sc_iter_df <- extract_iteration_emission(sc_em_obj, em_type)
        if (is.null(sc_iter_df)) {
          sc_iter_df <- sc_base_iter_df
        }

        # Normalize iteration 0 (historical reference period): scenario emission must
        # equal baseline emission at iteration 0 because interventions do not alter
        # the past.  JS may compute them differently when scenario.landcover carries
        # custom carbon stocks, so we force alignment here.
        if (!is.null(sc_iter_df) && !is.null(sc_base_iter_df) &&
            0 %in% sc_iter_df$iteration && 0 %in% sc_base_iter_df$iteration) {
          base_em0 <- sc_base_iter_df$emission[sc_base_iter_df$iteration == 0]
          sc_iter_df$emission[sc_iter_df$iteration == 0] <- base_em0[1]
        }

        sc_base_cum <- cumulate(sc_base_iter_df, sc_years)
        sc_cum <- cumulate(sc_iter_df, sc_years)

        fill_col <- grDevices::adjustcolor(sc_col, alpha.f = 0.18)

        # Period-range labels for hover (e.g. "2000–2010"), derived from the
        # scenario's own year step so they stay correct for custom periods.
        sc_p_yr <- if (!is.null(sc_base_cum) && nrow(sc_base_cum) >= 2) {
          sc_base_cum$year[2] - sc_base_cum$year[1]
        } else {
          bl_p_yr
        }
        all_sc_years_list[[i]] <- sc_years
        all_sc_p_yr_list[[i]]  <- sc_p_yr
        sc_base_period_lbl <- paste0(sc_base_cum$year, "\u2013", sc_base_cum$year + sc_p_yr)
        sc_period_lbl      <- paste0(sc_cum$year,      "\u2013", sc_cum$year      + sc_p_yr)

        # Baseline projection trace — rendered first so the fill references it
        p <- p %>% plotly::add_trace(
          x = sc_base_cum$year,
          y = sc_base_cum$cum_emission_kt,
          name = paste0(sc_label, " (Baseline)"),
          type = "scatter",
          mode = "lines+markers",
          line = list(color = sc_col, width = 2, dash = "dash"),
          marker = list(color = sc_col, size = 6, symbol = "circle-open"),
          customdata = sc_base_period_lbl,
          hovertemplate = paste0("<b>", sc_label, " (Baseline)</b> [%{customdata}]: %{y:,.1f} kt CO\u2082e<extra></extra>"),
          showlegend = TRUE
        )

        # Intervention projection trace — fill = "tonexty" shades between this
        # line and the preceding baseline trace for the same scenario.
        p <- p %>% plotly::add_trace(
          x = sc_cum$year,
          y = sc_cum$cum_emission_kt,
          name = sc_label,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = sc_col, width = 2.5),
          marker = list(color = sc_col, size = 7),
          fill = "tonexty",
          fillcolor = fill_col,
          customdata = sc_period_lbl,
          hovertemplate = paste0("<b>", sc_label, "</b> [%{customdata}]: %{y:,.1f} kt CO\u2082e<extra></extra>"),
          showlegend = TRUE
        )
      }
    }

    y_title <- switch(em_type,
      "positive" = "Cumulative Positive Emissions (kt CO\u2082e)",
      "negative" = "Cumulative Sequestration (kt CO\u2082e)",
      "Cumulative Net Emissions (kt CO\u2082e)"
    )

    # Compute full scope timeline across ALL scenarios and baseline
    all_trace_years <- sort(unique(c(
      bl_cum$year,
      if (length(all_sc_years_list) > 0) unlist(all_sc_years_list) else numeric(0)
    )))
    if (length(all_trace_years) == 0) all_trace_years <- bl_years

    min_start_year <- min(all_trace_years, na.rm = TRUE)
    max_start_year <- max(all_trace_years, na.rm = TRUE)
    span_yr <- max_start_year - min_start_year

    all_steps <- unique(c(bl_p_yr, if (length(all_sc_p_yr_list) > 0) unlist(all_sc_p_yr_list) else numeric(0)))
    all_steps <- all_steps[!is.na(all_steps) & all_steps > 0]

    # Primary scenario period step (modal value or default 10)
    primary_p_yr <- if (length(all_steps) > 0) {
      step_tab <- table(all_steps)
      as.numeric(names(step_tab)[which.max(step_tab)])
    } else 10L
    if (is.na(primary_p_yr) || primary_p_yr <= 0) primary_p_yr <- 10L

    # Choose clean, comfortable tick spacing to prevent crowded/overlapping labels (aim for 4-7 ticks)
    n_primary <- if (span_yr > 0) ceiling(span_yr / primary_p_yr) + 1 else 1
    if (n_primary >= 3 && n_primary <= 7) {
      tick_step <- primary_p_yr
    } else if (span_yr <= 15) {
      tick_step <- if (span_yr > 0 && span_yr <= 6) 2L else 5L
    } else if (span_yr <= 35) {
      tick_step <- if (primary_p_yr == 5L && (ceiling(span_yr / 5L) + 1) <= 7) 5L else 10L
    } else if (span_yr <= 70) {
      tick_step <- 10L
    } else {
      tick_step <- 20L
    }

    end_tick <- if (span_yr == 0) min_start_year + tick_step else ceiling(span_yr / tick_step) * tick_step + min_start_year
    chart_tick_vals <- seq(from = min_start_year, to = end_tick, by = tick_step)
    chart_tick_text <- as.character(chart_tick_vals + primary_p_yr)

    p %>%
      plotly::layout(
        xaxis = list(
          title = list(text = "Projection Year", font = list(size = 12)),
          tickmode = "array",
          tickvals = chart_tick_vals,
          ticktext = chart_tick_text,
          automargin = TRUE,
          showgrid = FALSE,
          zeroline = FALSE,
          showspikes = FALSE,
          hoverformat = " ",
          unifiedhovertitle = list(text = "")
        ),
        yaxis = list(
          title = list(text = y_title, font = list(size = 12)),
          showgrid = TRUE,
          gridcolor = "rgba(0,0,0,0.08)",
          gridwidth = 1,
          zeroline = TRUE,
          zerolinecolor = "rgba(0,0,0,0.15)"
        ),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#cccccc", font = list(size = 12)),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "rgba(0,0,0,0.1)",
          borderwidth = 1
        ),
        margin = list(t = 10, r = 15, l = 60, b = 45)
      ) %>%
      plotly::config(
        displayModeBar = "hover",
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          "sendDataToCloud", "lasso2d", "select2d", "autoScale2d"
        )
      )
  })

  output$scenarios_emission_summary_plot <- renderPlotly({
    scenarios_emission_summary_widget()
  })

  scenarios_emission_summary_data <- reactive({
    em_type <- input$projection_summary_emission_type %||% "net"
    if (!(em_type %in% c("net", "positive", "negative"))) em_type <- "net"

    baseline_emissions <- baseline_emission_data() %||% v$abacus_baseline$emission
    if (is.null(baseline_emissions) && !is.null(v$abacus_data)) {
      v$abacus_baseline <- tryCatch(calculate_abacus_emission(v$abacus_data), error = function(e) NULL)
      baseline_emissions <- if (!is.null(v$abacus_baseline)) v$abacus_baseline$emission else NULL
    }
    baseline_iter_df <- extract_iteration_emission(baseline_emissions, em_type)
    if (is.null(baseline_iter_df) || nrow(baseline_iter_df) == 0) return(NULL)

    d1_bl <- as_iso_date(if (!is.null(v$abacus_data) && !is.null(v$abacus_data$project)) v$abacus_data$project$date1 else NULL, v$map1_year %||% 2000L)
    d2_bl <- as_iso_date(if (!is.null(v$abacus_data) && !is.null(v$abacus_data$project)) v$abacus_data$project$date2 else NULL, v$map2_year %||% 2010L)
    bl_y_start <- as.integer(format(d1_bl, "%Y"))
    if (is.na(bl_y_start) || bl_y_start < 1000L) bl_y_start <- (v$map1_year %||% 2000L)
    bl_p_yr <- round(as.numeric(difftime(d2_bl, d1_bl, units = "days")) / 7 / 52.25)
    if (is.na(bl_p_yr) || bl_p_yr <= 0) bl_p_yr <- (v$period_year %||% 10L)
    if (is.na(bl_p_yr) || bl_p_yr <= 0) bl_p_yr <- 10L

    get_sc_years <- function(sc_item, sc_ab, n_it) {
      d1_raw <- if (!is.null(sc_ab) && !is.null(sc_ab$project)) sc_ab$project$date1 else (sc_item$map1_year %||% bl_y_start)
      d2_raw <- if (!is.null(sc_ab) && !is.null(sc_ab$project)) sc_ab$project$date2 else (sc_item$map2_year %||% (bl_y_start + bl_p_yr))
      d1 <- as_iso_date(d1_raw, bl_y_start)
      d2 <- as_iso_date(d2_raw, bl_y_start + bl_p_yr)
      y1 <- as.integer(format(d1, "%Y"))
      if (is.na(y1) || y1 < 1000L) y1 <- bl_y_start
      p_yr <- round(as.numeric(difftime(d2, d1, units = "days")) / 7 / 52.25)
      if (is.na(p_yr) || p_yr <= 0) p_yr <- bl_p_yr
      if (is.na(p_yr) || p_yr <= 0) p_yr <- 10L
      n_count <- suppressWarnings(as.integer(n_it))
      if (is.na(n_count) || n_count < 1L) n_count <- 4L
      y1 + (0:n_count) * p_yr
    }

    cumulate <- function(df, sc_years = NULL) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df <- df[order(df$iteration), , drop = FALSE]
      df$cum_emission_kt <- cumsum(df$emission) / 1e3
      max_it <- max(df$iteration)
      if (!is.null(sc_years) && length(sc_years) >= max_it + 1) {
        df$year <- sc_years[df$iteration + 1]
      } else {
        base_y <- if (!is.null(sc_years) && length(sc_years) > 0) sc_years[1] else bl_y_start
        p_step <- if (!is.null(sc_years) && length(sc_years) >= 2) sc_years[2] - sc_years[1] else bl_p_yr
        if (is.na(p_step) || p_step <= 0) p_step <- bl_p_yr
        df$year <- base_y + df$iteration * p_step
      }
      df
    }

    type_suffix <- switch(em_type,
      "positive" = "_Positive",
      "negative" = "_Negative",
      ""
    )
    ref_col <- paste0("Reference_Baseline", type_suffix, "_kt_CO2e")

    bl_years <- bl_y_start + (0:max(baseline_iter_df$iteration, 4L)) * bl_p_yr
    bl_cum <- cumulate(baseline_iter_df, bl_years)
    sc_cat <- scenario_catalog()
    if (length(sc_cat) == 0 && !is.null(v$scenario_list) && length(v$scenario_list) > 0) {
      sc_cat <- v$scenario_list
    }
    cur_sc_idx <- suppressWarnings(as.integer(selected_scenario_index()))

    # Determine maximum iteration across baseline and all scenarios
    sc_max_iters <- vapply(seq_along(sc_cat), function(i) {
      sc_item_i <- (if (exists("scenario_overrides", mode = "function")) scenario_overrides()[[as.character(i)]] else NULL) %||% sc_cat[[i]]
      as.integer(sc_item_i$n_iteration %||% 4L)
    }, integer(1))
    max_it <- max(c(bl_cum$iteration, sc_max_iters, 4L), na.rm = TRUE)
    if (is.na(max_it) || max_it < 1L) max_it <- 4L
    all_iters <- 0:max_it

    # Gather start years across all scenarios to anchor global scope
    all_sc_start_years <- unlist(lapply(seq_along(sc_cat), function(i) {
      sc_item_i <- (if (exists("scenario_overrides", mode = "function")) scenario_overrides()[[as.character(i)]] else NULL) %||% sc_cat[[i]]
      sc_ab_i <- sc_item_i$abacus_data %||%
        (if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) v$scenario_list[[i]]$abacus_scenario$scenario$abacus_data else NULL) %||%
        v$abacus_data
      sc_n_i <- sc_item_i$n_iteration %||% 4L
      get_sc_years(sc_item_i, sc_ab_i, sc_n_i)
    }))
    global_start_year <- min(c(bl_y_start, all_sc_start_years), na.rm = TRUE)
    if (is.na(global_start_year) || global_start_year < 1000L) global_start_year <- bl_y_start

    out_df <- data.frame(
      Iteration = all_iters,
      Year = global_start_year + all_iters * bl_p_yr,
      stringsAsFactors = FALSE
    )

    # Reference baseline column matched by iteration
    idx_ref <- match(out_df$Iteration, bl_cum$iteration)
    out_df[[ref_col]] <- ifelse(!is.na(idx_ref), round(bl_cum$cum_emission_kt[idx_ref], 2), NA_real_)

    for (i in seq_along(sc_cat)) {
      sc_item <- (if (exists("scenario_overrides", mode = "function")) scenario_overrides()[[as.character(i)]] else NULL) %||% sc_cat[[i]]
      sc_label <- sc_item$label %||% paste("Scenario", sc_item$id %||% i)
      safe_label <- gsub("[^a-zA-Z0-9_]+", "_", sc_label)
      col_base <- paste0(safe_label, "_Baseline", type_suffix, "_kt_CO2e")
      col_proj <- paste0(safe_label, type_suffix, "_kt_CO2e")
      col_base_idx <- paste0("S", i, "_", safe_label, "_Baseline", type_suffix, "_kt_CO2e")
      col_proj_idx <- paste0("S", i, "_", safe_label, type_suffix, "_kt_CO2e")

      sc_ab <- sc_item$abacus_data %||%
        (if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) v$scenario_list[[i]]$abacus_scenario$scenario$abacus_data else NULL) %||%
        v$abacus_data
      sc_n <- sc_item$n_iteration %||% 4L
      sc_years <- get_sc_years(sc_item, sc_ab, sc_n)

      # 1. Baseline emissions for this scenario
      sc_base_obj <- NULL
      if (!is.na(cur_sc_idx) && cur_sc_idx == i && !is.null(baseline_emission_data())) {
        sc_base_obj <- baseline_emission_data()
      } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) &&
                 !is.null(v$scenario_list[[i]]$abacus_scenario$baseline_emission)) {
        sc_base_obj <- v$scenario_list[[i]]$abacus_scenario$baseline_emission
      } else if (!is.null(sc_item$baseline_emission)) {
        sc_base_obj <- sc_item$baseline_emission
      } else if (!is.null(sc_ab)) {
        sc_base_calc <- tryCatch(calculate_abacus_emission(sc_ab, n_iteration = sc_n), error = function(e) NULL)
        if (!is.null(sc_base_calc)) {
          sc_base_obj <- sc_base_calc
          if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) {
            if (is.null(v$scenario_list[[i]]$abacus_scenario)) {
              v$scenario_list[[i]]$abacus_scenario <- list(scenario = sc_item)
            }
            v$scenario_list[[i]]$abacus_scenario$baseline_emission <- sc_base_calc
          }
        }
      }
      sc_base_iter_df <- extract_iteration_emission(sc_base_obj, em_type)
      if (is.null(sc_base_iter_df)) {
        sc_base_iter_df <- baseline_iter_df
      }

      # 2. Projected emissions for this scenario
      sc_em_obj <- NULL
      if (!is.na(cur_sc_idx) && cur_sc_idx == i && !is.null(emission_data())) {
        sc_em_obj <- emission_data()
      } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) &&
                 !is.null(v$scenario_list[[i]]$abacus_scenario$emission)) {
        sc_em_obj <- v$scenario_list[[i]]$abacus_scenario$emission
      } else if (!is.null(sc_item$emission)) {
        sc_em_obj <- sc_item$emission
      } else {
        sc_tpm <- sc_item$tpm %||%
          (if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) v$scenario_list[[i]]$abacus_scenario$scenario$tpm else NULL)
        if (!is.null(sc_tpm) && is.data.frame(sc_tpm) && nrow(sc_tpm) > 0 && !is.null(sc_ab)) {
          sc_proj_calc <- tryCatch(calculate_abacus_emission(sc_ab, scenario_tpm = sc_tpm, n_iteration = sc_n), error = function(e) NULL)
          if (!is.null(sc_proj_calc)) {
            sc_em_obj <- sc_proj_calc
            if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) {
              if (is.null(v$scenario_list[[i]]$abacus_scenario)) {
                v$scenario_list[[i]]$abacus_scenario <- list(scenario = sc_item)
              }
              v$scenario_list[[i]]$abacus_scenario$emission <- sc_proj_calc
            }
          }
        }
      }
      sc_iter_df <- extract_iteration_emission(sc_em_obj, em_type)
      if (is.null(sc_iter_df)) {
        sc_iter_df <- sc_base_iter_df
      }

      # Normalize iteration 0: historical period must be identical for baseline
      # and scenario traces (interventions cannot alter the past).
      if (!is.null(sc_iter_df) && !is.null(sc_base_iter_df) &&
          0 %in% sc_iter_df$iteration && 0 %in% sc_base_iter_df$iteration) {
        base_em0 <- sc_base_iter_df$emission[sc_base_iter_df$iteration == 0]
        sc_iter_df$emission[sc_iter_df$iteration == 0] <- base_em0[1]
      }

      sc_base_cum <- cumulate(sc_base_iter_df, sc_years)
      sc_cum <- cumulate(sc_iter_df, sc_years)

      # Match by Iteration — prevents row-count mismatches dropping scenarios!
      if (!is.null(sc_base_cum) && nrow(sc_base_cum) > 0) {
        idx_b <- match(out_df$Iteration, sc_base_cum$iteration)
        vals_b <- ifelse(!is.na(idx_b), round(sc_base_cum$cum_emission_kt[idx_b], 2), NA_real_)
        out_df[[col_base]] <- vals_b
        out_df[[col_base_idx]] <- vals_b
      }
      if (!is.null(sc_cum) && nrow(sc_cum) > 0) {
        idx_s <- match(out_df$Iteration, sc_cum$iteration)
        vals_s <- ifelse(!is.na(idx_s), round(sc_cum$cum_emission_kt[idx_s], 2), NA_real_)
        out_df[[col_proj]] <- vals_s
        out_df[[col_proj_idx]] <- vals_s
      }
    }
    out_df
  })

  output$download_scenarios_emission_summary_csv <- downloadHandler(
    filename = function() {
      em_type <- input$projection_summary_emission_type %||% "net"
      paste0("cumulative_", em_type, "_emissions_all_scenarios_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- scenarios_emission_summary_data()
      if (!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE, na = "")
      }
    }
  )

  output$download_scenarios_emission_summary_html <- downloadHandler(
    filename = function() {
      em_type <- input$projection_summary_emission_type %||% "net"
      paste0("cumulative_", em_type, "_emissions_all_scenarios_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      p <- scenarios_emission_summary_widget()
      if (!is.null(p)) {
        htmlwidgets::saveWidget(plotly::as_widget(p), file = file, selfcontained = TRUE)
      }
    }
  )

  output$projection_summary_emission_type_hint <- renderUI({
    em_type <- input$projection_summary_emission_type %||% "net"
    switch(em_type,
      "positive" = tags$span(icon("fire", class = "me-1 text-danger"),
                             "Gross emissions only (> 0) from deforestation, forest degradation, and peat drainage."),
      "negative" = tags$span(icon("seedling", class = "me-1 text-success"),
                             "Carbon sequestration and removals only (< 0) from forest regrowth and reforestation."),
      tags$span(icon("scale-balanced", class = "me-1 text-primary"),
                "Sum of all carbon fluxes (gross emissions and sequestration/removals).")
    )
  })

  output$scenarios_emission_summary_table_title <- renderText({
    em_type <- input$projection_summary_emission_type %||% "net"
    switch(em_type,
      "positive" = "Projected Gross (Positive) Emission Deviation Summary",
      "negative" = "Projected Sequestration (Negative Emission) Deviation Summary",
      "Projected Net Emission Deviation Summary"
    )
  })

  ## ---- Projection Summary Cards ------------------------------------------ ##
  ## Renders one deviation-style summary block per scenario (matching the style
  ## of the Projected Emission Deviation card in the Scenario Editor tab).
  output$scenarios_emission_summary_table <- renderUI({
    em_type <- input$projection_summary_emission_type %||% "net"
    if (!(em_type %in% c("net", "positive", "negative"))) em_type <- "net"
    type_suffix <- switch(em_type,
      "positive" = "_Positive",
      "negative" = "_Negative",
      ""
    )

    df <- scenarios_emission_summary_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    sc_cat <- scenario_catalog()
    if (length(sc_cat) == 0 && !is.null(v$scenario_list) && length(v$scenario_list) > 0) {
      sc_cat <- v$scenario_list
    }
    if (length(sc_cat) == 0) return(NULL)

    palette <- c("#E09F3E", "#2A9D8F", "#E76F51", "#264653", "#457B9D",
                 "#F4A261", "#9B5DE5", "#00BBF9", "#00F5D4", "#F15BB5",
                 "#6A994E", "#DDA15E")
    if (length(sc_cat) > length(palette)) {
      palette <- grDevices::colorRampPalette(palette)(length(sc_cat))
    }

    fmt_co2 <- function(val_kt) {
      if (is.null(val_kt) || !is.finite(val_kt)) return("0 kt")
      val_t <- val_kt * 1e3
      abs_val <- abs(val_t)
      if (abs_val >= 1e6) {
        paste0(format(round(val_t / 1e6, 2), big.mark = ","), " Mt")
      } else if (abs_val >= 1e3) {
        paste0(format(round(val_t / 1e3, 2), big.mark = ","), " kt")
      } else {
        paste0(format(round(val_t, 1), big.mark = ","), " t")
      }
    }

    split_unit <- function(val_kt) {
      txt <- fmt_co2(val_kt)
      m <- regmatches(txt, regexec("^(.*?)\\s+(Mt|kt|t)$", txt))[[1]]
      if (length(m) == 3) list(value = m[2], unit = m[3]) else list(value = txt, unit = NULL)
    }

    info_block <- function(icon_name, label, value, unit = NULL, value_size = "inherit") {
      tags$div(
        class = "d-flex flex-column align-items-start text-start",
        style = "min-width: 0;",
        tags$span(class = "text-muted fw-normal",
                  style = "white-space: nowrap; font-size: 0.72rem; line-height: 1.2; margin-bottom: 0.25rem;",
                  icon(icon_name, class = "me-1 info_icon_red"), label),
        tags$span(
          class = "fw-semibold",
          style = paste0("display: inline-flex; flex-wrap: wrap; justify-content: flex-start; column-gap: 0.25em; align-items: baseline; min-width: 0; line-height: 1.3; font-size: ", value_size, ";"),
          tags$span(style = "white-space: nowrap;", value),
          if (!is.null(unit)) tags$span(class = "fw-normal opacity-75",
                                         style = "white-space: nowrap; font-size: 0.78em;", unit)
        )
      )
    }

    detail_box <- function(..., bg = NULL) {
      tags$div(
        class = "d-flex flex-column flex-fill align-items-stretch",
        style = "min-width: 0;",
        tags$div(class = "border rounded px-2 py-1 h-100 d-flex flex-column justify-content-center",
                 style = paste0("min-height: 0;",
                                if (!is.null(bg)) paste0(" background-color: ", bg, "; border-color: ", bg, " !important;")),
                 ...)
      )
    }

    cards <- lapply(seq_along(sc_cat), function(i) {
      sc_item  <- (if (exists("scenario_overrides", mode = "function")) scenario_overrides()[[as.character(i)]] else NULL) %||% sc_cat[[i]]
      sc_label <- sc_item$label %||% paste("Scenario", sc_item$id %||% i)
      safe_lbl <- gsub("[^a-zA-Z0-9_]+", "_", sc_label)
      col_b    <- paste0(safe_lbl, "_Baseline", type_suffix, "_kt_CO2e")
      col_s    <- paste0(safe_lbl, type_suffix, "_kt_CO2e")
      col_b_idx <- paste0("S", i, "_", safe_lbl, "_Baseline", type_suffix, "_kt_CO2e")
      col_s_idx <- paste0("S", i, "_", safe_lbl, type_suffix, "_kt_CO2e")

      sc_col    <- palette[((i - 1L) %% length(palette)) + 1L]
      bg_sc     <- grDevices::adjustcolor(sc_col, alpha.f = 0.15)
      bg_bl     <- grDevices::adjustcolor("#335C67", alpha.f = 0.15)
      bg_period <- grDevices::adjustcolor("#791b1d", alpha.f = 0.15)

      # Resolve scenario abacus_data and iteration count
      sc_ab  <- sc_item$abacus_data %||%
        (if (!is.null(v$scenario_list) && i <= length(v$scenario_list))
          v$scenario_list[[i]]$abacus_scenario$scenario$abacus_data else NULL) %||%
        v$abacus_data
      sc_n   <- sc_item$n_iteration %||% 4L

      # Projection period string: first start year → last end year
      d1_raw <- if (!is.null(sc_ab) && !is.null(sc_ab$project)) sc_ab$project$date1 else (sc_item$map1_year %||% v$map1_year %||% 2000L)
      d2_raw <- if (!is.null(sc_ab) && !is.null(sc_ab$project)) sc_ab$project$date2 else (sc_item$map2_year %||% v$map2_year %||% 2010L)
      d1_sc <- as_iso_date(d1_raw, v$map1_year %||% 2000L)
      d2_sc <- as_iso_date(d2_raw, v$map2_year %||% 2010L)
      y1_sc <- as.integer(format(d1_sc, "%Y"))
      if (is.na(y1_sc) || y1_sc < 1000L) y1_sc <- (v$map1_year %||% 2000L)
      p_yr_sc <- round(as.numeric(difftime(d2_sc, d1_sc, units = "days")) / 7 / 52.25)
      if (is.na(p_yr_sc) || p_yr_sc <= 0) p_yr_sc <- (v$period_year %||% 10L)
      if (is.na(p_yr_sc) || p_yr_sc <= 0) p_yr_sc <- 10L
      sc_yrs <- y1_sc + (0:sc_n) * p_yr_sc

      period_str <- if (!is.null(sc_yrs) && length(sc_yrs) >= 2) {
        p_step    <- sc_yrs[2] - sc_yrs[1]
        y_start   <- sc_yrs[1]
        y_end     <- tail(sc_yrs, 1) + p_step
        total_yrs <- y_end - y_start
        format_period_with_length(y_start, y_end, total_yrs)
      } else "N/A"

      # Last (cumulative total) row values
      col_b_target <- if (col_b_idx %in% names(df)) col_b_idx else if (col_b %in% names(df)) col_b else NULL
      col_s_target <- if (col_s_idx %in% names(df)) col_s_idx else if (col_s %in% names(df)) col_s else NULL

      bl_vec <- if (!is.null(col_b_target)) na.omit(df[[col_b_target]]) else numeric(0)
      sc_vec <- if (!is.null(col_s_target)) na.omit(df[[col_s_target]]) else numeric(0)

      bl_kt <- if (length(bl_vec) > 0) tail(bl_vec, 1) else NA_real_
      sc_kt <- if (length(sc_vec) > 0) tail(sc_vec, 1) else NA_real_

      if (is.na(bl_kt)) {
        sc_base_obj <- if (!is.na(cur_sc_idx) && cur_sc_idx == i && !is.null(baseline_emission_data())) {
          baseline_emission_data()
        } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) &&
                   !is.null(v$scenario_list[[i]]$abacus_scenario$baseline_emission)) {
          v$scenario_list[[i]]$abacus_scenario$baseline_emission
        } else if (!is.null(sc_item$baseline_emission)) {
          sc_item$baseline_emission
        } else if (!is.null(sc_ab)) {
          tryCatch(calculate_abacus_emission(sc_ab, n_iteration = sc_n), error = function(e) NULL)
        } else NULL
        b_iter <- extract_iteration_emission(sc_base_obj, em_type)
        bl_kt <- if (!is.null(b_iter) && nrow(b_iter) > 0) sum(b_iter$emission, na.rm = TRUE) / 1e3 else 0
      }

      if (is.na(sc_kt)) {
        sc_em_obj <- if (!is.na(cur_sc_idx) && cur_sc_idx == i && !is.null(emission_data())) {
          emission_data()
        } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) &&
                   !is.null(v$scenario_list[[i]]$abacus_scenario$emission)) {
          v$scenario_list[[i]]$abacus_scenario$emission
        } else if (!is.null(sc_item$emission)) {
          sc_item$emission
        } else {
          sc_tpm <- sc_item$tpm %||%
            (if (!is.null(v$scenario_list) && i <= length(v$scenario_list)) v$scenario_list[[i]]$abacus_scenario$scenario$tpm else NULL)
          if (!is.null(sc_tpm) && is.data.frame(sc_tpm) && nrow(sc_tpm) > 0 && !is.null(sc_ab)) {
            tryCatch(calculate_abacus_emission(sc_ab, scenario_tpm = sc_tpm, n_iteration = sc_n), error = function(e) NULL)
          } else NULL
        }
        s_iter <- extract_iteration_emission(sc_em_obj, em_type)
        sc_kt <- if (!is.null(s_iter) && nrow(s_iter) > 0) sum(s_iter$emission, na.rm = TRUE) / 1e3 else bl_kt
      }
      diff_kt  <- sc_kt - bl_kt           # positive = more emissions / less sink than baseline
      margin   <- if (!is.na(bl_kt) && bl_kt != 0) diff_kt / abs(bl_kt) else NA_real_

      offset_pct <- if (!is.na(margin)) round(margin * 100, 1) else NA_real_
      offset_str <- if (!is.na(offset_pct)) {
        paste0(if (offset_pct > 0) "+" else "", offset_pct, "%")
      } else "N/A"

      if (!is.na(diff_kt) && diff_kt > 0) {
        bdr <- "border-danger"; bg_banner <- "bg-danger-subtle text-danger-emphasis"
      } else if (!is.na(diff_kt) && diff_kt < 0) {
        bdr <- "border-success"; bg_banner <- "bg-success-subtle text-success-emphasis"
      } else {
        bdr <- "border-secondary"; bg_banner <- "bg-light"
      }

      diff_str  <- paste0(if (!is.na(diff_kt) && diff_kt > 0) "+" else "", fmt_co2(diff_kt))
      bl_parts  <- split_unit(bl_kt)
      sc_parts  <- split_unit(sc_kt)

      # Mode badge
      em_badge <- switch(em_type,
        "positive" = tags$span(class = "badge bg-danger-subtle text-danger-emphasis border border-danger-subtle", style = "font-size: 0.7rem;", "Positive (Gross)"),
        "negative" = tags$span(class = "badge bg-success-subtle text-success-emphasis border border-success-subtle", style = "font-size: 0.7rem;", "Negative (Sink)"),
        tags$span(class = "badge bg-secondary-subtle text-secondary-emphasis border border-secondary-subtle", style = "font-size: 0.7rem;", "Net")
      )

      # Resolve full scenario object for settings info
      sc_obj <- if (!is.null(sc_item$abacus_scenario$scenario)) {
        sc_item$abacus_scenario$scenario
      } else if (!is.null(v$scenario_list) && i <= length(v$scenario_list) && !is.null(v$scenario_list[[i]]$abacus_scenario$scenario)) {
        v$scenario_list[[i]]$abacus_scenario$scenario
      } else {
        sc_item
      }

      # 1. Baseline period (reference period)
      p_base_str <- sc_obj$period %||% sc_item$period %||% ""
      if (!nzchar(p_base_str)) {
        m1 <- sc_obj$map1_year %||% sc_item$map1_year %||% v$map1_year
        m2 <- sc_obj$map2_year %||% sc_item$map2_year %||% v$map2_year
        if (!is.null(m1) && !is.null(m2)) {
          p_base_str <- format_period_with_length(m1, m2)
        } else {
          p_base_str <- "N/A"
        }
      } else {
        p_base_str <- format_period_str_with_length(p_base_str)
      }

      # 2. Zonation
      z_str <- sc_obj$zonation %||% sc_item$zonation %||% ""
      if (!nzchar(z_str)) {
        zm <- sc_obj$zone_maps %||% sc_obj$zone_map %||% sc_item$zone_maps %||% sc_item$zone_map
        valid_zm <- if (!is.null(zm)) setdiff(zm, "none") else character(0)
        if (length(valid_zm) > 0) {
          zc <- tryCatch(get_scenario_zone_choices(), error = function(e) character(0))
          lbls <- vapply(valid_zm, function(x) {
            idx_c <- which(unname(zc) == x)
            if (length(idx_c) > 0) names(zc)[idx_c[1]] else x
          }, character(1))
          z_str <- paste(lbls, collapse = ", ")
        } else {
          z_str <- "All zones (no zonation)"
        }
      }

      # 3. Iterations
      iters <- sc_obj$n_iteration %||% sc_item$n_iteration %||% sc_n %||% 4L
      iter_str <- paste0(iters, " periods")

      # 4. Flux emission sources
      fluxes <- sc_obj$flux_sources %||% sc_item$flux_sources
      flux_ui <- if (!is.null(fluxes) && length(fluxes) > 0) {
        tags$div(
          class = "d-flex flex-wrap gap-1 justify-content-end",
          lapply(fluxes, function(f) {
            tags$span(class = "badge bg-info-subtle text-info-emphasis border border-info-subtle", style = "font-size: 0.7rem;", f)
          })
        )
      } else {
        tags$span(class = "text-muted fst-italic", style = "font-size: 0.72rem;", "None")
      }

      # 5. Description
      desc <- trimws(sc_obj$description %||% sc_item$desc %||% sc_item$description %||% "")

      tags$div(
        class = "flex-fill",
        style = "min-width: 260px; max-width: 380px; flex: 1 1 280px;",
        tags$div(
          class = "border rounded p-2 h-100 d-flex flex-column gap-2",
          style = paste0("border-left: 4px solid ", sc_col, " !important;"),
          # Scenario label + mode badge
          tags$div(
            class = "d-flex align-items-center justify-content-between",
            tags$div(class = "fw-bold", style = paste0("color:", sc_col, "; font-size: 0.85rem;"),
                     icon("circle", class = "me-1"), sc_label),
            em_badge
          ),
          # Banner: % deviation + absolute diff
          tags$div(
            class = paste("px-2 py-2 rounded border", bdr, bg_banner),
            tags$div(
              class = "d-flex flex-column align-items-start text-start",
              tags$span(style = "font-size: 1.5rem; font-weight: 700; white-space: nowrap; line-height: 1.15;",
                        offset_str),
              tags$span(class = "fw-semibold text-muted",
                        style = "font-size: 0.85rem; white-space: nowrap;",
                        diff_str, tags$span(class = "fw-normal opacity-75", " CO\u2082e"))
            )
          ),
          # Detail boxes: Baseline | Scenario side by side
          tags$div(
            class = "d-flex flex-row align-items-stretch gap-2",
            detail_box(info_block("arrows-down-to-line", paste0("Baseline (", switch(em_type, "positive" = "Gross", "negative" = "Sink", "Net"), ")"),
                                  bl_parts$value, paste(bl_parts$unit, "CO\u2082e")),
                       bg = bg_bl),
            detail_box(info_block("arrows-split-up-and-left", paste0("Scenario (", switch(em_type, "positive" = "Gross", "negative" = "Sink", "Net"), ")"),
                                  sc_parts$value, paste(sc_parts$unit, "CO\u2082e")),
                       bg = bg_sc)
          ),
          # Period box below
          detail_box(
            info_block("calendar", "Projection Period", period_str, value_size = "1.05rem"),
            bg = bg_period
          ),
          # Scenario Settings Section
          tags$div(
            class = "border rounded p-2 mt-1",
            style = "background-color: #f8f9fa; font-size: 0.78rem;",
            tags$div(
              class = "fw-bold text-muted text-uppercase mb-2 d-flex align-items-center",
              style = "font-size: 0.7rem; letter-spacing: 0.5px;",
              icon("sliders", class = "me-1 text-secondary"), "Scenario Settings"
            ),
            tags$table(
              class = "table table-sm table-borderless mb-0",
              style = "font-size: 0.78rem; --bs-table-bg: transparent;",
              tags$tbody(
                tags$tr(
                  tags$td(class = "py-1 text-muted fw-semibold ps-0 text-nowrap", icon("calendar-days", class = "me-1 text-secondary"), "Base Period:"),
                  tags$td(class = "py-1 text-end fw-semibold pe-0 text-truncate", style = "max-width: 150px;", title = p_base_str, p_base_str)
                ),
                tags$tr(
                  tags$td(class = "py-1 text-muted fw-semibold ps-0 text-nowrap", icon("layer-group", class = "me-1 text-secondary"), "Zonation:"),
                  tags$td(class = "py-1 text-end fw-semibold pe-0 text-truncate", style = "max-width: 150px;", title = z_str, z_str)
                ),
                tags$tr(
                  tags$td(class = "py-1 text-muted fw-semibold ps-0 text-nowrap", icon("clock-rotate-left", class = "me-1 text-secondary"), "Iterations:"),
                  tags$td(class = "py-1 text-end fw-semibold pe-0", iter_str)
                ),
                tags$tr(
                  tags$td(class = "py-1 align-middle text-muted fw-semibold ps-0 text-nowrap", icon("wind", class = "me-1 text-secondary"), "Flux:"),
                  tags$td(class = "py-1 text-end pe-0 align-middle", flux_ui)
                )
              )
            ),
            if (nzchar(desc)) {
              tags$div(
                class = "border-top pt-1 mt-1 text-muted small",
                style = "font-size: 0.72rem; line-height: 1.25; max-height: 45px; overflow-y: auto;",
                tags$span(class = "fw-semibold", "Note: "), desc
              )
            }
          )
        )
      )
    })

    cards <- Filter(Negate(is.null), cards)
    if (length(cards) == 0) return(NULL)

    tags$div(
      class = "d-flex flex-row flex-wrap gap-3 align-items-stretch",
      style = "font-size: 0.9rem; line-height: 1.35;",
      tagList(cards)
    )
  })

  output$emission_offset <- renderText({
    bl_sum <- emission_val$bl_sum
    if (is.null(bl_sum) || is.na(bl_sum) || bl_sum == 0) return("")
    margin <- (emission_val$sc_sum - bl_sum) / abs(bl_sum)
    if (is.na(margin)) return("")
    t <- f_percent(margin)
    if (margin > 0) paste0("+", t) else t
  })

  ## Projected emission deviation, laid out like the carbon / flux / combined
  ## summary cards: hero banner + breakdown tiles + key details table.
  ##
  ## Status is derived once here so the full-width header (title + flag) and the
  ## summary box below it can never disagree.
  emission_deviation_status <- function() {
    bl_sum <- emission_val$bl_sum
    sc_sum <- emission_val$sc_sum

    valid <- !is.null(bl_sum) && !is.null(sc_sum) &&
      is.finite(bl_sum) && is.finite(sc_sum) && bl_sum != 0

    if (!valid) {
      return(list(valid = FALSE, bl_sum = bl_sum, sc_sum = sc_sum))
    }

    ## Totals are cumulative kt CO2e, as computed by the emission plot.
    base_t <- bl_sum * 1e3
    scen_t <- sc_sum * 1e3
    diff_t <- scen_t - base_t

    ## Sign is read from the Scenario \u2212 Baseline difference: a positive value
    ## is an increase in emissions relative to the baseline, so it is flagged
    ## red; a negative value is a decrease and is flagged green.
    if (isTRUE(diff_t > 0)) {
      status_label  <- "Increase"
      status_badge  <- "bg-danger"
      banner_border <- "border-danger"
      banner_bg     <- "bg-danger-subtle text-danger-emphasis"
    } else if (isTRUE(diff_t < 0)) {
      status_label  <- "Decrease"
      status_badge  <- "bg-success"
      banner_border <- "border-success"
      banner_bg     <- "bg-success-subtle text-success-emphasis"
    } else {
      status_label  <- ""          ## Neutral (zero deviation): no flag is shown
      status_badge  <- "bg-secondary"
      banner_border <- "border-secondary"
      banner_bg     <- "bg-light"
    }

    list(
      valid = TRUE,
      bl_sum = bl_sum, sc_sum = sc_sum,
      base_t = base_t, scen_t = scen_t, diff_t = diff_t,
      margin = if (base_t != 0) diff_t / abs(base_t) else 0,
      status_label = status_label, status_badge = status_badge,
      banner_border = banner_border, banner_bg = banner_bg
    )
  }

  ## Full-width header above the plot + summary row: title on the left, flag
  ## on the right, reaching the edges of the card.
  output$box_emission_header <- renderUI({
    st <- emission_deviation_status()
    ## No flag when the deviation is exactly zero (neutral) or while invalid.
    flags <- if (isTRUE(st$valid) && nzchar(st$status_label)) {
      tags$span(class = paste("badge", st$status_badge), st$status_label)
    } else {
      NULL
    }

    tags$div(
      class = "area-plot-heading mb-0 d-flex align-items-center gap-2",
      tags$div("Projected Emission Deviation", class = "area-plot-title"),
      flags
    )
  })

  output$box_emission_info <- renderUI({
    st <- emission_deviation_status()

    if (!isTRUE(st$valid)) {
      return(tags$div(
        class = "text-muted text-center py-4",
        icon("chart-line", class = "fa-2x mb-2 text-secondary"),
        tags$p(class = "small", "Add a scenario to compare its emissions against the baseline.")
      ))
    }

    bl_sum <- st$bl_sum
    sc_sum <- st$sc_sum

    fmt_co2 <- function(val_ton) {
      if (is.null(val_ton) || !is.finite(val_ton)) return("0 t")
      abs_val <- abs(val_ton)
      if (abs_val >= 1e6) {
        paste0(format(round(val_ton / 1e6, 2), big.mark = ","), " Mt")
      } else if (abs_val >= 1e3) {
        paste0(format(round(val_ton / 1e3, 2), big.mark = ","), " kt")
      } else {
        paste0(format(round(val_ton, 1), big.mark = ","), " t")
      }
    }

    base_t <- st$base_t
    scen_t <- st$scen_t
    diff_t <- st$diff_t
    margin <- st$margin
    banner_border <- st$banner_border
    banner_bg <- st$banner_bg

    period_str <- tryCatch(as.character(period_year_total()), error = function(e) "N/A")
    ## Deviation percentage relative to baseline.
    ## The sign is inline with the deviation value (Scenario - Baseline):
    ## positive difference = increase (+), negative difference = decrease (-).
    offset_pct <- round(margin * 100, 1)
    offset_str <- paste0(if (offset_pct > 0) "+" else "", offset_pct, "%")

    ## Split "1,234.5 kt" into the number and its unit, so the unit can be
    ## rendered separately (smaller / dimmer) while still being shown in full.
    split_unit <- function(val_ton) {
      txt <- fmt_co2(val_ton)
      m <- regmatches(txt, regexec("^(.*?)\\s+(Mt|kt|t)$", txt))[[1]]
      if (length(m) == 3) {
        list(value = m[2], unit = m[3])
      } else {
        list(value = txt, unit = NULL)
      }
    }

    ## A labelled value block (label on top, value below, both left-aligned),
    ## used inside the framed detail boxes. Baseline and Scenario show the complete unit (CO2e); Period
    ## is a year range and has no unit.
    info_block <- function(icon_name, icon_class, label, value, unit = NULL,
                           value_size = "inherit") {
      ## Icons are rendered in the card's red accent unless a caller overrides it.
      if (!nzchar(icon_class) || identical(icon_class, "text-secondary")) {
        icon_class <- "info_icon_red"
      }
      tags$div(
        class = "d-flex flex-column align-items-start text-start",
        style = "min-width: 0;",
        tags$span(class = "text-muted fw-normal",
                  style = "white-space: nowrap; font-size: 0.72rem; line-height: 1.2; margin-bottom: 0.25rem;",
                  icon(icon_name, class = paste("me-1", icon_class)), label),
        ## Value and unit are separate inline items inside a wrapping flex
        ## row, so on tight space the unit drops onto its own line below the
        ## value instead of overflowing the box.
        tags$span(
          class = "fw-semibold",
          style = paste0("display: inline-flex; flex-wrap: wrap; justify-content: flex-start; ",
                         "column-gap: 0.25em; ",
                         "align-items: baseline; min-width: 0; line-height: 1.3; font-size: ",
                         value_size, ";"),
          tags$span(style = "white-space: nowrap;", value),
          if (!is.null(unit)) {
            tags$span(class = "fw-normal opacity-75", style = "white-space: nowrap; font-size: 0.78em;", unit)
          })
      )
    }

    ## One framed box. In a row of flex-fill boxes the siblings split the width
    ## equally; the row is align-items-stretch, so their borders always line up.
    ## `bg` fills the box with a light tint of the given colour and draws its
    ## border in that colour (used to match the plot legend colours).
    detail_box <- function(..., bg = NULL) {
      tags$div(
        class = "d-flex flex-column flex-fill align-items-stretch",
        style = "min-width: 0;",
        tags$div(class = "border rounded px-2 py-1 h-100 d-flex flex-column justify-content-center",
                 style = paste0("min-height: 0;",
                                if (!is.null(bg)) paste0(" background-color: ", bg, "; border-color: ", bg, " !important;")),
                 ...)
      )
    }

    bl_parts <- split_unit(base_t)
    sc_parts <- split_unit(scen_t)

    ## Same colours as the projected-emission plot traces, tinted for the box
    ## background / border so the tiles read as the plot legend entries.
    col_baseline <- "#335C67"
    col_scenario <- "#E09F3E"
    bg_baseline <- grDevices::adjustcolor(col_baseline, alpha.f = 0.15)
    bg_scenario <- grDevices::adjustcolor(col_scenario, alpha.f = 0.15)
    ## Period tile uses a light tint of the app's period colour (#791b1d).
    bg_period <- grDevices::adjustcolor("#791b1d", alpha.f = 0.15)

    ## Title and status flag live in the full-width header above this row; the
    ## box itself only carries the deviation content. The column is full height
    ## with justify-content: space-between, so the coloured box sits at the top
    ## and the details table fills everything below it instead of leaving a gap.
    tags$div(
      class = "d-flex flex-column justify-content-between",
      style = "height: 100%; font-size: 0.9rem; line-height: 1.35;",
      tags$div(
        class = paste("px-2 py-2 rounded border", banner_border, banner_bg),
        ## Main label is the percentage deviation; the absolute value is shown
        ## underneath as the sub label.
        tags$div(
          class = "d-flex flex-column align-items-start text-start",
          tags$span(style = "font-size: 1.6rem; font-weight: 700; white-space: nowrap; line-height: 1.15;",
                    offset_str),
          tags$span(class = "fw-semibold text-muted",
                    style = "font-size: 0.9rem; white-space: nowrap; line-height: 1.3;",
                    paste0(if (diff_t > 0) "+" else "", fmt_co2(diff_t)),
                    tags$span(class = "fw-normal opacity-75", " CO\u2082e"))
        )
      ),
      ## Key details, outside (below) the coloured box, stretched to use the
      ## remaining height: Baseline and Scenario sit side by side as two equal
      ## boxes, with the Period underneath.
      tags$div(
        class = "flex-grow-1 d-flex flex-column justify-content-between gap-1",
        style = "min-height: 0; padding-top: 6px; font-size: 0.9rem;",
        tags$div(
          class = "d-flex flex-row align-items-stretch gap-2 flex-grow-1",
          style = "min-height: 0;",
          detail_box(
            info_block("arrows-down-to-line", "info_icon_red", "Baseline",
                       bl_parts$value, paste(bl_parts$unit, "CO\u2082e")),
            bg = bg_baseline
          ),
          detail_box(
            info_block("arrows-split-up-and-left", "info_icon_red", "Scenario",
                       sc_parts$value, paste(sc_parts$unit, "CO\u2082e")),
            bg = bg_scenario
          )
        ),
        detail_box(
          info_block("calendar", "info_icon_red", "Period", period_str,
                     value_size = "0.82rem"),
          bg = bg_period
        )
      )
    )
  })

  ### INPUT OUTPUT FILE #############################

  observeEvent(input$btn_options, {
    showModal(modalDialog(
      title = tagList(icon("gear"), " Project Options & Data"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        class = "d-flex flex-column gap-3 py-1",
        tags$div(
          class = "card p-3 border shadow-sm",
          tags$div(
            class = "d-flex align-items-center gap-2 mb-1 fw-bold text-primary",
            icon("upload"), " Upload Project Archive (.zip)"
          ),
          tags$p(
            class = "text-muted small mb-2",
            "Restore all land cover maps, zonation overlays, carbon stocks, emissions, and scenario projections from a previously exported REDD-Abacus .zip archive."
          ),
          fileInput(
            "upload_parameter",
            label = NULL,
            accept = ".zip",
            width = "100%",
            placeholder = "Select .zip archive"
          )
        ),
        tags$div(
          class = "card p-3 border shadow-sm",
          tags$div(
            class = "d-flex align-items-center gap-2 mb-1 fw-bold text-success",
            icon("download"), " Download Project Archive (.zip)"
          ),
          tags$p(
            class = "text-muted small mb-2",
            "Download all current project data, uploaded maps, parameter tables, and projections into a standalone .zip package."
          ),
          uiOutput("download_project_btn_ui")
        )
      )
    ))
  })

  observeEvent(input$upload_params, {
    showModal(modalDialog(
      title = tagList(icon("upload"), " Upload Project Data & Parameters"),
      easyClose = TRUE,
      footer = modalButton("Cancel"),
      fileInput(
        "upload_parameter",
        "Select REDD-Abacus project archive (.zip)",
        accept = ".zip",
        width = "100%",
        placeholder = "No file chosen"
      ),
      tags$div(
        class = "text-muted small mt-2",
        "Upload a previously saved REDD-Abacus project bundle (.zip) to restore all maps, tables, carbon stocks, emissions, and scenario projections."
      )
    ))
  })

  observeEvent(input$import_abacus_1, {
    showModal(modalDialog(
      title = tagList(icon("file-import"), " Import REDD-Abacus-1 Project"),
      easyClose = TRUE,
      footer = modalButton("Cancel"),
      fileInput(
        "abacus_file",
        "Load REDD-Abacus-1 project file (.car)",
        accept = ".car",
        width = "100%"
      )
    ))
  })

  config_data <- c("title", "description", "map1_year", "map2_year", "n_iteration")
  config_file <- "config.yaml"

  to_memory_stars <- function(s) {
    if (is.null(s)) return(NULL)
    if (inherits(s, "stars_proxy")) {
      tryCatch(stars::st_as_stars(s), error = function(e) s)
    } else {
      s
    }
  }

  create_zip_file <- function(zipfile, source_dir) {
    zipfile <- normalizePath(zipfile, winslash = '/', mustWork = FALSE)
    source_dir <- normalizePath(source_dir, winslash = '/', mustWork = TRUE)
    if (file.exists(zipfile)) unlink(zipfile)

    # 1. Try Windows built-in tar
    tar_bin <- Sys.which('tar')
    if (nzchar(tar_bin)) {
      cmd <- paste('tar -a -c -f', shQuote(zipfile), '-C', shQuote(source_dir), '.')
      res <- tryCatch(system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE), error = function(e) 1)
      if (res == 0 && file.exists(zipfile) && file.size(zipfile) > 0) return(TRUE)
    }

    # 2. Try PowerShell Compress-Archive
    ps_cmd <- sprintf(
      'powershell -NoProfile -Command "Compress-Archive -Path \'%s/*\' -DestinationPath \'%s\' -Force"',
      gsub('/', '\\\\', source_dir),
      gsub('/', '\\\\', zipfile)
    )
    res <- tryCatch(system(ps_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE), error = function(e) 1)
    if (res == 0 && file.exists(zipfile) && file.size(zipfile) > 0) return(TRUE)

    # 3. Fallback to utils::zip
    old_wd <- getwd()
    setwd(source_dir)
    on.exit(setwd(old_wd), add = TRUE)
    files_to_zip <- list.files('.', all.files = FALSE, recursive = TRUE)
    res <- tryCatch(utils::zip(zipfile, files = files_to_zip), error = function(e) 1)
    file.exists(zipfile) && file.size(zipfile) > 0
  }

  ## Reactive download button: disabled when the project has no uploaded maps.
  output$download_project_btn_ui <- renderUI({
    has_maps <- !is.null(v$map_catalog) && length(v$map_catalog) > 0
    if (has_maps) {
      downloadButton(
        "download_all_data",
        label = "Download data & parameters (.zip)",
        class = "btn btn-success w-100",
        icon  = icon("download")
      )
    } else {
      tagList(
        tags$button(
          class    = "btn btn-success w-100 disabled",
          type     = "button",
          disabled = NA,
          icon("download"), " Download data & parameters (.zip)"
        ),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          icon("circle-info", class = "me-1"),
          "Upload land cover maps first to enable the download."
        )
      )
    }
  })

  download_all_data_handler <- downloadHandler(
    filename = function() {
      paste0("redd_abacus_project_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      export_dir <- file.path(tempdir(), paste0("export_", as.integer(Sys.time())))
      dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)

      # 1. Prepare map catalog with in-memory rasters
      map_cat <- isolate(v$map_catalog)
      if (!is.null(map_cat) && length(map_cat) > 0) {
        for (k in names(map_cat)) {
          if (!is.null(map_cat[[k]]$stars)) {
            map_cat[[k]]$stars <- to_memory_stars(map_cat[[k]]$stars)
          }
        }
      }

      # Prepare scenario catalog and overrides without duplicate raw raster copies
      sc_cat_raw <- isolate(scenario_catalog())
      sc_cat_clean <- if (!is.null(sc_cat_raw) && length(sc_cat_raw) > 0) {
        lapply(sc_cat_raw, function(sc) {
          sc$map1_stars  <- NULL
          sc$map2_stars  <- NULL
          sc$mapz_stars  <- NULL
          sc$zone_layers <- NULL
          sc
        })
      } else {
        list()
      }

      sc_ov_raw <- isolate(scenario_overrides())
      sc_ov_clean <- if (!is.null(sc_ov_raw) && length(sc_ov_raw) > 0) {
        lapply(sc_ov_raw, function(sc) {
          sc$map1_stars  <- NULL
          sc$map2_stars  <- NULL
          sc$mapz_stars  <- NULL
          sc$zone_layers <- NULL
          sc
        })
      } else {
        list()
      }

      # 2. Assemble complete project state object
      proj_state <- list(
        format                  = "REDD_ABACUS_PROJECT",
        version                 = "2.0",
        created_at              = Sys.time(),
        title                   = v$title %||% "REDD-Abacus Project",
        description             = v$description %||% "",
        n_iteration             = v$n_iteration %||% 4L,
        map1_year               = v$map1_year,
        map2_year               = v$map2_year,
        map1_date               = v$map1_date,
        map2_date               = v$map2_date,
        period_year             = v$period_year,
        lc_palette_set          = v$lc_palette_set,
        geo_location            = v$geo_location,
        study_area_boundary     = v$study_area_boundary,
        # Tables
        lc_list_df              = v$lc_list_df,
        cstock_list             = v$cstock_list,
        npv_list                = v$npv_list,
        zone_list_df            = v$zone_list_df,
        zone_legends            = v$zone_legends,
        zone_palette_sets       = v$zone_palette_sets,
        other_emission_df       = v$other_emission_df,
        lc_changes_df           = v$lc_changes_df,
        # Maps (IDs stored to re-link stars from map_catalog without duplicate arrays)
        map_catalog             = map_cat,
        next_map_id             = v$next_map_id,
        map1_id                 = isolate(input$analysis_map1),
        map2_id                 = isolate(input$analysis_map2),
        mapz_id                 = isolate(input$analysis_zone_map),
        map1_stars              = NULL,
        map2_stars              = NULL,
        mapz_stars              = NULL,
        mapz_shp_file           = v$mapz_shp_file,
        # Projection & Scenarios
        abacus_data             = v$abacus_data,
        abacus_baseline         = v$abacus_baseline,
        scenario_list           = v$scenario_list,
        scenario_catalog        = sc_cat_clean,
        selected_scenario_index = isolate(selected_scenario_index()),
        scenario_overrides      = sc_ov_clean,
        scenario_tpm_edits      = isolate(scenario_tpm_edits()),
        description_store       = isolate(description_store()),
        emission_data           = isolate(emission_data()),
        baseline_emission_data  = isolate(baseline_emission_data())
      )

      # Save serialized project file
      saveRDS(proj_state, file = file.path(export_dir, "project.rds"))

      # 3. Export human-readable CSVs and config.yaml
      tryCatch({
        cd <- list(
          title       = proj_state$title,
          description = proj_state$description,
          map1_year   = proj_state$map1_year %||% "",
          map2_year   = proj_state$map2_year %||% "",
          n_iteration = proj_state$n_iteration %||% 4L
        )
        if (!is.null(proj_state$scenario_catalog) && length(proj_state$scenario_catalog) > 0) {
          cd$scenario <- lapply(seq_along(proj_state$scenario_catalog), function(i) {
            sc <- proj_state$scenario_catalog[[i]]
            list(id = i, label = sc$label %||% paste("Scenario", i), desc = sc$description %||% "")
          })
        }
        write_yaml(cd, file.path(export_dir, config_file))
      }, error = function(e) NULL)

      if (!is.null(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
        write.csv(v$lc_list_df, file.path(export_dir, "landcover.csv"), row.names = FALSE, na = "")
      }
      if (!is.null(v$cstock_list) && nrow(v$cstock_list) > 0) {
        write.csv(v$cstock_list, file.path(export_dir, "carbonstock.csv"), row.names = FALSE, na = "")
      }
      if (!is.null(v$npv_list) && nrow(v$npv_list) > 0) {
        write.csv(v$npv_list, file.path(export_dir, "npv.csv"), row.names = FALSE, na = "")
      }
      if (!is.null(v$zone_list_df) && nrow(v$zone_list_df) > 0) {
        write.csv(v$zone_list_df, file.path(export_dir, "zonation.csv"), row.names = FALSE, na = "")
      }
      if (!is.null(v$other_emission_df) && nrow(v$other_emission_df) > 0) {
        write.csv(v$other_emission_df, file.path(export_dir, "other_emission.csv"), row.names = FALSE, na = "")
      }
      if (!is.null(v$lc_changes_df) && nrow(v$lc_changes_df) > 0) {
        write.csv(v$lc_changes_df, file.path(export_dir, "lc_changes.csv"), row.names = FALSE, na = "")
      }

      # Export scenarios CSVs
      sc_cat <- isolate(scenario_catalog())
      if (!is.null(sc_cat) && length(sc_cat) > 0) {
        for (i in seq_along(sc_cat)) {
          sc <- sc_cat[[i]]
          if (!is.null(sc$tpm) && is.data.frame(sc$tpm)) {
            write.csv(sc$tpm, file.path(export_dir, paste0("scenario_modif_", i, ".csv")), row.names = FALSE, na = "")
          }
          if (!is.null(sc$baseline_tpm) && is.data.frame(sc$baseline_tpm)) {
            write.csv(sc$baseline_tpm, file.path(export_dir, paste0("scenario_tpm_", i, ".csv")), row.names = FALSE, na = "")
          }
          if (!is.null(sc$baseline_area) && is.data.frame(sc$baseline_area)) {
            write.csv(sc$baseline_area, file.path(export_dir, paste0("scenario_area_", i, ".csv")), row.names = FALSE, na = "")
          }
        }
      }

      # 4. Create ZIP archive
      ok <- create_zip_file(file, export_dir)
      if (!ok) {
        showNotification("Failed to compress project files.", type = "error")
      }
    },
    contentType = "application/zip"
  )

  output$download_all_data <- download_all_data_handler
  output$download_params   <- download_all_data_handler

  observeEvent(input$upload_parameter, {
    req(input$upload_parameter)
    dpath <- input$upload_parameter$datapath
    upload_parameter(dpath)
    removeModal()
  })

  show_alert_file_error <- function(file_error) {
    showNotification(paste("File error! Or it was not a", file_error, "file!"),
                     type = "error")
  }

  upload_parameter <- function(dpath) {
    file_list <- NULL
    try({ file_list <- unzip(dpath, list = TRUE) }, silent = TRUE)
    if (is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }

    withProgress(message = 'Uploading project data', value = 0, {
      incProgress(0.1, detail = "Extracting archive...")
      data_dir <- file.path(tempdir(), paste0("upload_", as.integer(Sys.time())))
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

      utils::unzip(dpath, exdir = data_dir)

      rds_path <- file.path(data_dir, "project.rds")
      if (file.exists(rds_path)) {
        incProgress(0.3, detail = "Restoring complete project state...")
        proj_state <- tryCatch(readRDS(rds_path), error = function(e) NULL)
        if (!is.null(proj_state) && is.list(proj_state)) {
          # 1. Reset current scenario
          reset_scenario()

          # 2. Restore basic metadata
          v$title       <- proj_state$title %||% "REDD-Abacus Project"
          v$description <- proj_state$description %||% ""
          v$n_iteration <- proj_state$n_iteration %||% 4L
          v$map1_year   <- proj_state$map1_year
          v$map2_year   <- proj_state$map2_year
          v$map1_date   <- proj_state$map1_date
          v$map2_date   <- proj_state$map2_date
          v$period_year <- proj_state$period_year
          v$lc_palette_set <- proj_state$lc_palette_set %||% "gyr"
          v$geo_location <- proj_state$geo_location
          v$study_area_boundary <- proj_state$study_area_boundary

          # 3. Restore tables
          v$lc_list_df        <- proj_state$lc_list_df
          v$cstock_list       <- proj_state$cstock_list
          v$npv_list          <- proj_state$npv_list
          if (is.null(v$npv_list) || nrow(v$npv_list) == 0) {
            npv_f <- file.path(data_dir, "npv.csv")
            if (!file.exists(npv_f)) npv_f <- file.path(data_dir, "npv_data.csv")
            if (file.exists(npv_f)) {
              tryCatch({
                df_npv <- read.csv(npv_f, stringsAsFactors = FALSE)
                if (all(c("lc_id", "npv") %in% colnames(df_npv))) {
                  v$npv_list <- df_npv[c("lc_id", "npv")]
                }
              }, error = function(e) NULL)
            }
          }
          v$zone_list_df      <- proj_state$zone_list_df
          v$zone_legends      <- proj_state$zone_legends %||% list()
          v$zone_palette_sets <- proj_state$zone_palette_sets %||% list()
          v$other_emission_df <- proj_state$other_emission_df
          v$lc_changes_df     <- proj_state$lc_changes_df

          # Palettes
          if (!is.null(v$lc_list_df) && nrow(v$lc_list_df) > 0) {
            lc_legend_pal <<- colorFactor(v$lc_list_df$color, v$lc_list_df$lc_id, na.color = NA)
          }
          if (!is.null(v$zone_list_df) && nrow(v$zone_list_df) > 0) {
            zone_legend_pal <<- colorFactor(v$zone_list_df$color, v$zone_list_df$zone_id, na.color = NA)
          }

          # 4. Restore Map Catalog & UI cards
          incProgress(0.5, detail = "Restoring maps & cards...")
          removeUI(selector = "#uploaded_cards_container > *", multiple = TRUE, immediate = TRUE)

          if (!is.null(proj_state$map_catalog) && length(proj_state$map_catalog) > 0) {
            for (rec_id in names(proj_state$map_catalog)) {
              rec <- proj_state$map_catalog[[rec_id]]
              card_ui <- build_map_card_ui(rec)
              insertUI(
                selector = "#uploaded_cards_container",
                where = "beforeEnd",
                ui = card_ui,
                immediate = TRUE
              )
              register_map_remove_handler(rec_id)
              register_map_preview(rec_id)
              if (identical(rec$type, "Zonation map")) {
                register_zone_map_handlers(rec_id)
              }
            }
          }
          v$map_catalog <- proj_state$map_catalog %||% list()
          v$next_map_id <- proj_state$next_map_id %||% (length(v$map_catalog) + 1L)
          v$map_catalog_version <- (v$map_catalog_version %||% 0L) + 1L

          # 5. Restore selected maps (re-link from map_catalog or fall back to stored raster)
          m1_id <- proj_state$map1_id
          if (!is.null(m1_id) && nzchar(m1_id) && !is.null(v$map_catalog[[m1_id]])) {
            v$map1_stars <- v$map_catalog[[m1_id]]$stars
          } else {
            v$map1_stars <- proj_state$map1_stars
          }

          m2_id <- proj_state$map2_id
          if (!is.null(m2_id) && nzchar(m2_id) && !is.null(v$map_catalog[[m2_id]])) {
            v$map2_stars <- v$map_catalog[[m2_id]]$stars
          } else {
            v$map2_stars <- proj_state$map2_stars
          }

          mz_id <- proj_state$mapz_id
          if (!is.null(mz_id) && nzchar(mz_id) && !is.null(v$map_catalog[[mz_id]])) {
            v$mapz_stars <- v$map_catalog[[mz_id]]$stars
          } else {
            v$mapz_stars <- proj_state$mapz_stars
          }
          v$mapz_shp_file <- proj_state$mapz_shp_file

          # 6. Restore Abacus & Scenarios
          incProgress(0.8, detail = "Restoring projection & scenarios...")
          v$abacus_data     <- proj_state$abacus_data
          v$abacus_baseline <- proj_state$abacus_baseline
          v$scenario_list   <- proj_state$scenario_list %||% list()

          # Re-link pointers to map_catalog for scenarios to avoid memory array duplication
          relink_scenario_maps <- function(sc) {
            if (is.null(sc$map1_stars) && !is.null(sc$map1_id) && !is.null(v$map_catalog[[sc$map1_id]])) {
              sc$map1_stars <- v$map_catalog[[sc$map1_id]]$stars
            }
            if (is.null(sc$map2_stars) && !is.null(sc$map2_id) && !is.null(v$map_catalog[[sc$map2_id]])) {
              sc$map2_stars <- v$map_catalog[[sc$map2_id]]$stars
            }
            if (is.null(sc$mapz_stars) && !is.null(sc$zone_map) && sc$zone_map != "none" && !is.null(v$map_catalog[[sc$zone_map]])) {
              sc$mapz_stars <- v$map_catalog[[sc$zone_map]]$stars
            }
            if (is.null(sc$zone_layers) && !is.null(sc$zone_maps) && length(sc$zone_maps) > 0) {
              layers <- lapply(sc$zone_maps, function(zid) {
                if (!is.null(v$map_catalog[[zid]])) v$map_catalog[[zid]]$stars else NULL
              })
              sc$zone_layers <- Filter(Negate(is.null), layers)
            }
            sc
          }

          sc_cat <- proj_state$scenario_catalog %||% list()
          if (length(sc_cat) > 0) {
            sc_cat <- lapply(sc_cat, relink_scenario_maps)
          }
          scenario_catalog(sc_cat)

          raw_sel_idx <- proj_state$selected_scenario_index
          sel_idx <- if (!is.null(raw_sel_idx) && length(raw_sel_idx) > 0 && nzchar(as.character(raw_sel_idx[1])) && as.character(raw_sel_idx[1]) %in% as.character(seq_along(sc_cat))) {
            as.character(raw_sel_idx[1])
          } else if (length(sc_cat) > 0) {
            "1"
          } else {
            ""
          }
          selected_scenario_index(sel_idx)

          sc_ov <- proj_state$scenario_overrides %||% list()
          if (length(sc_ov) > 0) {
            sc_ov <- lapply(sc_ov, relink_scenario_maps)
          }
          scenario_overrides(sc_ov)

          scenario_tpm_edits(proj_state$scenario_tpm_edits)
          description_store(proj_state$description_store %||% list())
          emission_data(proj_state$emission_data)
          baseline_emission_data(proj_state$baseline_emission_data)

          # 7. Update UI controls (only re-fetch geographic location if not already restored)
          if (!is.null(v$study_area_boundary) && is.null(v$geo_location)) {
            update_study_area_location(v$study_area_boundary)
          }
          if (length(sc_cat) > 0) {
            choices <- setNames(
              as.character(seq_along(sc_cat)),
              vapply(sc_cat, function(s) s$label %||% paste("Scenario", s$id), character(1))
            )
            update_toolbar_input_select("sel_scenario", choices = choices, selected = sel_idx)
          } else {
            update_toolbar_input_select("sel_scenario", choices = c("(No scenarios)" = ""), selected = "")
          }

          incProgress(1.0, detail = "Done!")
          showNotification("Project data and parameters successfully restored!", type = "message")
          return()
        }
      }

      # Fallback: legacy archive without project.rds (CSVs + YAML + TIFFs)
      incProgress(0.3, detail = "Restoring from legacy format...")
      apply(table_file_df, 1, function(d) {
        iv <- unlist(strsplit(d[["var"]], split = ".", fixed = TRUE))
        fpath <- file.path(data_dir, d[["file"]])
        if (file.exists(fpath) && file.size(fpath) > 0) {
          i1 <- iv[1]
          try(if (length(iv) == 1) {
            v[[i1]] <- read.csv(fpath, stringsAsFactors = FALSE)
          } else if (length(iv) == 2) {
            i2 <- iv[2]
            v[[i1]][[i2]] <- read.csv(fpath, stringsAsFactors = FALSE)
          }, silent = TRUE)
          if (i1 == "other_emission_df") v[[i1]] <- validate_other_df(v[[i1]])
          if (i1 == "zone_list_df") zone_legend_pal <<- colorFactor(v[[i1]]$color, v[[i1]]$zone_id, na.color = NA)
          if (i1 == "lc_list_df") lc_legend_pal <<- colorFactor(v[[i1]]$color, v[[i1]]$lc_id, na.color = NA)
        }
      })

      if (is.null(v$npv_list) || nrow(v$npv_list) == 0) {
        npv_f <- file.path(data_dir, "npv.csv")
        if (!file.exists(npv_f)) npv_f <- file.path(data_dir, "npv_data.csv")
        if (file.exists(npv_f)) {
          tryCatch({
            df_npv <- read.csv(npv_f, stringsAsFactors = FALSE)
            if (all(c("lc_id", "npv") %in% colnames(df_npv))) {
              v$npv_list <- df_npv[c("lc_id", "npv")]
            }
          }, error = function(e) NULL)
        }
      }

      apply(map_file_df, 1, function(d) {
        fpath <- file.path(data_dir, d[["file"]])
        if (file.exists(fpath)) {
          var_name <- d[["var"]]
          v[[var_name]] <- suppressWarnings(read_stars(fpath, proxy = FALSE))
        }
      })

      config_path <- file.path(data_dir, config_file)
      sc <- NULL
      if (file.exists(config_path)) {
        cd <- read_yaml(config_path)
        for (i in seq_along(cd)) {
          x <- names(cd[i])
          if (x %in% c("map1_year", "map2_year", "map1_date", "map2_date")) {
            val <- cd[[i]]
            yr <- if (grepl("^[0-9]{4}$", as.character(val))) as.integer(val) else suppressWarnings(as.integer(format(as.Date(val, "%d-%b-%Y"), "%Y")))
            if (grepl("1", x)) {
              v$map1_year <- yr; v$map1_date <- yr
            } else {
              v$map2_year <- yr; v$map2_date <- yr
            }
          } else if (x == "scenario") {
            sc <- cd[i]$scenario
          } else {
            v[[x]] <- cd[[i]]
          }
        }
      }

      tpm_f <- file.path(data_dir, "scenario_tpm_1.csv")
      area_f <- file.path(data_dir, "scenario_area_1.csv")
      if (file.exists(tpm_f) && file.exists(area_f)) {
        tryCatch({
          tpm_df <- read.csv(tpm_f, stringsAsFactors = FALSE)
          area_df <- read.csv(area_f, stringsAsFactors = FALSE)
          lc_ch <- merge(tpm_df, area_df, by = c("zone_id", "lc1_id"), all.x = TRUE)
          lc_ch$area <- lc_ch$r * lc_ch$area
          lc_ch$area[is.na(lc_ch$area)] <- 0
          lc_ch$scenario_id <- 0L
          lc_ch$iteration_id <- 0L
          lc_ch <- lc_ch[lc_ch$area > 0, c("zone_id", "lc1_id", "lc2_id", "area", "scenario_id", "iteration_id")]
          v$lc_changes_df <- lc_ch
        }, error = function(e) NULL)
      }

      if (!is.null(sc)) {
        sc_list <- list()
        for (i in seq_along(sc)) {
          asc <- list(label = sc[[i]]$label, desc = sc[[i]]$desc)
          s <- list()
          id <- sc[[i]]$id
          for (r in seq_len(nrow(scenario_file_df))) {
            d <- scenario_file_df[r, ]
            fpath <- file.path(data_dir, paste0(d[["file"]], "_", id, ".csv"))
            if (file.exists(fpath)) {
              try(s[[d[["var"]]]] <- read.csv(fpath, stringsAsFactors = FALSE), silent = TRUE)
            }
          }
          if (!is.null(s$new_lc_id) && length(s$new_lc_id) > 0) s$new_lc_id <- s$new_lc_id[[1]] else s$new_lc_id <- list()
          if (!is.null(s$tpm) && is.data.frame(s$tpm) && "lock" %in% names(s$tpm)) {
            locked <- s$tpm$lock == TRUE | s$tpm$lock == "TRUE" | s$tpm$lock == 1
            s$tpm <- s$tpm[locked, , drop = FALSE]
          }
          asc$abacus_scenario$scenario <- s
          sc_list[[i]] <- asc
        }
        v$scenario_list <- sc_list
      }

      if (!is.null(v$lc_list_df) && !is.null(v$lc_changes_df)) {
        cs <- v$cstock_list
        if (!is.null(cs)) { cs$scenario_id <- 0L; cs$iteration_id <- 0L; cs$zone_id <- 0L }
        d1_val <- if (!is.null(v$map1_date)) v$map1_date else if (!is.null(v$map1_year)) v$map1_year else 2000
        d2_val <- if (!is.null(v$map2_date)) v$map2_date else if (!is.null(v$map2_year)) v$map2_year else 2010

        ab <- prepare_projection_abacus(
          landcover = v$lc_list_df,
          landcover_change = v$lc_changes_df,
          zone = v$zone_list_df,
          carbonstock = cs,
          date1 = d1_val,
          date2 = d2_val,
          n_iteration = as.integer(v$n_iteration %||% 4L),
          other_emission_factor = v$other_emission_df
        )
        if (!is.null(ab)) v$abacus_data <- ab

        if (length(v$scenario_list) > 0) {
          sc_cat <- lapply(seq_along(v$scenario_list), function(si) {
            item <- v$scenario_list[[si]]
            sc_obj <- item$abacus_scenario$scenario %||% item
            list(
              id = as.integer(si),
              label = item$label %||% paste("Scenario", si),
              description = item$desc %||% item$description %||% "",
              tpm = sc_obj$tpm,
              baseline_tpm = sc_obj$baseline_tpm,
              baseline_area = sc_obj$baseline_area,
              landcover = sc_obj$landcover %||% v$lc_list_df,
              new_lc_id = sc_obj$new_lc_id %||% list(),
              n_iteration = as.integer(sc_obj$n_iteration %||% v$n_iteration %||% 4L)
            )
          })
          scenario_catalog(sc_cat)
          selected_scenario_index("1")
          scenario_overrides(list())
          emission_data(NULL)
          baseline_emission_data(NULL)
          scenario_tpm_edits(NULL)
        }
      }

      incProgress(1.0, detail = "Done!")
      showNotification("Data and parameters successfully uploaded!", type = "message")
    })
  }

  #### Output Table #########################

  apply(rbind(table_file_df, output_table_file_df), 1, function(d) {
    output[[paste0(d[["var"]], "_id")]] <- downloadHandler(
      filename = function() {
        d[["file"]]
      },
      content = function(file) {
        iv <- unlist(strsplit(d[["var"]], split = ".", fixed = T))

        df <- v[[iv[1]]]

        i <- 2

        while (i <= length(iv)) {
          df <- df[[iv[i]]]

          i <- i + 1
        }

        if (is.null(df))

          return()

        if (nrow(df) == 0)

          return()

        write.csv(df, file, row.names = F, na = "")
      }
    )
  })

  #### Output Map #########################

  apply(rbind(map_file_df, output_map_file_df) , 1, function(d) {
    output[[paste0(d[["var"]], "_id")]] <- downloadHandler(
      filename = function() {
        d[["file"]]
      },
      content = function(file) {
        m <- v[[d[["var"]]]]

        if (!is.null(m)) {
          write_stars(m, file)
        }
      }
    )
  })

  #### Output Scenario #########################

  output$scenario_output_params <- renderUI({
    apply(scenario_file_df, 1, function(d) {
      downloadLink(paste0(d[["var"]], "_id"), (d[["label"]]))
    })
  })

  apply(scenario_file_df, 1, function(d) {
    output[[paste0(d[["var"]], "_id")]] <- downloadHandler(
      filename = function() {
        paste0(d[["file"]], ".zip")
      },
      content = function(file) {
        setwd(tempdir())

        fs <- c()

        for (i in c(1:length(v$scenario_list))) {
          sc <- v$scenario_list[[i]]$abacus_scenario$scenario

          f <- paste0(d[["file"]], "_", i, ".csv")

          write.csv(sc[[d[["var"]]]], f, row.names = F, na = "")

          fs <- c(fs, f)
        }

        zip::zip(zipfile = file, files = fs)
      }
    )
  })

  output$scenario_output_download <- renderUI({
    apply(output_scenario_file_df, 1, function(d) {
      downloadLink(paste0(d[["var"]], "_id"), (d[["label"]]))
    })
  })

  apply(output_scenario_file_df, 1, function(d) {
    output[[paste0(d[["var"]], "_id")]] <- downloadHandler(
      filename = function() {
        paste0(d[["file"]], ".zip")
      },
      content = function(file) {
        td <- tempfile("abacus_download_")

        dir.create(td, showWarnings = FALSE)

        on.exit(unlink(td, recursive = TRUE), add = TRUE)

        fs <- c()

        for (si in seq_along(v$scenario_list)) {
          s <- v$scenario_list[[si]]

          iv <- unlist(strsplit(d[["var"]], split = ".", fixed = TRUE))

          df <- s[[iv[1]]]

          idx <- 2

          while (idx <= length(iv)) {
            df <- df[[iv[idx]]]

            idx <- idx + 1
          }

          if (is.null(df) || nrow(df) == 0)

            next

          f <- paste0(d[["file"]], "_", si, ".csv")

          write.csv(df, file.path(td, f), row.names = FALSE, na = "")

          fs <- c(fs, f)
        }

        if (length(fs) > 0) {
          curr_wd <- getwd()

          setwd(td)

          on.exit(setwd(curr_wd), add = TRUE)

          zip::zip(zipfile = file, files = fs)
        }
      }
    )
  })
}

blend_hex_colors <- function(hex_vec) {
  hex_vec <- hex_vec[!is.na(hex_vec) & nzchar(hex_vec)]
  if (length(hex_vec) == 0) return("#808080")
  if (length(hex_vec) == 1) return(hex_vec[1])
  rgb_mat <- tryCatch(grDevices::col2rgb(hex_vec), error = function(e) NULL)
  if (is.null(rgb_mat) || ncol(rgb_mat) == 0) return(hex_vec[1])
  avg <- rowMeans(rgb_mat)
  grDevices::rgb(avg[1], avg[2], avg[3], maxColorValue = 255)
}
## Helpers that turn redd-abacus map/table state into the abacus() object
## expected by abacuslib's scenario_editor and lc_area_chart widgets.

as_iso_date <- function(d, default_year = 2000) {
  if (is.null(d) || length(d) == 0 || is.na(d[[1]])) {
    return(as.Date(paste0(default_year, "-07-01")))
  }
  val <- d[[1]]
  if (inherits(val, "Date")) return(val)
  if (is.numeric(val)) return(as.Date(paste0(as.integer(val), "-07-01")))
  str <- as.character(val)
  if (grepl("^[0-9]{4}$", str)) return(as.Date(paste0(str, "-07-01")))
  res <- tryCatch(as.Date(str, "%d-%b-%Y"), error = function(e) NA)
  if (!is.na(res)) return(res)
  res <- tryCatch(as.Date(str), error = function(e) NA)
  if (!is.na(res)) return(res)
  as.Date(paste0(default_year, "-07-01"))
}

unwrap_abacus_project <- function(data, project_id = 1L) {
  if (is.null(data)) return(NULL)
  ver <- data$version
  if (is.null(ver)) ver <- attr(data, "version")
  if (!is.null(ver) && as.integer(ver)[[1]] == 1L && !is.null(data$project_list)) {
    pid <- suppressWarnings(as.integer(project_id))
    if (is.na(pid) || pid < 1L || pid > length(data$project_list)) pid <- 1L
    p <- data$project_list[[pid]]
    if (is.null(p)) return(data)
    p$version <- 1
    return(p)
  }
  data
}

## Compose multiple zonation maps into a composite zone table and assign composite zone_ids
build_composite_zone_data <- function(lc_ch, zone_map_ids, zone_legends = list()) {
  if (is.null(lc_ch) || !is.data.frame(lc_ch) || nrow(lc_ch) == 0) {
    empty_zone <- data.frame(zone_id = 0L, label = "All zones", description = "", stringsAsFactors = FALSE)
    return(list(zone_df = empty_zone, lc_ch = lc_ch))
  }
  if (is.null(zone_map_ids) || length(zone_map_ids) == 0) {
    ch <- lc_ch
    ch$zone_id <- 0L
    empty_zone <- data.frame(zone_id = 0L, label = "All zones", description = "", stringsAsFactors = FALSE)
    return(list(zone_df = empty_zone, lc_ch = ch))
  }
  if (length(zone_map_ids) == 1L) {
    zid <- zone_map_ids[[1]]
    ch <- lc_ch
    col <- paste0("zone_", zid)
    if (!(col %in% names(ch))) {
      cols <- grep("^zone_", names(ch), value = TRUE)
      col <- if (length(cols) > 0) cols[[1]] else NULL
    }
    raw_z <- if (!is.null(col) && col %in% names(ch)) ch[[col]] else 0L
    raw_z <- suppressWarnings(as.integer(raw_z))
    raw_z[is.na(raw_z)] <- 0L
    ch$zone_id <- raw_z
    leg <- if (!is.null(zone_legends)) zone_legends[[zid]] else NULL
    if (!is.null(leg) && is.data.frame(leg) && nrow(leg) > 0 && "zone_id" %in% names(leg)) {
      zone_df <- leg
    } else {
      u_ids <- sort(unique(ch$zone_id))
      zone_df <- data.frame(
        zone_id = u_ids,
        label = ifelse(u_ids == 0L, "All zones", paste("Zone", u_ids)),
        description = "",
        stringsAsFactors = FALSE
      )
    }
    return(list(zone_df = zone_df, lc_ch = ch))
  }
  ch <- lc_ch
  zcols <- paste0("zone_", zone_map_ids)
  zcols <- zcols[zcols %in% names(ch)]
  if (length(zcols) == 0) zcols <- grep("^zone_", names(ch), value = TRUE)
  if (length(zcols) <= 1) {
    return(build_composite_zone_data(lc_ch, if (length(zcols) == 1) sub("^zone_", "", zcols) else character(0), zone_legends))
  }
  for (zc in zcols) ch[[zc]] <- suppressWarnings(as.integer(ch[[zc]]))
  combo_df <- unique(ch[, zcols, drop = FALSE])
  has_valid <- rowSums(!is.na(combo_df) & combo_df != 0L) > 0
  combo_df <- combo_df[has_valid, , drop = FALSE]
  if (nrow(combo_df) == 0) {
    ch$zone_id <- 0L
    empty_zone <- data.frame(zone_id = 0L, label = "All zones", description = "", stringsAsFactors = FALSE)
    return(list(zone_df = empty_zone, lc_ch = ch))
  }
  ord_args <- lapply(zcols, function(col) combo_df[[col]])
  combo_df <- combo_df[do.call(order, ord_args), , drop = FALSE]
  rownames(combo_df) <- NULL
  labels <- character(nrow(combo_df))
  colors <- character(nrow(combo_df))
  for (i in seq_len(nrow(combo_df))) {
    parts <- character(0)
    col_parts <- character(0)
    for (zc in zcols) {
      mid <- sub("^zone_", "", zc)
      val <- combo_df[i, zc]
      mleg <- if (!is.null(zone_legends)) zone_legends[[mid]] else NULL
      plabel <- NULL
      pcolor <- NULL
      if (!is.null(mleg) && is.data.frame(mleg) && "zone_id" %in% names(mleg)) {
        midx <- which(mleg$zone_id == val)
        if (length(midx) > 0) {
          if ("label" %in% names(mleg)) plabel <- as.character(mleg$label[midx[1]])
          if ("color" %in% names(mleg)) pcolor <- as.character(mleg$color[midx[1]])
        }
      }
      if (is.null(plabel) || !nzchar(plabel)) plabel <- if (is.na(val) || val == 0L) "Other" else paste0("Zone ", val)
      parts <- c(parts, plabel)
      if (!is.null(pcolor) && !is.na(pcolor) && nzchar(pcolor)) {
        col_parts <- c(col_parts, pcolor)
      }
    }
    labels[i] <- paste(parts, collapse = " - ")
    # Combine colors from the applied zonation maps
    colors[i] <- if (length(col_parts) > 0) blend_hex_colors(col_parts) else NA_character_
  }
  combo_df$composite_id <- seq_len(nrow(combo_df))
  combo_df$composite_label <- labels
  combo_df$composite_color <- colors

  missing_c <- is.na(colors) | !nzchar(colors)
  if (any(missing_c)) {
    fallback_pal <- grDevices::hcl.colors(sum(missing_c), "Viridis")
    colors[missing_c] <- fallback_pal
    combo_df$composite_color[missing_c] <- fallback_pal
  }

  key_ch <- do.call(paste, c(lapply(zcols, function(col) ch[[col]]), sep = "\001"))
  key_combo <- do.call(paste, c(lapply(zcols, function(col) combo_df[[col]]), sep = "\001"))
  m_idx <- match(key_ch, key_combo)
  ch$zone_id <- ifelse(is.na(m_idx), 0L, combo_df$composite_id[m_idx])
  zone_df <- data.frame(
    zone_id = combo_df$composite_id,
    label = combo_df$composite_label,
    description = "",
    color = colors,
    stringsAsFactors = FALSE
  )
    list(zone_df = zone_df, lc_ch = ch)
}

## Collapse the changes table into the zone_id / scenario_id / iteration_id layout
normalize_landcover_change <- function(lc_ch) {
  if (is.null(lc_ch) || !is.data.frame(lc_ch) || nrow(lc_ch) == 0) {
    return(NULL)
  }
  ch <- lc_ch
  if (!("zone_id" %in% names(ch))) {
    zone_cols <- grep("^zone_", names(ch), value = TRUE)
    if (length(zone_cols) == 1L) {
      ch$zone_id <- ch[[zone_cols[[1]]]]
    } else if (length(zone_cols) > 1L) {
      comp <- build_composite_zone_data(ch, sub("^zone_", "", zone_cols), NULL)
      ch <- comp$lc_ch
    } else {
      ch$zone_id <- 0L
    }
  }
  ch$zone_id <- suppressWarnings(as.integer(ch$zone_id))
  ch$zone_id[is.na(ch$zone_id)] <- 0L
  if (!("scenario_id" %in% names(ch))) ch$scenario_id <- 0L
  if (!("iteration_id" %in% names(ch))) ch$iteration_id <- 0L
  ch$scenario_id <- suppressWarnings(as.integer(ch$scenario_id))
  ch$iteration_id <- suppressWarnings(as.integer(ch$iteration_id))
  ch$scenario_id[is.na(ch$scenario_id)] <- 0L
  ch$iteration_id[is.na(ch$iteration_id)] <- 0L
  ch$lc1_id <- suppressWarnings(as.integer(ch$lc1_id))
  ch$lc2_id <- suppressWarnings(as.integer(ch$lc2_id))
  ch$area <- suppressWarnings(as.numeric(ch$area))
  ch <- ch[!is.na(ch$lc1_id) & !is.na(ch$lc2_id) & !is.na(ch$area) & ch$area > 0, , drop = FALSE]
  if (nrow(ch) == 0) return(NULL)
  ch[, c("zone_id", "lc1_id", "lc2_id", "area", "scenario_id", "iteration_id"), drop = FALSE]
}

normalize_zone_table <- function(zone_df, zone_ids) {
  zone_ids <- sort(unique(as.integer(zone_ids)))
  if (length(zone_ids) == 0) zone_ids <- 0L
  zn <- zone_df
  if (!is.null(zn) && is.data.frame(zn) && nrow(zn) > 0 && "zone_id" %in% names(zn)) {
    zn$zone_id <- suppressWarnings(as.integer(zn$zone_id))
    if (!("label" %in% names(zn))) zn$label <- paste("Zone", zn$zone_id)
    if (!("description" %in% names(zn))) zn$description <- ""
    missing <- setdiff(zone_ids, zn$zone_id)
    if (length(missing) > 0) {
      extra <- data.frame(
        zone_id = missing,
        label = ifelse(missing == 0L, "All zones", paste("Zone", missing)),
        description = "",
        stringsAsFactors = FALSE
      )
      keep <- intersect(names(zn), names(extra))
      extra <- extra[, keep, drop = FALSE]
      for (nm in setdiff(names(zn), names(extra))) {
        extra[[nm]] <- if (nm == "color") "#888888" else NA
      }
      extra <- extra[, names(zn), drop = FALSE]
      zn <- rbind(zn, extra)
    }
    zn <- zn[zn$zone_id %in% zone_ids, , drop = FALSE]
    if (nrow(zn) > 0) return(zn)
  }
  data.frame(
    zone_id = zone_ids,
    label = ifelse(zone_ids == 0L, "All zones", paste("Zone", zone_ids)),
    description = "",
    stringsAsFactors = FALSE
  )
}

normalize_carbonstock <- function(carbonstock, landcover) {
  cs <- carbonstock
  if (is.null(cs) || !is.data.frame(cs) || nrow(cs) == 0) {
    cs <- data.frame(lc_id = landcover$lc_id, c = 0, stringsAsFactors = FALSE)
  }
  if (!("lc_id" %in% names(cs))) cs$lc_id <- landcover$lc_id[seq_len(nrow(cs))]
  if (!("c" %in% names(cs))) cs$c <- 0
  cs$c <- suppressWarnings(as.numeric(cs$c))
  cs$c[is.na(cs$c)] <- 0
  cs$lc_id <- suppressWarnings(as.integer(cs$lc_id))
  cs$scenario_id <- 0L
  cs$iteration_id <- 0L
  if (!("zone_id" %in% names(cs))) cs$zone_id <- 0L
  cs$zone_id <- suppressWarnings(as.integer(cs$zone_id))
  cs$zone_id[is.na(cs$zone_id)] <- 0L
  cs
}

normalize_landcover <- function(landcover, lc_ch = NULL) {
  lc <- landcover
  if (is.null(lc) || !is.data.frame(lc) || nrow(lc) == 0 || !("lc_id" %in% names(lc))) {
    if (!is.null(lc_ch) && is.data.frame(lc_ch)) {
      all_lc <- sort(unique(c(lc_ch$lc1_id, lc_ch$lc2_id)))
      all_lc <- all_lc[!is.na(all_lc) & all_lc > 0]
      if (length(all_lc) > 0) {
        lc <- data.frame(
          lc_id = all_lc,
          label = paste0("Landcover_", all_lc),
          description = "",
          color = "#808080",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (is.null(lc) || !is.data.frame(lc) || nrow(lc) == 0) return(NULL)
  lc$lc_id <- suppressWarnings(as.integer(lc$lc_id))
  if (!("label" %in% names(lc))) lc$label <- paste0("Landcover_", lc$lc_id)
  lc$label[is.na(lc$label) | !nzchar(as.character(lc$label))] <- paste0("Landcover_", lc$lc_id[is.na(lc$label) | !nzchar(as.character(lc$label))])
  if (!("description" %in% names(lc))) lc$description <- ""
  if (!("color" %in% names(lc))) lc$color <- "#808080"
  lc
}

#' Update land cover colors and labels in a landcover data frame using a reference landcover table
update_landcover_colors <- function(lc_df, new_lc_df) {
  if (is.null(lc_df)) return(new_lc_df)
  if (is.null(new_lc_df)) return(lc_df)
  if (!is.data.frame(lc_df) && is.list(lc_df) && length(lc_df) > 0) {
    lc_df <- tryCatch(as.data.frame(lc_df, stringsAsFactors = FALSE), error = function(e) lc_df)
  }
  if (!is.data.frame(new_lc_df) && is.list(new_lc_df) && length(new_lc_df) > 0) {
    new_lc_df <- tryCatch(as.data.frame(new_lc_df, stringsAsFactors = FALSE), error = function(e) new_lc_df)
  }
  if (!is.data.frame(lc_df) || nrow(lc_df) == 0) return(new_lc_df)
  if (!is.data.frame(new_lc_df) || nrow(new_lc_df) == 0) return(lc_df)
  if (!("color" %in% names(lc_df))) lc_df$color <- "#808080"
  m <- match(as.character(lc_df$lc_id), as.character(new_lc_df$lc_id))
  valid <- !is.na(m)
  if (any(valid)) {
    if ("color" %in% names(new_lc_df)) {
      lc_df$color[valid] <- as.character(new_lc_df$color[m[valid]])
    }
    if ("label" %in% names(new_lc_df)) {
      new_lbls <- as.character(new_lc_df$label[m[valid]])
      has_lbl <- !is.na(new_lbls) & nzchar(trimws(new_lbls))
      if (any(has_lbl)) {
        if (!("label" %in% names(lc_df))) lc_df$label <- paste0("Landcover_", lc_df$lc_id)
        valid_idx <- which(valid)[has_lbl]
        lc_df$label[valid_idx] <- new_lbls[has_lbl]
      }
    }
    if ("description" %in% names(new_lc_df)) {
      if (!("description" %in% names(lc_df))) lc_df$description <- ""
      lc_df$description[valid] <- as.character(new_lc_df$description[m[valid]])
    }
  }
  lc_df
}

#' Build an abacuslib-ready project from the app's current tables.
prepare_projection_abacus <- function(landcover,
                                      landcover_change,
                                      zone = NULL,
                                      carbonstock = NULL,
                                      date1 = NULL,
                                      date2 = NULL,
                                      n_iteration = 4L,
                                      other_emission_factor = NULL) {
  ch <- normalize_landcover_change(landcover_change)
  if (is.null(ch)) return(NULL)
  lc <- normalize_landcover(landcover, ch)
  if (is.null(lc)) return(NULL)
  zn <- normalize_zone_table(zone, ch$zone_id)
  cs <- normalize_carbonstock(carbonstock, lc)
  ot <- other_emission_factor
  if (!is.null(ot) && is.data.frame(ot) && nrow(ot) == 0) ot <- NULL
  d1 <- as_iso_date(date1, 2000)
  d2 <- as_iso_date(date2, 2010)
  n_it <- suppressWarnings(as.integer(n_iteration))
  if (is.na(n_it) || n_it < 1L) n_it <- 4L

  if (exists("abacus", mode = "function")) {
    ab_fn <- get("abacus", mode = "function")
    return(ab_fn(
      date1 = d1,
      date2 = d2,
      landcover = lc,
      landcover_change = ch,
      zone = zn,
      carbonstock = cs,
      n_iteration = n_it,
      other_emission_factor = ot
    ))
  }
  if (requireNamespace("abacuslib", quietly = TRUE)) {
    return(abacuslib::abacus(
      date1 = d1,
      date2 = d2,
      landcover = lc,
      landcover_change = ch,
      zone = zn,
      carbonstock = cs,
      n_iteration = n_it,
      other_emission_factor = ot
    ))
  }

  structure(
    list(
      project = list(
        title = NULL,
        description = NULL,
        date1 = d1,
        date2 = d2,
        n_iteration = n_it
      ),
      landcover = lc,
      landcover_change = ch,
      zone = zn,
      carbonstock = cs,
      other_emission_factor = ot,
      scenario = NULL,
      costbenefit = NULL,
      version = 2
    ),
    class = "abacus"
  )
}





#' Convert lc_trend_data to an abacus data object for lc_area_chart
#'
#' When supplied with actual observed areas from uploaded maps, this builds
#' an abacus object carrying `observed_areas` (with actual map years and areas).
#' The widget displays this observed data as it is without interpolation or
#' forward projection, showing only the years of available data on the axis.
#'
#' @param trend_df    data.frame from v$lc_trend_data (columns: id, area, year, label, map_id)
#' @param lc_list_df  data.frame from v$lc_list_df   (columns: lc_id, label, color, description)
#' @param lc_changes  optional data.frame of land cover changes (e.g. from v$lc_changes_df)
#' @param n_iteration integer projection iterations (optional)
#' @return abacus object (with `observed_areas` and `available_years`) or NULL
trend_to_abacus <- function(trend_df, lc_list_df = NULL, lc_changes = NULL, n_iteration = NULL) {
  if (is.null(trend_df) || !is.data.frame(trend_df) || nrow(trend_df) == 0) return(NULL)

  years <- sort(unique(trend_df$year))
  if (length(years) < 2) return(NULL)

  # ---- landcover table ----
  lc_ids <- sort(unique(trend_df$id))
  if (!is.null(lc_list_df) && is.data.frame(lc_list_df) && nrow(lc_list_df) > 0) {
    lc <- lc_list_df[lc_list_df$lc_id %in% lc_ids, , drop = FALSE]
    missing_ids <- setdiff(lc_ids, lc$lc_id)
    if (length(missing_ids) > 0) {
      extra <- data.frame(
        lc_id       = missing_ids,
        label       = paste0("LC_", missing_ids),
        description = "",
        color       = "#808080",
        stringsAsFactors = FALSE
      )
      shared_cols <- intersect(names(lc), names(extra))
      lc <- rbind(lc[, shared_cols, drop = FALSE], extra[, shared_cols, drop = FALSE])
    }
  } else {
    lc <- data.frame(
      lc_id       = lc_ids,
      label       = paste0("LC_", lc_ids),
      description = "",
      color       = "#808080",
      stringsAsFactors = FALSE
    )
  }

  # ---- landcover_change (identity transitions at year 1 for structure validity) ----
  yr0_df <- trend_df[trend_df$year == years[1], , drop = FALSE]
  lc_ch <- data.frame(
    zone_id      = 0L,
    lc1_id       = as.integer(yr0_df$id),
    lc2_id       = as.integer(yr0_df$id),
    area         = as.numeric(yr0_df$area),
    scenario_id  = 0L,
    iteration_id = 0L,
    stringsAsFactors = FALSE
  )
  lc_ch <- lc_ch[is.finite(lc_ch$area) & lc_ch$area > 0, , drop = FALSE]

  # ---- observed_areas: exact observed data across all uploaded map years ----
  obs_rows <- do.call(rbind, lapply(seq_along(years), function(i) {
    yr <- years[i]
    yr_df <- trend_df[trend_df$year == yr, , drop = FALSE]
    data.frame(
      iteration = as.integer(yr),
      year      = as.integer(yr),
      zone_id   = 0L,
      lc1_id    = as.integer(yr_df$id),
      area      = as.numeric(yr_df$area),
      stringsAsFactors = FALSE
    )
  }))
  obs_rows <- obs_rows[is.finite(obs_rows$area) & obs_rows$area >= 0, , drop = FALSE]

  # ---- zone ----
  zone <- data.frame(
    zone_id     = 0L,
    label       = "All zones",
    description = "",
    stringsAsFactors = FALSE
  )

  d1 <- as.Date(paste0(years[1], "-07-01"))
  d2 <- as.Date(paste0(years[2], "-07-01"))

  ab <- prepare_projection_abacus(
    landcover        = lc,
    landcover_change = lc_ch,
    zone             = zone,
    carbonstock      = NULL,
    date1            = d1,
    date2            = d2,
    n_iteration      = as.integer(length(years) - 1L),
    other_emission_factor = NULL
  )

  if (!is.null(ab)) {
    ab$observed_areas <- obs_rows
    ab$available_years <- as.integer(years)
  }
  ab
}

#' Calculate REDD-Abacus baseline or scenario emissions directly in R
#'
#' Replicates the abacus.js projection and emission engine in R so that
#' projections and emissions can be computed independently for any scenario
#' without requiring the client-side htmlwidget to render.
#'
#' @param abacus_data An abacus object or list containing landcover_change,
#'   carbonstock, other_emission_factor, project (date1, date2, n_iteration).
#' @param scenario_tpm Optional data.frame of scenario TPM modifications
#'   (columns: zone_id, lc1_id, lc2_id, r, iteration).
#' @param n_iteration Optional integer number of projection iterations.
#' @return A list with `iteration_emission` and `lc_emission`.
calculate_abacus_emission <- function(abacus_data, scenario_tpm = NULL, n_iteration = NULL) {
  if (is.null(abacus_data)) return(NULL)
  ch <- abacus_data$landcover_change
  if (is.null(ch) || nrow(ch) == 0) return(NULL)

  base_data <- ch[ch$scenario_id == 0 & ch$iteration_id == 0 & !is.na(ch$area) & ch$area > 0, , drop = FALSE]
  if (nrow(base_data) == 0) return(NULL)

  lc_area <- stats::aggregate(area ~ zone_id + lc1_id, data = base_data, FUN = sum)

  base_tpm <- merge(base_data[, c("zone_id", "lc1_id", "lc2_id", "area")],
                    lc_area, by = c("zone_id", "lc1_id"), suffixes = c("", "_total"))
  base_tpm$r <- ifelse(base_tpm$area_total > 0, base_tpm$area / base_tpm$area_total, 0)
  base_tpm <- base_tpm[, c("zone_id", "lc1_id", "lc2_id", "r")]

  unique_zones <- unique(base_tpm$zone_id)
  extra_tpm <- list()
  extra_area <- list()
  for (zid in unique_zones) {
    z_tpm <- base_tpm[base_tpm$zone_id == zid, , drop = FALSE]
    u_lc1 <- unique(z_tpm$lc1_id)
    u_lc2 <- unique(z_tpm$lc2_id)
    diff_lc <- setdiff(u_lc2, u_lc1)
    if (length(diff_lc) > 0) {
      extra_tpm[[length(extra_tpm) + 1]] <- data.frame(
        zone_id = zid, lc1_id = diff_lc, lc2_id = diff_lc, r = 1, stringsAsFactors = FALSE
      )
      extra_area[[length(extra_area) + 1]] <- data.frame(
        zone_id = zid, lc1_id = diff_lc, area = 0, stringsAsFactors = FALSE
      )
    }
  }
  if (length(extra_tpm) > 0) {
    base_tpm <- rbind(base_tpm, do.call(rbind, extra_tpm))
    lc_area <- rbind(lc_area, do.call(rbind, extra_area))
  }

  n <- if (!is.null(n_iteration)) suppressWarnings(as.integer(n_iteration[1])) else NA_integer_
  if (is.na(n) || n < 1L) {
    n <- if (!is.null(abacus_data$project$n_iteration))
      suppressWarnings(as.integer(abacus_data$project$n_iteration[[1]]))
    else NA_integer_
    if (is.na(n) || n < 1L) n <- 4L
  }

  sc_tpm_df <- if (!is.null(scenario_tpm) && is.data.frame(scenario_tpm) && nrow(scenario_tpm) > 0) scenario_tpm else NULL

  current_area <- lc_area
  transitions_list <- list()

  for (i in 0:n) {
    step_df <- merge(base_tpm, current_area, by = c("zone_id", "lc1_id"), all.x = TRUE)
    step_df$area[is.na(step_df$area)] <- 0

    if (!is.null(sc_tpm_df) && nrow(sc_tpm_df) > 0) {
      sc_sub <- sc_tpm_df[sc_tpm_df$iteration <= i, , drop = FALSE]
      if (nrow(sc_sub) > 0) {
        sc_sub <- sc_sub[order(sc_sub$iteration, decreasing = TRUE), , drop = FALSE]
        m_key_step <- paste(step_df$zone_id, step_df$lc1_id, step_df$lc2_id, sep = "_")
        m_key_sc   <- paste(sc_sub$zone_id, sc_sub$lc1_id, sc_sub$lc2_id, sep = "_")
        match_idx  <- match(m_key_step, m_key_sc)
        has_match  <- !is.na(match_idx)
        if (any(has_match)) {
          step_df$r[has_match] <- sc_sub$r[match_idx[has_match]]
        }
      }
    }

    step_df$trans_area <- step_df$area * step_df$r
    step_df$iteration <- i
    transitions_list[[i + 1]] <- step_df

    next_area <- stats::aggregate(trans_area ~ zone_id + lc2_id, data = step_df, FUN = sum)
    names(next_area)[names(next_area) == "lc2_id"] <- "lc1_id"
    names(next_area)[names(next_area) == "trans_area"] <- "area"
    current_area <- next_area
  }

  all_transitions <- do.call(rbind, transitions_list)

  cstock_df <- abacus_data$carbonstock
  c_map <- numeric(0)
  if (!is.null(cstock_df) && nrow(cstock_df) > 0) {
    if ("scenario_id" %in% names(cstock_df)) {
      cstock_df <- cstock_df[cstock_df$scenario_id == 0 | is.na(cstock_df$scenario_id), , drop = FALSE]
    }
    for (row_i in seq_len(nrow(cstock_df))) {
      c_map[as.character(cstock_df$lc_id[row_i])] <- cstock_df$c[row_i]
    }
  }

  get_c_val <- function(lc_ids) {
    vals <- c_map[as.character(lc_ids)]
    vals[is.na(vals)] <- 0
    vals
  }

  c1 <- get_c_val(all_transitions$lc1_id)
  c2 <- get_c_val(all_transitions$lc2_id)
  c_emission <- ((c1 - c2) * all_transitions$trans_area * 44) / 12

  other_emission <- rep(0, nrow(all_transitions))
  ot_df <- abacus_data$other_emission_factor
  if (!is.null(ot_df) && is.data.frame(ot_df) && nrow(ot_df) > 0) {
    d1 <- tryCatch(as.Date(abacus_data$project$date1), error = function(e) as.Date("2000-01-01"))
    d2 <- tryCatch(as.Date(abacus_data$project$date2), error = function(e) as.Date("2010-01-01"))
    p_yr <- round(as.numeric(difftime(d2, d1, units = "days")) / 7 / 52.25)
    if (is.na(p_yr) || p_yr <= 0) p_yr <- 1

    has_zid <- "zone_id" %in% names(ot_df) && any(ot_df$zone_id != 0)
    get_ef <- function(zids, lcids) {
      if (has_zid) {
        k_target <- paste(zids, lcids, sep = "_")
        k_ref <- paste(ot_df$zone_id, ot_df$lc_id, sep = "_")
        m <- match(k_target, k_ref)
        ef <- ot_df$efactor[m]
        na_idx <- which(is.na(ef))
        if (length(na_idx) > 0) {
          m0 <- match(lcids[na_idx], ot_df$lc_id)
          ef[na_idx] <- ot_df$efactor[m0]
        }
      } else {
        m <- match(lcids, ot_df$lc_id)
        ef <- ot_df$efactor[m]
      }
      ef[is.na(ef)] <- 0
      ef * p_yr
    }
    ef1 <- get_ef(all_transitions$zone_id, all_transitions$lc1_id)
    ef2 <- get_ef(all_transitions$zone_id, all_transitions$lc2_id)
    other_emission <- ((ef1 + ef2) * all_transitions$trans_area) / 2
  }

  total_emission <- c_emission + other_emission
  all_transitions$emission <- total_emission

  iter_emission <- stats::aggregate(emission ~ iteration, data = all_transitions, FUN = sum)
  iter_emission <- iter_emission[order(iter_emission$iteration), , drop = FALSE]

  list(
    iteration_emission = iter_emission,
    lc_emission = all_transitions
  )
}

#' Extract iteration-level emissions filtered by emission type
#'
#' @param emission_obj An emission object (containing `lc_emission` and/or `iteration_emission`).
#' @param mode Character, one of `"net"`, `"positive"`, or `"negative"`.
#'   - `"net"`: Net emissions (sum of all carbon fluxes / removals).
#'   - `"positive"`: Positive emissions only (gross emissions from deforestation/degradation, emission > 0).
#'   - `"negative"`: Negative emissions only (carbon sequestration / removals, emission < 0).
#' @return A data.frame with columns `iteration` and `emission`.
extract_iteration_emission <- function(emission_obj, mode = "net") {
  if (is.null(emission_obj)) return(NULL)
  if (is.null(mode) || !nzchar(mode)) mode <- "net"
  if (!(mode %in% c("net", "positive", "negative"))) mode <- "net"

  # Look for lc_emission at top-level or inside emission_obj$emission
  lc_e <- if (!is.null(emission_obj$lc_emission)) {
    emission_obj$lc_emission
  } else if (!is.null(emission_obj$emission$lc_emission)) {
    emission_obj$emission$lc_emission
  } else {
    NULL
  }

  if (!is.null(lc_e) && is.data.frame(lc_e) && nrow(lc_e) > 0 && "iteration" %in% names(lc_e)) {
    e_val <- if ("emission" %in% names(lc_e)) lc_e$emission else 0
    e_other <- if ("emission_other" %in% names(lc_e)) ifelse(is.na(lc_e$emission_other), 0, lc_e$emission_other) else 0
    tot_e <- e_val + e_other

    all_iters <- sort(unique(as.integer(lc_e$iteration)))
    if (mode == "positive") {
      keep <- tot_e > 0
    } else if (mode == "negative") {
      keep <- tot_e < 0
    } else {
      keep <- rep(TRUE, length(tot_e))
    }

    sub_df <- data.frame(iteration = as.integer(lc_e$iteration[keep]), emission = tot_e[keep])
    if (nrow(sub_df) > 0) {
      agg <- stats::aggregate(emission ~ iteration, data = sub_df, FUN = sum)
    } else {
      agg <- data.frame(iteration = integer(0), emission = numeric(0))
    }

    res <- merge(data.frame(iteration = all_iters), agg, by = "iteration", all.x = TRUE)
    res$emission[is.na(res$emission)] <- 0
    return(res[order(res$iteration), , drop = FALSE])
  }

  # Fallback to iteration_emission if lc_emission is unavailable
  iter_e <- if (!is.null(emission_obj$iteration_emission)) {
    emission_obj$iteration_emission
  } else if (!is.null(emission_obj$emission$iteration_emission)) {
    emission_obj$emission$iteration_emission
  } else {
    NULL
  }
  if (!is.null(iter_e) && is.data.frame(iter_e) && "iteration" %in% names(iter_e) && "emission" %in% names(iter_e)) {
    return(iter_e[order(iter_e$iteration), c("iteration", "emission"), drop = FALSE])
  }
  NULL
}

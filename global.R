### REQUIRED LIBRARY ######################

options(repos = c(CRAN = "https://cloud.r-project.org"))
options(shiny.maxRequestSize = 200*1024^2)  # 200 MB max upload size

#' Resolve Application Working Directory
#' 
#' Determines and validates the root directory of the REDD Abacus application
#' whether run from RStudio, command line, or sourced scripts.
#' 
#' @return Character path of the working directory.
resolve_app_dir <- function() {
  current <- getwd()

  if (file.exists(file.path(current, "ui.R")) && file.exists(file.path(current, "server.R"))) {
    return(current)
  }

  parent_guess <- normalizePath(file.path(current, ".."), winslash = "/")
  if (file.exists(file.path(parent_guess, "ui.R")) && file.exists(file.path(parent_guess, "server.R"))) {
    return(parent_guess)
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(dirname(sys.frames()[[1]]$ofile), winslash = "/"))
  }

  current
}

app_dir <- resolve_app_dir()
if (file.exists(file.path(app_dir, "ui.R")) && file.exists(file.path(app_dir, "server.R"))) {
  setwd(app_dir)
}

#' Install and Load Required CRAN Packages
#' 
#' Verifies package presence in library paths, installs missing packages from CRAN,
#' and loads each package namespace into the session.
#' 
#' @param package1 Character name of first package.
#' @param ... Additional package names to verify and load.
install_load <- function(package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

install_load(
  "shiny",
  "shinyWidgets",
  "shinyBS",
  "bslib",
  "bsicons",
  "htmltools",
  "reactable",
  "plotly",
  "leaflet",
  ## Map operations are stars/sf only: every map is read, gridded, warped and
  ## tabulated with read_stars()/st_rasterize()/st_warp()/[[ ]]. terra is
  ## deliberately not loaded, so no map is ever converted to a SpatRaster.
  "stars",
  "sf",
  "mapview",
  "excelR",
  "RColorBrewer",
  "jsonlite",
  "reshape2",
  "leafem",
  "yaml",
  "paletteer"
)

if (!exists("accordion", mode = "function")) {
  accordion_panel <- function(title, ...) {
    tags$details(
      tags$summary(title),
      ..., class = "accordion-item"
    )
  }

  accordion <- function(..., open = TRUE) {
    panels <- list(...)
    if (length(panels) > 0 && isTRUE(open)) {
      panels[[1]] <- htmltools::tagAppendAttributes(panels[[1]], open = NA)
    }
    tags$div(class = "accordion", panels)
  }
}

if (!exists("nav_panel", mode = "function")) {
  nav_panel <- function(title, ..., value = title, icon = NULL) {
    bslib::nav(title, ..., value = value, icon = icon)
  }
}

if (!exists("navset_tab", mode = "function")) {
  navset_tab <- function(..., id = NULL, selected = NULL) {
    bslib::navs_tab(..., id = id, selected = selected)
  }
}

if (!exists("navset_underline", mode = "function")) {
  navset_underline <- function(..., id = NULL, selected = NULL) {
    bslib::navs_tab(..., id = id, selected = selected)
  }
}

#' Create Toolbar Input Button
#' 
#' Constructs an action button styled specifically for bslib card toolbars and headers,
#' with responsive label/icon display and optional hover tooltip.
#' 
#' @param id Shiny input ID.
#' @param label Display label for button and tooltip.
#' @param icon Optional font-awesome or bootstrap icon element.
#' @param show_label Logical indicating whether text label should be visible.
#' @param tooltip Logical or text tooltip to display on hover.
#' @param ... Additional HTML attributes.
#' @param disabled Logical indicating whether button starts disabled.
#' @param border Logical indicating whether button has a border.
#' @return A Shiny actionButton tag object.
toolbar_input_button <- function(
  id,
  label,
  icon = NULL,
  show_label = is.null(icon),
  tooltip = !show_label,
  ...,
  disabled = FALSE,
  border = FALSE
) {
  btn_type <- if (is.null(icon)) {
    if (!show_label) {
      rlang::abort("If `show_label` is FALSE, `icon` must be provided.")
    }
    "label"
  } else {
    if (show_label) "both" else "icon"
  }

  label_id <- paste0("btn-label-", as.integer(stats::runif(1, 1000, 10000)))
  label_elem <- tags$span(
    id = label_id,
    class = "bslib-toolbar-label",
    hidden = if (!show_label) NA else NULL,
    label
  )

  icon_elem <- if (!is.null(icon)) {
    tags$span(
      class = "bslib-toolbar-icon",
      `aria-hidden` = "true",
      style = "pointer-events: none;",
      icon
    )
  } else NULL

  button <- shiny::actionButton(
    inputId = id,
    label = htmltools::tagList(icon_elem, label_elem),
    icon = NULL,
    disabled = disabled,
    class = paste(
      "bslib-toolbar-input-button btn-sm",
      if (!border) "border-0" else "border-1"
    ),
    `data-type` = btn_type,
    `aria-labelledby` = label_id,
    ...
  )

  if (isTRUE(tooltip)) {
    tooltip <- label
  }
  if (isFALSE(tooltip)) {
    tooltip <- NULL
  }
  if (!is.null(tooltip) && nzchar(as.character(tooltip))) {
    if (exists("tooltip", where = asNamespace("bslib"), mode = "function")) {
      button <- bslib::tooltip(button, tooltip, id = sprintf("%s_tooltip", id), placement = "bottom")
    }
  }

  button
}

#' Format period years with length in parentheses
#' e.g. 2000, 2010 -> "2000 – 2010 (10 yrs)"
#' or "2000-2010" -> "2000 – 2010 (10 yrs)"
format_period_with_length <- function(y1, y2, period_yrs = NULL, sep = " \u2013 ") {
  y1_clean <- if (!is.null(y1) && !is.na(y1)) trimws(as.character(y1)) else ""
  y2_clean <- if (!is.null(y2) && !is.na(y2)) trimws(as.character(y2)) else ""

  y1_num <- suppressWarnings(as.numeric(y1_clean))
  y2_num <- suppressWarnings(as.numeric(y2_clean))

  if (is.null(period_yrs) || is.na(period_yrs) || period_yrs <= 0) {
    if (!is.na(y1_num) && !is.na(y2_num)) {
      period_yrs <- abs(y2_num - y1_num)
    }
  }

  has_years <- nzchar(y1_clean) && nzchar(y2_clean) && !is.na(y1_num) && !is.na(y2_num)
  has_len   <- !is.null(period_yrs) && !is.na(period_yrs) && period_yrs > 0

  if (has_years && has_len) {
    paste0(y1_clean, sep, y2_clean, " (", round(period_yrs, 1), " yrs)")
  } else if (has_years) {
    paste0(y1_clean, sep, y2_clean)
  } else if (has_len) {
    paste0(round(period_yrs, 1), " years")
  } else {
    "N/A"
  }
}

#' Format Period String with Length
#' 
#' Parses an existing period string (e.g. "2000-2010") and appends the calculated length in years.
#' 
#' @param p_str Character period string.
#' @param period_yrs Optional explicit numeric length in years.
#' @return Formatted character string, e.g. "2000 – 2010 (10 yrs)".
format_period_str_with_length <- function(p_str, period_yrs = NULL) {
  if (is.null(p_str) || is.na(p_str)) return("N/A")
  p_str <- trimws(as.character(p_str))
  if (!nzchar(p_str) || identical(p_str, "N/A")) return("N/A")

  # If already contains "(... yrs)" or "(... years)", return as-is
  if (grepl("\\(\\s*\\d+(\\.\\d+)?\\s*(yrs?|years?)\\s*\\)", p_str, ignore.case = TRUE)) {
    return(p_str)
  }

  # Match two 4-digit years with separator: e.g. "2000 - 2010", "2000–2010", "2000 → 2010", "2000 to 2010"
  m <- regmatches(p_str, regexec("^(\\d{4})\\s*[-–—→/to]+\\s*(\\d{4})$", p_str))[[1]]
  if (length(m) == 3) {
    y1 <- as.numeric(m[2])
    y2 <- as.numeric(m[3])
    diff_y <- if (!is.null(period_yrs) && !is.na(period_yrs) && period_yrs > 0) period_yrs else abs(y2 - y1)
    if (diff_y > 0) {
      return(paste0(m[2], " \u2013 ", m[3], " (", round(diff_y, 1), " yrs)"))
    } else {
      return(paste0(m[2], " \u2013 ", m[3]))
    }
  }

  if (!is.null(period_yrs) && !is.na(period_yrs) && period_yrs > 0) {
    return(paste0(p_str, " (", round(period_yrs, 1), " yrs)"))
  }

  p_str
}

# Prefer local abacuslib widget if available
local_abacuslib_dir <- "d:/google_drive/ecomodels/code/R/widget/abacuslib"
abacuslib_loaded <- FALSE

if (dir.exists(local_abacuslib_dir)) {
  # Clean up any previously sourced conflicts in .GlobalEnv before loading package
  conflict_fns <- c("abacost_curve", "abacost_curveOutput", "abacostCurve",
                    "abacostCurveOutput", "renderAbacost_curve", "renderAbacostCurve")
  rm(list = intersect(conflict_fns, ls(envir = .GlobalEnv)), envir = .GlobalEnv)

  tryCatch({
    if (requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(local_abacuslib_dir, export_all = TRUE)
      abacuslib_loaded <- "package:abacuslib" %in% search() || isNamespaceLoaded("abacuslib")
    } else if (requireNamespace("devtools", quietly = TRUE)) {
      devtools::load_all(local_abacuslib_dir, export_all = TRUE)
      abacuslib_loaded <- "package:abacuslib" %in% search() || isNamespaceLoaded("abacuslib")
    }
  }, error = function(e) {
    message("pkgload/devtools load_all failed for abacuslib: ", conditionMessage(e))
  })
}

if (!abacuslib_loaded) {
  if (!requireNamespace("abacuslib", quietly = TRUE)) {
    if (dir.exists(local_abacuslib_dir) && requireNamespace("remotes", quietly = TRUE)) {
      tryCatch(remotes::install_local(local_abacuslib_dir, upgrade = "never", force = TRUE), error = function(e) NULL)
    }
    if (!requireNamespace("abacuslib", quietly = TRUE)) {
      # Try pak first with non-interactive flag
      tryCatch({
        if (!requireNamespace("pak", quietly = TRUE)) {
          install.packages("pak", repos = "https://cloud.r-project.org")
        }
        if (requireNamespace("pak", quietly = TRUE)) {
          pak::pkg_install("degi/abacuslib", ask = FALSE)
        }
      }, error = function(e) {
        message("Automatic installation of abacuslib via pak failed: ", conditionMessage(e))
      })
    }
    if (!requireNamespace("abacuslib", quietly = TRUE)) {
      # Fallback to remotes::install_github
      tryCatch({
        if (!requireNamespace("remotes", quietly = TRUE)) {
          install.packages("remotes", repos = "https://cloud.r-project.org")
        }
        if (requireNamespace("remotes", quietly = TRUE)) {
          remotes::install_github("degi/abacuslib", upgrade = "never")
        }
      }, error = function(e) {
        message("Automatic installation of abacuslib via remotes failed: ", conditionMessage(e))
      })
    }
  }

  if (requireNamespace("abacuslib", quietly = TRUE)) {
    if (!"package:abacuslib" %in% search()) {
      tryCatch(library(abacuslib), error = function(e) {
        warning("Failed to load installed abacuslib: ", conditionMessage(e))
      })
    }
    abacuslib_loaded <- "package:abacuslib" %in% search() || isNamespaceLoaded("abacuslib")
  }
}

if (abacuslib_loaded || requireNamespace("abacuslib", quietly = TRUE)) {
  if (!exists("abacostCurve", mode = "function")) {
    local_abacost <- file.path(local_abacuslib_dir, "R", "abacost_curve.R")
    if (file.exists(local_abacost)) {
      tryCatch(source(local_abacost), error = function(e) NULL)
    }
  }
  if (!exists("abacuslib", mode = "function") && exists("scenario_editor", mode = "function")) {
    abacuslib <- scenario_editor
  }
  if (!exists("abacuslibOutput", mode = "function") && exists("scenario_editor_output", mode = "function")) {
    abacuslibOutput <- scenario_editor_output
  }
  if (!exists("renderAbacuslib", mode = "function") && exists("render_scenario_editor", mode = "function")) {
    renderAbacuslib <- render_scenario_editor
  }
}

# Robust fallback dummy definitions: ensures the Shiny app loads cleanly without
# crashing even if 'abacuslib' is missing, not installed, or failed to compile
if (!exists("scenario_editor_output", mode = "function")) {
  scenario_editor_output <- function(outputId, width = "100%", height = "560px") {
    shiny::div(
      class = "alert alert-warning m-3 p-3 border shadow-sm",
      style = "border-radius: 8px;",
      shiny::tags$h5(class = "alert-heading fw-bold mb-2", shiny::icon("triangle-exclamation"), " 'abacuslib' Package Required"),
      shiny::tags$p("The interactive scenario editor widget requires the ", shiny::tags$code("abacuslib"), " package from GitHub."),
      shiny::tags$p("To enable this interactive widget, run the following in your R console:"),
      shiny::tags$pre(class = "bg-light p-2 rounded border small", "pak::pak(\"degi/abacuslib\")\n# or:\nremotes::install_github(\"degi/abacuslib\")")
    )
  }
}
if (!exists("abacuslibOutput", mode = "function")) {
  abacuslibOutput <- scenario_editor_output
}
if (!exists("render_scenario_editor", mode = "function")) {
  render_scenario_editor <- function(expr, env = parent.frame(), quoted = FALSE) {
    shiny::renderUI({ NULL })
  }
}
if (!exists("renderAbacuslib", mode = "function")) {
  renderAbacuslib <- render_scenario_editor
}
if (!exists("scenario_editor", mode = "function")) {
  scenario_editor <- function(...) NULL
}
if (!exists("abacuslib", mode = "function")) {
  abacuslib <- scenario_editor
}
if (!exists("abacostCurveOutput", mode = "function")) {
  abacostCurveOutput <- function(outputId, width = "100%", height = "560px") {
    shiny::div(
      class = "alert alert-warning m-3 p-3 border shadow-sm",
      style = "border-radius: 8px;",
      shiny::tags$h5(class = "alert-heading fw-bold mb-2", shiny::icon("triangle-exclamation"), " 'abacuslib' Package Required"),
      shiny::tags$p("The Abatement Cost Curve widget requires the ", shiny::tags$code("abacuslib"), " package from GitHub."),
      shiny::tags$p("To enable this interactive widget, run the following in your R console:"),
      shiny::tags$pre(class = "bg-light p-2 rounded border small", "pak::pak(\"degi/abacuslib\")\n# or:\nremotes::install_github(\"degi/abacuslib\")")
    )
  }
}
if (!exists("abacost_curveOutput", mode = "function")) {
  abacost_curveOutput <- abacostCurveOutput
}
if (!exists("renderAbacostCurve", mode = "function")) {
  renderAbacostCurve <- function(expr, env = parent.frame(), quoted = FALSE) {
    shiny::renderUI({ NULL })
  }
}
if (!exists("renderAbacost_curve", mode = "function")) {
  renderAbacost_curve <- renderAbacostCurve
}
if (!exists("abacost_curve", mode = "function")) {
  abacost_curve <- function(...) NULL
}
if (!exists("abacostCurve", mode = "function")) {
  abacostCurve <- abacost_curve
}

# Source abacus projection helpers
if (file.exists("R/abacus_projection.R")) {
  source("R/abacus_projection.R")
}

# Source abacus module documentation views
if (file.exists("R/abacus_info_docs.R")) {
  source("R/abacus_info_docs.R")
}

if (!exists("popover", mode = "function")) {
  popover <- function(obj, ..., title = NULL, content = NULL, id = NULL,
                      placement = "right", trigger = "click") {
    args <- list(...)

    if (is.null(content) && length(args) > 0) {
      if (identical(names(args), "content")) {
        content <- args[["content"]]
      } else {
        content <- htmltools::tagList(args)
      }
    }

    if (is.null(id) && !is.null(obj$attribs$id)) {
      id <- obj$attribs$id
    }
    if (is.null(id)) {
      id <- paste0("popover_", as.integer(runif(1) * 1e8))
    }
    if (is.null(content)) {
      content <- ""
    }

    obj <- htmltools::tagAppendAttributes(
      obj,
      id = id,
      title = title,
      `data-toggle` = "popover",
      `data-trigger` = trigger,
      `data-placement` = placement,
      `data-content` = as.character(content),
      `data-html` = "true"
    )

    htmltools::tagList(
      obj,
      htmltools::singleton(htmltools::tags$script(HTML("
        Shiny.addCustomMessageHandler('togglePopover', function(message) {
          var element = $('#' + message.id);
          if (element.length) element.popover(message.action);
        });
      "))),
      htmltools::tags$script(HTML(sprintf("$(function() { if ($('#%s').length) { $('#%s').popover({ html: true, trigger: '%s', placement: '%s' }); } });", id, id, trigger, placement)))
    )
  }
}

if (!exists("toggle_popover", mode = "function")) {
  toggle_popover <- function(id, show = FALSE) {
    action <- if (isTRUE(show)) "show" else "hide"
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) {
      session$sendCustomMessage("togglePopover", list(id = id, action = action))
    } else {
      message(sprintf("toggle_popover(%s, %s) requires an active Shiny session.", id, action))
    }
  }
}

#' Create Drag and Drop File Input
#' 
#' Constructs a styled drag-and-drop zone widget for uploading raster and shapefile datasets.
#' 
#' @param input_id Shiny file input ID.
#' @param accept File extensions / MIME types accepted.
#' @param helper_text Instruction text displayed inside the dropzone.
#' @return A Shiny tagList containing HTML markup, styles, and event listeners.
drag_drop_file_input <- function(input_id, accept = NULL, helper_text = "Drag and drop a file here") {
  accept_value <- if (is.null(accept)) "" else paste(accept, collapse = ",")
  tagList(
    tags$style(HTML(".drag-drop-upload { border: 2px dashed #cbd5e1; border-radius: 8px; padding: 16px; background: #f8fafc; text-align: center; cursor: pointer; min-height: 300px; height: 300px; display: flex; align-items: center; justify-content: center; flex-direction: column; gap: 0.5rem; transition: all 0.2s ease; width: 100%; box-sizing: border-box; } .drag-drop-upload.drag-drop-over, .drag-drop-upload:hover { border-color: #BE191E; background: #fdf6f6; box-shadow: inset 0 0 0 1px rgba(190, 25, 30, 0.25); } .drag-drop-upload .drag-drop-icon { font-size: 2rem; color: #540b0e; transition: transform 0.2s ease; } .drag-drop-upload:hover .drag-drop-icon { transform: translateY(-2px); } .drag-drop-upload .drag-drop-title { font-weight: 600; color: #1e293b; font-size: 0.95rem; } .drag-drop-upload .drag-drop-hint { font-size: 0.82rem; color: #64748b; } .card-upload-dropzone-wrap { width: 100%; min-height: 300px; height: 300px; padding: 0; } .card-upload-dropzone-wrap .drag-drop-upload { margin: 0; } .map-upload-layout { width: 100%; } .map-upload-layout > * { min-height: 300px; }")),
    tags$div(
      class = "drag-drop-upload",
      id = paste0(input_id, "_dropzone"),
      onclick = sprintf("document.getElementById('%s').click();", input_id),
      tags$div(class = "drag-drop-icon", icon("cloud-arrow-up")),
      tags$div(class = "drag-drop-title", helper_text),
      tags$div(class = "drag-drop-hint", "or choose a file"),
      tags$div(style = "display:none;",
        fileInput(input_id, NULL, accept = accept_value, placeholder = "Upload file")
      )
    ),
    tags$script(HTML(paste0("(function() { const dropzone = document.getElementById('", input_id, "_dropzone'); const input = document.getElementById('", input_id, "'); if (!dropzone || !input) return; const activate = function(event) { event.preventDefault(); event.stopPropagation(); dropzone.classList.add('drag-drop-over'); }; const deactivate = function(event) { event.preventDefault(); event.stopPropagation(); dropzone.classList.remove('drag-drop-over'); }; const handleDrop = function(event) { event.preventDefault(); event.stopPropagation(); deactivate(event); const files = event.dataTransfer && event.dataTransfer.files ? event.dataTransfer.files : null; if (!files || !files.length) return; const dt = new DataTransfer(); Array.from(files).forEach(function(file) { dt.items.add(file); }); input.files = dt.files; input.dispatchEvent(new Event('change', { bubbles: true })); }; ['dragenter', 'dragover'].forEach(function(evt) { dropzone.addEventListener(evt, activate); }); ['dragleave', 'drop'].forEach(function(evt) { dropzone.addEventListener(evt, deactivate); }); dropzone.addEventListener('drop', handleDrop); })();")))
  )
}

#' Guess Acquisition Year from Filename
#' 
#' Extracts a 4-digit calendar year (1800-current) from a filename string.
#' 
#' @param filename File name or path string.
#' @param current_year Integer maximum allowable year.
#' @return Integer calendar year, or current_year if none detected.
guess_acquisition_year <- function(filename, current_year = as.integer(format(Sys.Date(), "%Y"))) {
  matches <- regmatches(
    filename,
    gregexpr("(?<![0-9])[0-9]{4}(?![0-9])", filename, perl = TRUE)
  )[[1]]
  years <- as.integer(matches)
  years <- years[!is.na(years) & years >= 1800 & years <= current_year]
  if (length(years) == 0)
    return(current_year)
  tail(years, 1)
}

abacus_palettes <- c("#335c67","#66827a","#99a88c","#fff3b0","#f0c977","#e09f3e","#bf6535","#9e2a2b","#791b1d","#540b0e")
logo_color <- "#ED2839"

legend_set_choices <- c(
  "REDD Abacus" = "abacus",
  "Green-Yellow-Red" = "gyr",
  "Default" = "default",
  "Set 1" = "Set1",
  "Set 2" = "Set2",
  "Set 3" = "Set3",
  "Dark 2" = "Dark2",
  "Paired" = "Paired",
  "Tableau 10" = "Tableau10",
  "Pastel" = "Pastel",
  "Viridis" = "Viridis",
  "Spectral" = "Spectral"
)

#' Retrieve Color Palette for Land Cover Legend
#' 
#' Generates an n-color interpolated palette for a named palette set.
#' Supports "abacus" (official REDD Abacus palette), Brewer sets, Viridis, Tableau10, and custom scales.
#' 
#' @param set_name Character name of the color palette set.
#' @param n Integer number of distinct colors to generate.
#' @return Character vector of hexadecimal color strings.
get_legend_set_colors <- function(set_name = "default", n = 10) {
  n <- max(1, as.integer(n))
  colors <- switch(set_name,
    "abacus" = suppressWarnings(colorRampPalette(abacus_palettes)(n)),
    "Set1" = suppressWarnings(colorRampPalette(brewer.pal(min(9, max(3, n)), "Set1"))(n)),
    "Set2" = suppressWarnings(colorRampPalette(brewer.pal(min(8, max(3, n)), "Set2"))(n)),
    "Set3" = suppressWarnings(colorRampPalette(brewer.pal(min(12, max(3, n)), "Set3"))(n)),
    "Dark2" = suppressWarnings(colorRampPalette(brewer.pal(min(8, max(3, n)), "Dark2"))(n)),
    "Paired" = suppressWarnings(colorRampPalette(brewer.pal(min(12, max(3, n)), "Paired"))(n)),
    "Tableau10" = suppressWarnings(colorRampPalette(as.character(paletteer_d("ggthemes::Tableau_10")))(n)),
    "Pastel" = suppressWarnings(colorRampPalette(brewer.pal(min(9, max(3, n)), "Pastel1"))(n)),
    "Viridis" = hcl.colors(n, "Viridis"),
    "Spectral" = suppressWarnings(colorRampPalette(brewer.pal(min(11, max(3, n)), "Spectral"))(n)),
    "gyr" = suppressWarnings(colorRampPalette(c("#004225", "#FFD700", "#E60026", "#663399"))(n)),
    suppressWarnings(colorRampPalette(c(
      as.character(paletteer_d("ggthemes::calc")),
      as.character(paletteer_d("ggsci::default_uchicago")),
      as.character(paletteer_d("ggthemes::Classic_10")),
      as.character(paletteer_d("RColorBrewer::Set1"))
    ))(n))
  )
  substr(colors, 1, 7)
}

#' Create Spatial Boundary for Study Area
#' 
#' Computes a unified polygon boundary from vector (sf) or raster (stars) data
#' and transforms coordinates to WGS84 (EPSG:4326) for leaflet mapping and GeoJSON export.
#' 
#' @param map A stars raster object or stars_proxy.
#' @param sf_data Optional sf vector object.
#' @param name Character name for the boundary feature.
#' @param type Character type description (e.g. "Land Cover", "Zonation").
#' @param year Optional acquisition year.
#' @return An sf polygon data frame in EPSG:4326, or NULL on failure.
create_map_boundary <- function(map, sf_data = NULL, name = "Study Area", type = "", year = "") {
  # --- Case 1: sf vector data ---
  if (!is.null(sf_data) && inherits(sf_data, "sf") && nrow(sf_data) > 0) {
    return(tryCatch({
      u <- sf::st_make_valid(suppressWarnings(sf::st_union(sf_data)))
      res <- sf::st_sf(
        name = name,
        type = type,
        year = as.character(year),
        geometry = sf::st_geometry(u)
      )
      sf::st_make_valid(sf::st_transform(res, 4326))
    }, error = function(e) NULL))
  }

  if (is.null(map)) return(NULL)

  crs_m <- tryCatch(sf::st_crs(map), error = function(e) NA)
  if (is.na(crs_m)) return(NULL)

  # --- Case 2: stars raster (memory or proxy) ---
  # Extract the boundary polygon surrounding all non-NA cells. Zero is a
  # legitimate cell value, so it must be included in the boundary.
  tryCatch({
    dims <- dim(map)
    nx <- dims[1]
    ny <- dims[2]
    max_dim <- max(nx, ny)
    ds <- if (max_dim > 800) as.integer(ceiling(max_dim / 800)) else 0L

    s <- if (inherits(map, "stars_proxy")) {
      suppressMessages(stars::st_as_stars(map, downsample = max(ds, 1L)))
    } else if (ds > 1L) {
      suppressMessages(stars::st_downsample(map, ds))
    } else {
      map
    }

    if (length(dim(s)) > 2) {
      s <- stars::adrop(s)
      if (length(dim(s)) > 2) s <- s[,,,1]
    }

    s <- s[1]
    vals <- s[[1]]
    mask <- !is.na(vals)
    if (!any(mask)) return(NULL)

    s[[1]] <- as.integer(mask)

    vec <- suppressWarnings(sf::st_as_sf(s, as_points = FALSE, merge = TRUE))
    if (is.null(vec) || nrow(vec) == 0) return(NULL)

    val_col <- names(vec)[1]
    valid_poly <- vec[vec[[val_col]] == 1L, ]
    if (nrow(valid_poly) == 0) return(NULL)

    boundary <- sf::st_make_valid(suppressWarnings(sf::st_union(valid_poly)))
    res <- sf::st_sf(
      name = name,
      type = type,
      year = as.character(year),
      geometry = sf::st_geometry(boundary),
      crs = crs_m
    )
    sf::st_make_valid(sf::st_transform(res, 4326))
  }, error = function(e) {
    message("create_map_boundary error: ", conditionMessage(e))
    NULL
  })
}

#' Reverse Geocode Coordinates via Nominatim
#' 
#' Queries OpenStreetMap Nominatim API for administrative district, province,
#' and country details at a given latitude and longitude.
#' 
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @return A list with country, province, district, and display_name, or NULL on failure.
lookup_geographic_location <- function(lat, lon) {
  if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) return(NULL)

  url <- sprintf(
    "https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat=%.5f&lon=%.5f&zoom=10&accept-language=en",
    lat, lon
  )

  tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(h, "User-Agent" = "REDD-Abacus/1.0 (shiny-ecomodels)")
    curl::handle_setopt(h, timeout = 3, connecttimeout = 2)
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code != 200) return(NULL)

    dat <- jsonlite::fromJSON(rawToChar(resp$content))
    addr <- dat$address
    if (is.null(addr)) return(NULL)

    country <- if (!is.null(addr$country)) addr$country else NULL
    state <- if (!is.null(addr$state)) {
      addr$state
    } else if (!is.null(addr$province)) {
      addr$province
    } else if (!is.null(addr$region)) {
      addr$region
    } else NULL

    district <- if (!is.null(addr$county)) {
      addr$county
    } else if (!is.null(addr$state_district)) {
      addr$state_district
    } else if (!is.null(addr$city)) {
      addr$city
    } else if (!is.null(addr$municipality)) {
      addr$municipality
    } else NULL

    list(
      country = country,
      province = state,
      district = district,
      display_name = dat$display_name
    )
  }, error = function(e) {
    NULL
  })
}

#' Create Card Header Download Button
#' 
#' Generates a compact icon download link designed for card headers.
#' 
#' @param output_id Shiny downloadHandler output ID.
#' @param label Optional text label next to icon.
#' @param title Tooltip title.
#' @param class CSS classes.
#' @return An HTML link tag.
card_download_button <- function(output_id, label = NULL, title = "Download", class = "card-toolbar-btn") {
  tags$a(
    id = output_id,
    class = paste("shiny-download-link", class),
    href = "",
    target = "_blank",
    download = NA,
    title = title,
    `aria-label` = title,
    icon("download"),
    if (!is.null(label) && nzchar(label)) span(style = "margin-left: 4px;", label) else NULL
  )
}

#' Create Chart Export & Download Toolbar
#' 
#' Generates a toolbar popover menu with options to export a chart as PNG, SVG, HTML widget, or CSV data.
#' 
#' @param button_id ID of the toolbar trigger button.
#' @param chart_container_selector CSS selector of the chart DOM container.
#' @param base_filename Default filename (without extension).
#' @param html_download_id Shiny download output ID for HTML format.
#' @param csv_download_id Shiny download output ID for CSV data.
#' @param title Popover title.
#' @return A toolbar UI element.
chart_download_toolbar <- function(button_id, chart_container_selector, base_filename, html_download_id, csv_download_id, title = "Download Chart") {
  toolbar(
    popover(
      trigger = toolbar_input_button(
        id      = button_id,
        label   = "Download",
        icon    = shiny::icon("download"),
        tooltip = FALSE
      ),
      title = title,
      tags$div(
        class = "d-flex flex-column gap-2",
        style = "min-width: 175px;",
        tags$button(
          type = "button",
          class = "btn btn-sm btn-primary w-100 text-start d-flex align-items-center gap-2",
          onclick = sprintf("downloadChartAsPng('%s', '%s.png');", chart_container_selector, base_filename),
          icon("image"), span("Download PNG")
        ),
        tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary w-100 text-start d-flex align-items-center gap-2",
          onclick = sprintf("downloadChartAsSvg('%s', '%s.svg');", chart_container_selector, base_filename),
          icon("file-code"), span("Download SVG")
        ),
        tags$a(
          id = html_download_id,
          class = "btn btn-sm btn-outline-primary w-100 text-start d-flex align-items-center gap-2 shiny-download-link",
          href = "",
          target = "_blank",
          download = NA,
          icon("file-lines"), span("Download HTML")
        ),
        tags$a(
          id = csv_download_id,
          class = "btn btn-sm btn-secondary w-100 text-start d-flex align-items-center gap-2 shiny-download-link",
          href = "",
          target = "_blank",
          download = NA,
          icon("table"), span("Download CSV")
        )
      ),
      id = paste0(button_id, "_pop"),
      placement = "bottom"
    ),
    align = "right"
  )
}

#' Build Uploaded Map Card UI
#' 
#' Constructs a modular card for an uploaded raster map (Land Cover or Zonation).
#' For zonation maps, includes a collapsible sidebar containing the editable zone legend table.
#' 
#' @param map_type Display label for map type/year.
#' @param file_name Original file name of the map.
#' @param output_id Shiny plot output ID.
#' @param card_id Optional DOM ID for the card.
#' @param full_screen Logical whether card can expand to full screen.
#' @param extra_ui Additional UI content (e.g. zone legend table).
#' @param menu_ui Optional popover menu UI.
#' @param delete_ui Optional delete action button.
#' @param download_ui Optional download link button.
#' @param edit_ui Optional edit modal action button.
#' @return A Shiny card UI component wrapped in a grid item div.
uploaded_map_card <- function(map_type, file_name, output_id, card_id = NULL, full_screen = FALSE, extra_ui = NULL, menu_ui = NULL, delete_ui = NULL, download_ui = NULL, edit_ui = NULL) {
  header_theme <- if (startsWith(map_type, "Zone ")) "bg_zone_theme" else "bg_lc_theme"
  is_zone <- !is.null(extra_ui)
  map_out <- plotOutput(output_id, width = "100%", height = "100%")
  ## Zonation cards use layout_sidebar() INSIDE a single card_body():
  ##   main    = the map
  ##   sidebar = the zone legend
  ## This replaces the old navset_card_tab() that split map and legend over two
  ## tabs, so both are visible at once and the map keeps the card's height.
  ##
  ## Note: bslib 0.11's card() has no `sidebar` parameter (only full_screen,
  ## height, max_height, min_height, fill, class, wrapper, id). Passing one is
  ## silently swallowed by `...`, rendered as a bogus `sidebar="..."` HTML
  ## attribute, and the legend disappears from the card.
  ## The map is given an explicit 245px height (card 300px minus the header)
  ## because a plotOutput/fill chain sized this way collapses to a sliver.
  body <- if (is_zone) {
    card_body(
      class = "p-0 m-0 map-card-body",
      padding = 0,
      gap = 0,
      fill = TRUE,
      fillable = TRUE,
      layout_sidebar(
        class = "p-0 m-0",
        fillable = TRUE,
        fill = TRUE,
        border = FALSE,
        border_radius = FALSE,
        gap = 0,
        padding = 0,
        sidebar = sidebar(
          id = paste0("zone_sidebar_", sub("_card$", "", card_id)),
          ## One grid column (~309px), so the legend is narrow; because it starts
          ## CLOSED the map gets the full card width until the user opens it.
          width = 210,
          padding = 4,
          gap = 4,
          class = "map-zone-sidebar",
          ## Collapsible and CLOSED by default, like any collapsible sidebar: the
          ## card keeps the same footprint as a land cover card and the legend
          ## is one click away. (open = "always" removes the toggle entirely.)
          ## No `title=`: it renders an extra title row under the chevron and
          ## pushed the legend past the card bottom.
          open = "closed",
          div(class = "map-zone-sidebar-table", extra_ui)
        ),
        div(
          class = "map-zone-map-plot",
          ## Fill whatever height the sidebar/main area has rather than a fixed
          ## 245px, which cropped the raster at the bottom of the card.
          style = "align-self: stretch; height: 100%;",
          plotOutput(output_id, width = "100%", height = "100%")
        )
      )
    )
  } else {
    card_body(class = "p-0 m-0 map-card-body", fill = TRUE, map_out)
  }

  title_ui <- if (!is.null(card_id)) {
    rec_id <- sub("^uploaded_", "", sub("_card$", "", card_id))
    tags$span(id = paste0("map_card_title_", rec_id), class = "shiny-html-output", map_type)
  } else {
    map_type
  }
  header_left <- span(icon("layer-group"), " ", title_ui)
  ## Header actions: the edit button, download button, zone options menu, and
  ## remove button.
  header_actions <- if (!is.null(menu_ui) || !is.null(delete_ui) || !is.null(download_ui) || !is.null(edit_ui)) {
    tags$div(
      class = "d-flex align-items-center gap-1",
      if (!is.null(edit_ui)) edit_ui else NULL,
      if (!is.null(download_ui)) download_ui else NULL,
      if (!is.null(menu_ui)) tags$div(class = "map-card-menu", menu_ui) else NULL,
      if (!is.null(delete_ui)) delete_ui else NULL
    )
  } else {
    NULL
  }
  header_content <- if (!is.null(header_actions)) {
    tags$div(
      class = "d-flex justify-content-between align-items-center w-100",
      header_left,
      header_actions
    )
  } else {
    header_left
  }

  tags$div(
    id = if (!is.null(card_id)) paste0("wrap_", card_id) else NULL,
    ## Fill the grid cell: the card sits in a 3-column grid, so a 560px cap only
    ## wasted space and squeezed the map next to the zone sidebar.
    style = "width: 100%;",
    ## Same footprint as a land cover card: one grid column. The legend
    ## sidebar starts closed so the map still gets the full card width.
    class = "map-card-wrap",
    title = file_name,
    ## A plain height (no fill/fillable) keeps this a regular card rather than
    ## bslib's `card-compact` variant, which caps the card at 350px wide.
    card(
      class = "map-card",
      style = "height: 300px;",
      card_header(header_content, class = header_theme),
      id = card_id,
      full_screen = full_screen,
      body
    )
  )
}


### UI TEXT IN MARKDOWN FORMAT ######################
main_head <-
  '<b>REDD-Abacus</b> is a tool for calculating land-based carbon emissions from land cover changes.
This application provides a simple estimation of future land cover area 
from the projection of the changes rate. The projection is iterated within a constrained map boundary area.
The projection rate modification is considered as a "<b>Land Use Planning</b>"
scenario, in which the future impact on carbon emission can be explored.'
main_desc <-
  "* **The minimum required data parameters** are a pair of time series **land cover maps (at 1 ha resolution)**
  and land-based **carbon stock (in tC ha<sup>-1</sup>)** for each land cover class.
* A zonation map can be added for your purposes. 
Notably, the projection of land cover changes will be based on each zonation area constrain. 
* Additional sources of land-based emissions can also be added.

This REDD-Abacus-2 is an update to the previous standalone application of REDD-Abacus-SP version 1. 
You may refer to the same documentation for the background and practical application of the tool."
  
other_emission_head <-
  "These are additional land-based emission from any other sources 
(i.e peat emission, fertilization, etc)"
other_emission_desc <-
  '* The unit is tCOe ha<sup>-1</sup> yr<sup>-1</sup>, which means emission in tCO2-equivalent
  on a land-based sacle (ha<sup>-1</sup>) and yearly time-average (yr<sup>-1</sup>).
* The emission factor for converted land cover is calculated as the average of 
emission factor between before-and-after converted land covers.
* Multiple selections can be applied to zones and land covers to set a similar emission factor.
* The table rows can be added by right-clicking on the table and selecting the "Insert row..." menu.'

### TABLE DEFINITION #################################
table_file_df <- data.frame(
  var = c(
    "lc_list_df",
    "cstock_list",
    "npv_list",
    "zone_list_df",
    "other_emission_df"
  ),
  file = c(
    "landcover.csv",
    "carbonstock.csv",
    "npv.csv",
    "zonation.csv",
    "other_emission.csv"
  ),
  label = c(
    "Land cover legend",
    "Carbon stock data",
    "Net Present Value (NPV) data",
    "Zonation legend",
    "Other emission data"
  )
)

scenario_file_df <- data.frame(
  var = c(
    "tpm",
    "baseline_tpm",
    "baseline_area",
    "landcover",
    "new_lc_id"
  ),
  file = c(
    "scenario_modif",
    "scenario_tpm",
    "scenario_area",
    "scenario_lc",
    "scenario_newlc"
  ),
  label = c(
    "Scenario modified projection rate",
    "Default projection rate",
    "Default land cover area",
    "Land cover list",
    "Added land cover"
  )
)


map_file_df <- data.frame(
  var = c("map1_stars", "map2_stars", "mapz_stars"),
  file = c("map1.tif", "map2.tif", "map_zone.tif"),
  label = c("Time series map #1", "Time series map #2", "Zonation map")
)

output_table_file_df <- data.frame(
  var = c(
    "abacus_baseline.projection.lc_area",
    "abacus_baseline.emission.lc_emission"
  ),
  file = c(
    "baseline_projection.csv",
    "baseline_emission_projection.csv"
  ),
  label = c(
    "Baseline land cover change projection",
    "Baseline emission projection"
  )
)

output_scenario_file_df <- data.frame(
  var = c(
    "abacus_scenario.projection.lc_area",
    "abacus_scenario.emission.lc_emission"
  ),
  file = c(
    "scenario_projection",
    "scenario_emission_projection"
  ),
  label = c(
    "Scenario land cover change projection",
    "Scenario emission projection"
  )
)

output_map_file_df <- data.frame(
  var = c("mapc_stars", "map_ghg_stars", "map_all_stars"),
  file = c(
    "map_c_emission.tif",
    "map_other_emission.tif",
    "map_all_emission.tif"
  ),
  label = c(
    "Map of carbon emission factor",
    "Map of additional emission factor",
    "Map of all combined emission factor"
  )
)


### abacuslib handler

#' Convert Client JSON Input to Data Frame
#' 
#' Shiny input handler callback that parses incoming JSON strings from widgets into R data frames.
#' 
#' @param x JSON string received from client.
#' @param session Active Shiny session.
#' @param inputname Name of the Shiny input.
#' @return Parsed data frame or list.
jslist_to_df <- function(x, session, inputname) {
  fromJSON(x)
}
try(removeInputHandler("js_to_df"), silent = TRUE)
registerInputHandler("js_to_df", jslist_to_df)

#' Convert Factor Raster to Numeric Matrix
#' 
#' Converts categorical factor levels in a single-attribute stars raster to numeric values.
#' 
#' @param map Single-attribute stars raster object.
#' @return Stars raster with numeric cell values.
map_factor_to_numeric <- function(map) {
  dims <- dim(map[[1]])
  vals <- suppressWarnings(as.numeric(as.character(map[[1]])))
  dim(vals) <- dims
  map[[1]] <- vals
  map
}

#' Calculate Single Pixel Area in Hectares
#' 
#' Computes the surface area of a single grid cell in hectares from raster dimension deltas.
#' 
#' @param m A stars raster object with projected coordinates (meters).
#' @return Numeric pixel area in hectares (ha), or NA_real_ if unprojected/invalid.
pixel_ha <- function(m) {
  if (is.null(m) || !inherits(m, "stars")) return(NA_real_)
  crs_obj <- tryCatch(sf::st_crs(m), error = function(e) NA)
  if (is.na(crs_obj) || sf::st_is_longlat(crs_obj)) return(NA_real_)
  dims <- tryCatch(stars::st_dimensions(m), error = function(e) NULL)
  if (is.null(dims) || is.null(dims[["x"]]) || is.null(dims[["y"]])) return(NA_real_)
  dx <- abs(dims[["x"]]$delta)
  dy <- abs(dims[["y"]]$delta)
  if (is.na(dx) || is.na(dy) || dx <= 0 || dy <= 0) return(NA_real_)
  (dx * dy) / 10000   # m² → ha
}

#' Reclassify Raster Cell Values
#' 
#' Applies a transition or reclassification mapping matrix to replace raster cell IDs with new target values.
#' 
#' @param map Stars raster to reclassify.
#' @param fromto_df Data frame with 2 columns: source value (`from`) and target value (`to`).
#' @return Reclassified stars raster object.
reclassify_map <- function(map, fromto_df) {
  if (inherits(map, "stars_proxy")) {
    map <- stars::st_as_stars(map)
  }
  fromto_df <- fromto_df[order(fromto_df[, 1]), ]
  from <- fromto_df[[1]]
  to <- fromto_df[[2]]
  map2 <- cut(map, c(min(from) - 1, from), labels = to)
  map_factor_to_numeric(map2)
}

#' Format CO2-equivalent HTML Unit Tag
#' 
#' Produces a typographic CO2e unit label with proper subscript "2".
#' 
#' @param prefix Optional string prefix (e.g. "(").
#' @param suffix Optional string suffix (e.g. ")").
#' @return An HTML tagList.
co2_unit <- function(prefix = "", suffix = "") {
  tags$html(
    paste0(prefix, "CO"),
    tags$sub(2, .noWS = c("after", "before")),
    paste0("e", suffix),
    .noWS = c("after", "before")
  )
}

#' Format per-Hectare HTML Unit Tag
#' 
#' Produces a typographic ha⁻¹ unit label with proper superscript "-1".
#' 
#' @param prefix Optional string prefix.
#' @param suffix Optional string suffix.
#' @return An HTML span tag.
per_ha_unit <- function(prefix = "", suffix = "") {
  span(
    paste0(prefix, "ha"),
    tags$sup(-1, .noWS = c("after", "before")),
    suffix,
    .noWS = c("after", "before")
  )
}

#' Default Coalescing Operator
#' 
#' Returns `a` if it is non-null, non-empty, and non-blank; otherwise returns `b`.
#' 
#' @param a Primary value.
#' @param b Fallback value.
#' @return Either `a` or `b`.
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && nzchar(as.character(a)[1])) a else b
}

#' Create ZIP Archive from Directory
#' 
#' Compresses a directory into a ZIP archive using the fastest available system utility
#' (Windows tar -> PowerShell Compress-Archive -> utils::zip fallback).
#' 
#' @param zipfile Destination path for the .zip archive.
#' @param source_dir Source directory whose contents will be archived.
#' @return Logical TRUE if zip creation succeeded with non-zero size, FALSE otherwise.
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

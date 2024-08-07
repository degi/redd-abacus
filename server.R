
server <- function(input, output, session) {
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
  emission_color <- colorRamp(c("darkgreen", "white", "red3"))
  emission_color_min <- colorRamp(c("darkgreen", "white"))
  emission_color_max <- colorRamp(c("white", "red3"))
  
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
  
  v <- reactiveValues(
    title = "",
    description = "",
    abacus_file = NULL,
    abacus_data = NULL,
    lc_list_df = NULL,
    zone_list_df = NULL,
    lc_changes_df = NULL,
    cstock_list = NULL,
    abacus_baseline = NULL,
    selected_project_id = 1,
    selected_scenario_id = 1,
    n_iteration = 5,
    baseline_period = c(0, 1),
    period_year = NULL,
    map1_date = NULL,
    map2_date = NULL,
    map1_file = NULL,
    map2_file = NULL,
    mapz_file = NULL,
    map1_stars = NULL,
    map2_stars = NULL,
    mapz_stars = NULL,
    mapc_stars = NULL,
    map_other_stars = NULL,
    map_all_stars = NULL,
    map1_df = NULL,
    map2_df = NULL,
    mapz_df = NULL,
    c_emission_total = 0,
    c_sequestration_total = 0,
    other_emission_df = data.frame(),
    other_emission_source = NULL,
    other_emission_total = 0,
    other_sequestration_total = 0,
    all_emission_total = 0,
    all_sequestration_total = 0,
    scenario_list = list()
  )
  
  lc_legend_pal = NULL
  zone_legend_pal = NULL
  
  ### Conditional panel UI logic ###
  conditional_id <-
    c(
      "is_map1",
      "is_map2",
      "is_mapz",
      "is_lc_list",
      "is_matrix",
      "is_zone_list",
      "is_cstock_list"
    )
  conditional_v <-
    c(
      "map1_stars",
      "map2_stars",
      "mapz_stars",
      "lc_list_df",
      "lc_changes_df",
      "zone_list_df",
      "cstock_list"
    )
  
  mapply(function(id, val) {
    output[[id]] <- reactive({
      if (is.null(v[[val]]))
        return(FALSE)
      TRUE
    })
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }, conditional_id, conditional_v)
  
  
  ### Date input UI ###
  lapply(c("map1", "map2"), function(x) {
    id <- paste0(x, "_date")
    observeEvent(input[[id]], {
      v[[id]] <- input[[id]]
    })
  })
  
  other_lc_id <- function(id) {
    oid <- ifelse(id == "map1", "map2", "map1")
    odf <- isolate(v[[paste0(oid, "_df")]])
    if (!is.null(odf))
      return(odf$id)
    return(NULL)
  }
  
  check_map_input <- function(map_id) {
    #TODO: validate the map input
    reset_scenario()
    return(T)
  }
  
  crs <- NULL
  
  ### MAP INPUT UI ####################################
  lapply(c("map1", "map2", "mapz"), function(id) {
    inp <- paste0(id, "_file")
    # inp_b <- paste0(id, "_file_b")
    f <- paste0(id, "_file")
    out_id <- paste0(id, "_plot")
    st <- paste0(id, "_stars")
    vd <- paste0(id, "_df")
    #### map input ####
    observeEvent(input[[inp]], {
      if (!check_map_input(st))
        return()
      v[[f]] <- input[[inp]]
      m <- read_stars(v[[f]]$datapath)
      if(is.na(st_crs(m))) {
        toggle_popover(paste0(inp, "_pop"), F)
        showModal(modalDialog(HTML("Please upload the map with <b>Coordinate Reference System</b> included"),
                              title = "Map File Error"))
        return()
      }
      if (is.null(crs) || is.na(crs)) {
        crs <<- st_crs(m)
      } else {
        if(st_crs(m) != crs) {
          toggle_popover(paste0(inp, "_pop"), F)
          showModal(modalDialog(HTML("Please upload the map with the same <b>Coordinate Reference System</b> as previous uploaded map"),
                                title = "Map File Error"))
          return()
        }
      }
      reset_scenario()
      v[[st]] <- m
      toggle_popover(paste0(inp, "_pop"), F)
    })
    #### map input from header icon ####
    # observeEvent(input[[inp_b]], {
    #   if (!check_map_input(st))
    #     return()
    #   v[[f]] <- input[[inp_b]]
    #   v[[st]] <- read_stars(v[[f]]$datapath)
    #   toggle_popover(paste0(inp_b, "_pop"), F)
    # })
    #### map plot ####
    output[[out_id]] <- renderLeaflet({
      m <- v[[st]]
      if (is.null(m)) {
        return()
      }
      #### Generate id and area table ####
      df <- as.data.frame(table(v[[st]]))
      colnames(df) <- c("id", "area")
      df$id <- as.numeric(as.character(df$id))
      v[[vd]] <- df
      #### plot map ####
      if (id == "mapz") {
        c <- zone_legend_pal
      } else {
        df <- isolate(v$lc_list_df)
        other_id <- other_lc_id(id)
        update_lc_df(df, v[[vd]]$id, other_id)
        update_lc_matrix(isolate(v$map1_stars),
                         isolate(v$map2_stars),
                         isolate(v$mapz_stars))
        c <- lc_legend_pal
      }
      mbb <- get_map_bbox(m)
      
      lf <- leaflet(options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
        fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax)
      
      if (!is.null(c))
        lf <- lf %>%
        addStarsImage(m,
                      project = T,
                      colors = c,
                      layerId = 'ID')
      return(lf)
    })
    
    ### Full screen
    observeEvent(input[[paste0(id, "_card_full_screen")]], ({
      m <- isolate(v[[paste0(id, "_stars")]])
      if (is.null(m))
        return()
      mbb <- get_map_bbox(m)
      lp <- leafletProxy(paste0(id, "_plot"), session) %>%
        fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax)
      if (input[[paste0(id, "_card_full_screen")]]) {
        if (id == "mapz") {
          df <- isolate(v$zone_list_df)[c("zone_id", "label")]
          p <- zone_legend_pal
        } else {
          lc_ids <- isolate(v[[paste0(id, "_df")]]$id)
          df <- isolate(v$lc_list_df)
          df <- df[df$lc_id %in% lc_ids, c("lc_id", "label")]
          p <- lc_legend_pal
        }
        colnames(df) <- c("id", "label")
        lp %>% addLegend(
          pal = p,
          values = df$id,
          labFormat = labelFormat(
            transform = function(x) {
              paste0("[<b>", x, "</b>] ", df[df$id == x, "label"])
            }
          ),
          opacity = 1
        )  %>%
          addImageQuery(
            m,
            project = TRUE,
            prefix = 'Land cover',
            layerId = "ID",
            position = "topleft",
            group = "Land cover"
          ) %>%
          addLayersControl(overlayGroups = "ID", position = "bottomleft")
      } else {
        lp %>% clearControls() %>% removeLayersControl()
      }
    }))
  })
  
  #### Delete zone ####
  observeEvent(input$zone_delete_button, {
    v$mapz_stars <- NULL
    v$mapz_file <- NULL
    v$zone_list_df <- NULL
    toggle_popover("zone_delete_pop", F)
    update_lc_matrix(isolate(v$map1_stars),
                     isolate(v$map2_stars),
                     isolate(v$mapz_stars))
    
    reset_scenario()
  })
  
  reset_scenario <- function() {
    v$scenario_list <- list(generate_scenario_object(1))
    final_area$sc_lc_df <- NULL
    emission_val$sc_sum <- NULL
    update_scenario_select_options()
    v$selected_scenario_id <- 1
  }
  
  observeEvent(input$zone_cancel_button, {
    toggle_popover("zone_delete_pop", F)
  })
  
  get_map_bbox <- function(m) {
    bb <- st_bbox(st_transform(st_as_sfc(st_bbox(m)), 4326))
    return(as.list(bb))
  }
  
  plot_map <- function(id) {
    m <- v[[paste0(id, "_stars")]]
    if (is.null(m)) {
      return()
    }
    mbb <- get_map_bbox(m)
    if (id == "mapz") {
      c <- zone_legend_pal
    } else {
      c <- lc_legend_pal
    }
    leafletProxy(paste0(id, "_plot"), session) %>%
      clearImages() %>%
      fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax) %>%
      addStarsImage(m,
                    project = T,
                    colors = c,
                    layerId = 'ID')
  }
  
  observe({
    if (is.null(v$mapz_stars))
      return()
    zm <- as.data.frame(table(v$mapz_stars))
    oldz_df <- isolate(v$zone_list_df)
    znew_id <- as.numeric(as.character(zm[[1]]))
    if (is.null(oldz_df)) {
      df <-
        data.frame("zone_id" = znew_id)
      df$color <- get_color(znew_id)
      df$label <- paste0("Zone_", znew_id)
      df$description <- ""
      df$area <- zm[[2]]
      oldz_df <- isolate(v$zone_list_df)
      v$zone_list_df <- df
    } else {
      add_id <- setdiff(znew_id, oldz_df$zone_id)
      if (length(add_id) > 0) {
        df <-
          data.frame("zone_id" = add_id)
        df$color <- get_color(add_id)
        df$label <- paste0("Zone_", add_id)
        df$description <- ""
        df$area <- zm[add_id, 2]
        v$zone_list_df <- rbind(oldz_df, df)
      } else {
        # rem_id <- setdiff(oldz_df$zone_id, znew_id)
        df <- isolate(v$zone_list_df)
        v$zone_list_df <- df[df$zone_id %in% znew_id,]
      }

    }
    
    df <- isolate(v$zone_list_df)
    zone_legend_pal <<-
      colorFactor(df$color, df$zone_id, na.color = NA)
    plot_map("mapz")
    update_lc_matrix(isolate(v$map1_stars),
                     isolate(v$map2_stars),
                     isolate(v$mapz_stars))
  })
  
  output$box_area_out <- renderText({
    if (!is.null(v$lc_changes_df)) {
      a <- sum(v$lc_changes_df$area)
    } else {
      if (is.null(v$map1_df))
        a1 <- 0
      else
        a1 <- sum(v$map1_df$area)
      if (is.null(v$map2_df))
        a2 <- 0
      else
        a2 <- sum(v$map2_df$area)
      if (is.null(v$lc_zone))
        z <- 0
      else
        z <- sum(v$lc_zone$area)
      a <- max(a1, a2, z)
    }
    paste(f_number(a), "ha")
  })
  
  output$box_area_info <- renderText({
    if (is.null(v$lc_list_df))
      return()
    paste("Number of land covers:", nrow(v$lc_list_df))
  })
  
  output$box_period_out <- renderText({
    if (is.null(v$map1_date) || is.null(v$map2_date))
      return()
    f_number(v$period_year, "years", 3)
  })
  
  observe({
    v$period_year <-
      as.numeric(difftime(as.Date(v$map2_date), as.Date(v$map1_date), unit =
                            "weeks")) / 52.25
  })
  
  output$box_period_info <- renderText({
    if (is.null(v$map1_date) && is.null(v$map2_date))
      return()
    paste(format(v$map1_date, "%d-%b-%Y"),
          "↔",
          format(v$map2_date, "%d-%b-%Y"))
  })
  
  get_iteration_year <- function(iteration) {
    y0 <- format(v$map1_date, "%Y")
    y <- as.integer(y0) + round(v$period_year) * iteration
    return(y)
  }
  
  ### LAND COVER LIST ##############################
  update_lc_df <- function(prev_df, lc_id_list, other_id) {
    if (is.null(prev_df)) {
      df <- data.frame("lc_id" = lc_id_list)
      df$color <- map_color(length(lc_id_list))
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
        a_df <- data.frame(
          lc_id = dif,
          color = chart_color[sample.int(length(chart_color), length(dif))],
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
      tableHeight = "430px",
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
      df <- df_old[df_old$color %in% newcol, ]
      isolate({
        if (any(df$lc_id %in% v$map1_df$id))
          plot_map("map1")
        if (any(df$lc_id %in% v$map2_df$id))
          plot_map("map2")
      })
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
      df$color <- map_color(nrow(df_inp))
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
    }
    toggle_popover("lc_input_pop", F)
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
  
  #### Zone data upload ####
  observeEvent(input$zone_input_upload, {
    #TODO: if(!check_file_csv()) return()
    f <- input$zone_input_upload
    df_inp <- read.csv(f$datapath)
    if (nrow(df_inp) == 0)
      return()
    if (ncol(df_inp) < 2)
      return()
    if (!is_numeric_str(df_inp[1, 1]))
      return()
    df <- data.frame("zone_id" = as.integer(as.character(df_inp[, 1])))
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
      df$color <- map_color(nrow(df_inp))
      df$label <- df_inp[, 2]
      df$description <- ""
      if (ncol(df_inp) >= 3) {
        df$description <- df_inp[, 3]
      }
    }
    old_df <- isolate(v$zone_list_df)
    valid_id <- intersect(old_df$zone_id, df$zone_id)
    if (length(valid_id) > 0) {
      old_df[old_df$zone_id %in% valid_id, c("zone_id", "color", "label", "description")] <- df[df$zone_id %in% valid_id, ]
      v$zone_list_df <- old_df
    }
    toggle_popover("zone_input_pop", F)
  })
  
  #### Zonation list table ####
  zone_table_def <-
    c("zone_id", "color", "label", "description", "area")
  zone_column <- data.frame(
    title = c("ZONE_ID", "Color", "Zone", "Description", "Area (ha)"),
    type = c("numeric", "color", "text", "text", "numeric"),
    render = c(NA, "square", NA, NA, NA),
    align = c("right", "center", "left", "left", "right"),
    width = c(NA, NA, 150, 150, 100),
    mask = c(NA, NA, NA, NA, "#,##"),
    readOnly = c(T, F, F, F, T)
  )
  output$zone_list <- renderExcel({
    if (is.null(v$zone_list_df))
      return()
    #TODO: this is a hack to zoom the zone map, to be checked again later
    m <- isolate(v$mapz_stars)
    if (!is.null(m)) {
      mbb <- get_map_bbox(m)
      leafletProxy("mapz_plot", session) %>%
        fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax)
    }
    #######################
    excelTable(
      data = v$zone_list_df,
      columns = zone_column,
      tableOverflow = T,
      tableWidth = "100%",
      allowDeleteColumn = F,
      allowRenameColumn = F,
      allowInsertColumn = F,
      allowDeleteRow = F,
      allowInsertRow = F,
      rowDrag = F,
      minDimensions = c(NA, 1),
      tableHeight = "430px",
      autoIncrement = T,
      csvFileName = "zonation_table",
      includeHeadersOnDownload = T
    )
  })
  
  observeEvent(input$zone_list, {
    df_old <- isolate(v$zone_list_df)
    inp <- input$zone_list
    df_input <- excel_to_R(inp)
    names(df_input) <- zone_table_def
    df_input <- transform(df_input, zone_id = as.numeric(zone_id))
    v$zone_list_df <- df_input
    newcol <- setdiff(df_old$color, df_input$color)
    if (length(newcol) > 0) {
      zone_legend_pal <<-
        colorFactor(df_input$color, df_input$lc_id, na.color = NA)
      plot_map("mapz")
    }
  })
  
  ### Carbon stock list table
  cstock_table_def <- c("lc_id", "c")
  cstock_column <- data.frame(
    title = c(
      "LC_ID",
      "Land cover",
      "<div>C-stock (tC ha<sup>-1</sup>)</div>"
    ),
    type = c("numeric", "text", "numeric"),
    align = c("right", "left", "right"),
    width = c(NA, 150, 150),
    readOnly = c(T, T, F)
  )
  
  output$cstock_list <- renderExcel({
    df <- v$cstock_list
    if (is.null(df))
      return()
    lc_df <- v$lc_list_df[c("lc_id", "label")]
    df <- merge(df[c("lc_id", "c")], lc_df, by = "lc_id")
    excelTable(
      data = df[c("lc_id", "label", "c")],
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
      tableHeight = "430px",
      autoIncrement = T,
      csvFileName = "zonation_table",
      includeHeadersOnDownload = T
    )
  })
  
  observeEvent(input$cstock_list, {
    inp <- input$cstock_list
    df_input <- excel_to_R(inp)
    names(df_input) <- c("lc_id", "lc", "c")
    df_input <- transform(df_input, lc_id = as.numeric(lc_id))
    suppressWarnings(df_input <-
                       transform(df_input, c = as.numeric(c)))
    df_input[is.na(df_input)] <- 0
    v$cstock_list <- df_input[cstock_table_def]
  })
  
  ### LAND COVER CHANGE MATRIX ##############################
  update_lc_matrix <- function(map1, map2, mapz) {
    #TODO: check for CRS consistency and also dimension!
    if (is.null(map1) || is.null(map2))
      return()
    if (is.null(mapz)) {
      m <- tryCatch({
        suppressWarnings(as.data.frame(table(c(map1, map2))))
      }, error = function(cond) {
        message("error in merging the maps")
        #TODO: this is a hack!
        st_dimensions(map2) <- st_dimensions(map1)
        suppressWarnings(as.data.frame(table(c(map1, map2))))
      })
      
      colnames(m) <- c("lc1_id", "lc2_id", "area")
      m$zone_id <- 0
    } else {
      m <- tryCatch({
        suppressWarnings(as.data.frame(table(c(
          map1, map2, mapz
        ))))
      }, error = function(cond) {
        message("error in merging the maps")
        #TODO: this is a hack!
        st_dimensions(mapz) <- st_dimensions(map1)
        st_dimensions(map2) <- st_dimensions(map1)
        suppressWarnings(as.data.frame(table(c(
          map1, map2, mapz
        ))))
      })
      colnames(m) <- c("lc1_id", "lc2_id", "zone_id", "area")
    }
    m <- m[m$area > 0, ]
    idcol <- c("lc1_id", "lc2_id", "zone_id")
    m[idcol] <-
      apply(m[idcol], 2, function(x)
        as.numeric(as.character(x)))
    m$scenario_id <- 0
    m$iteration_id <- 0
    v$lc_changes_df <- m
  }
  
  observe({
    if (is.null(v$zone_list_df)) {
      return()
    }
    z <- v$zone_list_df$zone_id
    names(z) <- v$zone_list_df$label
    z <- append(list("All zones" = all_id), z)
    updateSelectInput(session, "zone_select1", choices = z)
  })
  
  get_lc_colDef <- function(lc_id_list,
                            align = "left",
                            sticky = NULL,
                            footer = NULL) {
    if (length(lc_id_list) == 0) {
      return(colDef(name = ""))
    }
    lc_df <- isolate(v$lc_list_df)
    lc_df <- lc_df[lc_df$lc_id %in% lc_id_list, ]
    lmax <- max(nchar(lc_df$label))
    lc_width <- lmax * 6 + 70
    c <- colDef(
      name = "Land cover ID",
      align = align,
      sticky = sticky,
      minWidth = lc_width,
      footer = footer,
      footerStyle = list(backgroundColor = "", fontWeight = "bold"),
      cell = function(value) {
        id <- as.numeric(as.character(value))
        lc <- lc_df[lc_df$lc_id == id, "label"]
        div(lc, span(
          id,
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
  
  output$lc_changes1 <- output$lc_changes2 <- renderReactable({
    if (is.null(v$lc_changes_df)) {
      return()
    }
    zone_id <- all_id
    if (!is.null(v$zone_list_df))
      zone_id <- input$zone_select1
    
    if (zone_id == all_id) {
      lcz <- v$lc_changes_df[c("lc1_id", "lc2_id", "area")]
    } else {
      lcz <- v$lc_changes_df[v$lc_changes_df$zone_id == zone_id, c("lc1_id", "lc2_id", "area")]
    }
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
    if (nrow(lcz) > 1) {
      if (ncol(m) == 2) {
        m$TOTAL <- m[[2]]
      } else {
        mt <- apply(m[, -1], 1, function(x) {
          sum(x, na.rm = T)
        })
        m$TOTAL <- mt
      }
    }
    lmax <-
      max(nchar(v$lc_list_df[v$lc_list_df$lc_id %in% m$lc1_id, "label"]))
    lc_width <- lmax * 8
    
    reactable(
      m,
      pagination = F,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      wrap = FALSE,
      sortable = F,
      fullWidth = F,
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
        class = function(v, r, c) {
          if (m[r, 1] == c)
            return("matrix_diagonal")
        },
        footerStyle = total_style,
        footer = function(values) {
          f_number(sum(values, na.rm = T))
        }
      )
    )
  })
  total_style = list(
    backgroundColor = "#FCE5E6",
    fontWeight = "bold",
    border = "1px solid white",
    borderRadius = "5px"
  )
  
  ### CARBON EMISSION CALCULATION ######################
  observe({
    if (is.null(v$lc_changes_df))
      return()
    c_df <- v$cstock_list
    if (is.null(c_df))
      return()
    if (sum(c_df$c) == 0)
      return()
    m1 <- isolate(v$map1_stars)
    m2 <- isolate(v$map2_stars)
    if (!is.null(m1) && !is.null(m2)) {
      m1c <- reclassify_map(m1, c_df)
      m2c <- reclassify_map(m2, c_df)
      ## calculate emission ##
      me <- (m1c - m2c) * 44 / 12
      v$mapc_stars <- me
      v$c_emission_total <- sum(me[me > 0][[1]], na.rm = T)
      v$c_sequestration_total <- abs(sum(me[me < 0][[1]], na.rm = T))
    } else {
      df <- v$lc_changes_df
      cdf <- v$cstock_list
      colnames(cdf) <- c("lc1_id", "c1")
      df <- merge(df, cdf, by = "lc1_id", all.x = T)
      colnames(cdf) <- c("lc2_id", "c2")
      df <- merge(df, cdf, by = "lc2_id", all.x = T)
      df$e <- (df$c1 - df$c2) * df$area * 44 / 12
      v$c_emission_total <- sum(df[df$e > 0, "e"], na.rm = T)
      v$c_sequestration_total <- sum(df[df$e < 0, "e"], na.rm = T)
    }
  })
  
  ### MAP PLOT OF EMISSION ###################
  lapply(c("mapc", "map_other", "map_all"), function(id) {
    output[[paste0(id, "_out")]] <- renderLeaflet({
      m <- v[[paste0(id, "_stars")]]
      if (is.null(m))
        return()
      vr <- range(as.vector(m[[1]]), na.rm = T)
      rm <- max(abs(vr[1]), vr[2])
      vals <- c(-rm, rm)
      p <- emission_color
      pal <-
        colorNumeric(
          palette = p,
          domain = vals,
          na.color = NA,
          reverse = F
        )
      pal_rev <-
        colorNumeric(
          palette = p,
          domain = vals,
          na.color = NA,
          reverse = T
        )
      leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
        addTiles() %>%
        addStarsImage(m,
                      project = T,
                      colors = pal,
                      layerId = 'ID') %>%
        addLegend(
          pal = pal_rev,
          values = vals,
          opacity = 1,
          labFormat = labelFormat(
            transform = function(x)
              sort(x, decreasing = TRUE)
          )
        )
    })
    observeEvent(input[[paste0(id, "_card_full_screen")]], ({
      m <- isolate(v[[paste0(id, "_stars")]])
      if (is.null(m))
        return()
      mbb <- get_map_bbox(m)
      lp <- leafletProxy(paste0(id, "_out"), session) %>%
        fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax)
    }))
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
  
  ### EMISSION FROM OTHER SOURCES ###############
  other_table_def <- c("zone_id", "lc_id", "efactor", "source")
  other_column <- data.frame(
    title = c("Zone", "Land cover", "Emission factor", "Source"),
    type = c("dropdown", "dropdown", "numeric", "text"),
    align = c("left", "left", "right", "left"),
    width = c(500, 500, 150, 200),
    multiple = c(T, T, F, F)
  )
  
  update_table <- reactiveVal(T)
  all_id <- -999
  
  output$other_emission <- renderExcel({
    if (is.null(v$lc_list_df))
      return()
    if (update_table())
      update_table(F)
    df <- isolate(v$other_emission_df)
    if (is.null(df) || ncol(df) == 0) {
      df <- data.frame(
        zone_id = character(),
        lc_id = character(),
        efactor = character(),
        source = character(),
        stringsAsFactors = FALSE
      )
    } else {
      id_df <- df[c("zone_id", "lc_id")]
      id_df[is.na(id_df)] <- all_id
      id_df[id_df == ""] <- all_id
      df[c("zone_id", "lc_id")] <- id_df
      df <- aggregate(df, list(x = df$efactor, y = df$source), function(x) {
        paste(unique(x), collapse = ";")
      })[other_table_def]
      
      for (i in c(1:nrow(df))) {
        if (!is.null(v$zone_list_df)) {
          zs <- unlist(strsplit(df[i, "zone_id"], split = ";"))
          
          if (all(v$zone_list_df$zone_id %in% zs)) {
            df[i, "zone_id"] <- all_id
          }
        }
        ls <- unlist(strsplit(df[i, "lc_id"], split = ";"))
        if (all(v$lc_list_df$lc_id %in% ls)) {
          df[i, "lc_id"] <- all_id
        }
      }
    }
    z <- 0
    if (!is.null(v$zone_list_df)) {
      zdf <- v$zone_list_df
      z <- zdf[c("zone_id", "label")]
      colnames(z) <- c("id", "name")
      z <- rbind(c(all_id, "All zones"), z)
    }
    lc <- v$lc_list_df[c("lc_id", "label")]
    colnames(lc) <- c("id", "name")
    lc <- rbind(c(all_id, "All land covers"), lc)
    
    other_column$source <- I(list(z, lc, 0, 0))
    excelTable(
      data = df,
      autoFill = T,
      columns = other_column,
      tableWidth = "100%",
      allowDeleteColumn = F,
      allowRenameColumn = F,
      allowInsertColumn = F,
      minDimensions = c(NA, 20),
      tableHeight = "100%",
      csvFileName = "other_emission_table",
      includeHeadersOnDownload = T,
      rowHeight = data.frame(r = c(0:100), h = 30),
      wordWrap = T
    )
  })
  
  observeEvent(input$other_emission, {
    inp <- input$other_emission
    df_input <- excel_to_R(inp)
    names(df_input) <- other_table_def
    df_input <- df_input[df_input$zone_id != "" |
                           df_input$lc_id != "", ]
    v$other_emission_df <- validate_other_df(df_input)
  })
  
  validate_other_df <- function(df_input) {
    df_input[df_input$source == "", "source"] <- "unknown"
    x_df <- apply(df_input, 1, function(r) {
      zs <- 0
      if(!is.null(v$zone_list_df)) {
      if (r[["zone_id"]] == "" ||
          r[["zone_id"]] == all_id || is.na(r[["zone_id"]])) {
        zs <- v$zone_list_df$zone_id
      } else {
        zs <- unlist(strsplit(r[["zone_id"]], split = ";"))
        if (all_id %in% zs)
          zs <- v$zone_list_df$zone_id
      }
      }
      
      if (r[["lc_id"]] == "" ||
          r[["lc_id"]] == all_id || is.na(r[["lc_id"]])) {
        ls = v$lc_list_df$lc_id
      } else {
        ls <- unlist(strsplit(r[["lc_id"]], split = ";"))
        if (all_id %in% ls)
          ls <- v$lc_list_df$lc_id
      }
      zone_id = unlist(rep(zs, each = length(ls)))
      lc_id = unlist(rep(ls, length(zs)))
      data.frame(zone_id, lc_id, efactor = r[["efactor"]], source = r[["source"]])
    })
    x_df <- do.call(rbind, x_df)
    cn <- c("zone_id", "lc_id", "efactor")
    x_df[, cn] <- apply(x_df[, cn], 2, function(x)
      as.numeric(as.character(x)))
    x_df[is.na(x_df)] <- 0
    return(x_df)
  }
  
  #update source selection
  observe({
    df <- v$other_emission_df
    if (is.null(df))
      return()
    o <- unique(df$source)
    o <- o[o != ""]
    v$other_emission_source <- o
    updateCheckboxGroupInput(session,
                             "select_source",
                             choices = o,
                             selected = o)
  })
  
  output$other_source_list <- renderUI({
    s <- v$other_emission_source
    if (is.null(s))
      return()
    if (length(s) == 0)
      return()
    enable <- input$select_source
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
    m1 <- v$map1_stars
    m2 <- v$map2_stars
    mz <- v$mapz_stars
    if (is.null(m1) || is.null(m2))
      return()
    if (v$period_year == 0)
      return()
    enable <- input$select_source
    e_df <- v$other_emission_df
    if (nrow(e_df) == 0)
      return()
    e_df <- e_df[e_df$source %in% enable, ]
    if (nrow(e_df) == 0) {
      mo <- m1
      mo[!is.na(mo)] <- 0
    } else {
      e_df <- aggregate(e_df, list(x = e_df$efactor, y = e_df$source), function(x) {
        paste(unique(x), collapse = ";")
      })[other_table_def]
      mo1 <- m1
      mo1[!is.na(m1)] <- 0
      mo2 <- m2
      mo2[!is.na(m2)] <- 0
      for (r in rownames(e_df)) {
        a <- e_df[[r, "lc_id"]]
        e <- as.numeric(e_df[r, "efactor"])
        i <- as.numeric(unlist(strsplit(e_df[[r, "lc_id"]], split = ";")))
        z <- as.numeric(unlist(strsplit(e_df[[r, "zone_id"]], split = ";")))
        if(is.null(mz)) {
          mo1[m1 %in% i] <- e
          mo2[m2 %in% i] <- e
        } else {
          mo1[m1 %in% i & mz %in% z] <- e
          mo2[m2 %in% i & mz %in% z] <- e
        }
      }
      mo <- (mo1 + mo2) * v$period_year / 2
    }
    v$map_other_stars <- mo
    if (!is.null(v$mapc_stars))
      v$map_all_stars <- mo + v$mapc_stars
    v$other_emission_total <- sum(mo[mo > 0][[1]], na.rm = T)
    v$other_sequestration_total <-
      abs(sum(mo[mo < 0][[1]], na.rm = T))
    v$all_emission_total <-
      v$c_emission_total + v$other_emission_total
    v$all_sequestration_total <-
      v$c_sequestration_total + v$other_sequestration_total
  })
  
  ### UPDATE ABACUS DATA ######################
  observe({
    if (is.null(v$lc_changes_df))
      return()
    ot <- NULL
    if (input$include_other)
      ot <- v$other_emission_df
    ab <- abacus(
      date1 = v$map1_date,
      date2 = v$map2_date,
      landcover = v$lc_list_df,
      landcover_change = v$lc_changes_df,
      zone = v$zone_list_df,
      carbonstock = v$cstock_list,
      n_iteration = isolate(v$n_iteration),
      other_emission_factor = ot
    )
    v$abacus_data <- ab
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
    v$mapc_stars <- NULL
    v$map_other_stars <- NULL
    v$map_all_stars <- NULL
    v$map1_df <- NULL
    v$map2_df <- NULL
    v$mapz_df <- NULL
    
    v$abacus_data <- read.abacus(v$abacus_file$datapath)
    p <- v$abacus_data$project_list[[v$selected_project_id]]
    p$landcover$color <- map_color(nrow(p$landcover))
    v$lc_list_df <- p$landcover[lc_table_def]
    p$zone$color <- map_color(nrow(p$zone))
    p$zone$area <- ""
    v$zone_list_df <- p$zone[zone_table_def]
    cdf <- p$carbonstock
    v$cstock_list <- cdf[cdf$scenario_id == 0 &
                           cdf$iteration_id == 0 &
                           cdf$zone_id == 0, c("lc_id", "c")]
    v$lc_changes_df <- p$landcover_change
    v$n_iteration <- p$project$n_iteration
    v$map1_date <- as.Date(paste0(p$project$baseyear0, "-07-01"))
    v$map2_date <- as.Date(paste0(p$project$baseyear1, "-07-01"))
    removeModal()
  })
  
  get_period_label <- function(iteration) {
    n <- abs(v$baseline_period[2] - v$baseline_period[1])
    p1 <- v$baseline_period[1] + iteration * n
    p2 <- v$baseline_period[1] + (iteration + 1) * n
    return(paste(p1, "-", p2))
  }
  
  get_landcover_label <- function(id) {
    df <- active_scenario()$abacus_scenario$scenario$landcover
    return(df[df$lc_id == id, "label"])
  }
  
  get_landcover_color <- function(id) {
    df <- active_scenario()$abacus_scenario$scenario$landcover
    return(df[df$lc_id == id, "color"])
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
  
  
  ### PROJECTION ######################
  
  sc_active_section <- list()
  
  output$edit_scenario <- renderAbacuslib({
    if (is.null(v$abacus_data)) {
      return(NULL)
    }
    if (is.null(v$selected_scenario_id))
      return()
    plot(
      v$abacus_data,
      active_scenario()$abacus_scenario$scenario,
      selected_iteration = sc_active_section$iteration,
      selected_zone = sc_active_section$zone,
      selected_lc = sc_active_section$lc
    )
  })
  
  output$n_scenario <- renderText({
    df <- active_scenario()$abacus_scenario$scenario$tpm
    if (is.null(df)) {
      return(0)
    }
    return(nrow(df[df$lock == 1, ]))
  })
  
  observeEvent(input$edit_scenario_baseline, {
    v$abacus_baseline <- input$edit_scenario_baseline
    v$n_iteration <- v$abacus_baseline$iteration
    final_area$bl_lc_df <-
      get_lc_area_total(v$abacus_baseline$projection$lc_sum_area)
    
    sc <- active_scenario()$abacus_scenario
    if (is.null(sc)) {
      apply_final_lc_area(final_area$bl_lc_df[c(1:3), ])
    }
    emission_val$bl_sum <-
      sum(v$abacus_baseline$emission$iteration_emission$emission)
    
    update_scenario_select_options()
  })
  
  final_area <- reactiveValues(sc_lc_df = NULL, bl_lc_df = NULL)
  
  output$final_area1 <- renderText({
    if (input$box_final_area_full_screen)
      return()
    if (is.null(final_area$sc_lc_df))
      return()
    f_number(max(final_area$sc_lc_df$area))
  })
  
  output$box_final_area_info <- renderUI({
    if (input$box_final_area_full_screen)
      return()
    df <- final_area$sc_lc_df
    if (is.null(df))
      return()
    df <- df[order(df$area, decreasing = TRUE), ]
    tagList(p("of", df[1, "lc"]), p("2nd", df[2, "lc"], ":", f_number(df[2, "area"])))
  })
  
  get_lc_area_total <- function(lc_sum_area) {
    a <- lc_sum_area
    i <- max(a$iteration)
    a <- a[a$iteration == i, ]
    ag <- aggregate(a$area, by = list(lc_id = a$lc1_id), FUN = sum)
    names(ag)[names(ag) == "x"] <- "area"
    ag <- ag[order(ag$area, decreasing = T), ]
    ag$lc <- lapply(ag$lc_id, get_landcover_label)
    return(ag)
  }
  
  active_scenario <- function() {
    if (length(v$scenario_list) == 0) {
      v$scenario_list <- list(generate_scenario_object(1))
    }
    # print("active_scenario")
    # print(length(v$scenario_list))
    # print(v$selected_scenario_id)
    v$scenario_list[[v$selected_scenario_id]]
  }
  
  scenario_lc_colums <- c("lc_id",
                          "color",
                          "label",
                          "description",
                          "zone_id",
                          "iteration_id",
                          "c")
  
  generate_scenario_object <- function(id = NULL,
                                       label = NULL,
                                       desc = "") {
    if (is.null(label))
      label <- paste("Scenario", id)
    l <- list(label = label, desc = desc)
    if (is.null(v$lc_list_df))
      return(l)
    if (is.null(v$cstock_list)) {
      lc_df <- v$lc_list_df
      lc_df$c <- 0
    } else {
      lc_df <- merge(v$lc_list_df, v$cstock_list, by = "lc_id")
    }
    lc_df$zone_id <- 0
    lc_df$iteration_id <- 0
    l$abacus_scenario$scenario$landcover <- lc_df[scenario_lc_colums]
    return(l)
  }
  
  observe({
    if (is.null(v$lc_list_df))
      return()
    if (is.null(v$cstock_list)) {
      lc_df <- v$lc_list_df
      lc_df$c <- 0
    } else {
      lc_df <- merge(v$lc_list_df, v$cstock_list, by = "lc_id")
    }
    
    if(length(v$scenario_list) == 0) return()
    for (i in 1:length(v$scenario_list)) {
      cn <- c(lc_table_def, "c")
      sclc <- v$scenario_list[[i]]$abacus_scenario$scenario$landcover
      if (!is.null(sclc)) {
        sel <- sclc$lc_id %in% lc_df$lc_id
        sclc[sclc$lc_id %in% lc_df$lc_id, cn] <- lc_df[lc_df$lc_id %in% sclc$lc_id, cn]
        # sclc[sclc$lc_id %in% lc_df$lc_id, cn] <- lc_df[cn]
      }
      v$scenario_list[[i]]$abacus_scenario$scenario$landcover <- sclc
    }
  })
  
  
  update_scenario_select_options <- function() {
    ch <- as.list(c(1:length(v$scenario_list)))
    n <- unlist(lapply(v$scenario_list, function(x)
      x$label))
    names(ch) <- n
    updateSelectInput(
      session,
      "select_scenario",
      choices = ch,
      selected = v$selected_scenario_id
    )
  }
  
  input_dialog <- function(title = "",
                           desc = "",
                           confirm_id,
                           confirm_label = "Add",
                           input_var = NULL,
                           input_label = NULL,
                           input_def = NULL,
                           input_pholder = NULL) {
    inp <- NULL
    if (!is.null(input_var)) {
      blank <- rep("", length(input_var))
      if (is.null(input_label))
        input_label <- blank
      if (is.null(input_def))
        input_def <- blank
      if (is.null(input_pholder))
        input_pholder <- blank
      inp <- mapply(function(v, l, d, p) {
        paste(textInput(v, HTML(l), d, width = "100%", p))
      },
      input_var,
      input_label,
      input_def,
      input_pholder)
    }
    names(inp) <- NULL
    inp <- HTML(inp)
    modalDialog(
      title = title,
      HTML(desc),
      inp,
      footer = tagList(
        actionButton("cancel_button_dialog", "Cancel"),
        actionButton(confirm_id, confirm_label)
      )
    )
  }
  
  observeEvent(input$cancel_button_dialog, removeModal())
  
  observeEvent(input$add_scenario, showModal(
    input_dialog(
      title = "Add new scenario",
      confirm_id = "add_scenario_confirm",
      confirm_label = "Add",
      input_var = c("scenario_label_dialog", "scenario_desc_dialog"),
      input_label = c("Label", "Description"),
      input_def = c(paste("Scenario", length(v$scenario_list) + 1), ""),
      input_pholder = c("", "Description of scenario")
    )
  ))
  
  observeEvent(input$add_scenario_confirm, {
    removeModal()
    id <- length(v$scenario_list) + 1
    v$scenario_list[[id]] <-
      generate_scenario_object(id,
                               input$scenario_label_dialog,
                               input$scenario_desc_dialog)
    update_scenario_select_options()
    v$selected_scenario_id <- id
  })
  
  observeEvent(input$remove_scenario, {
    showModal(
      input_dialog(
        title = "Remove scenario",
        desc = paste("Remove scenario <b>", active_scenario()$label, "</b>?"),
        confirm_id = "confirm_remove_scenario",
        confirm_label = "Remove"
      )
    )
  })
  
  observeEvent(input$confirm_remove_scenario, {
    removeModal()
    if(length(v$scenario_list) == 1) return()
    v$scenario_list[[v$selected_scenario_id]] <- NULL
    update_scenario_select_options()
  })
  
  observeEvent(input$edit_scenario_label_btn, showModal(
    input_dialog(
      title = "Edit scenario",
      confirm_id = "edit_scenario_label_confirm",
      confirm_label = "Done",
      input_var = c("scenario_label_dialog", "scenario_desc_dialog"),
      input_label = c("Label", "Description"),
      input_def = c(active_scenario()$label, active_scenario()$desc)
    )
  ))
  
  observeEvent(input$edit_scenario_label_confirm, {
    removeModal()
    id <- v$selected_scenario_id
    v$scenario_list[[id]]$label <- input$scenario_label_dialog
    v$scenario_list[[id]]$desc <- input$scenario_desc_dialog
    update_scenario_select_options()
  })
  
  
  observe({
    updateSelectInput(session,
                      "select_scenario",
                      selected = v$selected_scenario_id)
  })
  
  output$scenario_label <- renderText(active_scenario()$label)
  output$scenario_desc <- renderText(active_scenario()$desc)
  
  observeEvent(input$select_scenario, {
    if (input$select_scenario == "")
      return()
    v$selected_scenario_id <- as.numeric(input$select_scenario)
  })
  
  ### UPDATE SCENARIO FROM ABACUSLIB WIDGET ##########
  
  observeEvent(input$edit_scenario_update, {
    # print("*** update scenario")
    # print(length(v$scenario_list))
    sc <- input$edit_scenario_update
    ## set active scenario ##
    v$scenario_list[[v$selected_scenario_id]]$abacus_scenario <- sc
    
    final_area$sc_lc_df <-
      get_lc_area_total(sc$projection$lc_sum_area)
    
    # print(final_area$sc_lc_df)
    cn <- c("lc_id", "area")
    adf <- NULL
    for (i in c(1:length(v$scenario_list))) {
      sc <- v$scenario_list[[i]]$abacus_scenario
      if (is.null(sc))
        next
      # print(paste("sc:", i))
      # print(sc$projection$lc_sum_area)
      if (is.null(sc$projection$lc_sum_area))
        next
      df <- get_lc_area_total(sc$projection$lc_sum_area)[cn]
      #merge dengan lc dsini
      
      colnames(df) <- c("lc_id", paste0("area_", i))
      # df$lc_id <- df[[paste0("lc_id_", i)]]
      
      if (is.null(adf)) {
        adf <- df
      } else {
        adf <- merge(
          x = adf,
          y = df,
          by = "lc_id",
          # by.y = colnames(df)[3],
          all = T
        )
      }
      
    }
    # print(adf)
    
    final_area$sc_lc_df <- merge(
      x = final_area$sc_lc_df,
      y = adf,
      by = "lc_id",
      all = T
    )
    
    # apply_final_lc_area(final_area$sc_lc_df[c(1:3), ])
  })
  
  observeEvent(input$edit_scenario_selection, {
    sc_active_section <<- input$edit_scenario_selection
  })
  
  output$final_plot <- renderPlotly({
    if (is.null(v$abacus_baseline)) {
      return()
    }
    # print("plot area")
    # print(input$box_area_full_screen)
    
    data <- final_area$bl_lc_df[c("lc_id", "lc", "area")]
    colnames(data) <- c("lc_id", "label", "baseline")
    
    if (!is.null(final_area$sc_lc_df)) {
      data <- merge(
        x = data,
        y = final_area$sc_lc_df,
        by = "lc_id",
        all = T
      )
      names(data)[names(data) == "area"] <- "scenario"
      data[is.na(data)] <- 0
      
      data$label <- data$lc
      data <- data[order(data$baseline, decreasing = T), ]
    }
    # print(data)
    fig <- plot_ly(
      data,
      x = ~ label,
      y = ~ baseline,
      type = "bar",
      name = "Baseline",
      color = I("white"),
      hovertemplate = paste("%{x}<br>%{y:,.0f} ha")
    )
    
    if (!is.null(final_area$sc_lc_df)) {
      # print(final_area$sc_lc_df)
      if (input$box_final_area_full_screen) {
        for (i in c(1:length(v$scenario_list))) {
          sc <- v$scenario_list[[i]]
          if (is.null(sc))
            next
          fig <- fig %>%
            add_trace(
              y = data[[paste0("area_", i)]],
              name = sc$label,
              color = I(get_color(i, T)),
              hovertemplate = paste("%{x}<br>%{y:,.0f} ha")
            )
        }
      } else {
        fig <- fig %>%
          add_trace(
            y = ~ scenario,
            name = "Scenario",
            color = I("orange"),
            hovertemplate = paste("%{y:,.0f} ha")
          )
      }
    }
    
    fig <- fig %>%
      layout(yaxis = list(title = "Total area (ha)"),
             barmode = "group")
    fig <- fig %>%
      layout(showlegend = FALSE)
    fig <- fig %>%
      layout(
        xaxis = list(
          visible = F,
          showgrid = F,
          title = ""
        ),
        yaxis = list(
          visible = F,
          showgrid = F,
          title = ""
        ),
        hoverlabel = list(align = "left"),
        hovermode = "x",
        margin = list(
          t = 0,
          r = 0,
          l = 0,
          b = 0
        ),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = color_theme$info
      ) %>%
      plotly::config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible,
         'yaxis.visible': visible,
         'showlegend': visible});
      });
      ro.observe(el);
    }"
      )
    
    fig
  })
  
  
  #### Scenario list ######################
  output$tpm_list <- renderReactable({
    df <- active_scenario()$abacus_scenario$scenario$tpm
    if (is.null(df)) {
      return()
    }
    # print(df)
    #zone was removed from js
    if (!("zone" %in% colnames(df))) {
      df$zone <- ""
    }
    df <- df[df$lock == 1, scenario_colums]
    nc <- c("area", "r", "def_area", "def_r")
    df[nc] <- sapply(df[nc], as.numeric)
    df$dif_area <- df$area - df$def_area
    df$dif_r <- df$r - df$def_r
    df[is.na(df)] <- ""
    
    # print(df)
    
    data <- unique(df[, c("lc1", "period", "zone")])
    #TODO if zone is not exist.. remove the grouping
    reactable(
      data,
      columns = list(
        zone = colDef(name = "Zone"),
        period = colDef(name = "Time period"),
        lc1 = colDef(name = "Original land cover")
      ),
      # groupBy = c("zone"),
      defaultExpanded = TRUE,
      details = function(index) {
        d_data <- df[df$zone == data$zone[index] & df$period ==
                       data$period[index] &
                       df$lc1 == data$lc1[index], ]
        reactable(
          d_data[c("lc2", "area", "dif_area")],
          outlined = TRUE,
          borderless = TRUE,
          compact = TRUE,
          theme = reactableTheme(# style = list(fontSize = "1em"),
            backgroundColor = "#f7f7f7"),
          columns = list(
            lc2 = colDef(name = "Converted to"),
            area = colDef(
              name = "Projected area",
              cell = function(value, i) {
                paste0(f_number(value), # format(value, digits = 1, big.mark = ","),
                       " ha [", f_percent(d_data[i, "r"]), "]")
              }
            ),
            dif_area = colDef(
              name = "Changes",
              style = function(value) {
                if (is.na(value))
                  return("black")
                if (value > 0) {
                  color <- "#008000"
                } else if (value < 0) {
                  color <- "#e00000"
                } else {
                  color <- "#777"
                }
                list(color = color, fontWeight = "bold")
              },
              cell = function(value, i) {
                if (is.na(value))
                  return(0)
                if (value == 0)
                  return(0)
                s <- if (value > 0)
                  "+"
                paste0(
                  s,
                  format(value, digits = 1, big.mark = ","),
                  " ha",
                  " [",
                  s,
                  f_percent(d_data[i, "dif_r"]),
                  "]"
                )
              }
            )
          )
        )
      }
    )
  })
  
  #### Scenario Land cover list ####
  
  
  output$new_lc_list <- renderReactable({
    s <- active_scenario()$abacus_scenario$scenario
    if (is.null(s$new_lc_id)) {
      return()
    }
    sc_lc_cols <- c("lc_id", "color", "label", "description", "c")
    df <- s$landcover[s$landcover$lc_id %in% s$new_lc_id, ]
    if (nrow(df) == 0)
      return()
    reactable(
      df[sc_lc_cols],
      pagination = F,
      compact = TRUE,
      rownames = F,
      selection = "single",
      onClick = "select",
      columns = list(
        lc_id = colDef(name = "LC_ID", width = 60),
        color = colDef(
          name = "",
          width = 35,
          html = TRUE,
          cell = function(value) {
            div(
              style = paste0(
                "width:12px;height:12px;border-radius:6px;margin:auto;background-color:",
                value
              )
            )
          }
        ),
        label = colDef(name = "Label", width = 100),
        description = colDef(name = "Description", width = 120),
        c = colDef(
          html = TRUE,
          # name = span(paste0("Carbon stock (", per_ha_unit("tC"), ")"), .noWS = c("after", "before")) ,
          name = markdown("Carbon stock (tC ha<sup>-1</sup>)"),
          format = colFormat(digits = 2, separators = T),
          width = 180
        )
      )
    )
  })
  
  observeEvent(input$add_lc_scenario, {
    s <- active_scenario()$abacus_scenario$scenario
    id <- max(s$landcover$lc_id) + 1
    def <- paste("Land cover", id)
    m <- paste(markdown("Carbon stock (tC ha<sup>-1</sup>)"))
    m <- gsub("<p>", "<div>", m)
    m <- gsub("</p>", "</div>", m)
    showModal(
      input_dialog(
        title = "Add new land cover",
        confirm_id = "add_lc_scenario_dialog",
        confirm_label = "Add",
        input_var = c(
          "scenario_lc_label_dialog",
          "scenario_lc_desc_dialog",
          "scenario_lc_c_dialog"
        ),
        input_label = c("Label", "Description", m),
        input_def = c(def, "", "0"),
        input_pholder = c("", "Description of scenario", "0")
      )
    )
  })
  
  observeEvent(input$add_lc_scenario_dialog, {
    removeModal()
    s <- active_scenario()$abacus_scenario$scenario
    c <- as.numeric(input$scenario_lc_c_dialog)
    if (is.na(c))
      c <- 0
    id <- max(s$landcover$lc_id) + 1
    df <- data.frame(
      lc_id = id,
      color = get_color(id),
      label = input$scenario_lc_label_dialog,
      description = input$scenario_lc_desc_dialog,
      zone_id = 0,
      iteration_id = 0,
      c = c
    )
    s$landcover <- rbind(s$landcover, df)
    s$new_lc_id <- c(s$new_lc_id, id)
    
    v$scenario_list[[v$selected_scenario_id]]$abacus_scenario$scenario <- s
  })
  
  edited_ld_id <- NULL
  
  observeEvent(input$edit_lc_scenario, {
    list_id <- getReactableState("new_lc_list", name = "selected", session = session)
    if (is.null(list_id))
      return()
    s <- active_scenario()$abacus_scenario$scenario
    edited_ld_id <<- s$new_lc_id[list_id]
    
    lc <- s$landcover[s$landcover$lc_id == edited_ld_id, ]
    # label <- lc$label
    # desc <- lc$description
    # c <- lc$c
    
    showModal(
      input_dialog(
        title = "Edit land cover",
        confirm_id = "edit_lc_scenario_dialog",
        confirm_label = "Add",
        input_var = c(
          "scenario_lc_label_dialog",
          "scenario_lc_desc_dialog",
          "scenario_lc_c_dialog"
        ),
        input_label = c(
          "Label",
          "Description",
          markdown("Carbon stock (tC ha<sup>-1</sup>)")
        ),
        input_def = c(lc$label, lc$description, lc$c),
        input_pholder = c("", "Description of scenario", "0")
      )
    )
    
  })
  
  
  
  observeEvent(input$edit_lc_scenario_dialog, {
    removeModal()
    s <- active_scenario()$abacus_scenario$scenario
    c <- as.numeric(input$scenario_lc_c_dialog)
    if (is.na(c))
      c <- 0
    s$landcover[s$landcover$lc_id == edited_ld_id, "label"] <- input$scenario_lc_label_dialog
    s$landcover[s$landcover$lc_id == edited_ld_id, "description"] <- input$scenario_lc_desc_dialog
    s$landcover[s$landcover$lc_id == edited_ld_id, "c"] <- c
    v$scenario_list[[v$selected_scenario_id]]$abacus_scenario$scenario <- s
    edited_ld_id <<- NULL
  })
  
  observeEvent(input$remove_lc_scenario, {
    list_id <- getReactableState("new_lc_list", name = "selected", session = session)
    if (is.null(list_id))
      return()
    s <- active_scenario()$abacus_scenario$scenario
    edited_ld_id <<- s$new_lc_id[list_id]
    label <- s$landcover[s$landcover$lc_id == edited_ld_id, ]$label
    showModal(
      input_dialog(
        title = "Remove land cover",
        desc = paste("Remove land cover <b>", label, "</b>?"),
        confirm_id = "confirm_remove_lc_scenario",
        confirm_label = "Remove"
      )
    )
  })
  
  observeEvent(input$confirm_remove_lc_scenario, {
    removeModal()
    id <- v$selected_scenario_id
    a <- v$scenario_list[[id]]$abacus_scenario$scenario$new_lc_id
    v$scenario_list[[id]]$abacus_scenario$scenario$new_lc_id <- a[a != edited_ld_id]
    edited_ld_id <<- 1
  })
  
  
  ### CALCULATE MARGIN OF EMISSION ###
  
  emission_val <- reactiveValues(bl_sum = 0, sc_sum = 0)
  
  output$emission_margin <- renderText({
    if (input$box_emission_full_screen)
      return()
    sc <- active_scenario()$abacus_scenario
    if (is.null(sc$emission$iteration_emission$emission)) {
      return()
    }
    bl_sum <- emission_val$bl_sum
    sc_sum <- sum(sc$emission$iteration_emission$emission)
    margin <- (sc_sum - bl_sum) / bl_sum
    emission_val$sc_sum <- sc_sum
    if (is.na(margin))
      return("")
    t <- f_percent(margin)
    if (margin > 0)
      return(paste0("+", t))
    return(t)
  })
  
  output$box_emission_info <- renderUI({
    if (input$box_emission_full_screen)
      return()
    tagList(markdown(paste(
      "Baseline:",
      f_number(emission_val$bl_sum, "tCO<sub>2</sub>-eq")
    )), markdown(paste(
      "Scenario:",
      f_number(emission_val$sc_sum, "tCO<sub>2</sub>-eq")
    )))
  })
  
  
  output$emission_plot <- renderPlotly({
    if (is.null(v$abacus_baseline)) {
      return()
    }
    # box_emission
    x <- v$abacus_baseline$emission$iteration_emission$iteration
    x_year <- get_iteration_year(x + 1)
    bl <-
      cumsum(v$abacus_baseline$emission$iteration_emission$emission)
    d <- data.frame(x = x, baseline = bl)
    sc <- active_scenario()$abacus_scenario
    if (!is.null(sc)) {
      d_sc <- cumsum(sc$emission$iteration_emission$emission)
      if (nrow(d) == length(d_sc)) {
        d$scenario <- d_sc
        d$offset <- (d$scenario - d$baseline) / d$baseline
      }
    }
    fig <- plot_ly(
      d,
      x = ~ x,
      y = ~ baseline,
      type = "scatter",
      mode = "lines+markers",
      name = "Baseline",
      color = I("white"),
      fill = "tozeroy",
      fillcolor = "rgba(255,255,255,0.1)",
      
      hovertemplate = paste0("Period:", x_year[1], "-%{x}<br>%{y:.3s} tCO2-eq"),
      span = I(1)
      
    )
    fig <- fig %>%
      layout(xaxis = list(
        ticktext = x_year,
        tickvals = x,
        tickmode = "array"
      ))
    sc <- active_scenario()$abacus_scenario
    if (!is.null(sc$emission$iteration_emission$emission)) {
      if (input$box_emission_full_screen) {
        for (i in c(1:length(v$scenario_list))) {
          sc <- v$scenario_list[[i]]
          if (is.null(sc))
            next
          d_sc <- cumsum(sc$abacus_scenario$emission$iteration_emission$emission)
          if (nrow(d) != length(d_sc))
            next
          cl <- get_color(i, T)
          offset <- (d_sc - d$baseline) / d$baseline
          fig <- fig %>%
            add_trace(
              y = d_sc,
              name = sc$label,
              color = I(cl),
              fill = 'none',
              customdata = offset,
              hovertemplate = paste0(
                "%{y:.3s} tCO2-eq<br>",
                "Offset:%{customdata:+.1%}"
              ),
              span = I(1)
            )
        }
      } else {
        fig <- fig %>%
          add_trace(
            y = ~ scenario,
            name = "Scenario",
            color = I("orange"),
            fill = 'tonexty',
            fillcolor = "rgba(255,165,0,0.1)",
            customdata = ~ offset,
            hovertemplate = paste0("%{y:.3s} tCO2-eq<br>", "Offset:%{customdata:+.1%}"),
            span = I(1)
          )
      }
    }
    fig <- fig %>% layout(showlegend = FALSE)
    fig <- fig %>%
      layout(
        xaxis = list(
          visible = F,
          showgrid = F,
          title = ""
        ),
        yaxis = list(
          visible = F,
          showgrid = F,
          title = ""
        ),
        hoverlabel = list(align = "left"),
        hovermode = "x",
        margin = list(
          t = 0,
          r = 0,
          l = 0,
          b = 0
        ),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        # paper_bgcolor = "#6C6C6C",
        plot_bgcolor = "#4D4D4D"
        # plot_bgcolor = "transparent"
      ) %>%
      plotly::config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible,
         'yaxis.visible': visible,
         'showlegend': visible});
      });
      ro.observe(el);
    }"
      )
    fig
  })
  
  
  ### INPUT OUTPUT FILE #############################
  
  
  observeEvent(input$upload_params, {
    showModal(modalDialog(
      easyClose = T,
      footer = NULL,
      fileInput("upload_parameter", "Load saved parameters (.zip)", accept = ".zip")
    ))
  })
  
  observeEvent(input$import_abacus_1, {
    showModal(modalDialog(
      easyClose = T,
      footer = NULL,
      fileInput(
        "abacus_file",
        "Load REDD-Abacus-1 project file (.car)",
        accept = ".car"
      )
    ))
  })
  
  config_data <-
    c("title",
      "description",
      "map1_date",
      "map2_date",
      "n_iteration")
  config_file <- "config.yaml"
  
  
  # "scenario.tpm",
  # "scenario.baseline_tpm",
  # "scenario.baseline_area"
  
  # "scenario_activities.csv",
  # "scenario_tpm.csv",
  # "baseline_lc_area.csv"
  
  output$download_params <- downloadHandler(
    filename = function() {
      paste("abacus_params.zip")
    },
    content = function(fname) {
      setwd(tempdir())
      ### Output parameter config data ###
      cd <- lapply(config_data, function(x) {
        if (x %in% c("map1_date", "map2_date"))
          return(format(v[[x]], "%d-%b-%Y"))
        v[[x]]
      })
      names(cd) <- config_data
      sc_list <- list()
      for (i in c(1:length(v$scenario_list))) {
        d <- v$scenario_list[[i]]
        sc_list[[i]] <- list(id = i,
                             label = d$label,
                             desc = d$desc)
      }
      cd$scenario <- sc_list
      write_yaml(cd, config_file)
      ### Output parameter table data ###
      fs <- c(config_file)
      apply(table_file_df, 1, function(d) {
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
        write.csv(df, d[["file"]], row.names = F, na = "")
        fs <<- c(fs, d[["file"]])
      })
      ### Output parameter map data ###
      apply(map_file_df, 1, function(d) {
        m <- v[[d[["var"]]]]
        if (!is.null(m)) {
          write_stars(m, d[["file"]])
          fs <<- c(fs, d[["file"]])
        }
      })
      ### Output parameter scenario data ###
      for (i in c(1:length(v$scenario_list))) {
        sc <- v$scenario_list[[i]]$abacus_scenario$scenario
        for (r in c(1:nrow(scenario_file_df))) {
          d <- scenario_file_df[r, ]
          f <- paste0(d[["file"]], "_", i, ".csv")
          write.csv(sc[[d[["var"]]]], f, row.names = F, na = "")
          fs <- c(fs, f)
        }
      }
      zip::zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$upload_parameter, {
    print(paste("Extracting the files:", input$upload_parameter$name))
    dpath <- input$upload_parameter$datapath
    upload_parameter(dpath)
    removeModal()
  })
  
  show_alert_file_error <- function(file_error) {
    showNotification(paste("File error! Or it was not a", file_error, "file!"),
                     type = "error")
  }
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  upload_parameter <- function(dpath) {
    file_list <- NULL
    try({file_list <- unzip(dpath, list = TRUE)}, silent = T)
    if (is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    reset_scenario()
    withProgress(message = 'Uploading data', value = 0, {
      n <- nrow(table_file_df) + nrow(map_file_df) + 2
      incProgress(1 / n, detail = paste("Extracting data"))
      
      unzip(dpath, exdir = data_dir)
      ### Input parameter table data ###
      apply(table_file_df, 1, function(d) {
        incProgress(1 / n, detail = paste("Uploading table:", d[["var"]]))
        iv <- unlist(strsplit(d[["var"]], split = ".", fixed = T))
        f <- d[["file"]]
        fpath <- paste0(data_dir, "/", f)
        if (file.exists(fpath)) {
          if (file.size(fpath) == 0)
            return()
          i1 <- iv[1]
          try(if (length(iv) == 1) {
            v[[i1]] <- read.csv(fpath)
          } else if (length(iv) == 2) {
            i2 <- iv[2]
            v[[i1]][[i2]] <- read.csv(fpath)
          }, silent = T)
          
          if (i1 == "other_emission_df") {
            v[[i1]] <- validate_other_df(v[[i1]])
          }
          if (i1 == "zone_list_df") {
            zone_legend_pal <<-
              colorFactor(v[[i1]]$color, v[[i1]]$zone_id, na.color = NA)
          }
          if (i1 == "lc_list_df") {
            lc_legend_pal <<-
              colorFactor(v[[i1]]$color, v[[i1]]$lc_id, na.color = NA)
          }
        }
      })
      ### Input parameter map data ###
      apply(map_file_df, 1, function(d) {
        incProgress(1 / n, detail = paste("Uploading map:", d[["var"]]))
        f <- d[["file"]]
        fpath <- paste0(data_dir, "/", f)
        if (file.exists(fpath)) {
          v[[d[["var"]]]] <- suppressWarnings(read_stars(fpath))
        }
      })
      incProgress(1 / n, detail = paste("Uploading configuration"))
      ### Input parameter config data ###
      sc <- NULL
      fpath <- paste0(data_dir, "/", config_file)
      if (file.exists(fpath)) {
        cd <- read_yaml(fpath)
        for (i in 1:length(cd)) {
          x <- names(cd[i])
          if (x %in% c("map1_date", "map2_date")) {
            v[[x]] <- as.Date(cd[[i]], "%d-%b-%Y")
            updateDateInput(session, x, value = v[[x]])
          } else if (x == "scenario") {
            sc <- cd[i]$scenario
          } else {
            v[[x]] <- cd[[i]]
          }
        }
      }
      
      if (is.null(sc))
        return()
      for (i in 1:length(sc)) {
        asc <- list(label = sc[[i]]$label, desc = sc[[i]]$desc)
        s <- list()
        id <- sc[[i]]$id
        for (r in c(1:nrow(scenario_file_df))) {
          d <- scenario_file_df[r, ]
          f <- paste0(d[["file"]], "_", id, ".csv")
          fpath <- paste0(data_dir, "/", f)
          if (file.exists(fpath)) {
            try(s[[d[["var"]]]] <- read.csv(fpath), silent = T)
          }
        }
        s$new_lc_id <- s$new_lc_id[[1]]
        asc$abacus_scenario$scenario <- s
        v$scenario_list[[i]] <- asc
      }
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
        setwd(tempdir())
        fs <- c()
        for (si in c(1:length(v$scenario_list))) {
          s <- v$scenario_list[[i]]
          iv <- unlist(strsplit(d[["var"]], split = ".", fixed = T))
          df <- s[[iv[1]]]
          i <- 2
          while (i <= length(iv)) {
            df <- df[[iv[i]]]
            i <- i + 1
          }
          if (is.null(df))
            return()
          if (nrow(df) == 0)
            return()
          
          f <- paste0(d[["file"]], "_", si, ".csv")
          write.csv(df, f, row.names = F, na = "")
          fs <- c(fs, f)
        }
        zip::zip(zipfile = file, files = fs)
      }
    )
  })
  
  
  #### Output JSON #########################
  # apply(json_file_df , 1, function(d) {
  #   output[[paste0(d[["var"]], "_id")]] <- downloadHandler(
  #     filename = function() {
  #       d[["file"]]
  #     },
  #     content = function(file) {
  #       listd <- v[[d[["var"]]]]
  #       if (!is.null(listd)) {
  #         write_json(listd, file, pretty = TRUE, auto_unbox = TRUE)
  #       }
  #     }
  #   )
  # })
  
}

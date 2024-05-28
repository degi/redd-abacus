library(excelR)
library(RColorBrewer)
# library(scales)
library(abacuslib)
library(jsonlite)
library(reshape2)
library(htmltools)
# library(terra)

# library(raster)
library(leafem)
# library(leafpop)
library(yaml)

server <- function(input, output, session) {
  ### TODO: read data from url
  # data <- reactive({
  #   url <- "http://example.com/data.csv"
  #   read.csv(url)
  # })
  
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
  
  qual_col_pals <-
    brewer.pal.info[brewer.pal.info$category == 'qual', ]
  chart_color <-
    unlist(mapply(
      brewer.pal,
      qual_col_pals$maxcolors,
      rownames(qual_col_pals)
    ))
  map_color <-
    colorRampPalette(c("darkgreen", "gold", "red", "blue"))
  emission_color <- colorRamp(c("darkgreen", "white", "red"))
  emission_color_min <- colorRamp(c("darkgreen", "white"))
  emission_color_max <- colorRamp(c("white", "red"))
  
  options(reactable.theme = reactableTheme(
    style = list(fontFamily = "Arial, Helvetica, sans-serif",
                 fontSize = "1em")
  ))
  
  f_number <- function(v, unit = "", digits = 1) {
    paste(format(
      v,
      digits = digits,
      big.mark = ",",
      scientific = F
    ),
    unit)
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
    abacus_scenario = NULL,
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
    scenario = NULL
  )
  
  lc_legend_pal = NULL
  zone_legend_pal = NULL
  
  
  ############################
  
  observe(nav_select("input_method", input$input_method_select))
  
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
    id_a <- paste0(x, "_date_a")
    id_b <- paste0(x, "_date_b")
    observeEvent(input[[id_a]], {
      v[[id]] <- input[[id_a]]
    })
    observeEvent(input[[id_b]], {
      v[[id]] <- input[[id_b]]
      toggle_popover(paste0(id_b, "_pop"), F)
    })
    output[[paste0(id, "_out")]] <- renderText({
      updateDateInput(session, id_a, value = v[[id]])
      updateDateInput(session, id_b, value = v[[id]])
      format(v[[id]], "%d-%b-%Y")
      # v[[id]]
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
    # if(!is.null(v$scenario)) v$scenario <- NULL
    reset_scenario()
    # dataModal <- function(failed = FALSE) {
    #   modalDialog(
    #     textInput("dataset", "Choose data set",
    #               placeholder = 'Try "mtcars" or "abc"'
    #     ),
    #     span('(Try the name of a valid data object like "mtcars", ',
    #          'then a name of a non-existent object like "abc")'),
    #     if (failed)
    #       div(tags$b("Invalid name of data object", style = "color: red;")),
    #     
    #     footer = tagList(
    #       modalButton("Cancel"),
    #       actionButton("ok", "OK")
    #     )
    #   )
    # }
    return(T)
  }
  
  
  
  ### MAP INPUT UI ####################################
  lapply(c("map1", "map2", "mapz"), function(id) {
    inp_a <- paste0(id, "_file_a")
    inp_b <- paste0(id, "_file_b")
    f <- paste0(id, "_file")
    out_id <- paste0(id, "_plot")
    st <- paste0(id, "_stars")
    vd <- paste0(id, "_df")
    #### map input ####
    observeEvent(input[[inp_a]], {
      if(!check_map_input(st)) return()
      v[[f]] <- input[[inp_a]]
      m <- read_stars(v[[f]]$datapath)
      # print(st_crs(m))
      v[[st]] <- m
    })
    #### map input from header icon ####
    observeEvent(input[[inp_b]], {
      if(!check_map_input(st)) return()
      v[[f]] <- input[[inp_b]]
      v[[st]] <- read_stars(v[[f]]$datapath)
      toggle_popover(paste0(inp_b, "_pop"), F)
    })
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
      # r = rast(as(m, "Raster"))
      
      
      leaflet(options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
        fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax) %>%
        addStarsImage(m,
                      project = T,
                      colors = c,
                      layerId = 'ID')  #%>%
      # addImageQuery(m, project = TRUE, prefix='Land cover', layerId = "ID") %>%
      # addLayersControl(baseGroups = "ID", position = "bottomleft")
      
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
            position = "topleft"
          ) %>%
          addLayersControl(baseGroups = "ID", position = "bottomleft")
        
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
    if(!is.null(v$scenario)) v$scenario <- NULL
    if(!is.null(v$abacus_scenario)) v$abacus_scenario <- NULL
    final_area$sc_lc_df <- NULL
    emission_val$sc_sum <- NULL
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
    # r = rast(as(m, "Raster"))
    leafletProxy(paste0(id, "_plot"), session) %>%
      clearImages() %>%
      # addRasterImage(r, project=FALSE, colors = c)  %>%
      fitBounds(mbb$xmin, mbb$ymin, mbb$xmax, mbb$ymax) %>%
      addStarsImage(m,
                    project = T,
                    colors = c,
                    layerId = 'ID') # %>%
    # addImageQuery(m, project = TRUE, prefix='Land cover', layerId = "ID") %>%
    # addLayersControl(baseGroups = "ID", position = "bottomleft")
  }
  
  observe({
    if (is.null(v$mapz_stars))
      return()
    zm <- as.data.frame(table(v$mapz_stars))
    df <-
      data.frame("zone_id" = as.numeric(as.character(zm[[1]])))
    df$color <- map_color(nrow(zm))
    df$label <- paste0("Zone_", zm[[1]])
    df$description <- ""
    df$area <- zm[[2]]
    oldz_df <- isolate(v$zone_list_df)
    if(is.null(oldz_df)) {
      v$zone_list_df <- df
    } else {
      add_id <- setdiff(df$zone_id, oldz_df$zone_id)
      if(length(add_id) > 0) {
        oldz_df <- rbind(oldz_df, df[df$zone_id %in% add_id,])
        v$zone_list_df <- oldz_df[oldz_df$zone_id %in% df$zone_id,]
      }
    }
    
    zone_legend_pal <<-
      colorFactor(df$color, df$zone_id, na.color = NA)
    plot_map("mapz")
    update_lc_matrix(isolate(v$map1_stars),
                     isolate(v$map2_stars),
                     isolate(v$mapz_stars))
    
    # print("update mapz")
    # print(df)
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
    d <-
      as.numeric(difftime(as.Date(v$map2_date), as.Date(v$map1_date), unit =
                            "weeks")) / 52.25
    v$period_year <- d
    f_number(d, "years", 3)
  })
  
  output$box_period_info <- renderText({
    if (is.null(v$map1_date) && is.null(v$map2_date))
      return()
    paste(format(v$map1_date, "%d-%b-%Y"),
          "↔",
          format(v$map2_date, "%d-%b-%Y"))
  })
  
  
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
    if(nrow(df_inp) == 0) return()
    if(ncol(df_inp) < 2) return()
    
    if(is.na(as.integer(as.character(df_inp[1,1])))) return() 
    df <- data.frame("lc_id" = as.integer(as.character(df_inp[,1])))
    if(is_color_code(df_inp[1,2])) {
      df$color <- df_inp[,2]
      
      if(ncol(df_inp) >= 3) {
        df$label <- df_inp[,3]
      } else {
        df$label <- paste0("Landcover_", df_inp[,1])
      }
      df$description <- ""
      if(ncol(df_inp) >= 4) {
        df$description <- df_inp[,4]
      }
    } else {
      df$color <- map_color(nrow(df_inp))
      df$label <- df_inp[,2]
      df$description <- ""
      if(ncol(df_inp) >= 3) {
        df$description <- df_inp[,3]
      }
    }
    old_df <- isolate(v$lc_list_df)
    valid_id <- intersect(old_df$lc_id,df$lc_id)
    if(length(valid_id) > 0) {
      old_df[old_df$lc_id %in% valid_id,] <- df[df$lc_id %in% valid_id,]
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
    if(nrow(df_inp) == 0) return()
    if(ncol(df_inp) < 2) return()
    
    if(!is_numeric_str(df_inp[1,1])) return() 
    df <- data.frame("lc_id" = as.integer(as.character(df_inp[,1])))
    for(i in c(2:ncol(df_inp))) {
      if(is_numeric_str(df_inp[1,i])) {
        df$c <- df_inp[,i]
        break
      }
    }
    
    old_df <- isolate(v$cstock_list)
    valid_id <- intersect(old_df$lc_id,df$lc_id)
    if(length(valid_id) > 0) {
      old_df[old_df$lc_id %in% valid_id,] <- df[df$lc_id %in% valid_id,]
      v$cstock_list <- old_df
    }
    toggle_popover("c_input_pop", F)
  })

  #### Zone data upload ####
  observeEvent(input$zone_input_upload, {
    #TODO: if(!check_file_csv()) return()
    f <- input$zone_input_upload
    df_inp <- read.csv(f$datapath)
    # print(df_inp)
    if(nrow(df_inp) == 0) return()
    if(ncol(df_inp) < 2) return()
    
    if(!is_numeric_str(df_inp[1,1])) return() 
    df <- data.frame("zone_id" = as.integer(as.character(df_inp[,1])))
    
    if(is_color_code(df_inp[1,2])) {
      df$color <- df_inp[,2]
      
      if(ncol(df_inp) >= 3) {
        df$label <- df_inp[,3]
      } else {
        df$label <- paste0("Landcover_", df_inp[,1])
      }
      df$description <- ""
      if(ncol(df_inp) >= 4) {
        df$description <- df_inp[,4]
      }
    } else {
      df$color <- map_color(nrow(df_inp))
      df$label <- df_inp[,2]
      # print(df)
      df$description <- ""
      if(ncol(df_inp) >= 3) {
        df$description <- df_inp[,3]
      }
      # print(df)
    }
    
    old_df <- isolate(v$zone_list_df)
    valid_id <- intersect(old_df$zone_id,df$zone_id)
    if(length(valid_id) > 0) {
      old_df[old_df$zone_id %in% valid_id, c("zone_id", "color", "label", "description")] <- df[df$zone_id %in% valid_id,]
      # print(old_df)
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
    #TODO: this is hack to zoom the zone map, to be checked again later
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
      # m <- as.data.frame(table(c(map1, map2)))
      
      m <- tryCatch(
        {
          suppressWarnings(as.data.frame(table(c(map1, map2))))
        },
        error = function(cond) {
          message("error in merging the maps")
          #TODO: this is a hack!
          st_dimensions(map2) <- st_dimensions(map1)
          suppressWarnings(as.data.frame(table(c(map1, map2))))
        }
      )
      
      colnames(m) <- c("lc1_id", "lc2_id", "area")
      m$zone_id <- 0
    } else {
      
      
      # m <- as.data.frame(table(c(map1, map2, mapz)))
      
      m <- tryCatch(
        {
          suppressWarnings(as.data.frame(table(c(map1, map2, mapz))))
        },
        error = function(cond) {
          message("error in merging the maps")
          #TODO: this is a hack!
          st_dimensions(mapz) <- st_dimensions(map1)
          st_dimensions(map2) <- st_dimensions(map1)
          suppressWarnings(as.data.frame(table(c(map1, map2, mapz))))
        }
      )
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
    updateSelectInput(session, "zone_select1", choices = z)
    updateSelectInput(session, "zone_select_c", choices = z)
  })
  
  get_lc_colDef <- function(lc_id_list,
                            align = "left",
                            sticky = NULL,
                            footer = NULL) {
    if (length(lc_id_list) == 0) {
      return(colDef(name = ""))
    }
    lc_df <- isolate(v$lc_list_df)
    lc_df <- lc_df[lc_df$lc_id %in% lc_id_list,]
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
        div(lc,
            span(
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
    zone_id <- 0
    if (!is.null(v$zone_list_df))
      zone_id <- input$zone_select1
    
    lcz <- v$lc_changes_df[v$lc_changes_df$zone_id == zone_id,
                           c("lc1_id", "lc2_id", "area")]
    if (nrow(lcz) == 0) {
      return()
    }
    m <- dcast(lcz, lc1_id ~ lc2_id, value.var = "area",
               fill = NA)
    if (nrow(lcz) > 1) {
      if (ncol(m) == 2) {
        m$TOTAL <- m[[2]]
      } else {
        mt <- apply(m[,-1], 1, function(x) {
          sum(x, na.rm = T)
        })
        m$TOTAL <- mt
      }
    }
    lmax <-
      max(nchar(v$lc_list_df[v$lc_list_df$lc_id %in% m$lc1_id,
                             "label"]))
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
      theme = reactableTheme(
        style = list(fontSize = "0.9em")
      ),
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
        format = colFormat(digit = 0,
                           separators = TRUE),
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
    if(!is.null(m1) && !is.null(m2)) {   
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
      df$e <- (df$c1 - df$c2)*df$area*44/12 
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
  
  ### OTHER EMISSION CALCULATION #################################
  other_emission_desc <- "
<p>This emission factor is assumed as an additional emission of a land cover
which emit continuously (i.e emission from a peatland, or any other activities).</p>
<ul>
<li>The unit is tCOe yr-1 ha-1, which defined as
yearly tCO2-equivalent land cover emission on a hectare area.</li>
<li>The emission factor for converted land cover is calculated as
the average of emission factor between before-and-after converted land covers.</li>
<li>Right click on the table to add and remove the emission factors.</li>
<li>Leave the zone selection empty to make it apply for all zones.</li>
</ul>"
  output$other_desc <- renderUI(HTML(other_emission_desc))
  
  other_table_def <- c("zone_id", "lc_id", "efactor", "source")
  other_column <- data.frame(
    title = c("Zone", "Land cover", "Emission factor", "Source"),
    type = c("dropdown", "dropdown", "numeric", "text"),
    align = c("left", "left", "right", "left"),
    width = c(80, 120, 150, 100)
  )
  
  update_table <- reactiveVal(T)
  
  output$other_emission <- renderExcel({
    if (is.null(v$lc_list_df))
      return()
    if (update_table())
      update_table(F)
    df <- v$other_emission_df
    if (nrow(df) == 0) {
      s <- "source_1"
      df <- rbind(df, c("", "", "0", s))
      colnames(df) <- other_table_def
      v$other_emission_df <- df
      v$other_emission_source <- c(s)
      updateCheckboxGroupInput(session,
                               "select_source",
                               choices = s,
                               selected = s)
    }
    z <- 0
    if (!is.null(v$zone_list_df)) {
      zdf <- v$zone_list_df
      z <- zdf[c("zone_id", "label")]
      colnames(z) <- c("id", "name")
    }
    lc <- v$lc_list_df[c("lc_id", "label")]
    colnames(lc) <- c("id", "name")
    other_column$source <- I(list(z, lc, 0, 0))
    excelTable(
      data = df,
      autoFill = T,
      columns = other_column,
      tableOverflow = T,
      tableWidth = "100%",
      allowDeleteColumn = F,
      allowRenameColumn = F,
      allowInsertColumn = F,
      minDimensions = c(NA, 1),
      tableHeight = "500px",
      autoIncrement = T,
      csvFileName = "other_emission_table",
      includeHeadersOnDownload = T
    )
  })
  
  observeEvent(input$other_emission, {
    old_df <- isolate(v$other_emission_df)
    inp <- input$other_emission
    df_input <- excel_to_R(inp)
    names(df_input) <- other_table_def
    df_input[df_input == "undefined"] <- 0
    if (nrow(df_input[df_input$efactor == "", ]) > 0) {
      r <- tail(old_df, n = 1)
      df_input[df_input$efactor == "", c("efactor", "source")] <-
        c(0, r$source)
    }
    suppressWarnings(df_input <-
                       transform(df_input, efactor = as.numeric(efactor)))
    if (sum(is.na(df_input)) > 0) {
      df <- old_df
    } else {
      df <- df_input
    }
    update_table(T)
    v$other_emission_df <- df
  })
  
  #update source selection
  observe({
    df <- v$other_emission_df
    if(is.null(df)) return()
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
  
  ### UPDATE EMISSION FROM OTHER SOURCES ###############
  observe({
    m1 <- v$map1_stars
    m2 <- v$map2_stars
    mz <- v$mapz_stars
    if (is.null(m1) || is.null(m2))
      return()
    enable <- input$select_source
    e_df <- v$other_emission_df
    if (nrow(e_df) == 0)
      return()
    e_df <- e_df[e_df$source %in% enable & e_df$lc_id != "", ]
    if (nrow(e_df) == 0) {
      mo <- m1
      mo[!is.na(mo)] <- 0
    } else {
      e_df$lc_id <- as.numeric(as.character(e_df$lc_id))
      mo1 <- m1
      mo1[!is.na(m1)] <- 0
      mo2 <- m2
      mo2[!is.na(m2)] <- 0
      for (r in rownames(e_df)) {
        z <- e_df[r, "zone_id"]
        i <- e_df[r, "lc_id"]
        e <- e_df[r, "efactor"]
        if (is.na(z) || z == "") {
          mo1[m1 == i] <- e
          mo2[m2 == i] <- e
        } else {
          mo1[m1 == i & mz == z] <- e
          mo2[m2 == i & mz == z] <- e
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
    if(input$include_other) ot <- v$other_emission_df
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
    # print("update abacus data")
    # print(v$zone_list_df)
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
    # scenario_id iteration_id zone_id
    v$cstock_list <- cdf[cdf$scenario_id==0 & cdf$iteration_id==0 & cdf$zone_id==0,c("lc_id", "c")]
    v$lc_changes_df <- p$landcover_change
    v$n_iteration <- p$project$n_iteration
    v$map1_date <- as.Date(paste0(p$project$baseyear0, "-07-01"))
    v$map2_date <- as.Date(paste0(p$project$baseyear1, "-07-01"))
    # 
    # v$baseline_period <-
    #   c(p$project$baseyear0, p$project$baseyear1)
  })
  
  get_period_label <- function(iteration) {
    n <- abs(v$baseline_period[2] - v$baseline_period[1])
    p1 <- v$baseline_period[1] + iteration * n
    p2 <- v$baseline_period[1] + (iteration + 1) * n
    return(paste(p1, "-", p2))
  }
  
  get_landcover_label <- function(id) {
    return(isolate(v$lc_list_df[v$lc_list_df$lc_id == id, "label"]))
  }
  
  get_landcover_color <- function(id) {
    return(isolate(v$lc_list_df[v$lc_list_df$lc_id == id, "color"]))
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
  
  ########### SPATIAL DATA ######################
  
  # output$lc_list_df <- renderReactable({
  #     if (is.null(v$lc_list_df)) {
  #         return()
  #     }
  #     reactable(v$lc_list_df, borderless = TRUE, compact = TRUE,
  #         columns = list(lc_id = colDef(name = "ID", minWidth = 40),
  #             label = colDef(name = "Label", minWidth = 200),
  #             description = colDef(name = "Description")))
  # })
  
  # output$zone_list <- renderReactable({
  #     if (is.null(v$zone_list_df)) {
  #         return()
  #     }
  #     reactable(v$zone_list_df, borderless = TRUE, compact = TRUE,
  #         columns = list(zone_id = colDef(name = "ID", minWidth = 40),
  #             label = colDef(name = "Label", minWidth = 200),
  #             description = colDef(name = "Description")))
  # })
  
  
  
  ### PROJECTION ######################
  
  
  output$edit_scenario <- renderAbacuslib({
    if (is.null(v$abacus_data)) {
      return(NULL)
    }
    print("plot projection")
    plot(v$abacus_data, isolate(v$scenario))
  })
  
  output$n_scenario <- renderText({
    df <- v$scenario$tpm
    if (is.null(df)) {
      return(0)
    }
    return(nrow(df[df$lock == 1,]))
  })
  
  observeEvent(input$edit_scenario_baseline, {
    v$abacus_baseline <- input$edit_scenario_baseline
    v$n_iteration <- v$abacus_baseline$iteration
    final_area$bl_lc_df <-
      get_lc_area_total(v$abacus_baseline$projection$lc_sum_area)
    if (is.null(v$abacus_scenario)) {
      apply_final_lc_area(final_area$bl_lc_df[c(1:3),])
    }
    emission_val$bl_sum <-
      sum(v$abacus_baseline$emission$iteration_emission$emission)
    
    # print("update baseline")
  })
  
  final_area <- reactiveValues(
    sc_lc_df = NULL,
    bl_lc_df = NULL,
    lc1 = "",
    area1 = 0,
    lc2 = "",
    area2 = 0,
    lc3 = "",
    area3 = 0
  )
  
  out_final_lc <- c("final_lc1", "final_lc2", "final_lc3")
  out_final_a <- c("final_area1", "final_area2", "final_area3")
  var_final_lc <- c("lc1", "lc2", "lc3")
  var_final_a <- c("area1", "area2", "area3")
  
  mapply(function(out_lc, out_area, var_lc, var_area) {
    output[[out_lc]] <- renderText(paste0("\"", final_area[[var_lc]],
                                          "\""))
    output[[out_area]] <-
      renderText(f_number(final_area[[var_area]],
                          "ha"))
  },
  out_final_lc,
  out_final_a,
  var_final_lc,
  var_final_a)
  
  get_lc_area_total <- function(lc_sum_area) {
    a <- lc_sum_area
    i <- max(a$iteration)
    a <- a[a$iteration == i,]
    ag <- aggregate(a$area, by = list(lc_id = a$lc1_id),
                    FUN = sum)
    names(ag)[names(ag) == "x"] <- "area"
    ag <- ag[order(ag$area, decreasing = T),]
    ag$lc <- lapply(ag$lc_id, get_landcover_label)
    return(ag)
  }
  
  apply_final_lc_area <- function(fin_df) {
    mapply(function(lc, area, ilc, iarea) {
      final_area[[ilc]] <- lc
      final_area[[iarea]] <- area
    },
    fin_df$lc,
    fin_df$area,
    var_final_lc,
    var_final_a)
  }
  
  observeEvent(input$edit_scenario_update, {
    # start.time <- Sys.time()
    v$abacus_scenario <- input$edit_scenario_update
    final_area$sc_lc_df <-
      get_lc_area_total(v$abacus_scenario$projection$lc_sum_area)
    
    apply_final_lc_area(final_area$sc_lc_df[c(1:3),])
    v$scenario <- v$abacus_scenario$scenario
    # print(v$scenario)
    # print("scenario update")
    
  })
  
  output$final_plot <- renderPlotly({
    if (is.null(v$abacus_baseline)) {
      return()
    }
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
      data[data$label == "NULL", "label"] <- data[data$label == "NULL", "lc"]
      data <- data[order(data$scenario, decreasing = T),]
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
    
    if (!is.null(final_area$sc_lc_df))
      fig <- fig %>%
      add_trace(
        y = ~ scenario,
        name = "Scenario",
        color = I("orange"),
        hovertemplate = paste("%{y:,.0f} ha")
      )
    
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
      config(displayModeBar = F) %>%
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
  
  output$scenario_list <- renderReactable({
    df <- v$scenario$tpm
    if (is.null(df)) {
      return()
    }
    # print(df)
    #zone was removed from js
    if (!("zone" %in% colnames(df))) {
      df$zone <-""
    }
    # print(df)
    df <- df[df$lock == 1, scenario_colums]
    nc <- c("area", "r", "def_area", "def_r")
    df[nc] <- sapply(df[nc], as.numeric)
    df$dif_area <- df$area - df$def_area
    df$dif_r <- df$r - df$def_r
    df[is.na(df)] <- ""
    data <- unique(df[, c("zone", "period", "lc1")])
    #TODO if zone is not exist.. remove the grouping
    reactable(
      data,
      columns = list(
        zone = colDef(name = "Zone"),
        period = colDef(name = "Time period"),
        lc1 = colDef(name = "Original land cover")
      ),
      groupBy = c("zone"),
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
          theme = reactableTheme(
            # style = list(fontSize = "1em"),
            backgroundColor = "#f7f7f7"
          ),
          columns = list(
            lc2 = colDef(name = "Converted to"),
            area = colDef(
              name = "Projected area",
              cell = function(value,
                              i) {
                paste0(format(value, digits = 1, big.mark = ","),
                       " ha",
                       " [",
                       f_percent(d_data[i, "r"]),
                       "]")
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
  
  
  
  emission_val <- reactiveValues(bl_sum = 0, sc_sum = 0)
  
  output$emission_margin <- renderText({
    if (is.null(v$abacus_scenario)) {
      return("0%")
    }
    bl_sum <- emission_val$bl_sum
    sc_sum <- sum(v$abacus_scenario$emission$iteration_emission$emission)
    margin <- (sc_sum - bl_sum) / bl_sum
    emission_val$sc_sum <- sc_sum
    if(is.na(margin)) return("")
    t <- f_percent(margin)
    if (margin > 0)
      return(paste0("+", t))
    return(t)
    
  })
  
  output$emission_baseline <-
    renderText(f_number(emission_val$bl_sum))
  output$emission_scenario <-
    renderText(f_number(emission_val$sc_sum))
  
  
  output$emission_plot <- renderPlotly({
    # if (is.null(v$abacus_scenario)) {
    #   return()
    # }
    if (is.null(v$abacus_baseline)) {
      return()
    }
    
    x = v$abacus_baseline$emission$iteration_emission$iteration
    bl <-
      # cumsum(v$abacus_scenario$emission$iteration_emission$emission)
      cumsum(v$abacus_baseline$emission$iteration_emission$emission)
    d <- data.frame(x = x, baseline = bl)
    
    if (!is.null(v$abacus_scenario)) {
      d_sc <- cumsum(v$abacus_scenario$emission$iteration_emission$emission)
      # print(d)
      # print(d_sc)
      if(nrow(d) != length(d_sc)) {
        print("output.emission_plot")
        return()
      }
      d$scenario <- d_sc
        # cumsum(v$abacus_scenario$emission$iteration_emission$emission)
      d$offset <- (d$scenario - d$baseline) / d$baseline
    }
    fig <- plot_ly(
      d,
      x = ~ x,
      y = ~ baseline,
      type = "scatter",
      mode = "lines+markers",
      name = "Baseline",
      color = I("white"),
      # hovertemplate = paste("%{y:,.0f} tCO2-eq"),
      # customdata = d$bl_m,
      hovertemplate = paste0("Period:", x[1], "-%{x}<br>%{y:.3s} tCO2-eq"),
      # hoverformat: '.2r',
      # fill = 'tozeroy', alpha = 0.2,
      span = I(1)
    )
    
    if (!is.null(v$abacus_scenario)) {
      fig <- fig %>%
        add_trace(
          y = ~ scenario,
          name = "Scenario",
          color = I("orange"),
          fill = 'tonexty',
          fillcolor = "rgba(255,165,0,0.1)",
          customdata = ~ offset,
          hovertemplate = paste0("%{y:.3s} tCO2-eq<br>",
                                 "Offset:%{customdata:+.1%}"),
          span = I(1)
        )
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
      config(displayModeBar = F) %>%
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
  
  ########### CARBON EMISSION ######################
  
  # output$cstock_list <- renderReactable({
  #     if (is.null(v$c_stock)) {
  #         return()
  #     }
  #     sc_id <- 0
  #     df <- v$c_stock
  #     df <- df[df$scenario_id == sc_id & df$zone_id == input$zone_select_c &
  #         df$iteration_id == input$iteration_select_c, c("lc_id",
  #         "c")]
  #     reactable(df, borderless = TRUE, compact = TRUE, pagination = F,
  #         columns = list(lc_id = get_lc_colDef(df$lc_id), c = colDef(name = "Carbon stock [t/ha]")))
  # })
  
  
  
  
  ### INPUT OUTPUT FILE #############################
  
  # table_file_df <- data.frame(
  #   var = c(
  #     "lc_list_df",
  #     "cstock_list",
  #     "zone_list_df",
  #     "other_emission_df",
  #     "scenario.tpm",
  #     "scenario.baseline_tpm",
  #     "scenario.baseline_area"
  #   ),
  #   file = c(
  #     "landcover.csv",
  #     "carbonstock.csv",
  #     "zonation.csv",
  #     "other_emission.csv",
  #     "scenario_tpm.csv",
  #     "scenario_base_tpm.csv",
  #     "scenario_base_area.csv"
  #   )
  # )
  
  # map_file_df <- data.frame(
  #   var = c("map1_stars", "map2_stars", "mapz_stars"),
  #   file = c("map1.tif", "map2.tif", "map_zone.tif")
  # )
  
  config_data <-
    c("title",
      "description",
      "map1_date",
      "map2_date",
      "n_iteration")
  config_file <- "config.yaml"
  
  output$download_params <- downloadHandler(
    filename = function() {
      paste("abacus_params.zip")
    },
    content = function(fname) {
      setwd(tempdir())
      
      cd <- lapply(config_data, function(x) {
        if (x %in% c("map1_date", "map2_date"))
          return(format(v[[x]], "%d-%b-%Y"))
        v[[x]]
      })
      names(cd) <- config_data
      write_yaml(cd, config_file)
      
      fs <- c(config_file)
      apply(table_file_df, 1, function(d) {
        iv <- unlist(strsplit(d[["var"]], split = ".", fixed = T))
        df <- v[[iv[1]]] 
        i <- 2
        while(i <= length(iv)) {
          df <- df[[iv[i]]]
          i <- i+1
        }
        if (!is.null(df)) {
          write.csv(df, d[["file"]], row.names = F, na = "")
          fs <<- c(fs, d[["file"]])
        }
      })
      
      apply(map_file_df, 1, function(d) {
        m <- v[[d[["var"]]]]
        if (!is.null(m)) {
          write_stars(m, d[["file"]])
          fs <<- c(fs, d[["file"]])
        }
      })
      zip::zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$upload_parameter, {
    print(paste("Extracting the files:", input$upload_parameter$name))
    dpath <- input$upload_parameter$datapath
    upload_parameter(dpath)
  })
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  upload_parameter <- function(dpath) {
    file_list <- NULL
    try(file_list <- unzip(dpath, list = TRUE), silent = T)
    if (is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    unzip(dpath, exdir = data_dir)
    apply(map_file_df, 1, function(d) {
      f <- d[["file"]]
      fpath <- paste0(data_dir, "/", f)
      if (file.exists(fpath)) {
        v[[d[["var"]]]] <- suppressWarnings(read_stars(fpath))
      }
    })
    
    apply(table_file_df, 1, function(d) {
      iv <-unlist(strsplit(d[["var"]], split = ".", fixed = T))
      f <- d[["file"]]
      fpath <- paste0(data_dir, "/", f)
      if (file.exists(fpath)) {
        i1 <- iv[1]
        if(length(iv) == 1) {
          v[[i1]] <- read.csv(fpath)
        } else if(length(iv) == 2) {
          i2 <- iv[2]
          v[[i1]][[i2]] <- read.csv(fpath)
        }
      }
    })
    
    fpath <- paste0(data_dir, "/", config_file)
    if (file.exists(fpath)) {
      cd <- read_yaml(fpath)
      for (i in 1:length(cd)) {
        x <- names(cd[i])
        if (x %in% c("map1_date", "map2_date")) {
          v[[x]] <- as.Date(cd[[i]], "%d-%b-%Y")
        } else {
          v[[x]] <- cd[[i]]
        }
      }
    }
    
    # print("upload data")
    # print(v$zone_list_df)
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
        while(i <= length(iv)) {
          df <- df[[iv[i]]]
          i <- i+1
        }
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
  
  #### Output JSON #########################
  apply(json_file_df , 1, function(d) {
    output[[paste0(d[["var"]], "_id")]] <- downloadHandler(
      filename = function() {
        d[["file"]]
      },
      content = function(file) {
        listd <- v[[d[["var"]]]]
        if (!is.null(listd)) {
          write_json(listd, file, pretty=TRUE, auto_unbox=TRUE)
        }
      }
    )
  })
  
}

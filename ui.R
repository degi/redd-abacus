
generate_download_link <- function(df, title) {
  p(p(tags$b(title)), apply(df, 1, function(x) {
    downloadLink(paste0(x[["var"]], "_id"), (x[["label"]]))
  }))
}

upload_file_button <- function(prefix_id, title) {
  span(icon("upload"), "Upload") |>
    popover(
      title = div(icon("upload"), title),
      id = paste0(prefix_id, "_pop"),
      fileInput(
        paste0(prefix_id, "_upload"),
        NULL,
        accept = c(".csv"),
        placeholder = "Upload data file"
      )
    )
}

pop_inp_map <- function(..., inp_map_id) {
  popover(
    ...,
    title = div(icon("upload"), "Upload map file"),
    id = paste0(inp_map_id, "_pop"),
    fileInput(
      inp_map_id,
      NULL,
      accept = c("image/tiff"),
      placeholder = "Upload map"
    )
  )
}

pop_delete <- function(..., inp_map_id) {
  popover(
    ...,
    title = div(
      bs_icon("exclamation-circle", size = "3em"),
      "Delete zonation map",
      style = "color: #E09F3E;"
    ),
    id = "zone_delete_pop",
    p(h5("Remove this zonation map?")),
    span(
      actionButton("zone_delete_button", "Yes"),
      actionButton("zone_cancel_button", "Cancel")
    )
  )
}

if_element <- function(cond, element, element_else = NULL) {
  if (cond)
    return(element)
  if (!is.null(element_else))
    return(element_else)
}

inp_map_card <- function(map_id, title, cond_panel, is_lc = T) {
  card(
    card_header(
      div(icon("layer-group"), title),
      class = "bg_theme d-flex justify-content-between",
      if_element(
        is_lc,
        dateInput(
          paste0(map_id, "_date"),
          NULL,
          format = "d-M-yyyy",
          width = "105px"
        )
      ),
      span(
        if_element(
          !is_lc,
          conditionalPanel(
            condition = paste0("!", cond_panel),
            span(icon("trash-can"), "Remove", style = "margin-right:20px;") |> pop_delete()
          )
        ),
        span(icon("upload"), "Upload") |> pop_inp_map(inp_map_id = paste0(map_id, "_file"))
      )
    ),
    id = paste0(map_id, "_card"),
    full_screen = TRUE,
    card_body(padding = 0, leafletOutput(paste0(map_id, "_plot")))
  )
}

out_map_card <- function(id, title) {
  card(
    card_header(div(
      title, co2_unit("(t"), per_ha_unit("", ")")
    ), class = "bg_theme"),
    id = paste0(id, "_card"),
    full_screen = TRUE,
    card_body(padding = 0, leafletOutput(paste0(id, "_out")))
  )
}

box_emission <- function(id, title, icon_str, theme) {
  value_box(
    title = title,
    value = htmlOutput(paste0("box_", id, "_emission_out")),
    showcase = icon(icon_str),
    htmlOutput(paste0("box_", id, "_emission_info")),
    theme = theme
  )
}

#### LANCOVER AND ZONE VALUE BOX ####
lc_input_value_box <- layout_column_wrap(
  fill = F,
  conditionalPanel(condition = "output.is_lc_list", card_body(
    padding = 0,
    value_box(
      title = "Total land cover area",
      value = textOutput("box_area_out"),
      showcase = icon("draw-polygon"),
      textOutput("box_area_info"),
      theme = "info"
    )
  )),
  conditionalPanel(condition = "output.is_matrix", card_body(
    padding = 0,
    value_box(
      title = "Map acquisition period",
      value = textOutput("box_period_out"),
      showcase = icon("arrows-left-right-to-line"),
      textOutput("box_period_info"),
      theme = "warning"
    )
  )),
  conditionalPanel(
    condition = "output.is_matrix",
    box_emission("c", "Net of carbon emission", "tree", "success")
  )
)

#### LANCOVER AND ZONE TABLE ####
lc_zone_table_input <- layout_column_wrap(
  fill = F,
  conditionalPanel(condition = "output.is_lc_list", card(
    card_header(
      "Land cover legend",
      class = "bg_theme d-flex justify-content-between",
      upload_file_button("lc_input", "Upload land cover data")
    ),
    card_body(padding = 0, excelOutput("lc_map_list", height = "100%"))
  )),
  conditionalPanel(condition = "output.is_lc_list", card(
    card_header(
      "Carbon stock of each land cover classes",
      class = "bg_theme d-flex justify-content-between",
      upload_file_button("c_input", "Upload carbon stock data")
    ),
    card_body(padding = 0, excelOutput("cstock_list", height = "100%"))
  )),
  conditionalPanel(condition = "output.is_zone_list", card(
    card_header(
      "Zonation list and area size",
      class = "bg_theme d-flex justify-content-between",
      upload_file_button("zone_input", "Upload zonation data")
    ),
    card_body(padding = 0, excelOutput("zone_list", height = "100%"))
  ))
)

#### LC MATRIX AND EMISSION MAP ####
lc_matrix_carbon_map <- layout_column_wrap(
  height = "100%",
  style = css(grid_template_columns = "2fr 1fr"),
  card(
    card_header(
      "Land cover changes matrix (ha)",
      class = "bg_theme d-flex justify-content-between",
      conditionalPanel(condition = "output.is_zone_list", div(
        span("Select the zone:", style = "float:left;margin: 0px 20px"),
        span(selectInput("zone_select1", NULL, NULL), style = "float:right")
      ))
    ),
    full_screen = TRUE,
    card_body(padding = 0, reactableOutput("lc_changes1"))
  ),
  out_map_card("mapc", "Map of carbon emission factor")
)

#### OTHER EMISSION SELECTOR ####
other_emission_selector <- div(
  span(
    checkboxInput("include_other", "Include emission from other sources", FALSE),
    style = "float:left;"
  ),
  conditionalPanel(
    condition = "input.include_other",
    span(icon("arrow-right-long"), htmlOutput("other_source_list")) |> popover(
      title = div(icon("check"), "Select the source of emission"),
      id = "select_source_pop",
      checkboxGroupInput("select_source", NULL, NULL)
    )
  )
)

#### OTHER EMISSION INPUT ####
other_emission_input <- layout_column_wrap(
  fill = F,
  fillable = F,
  style = css(grid_template_columns = "1fr 2fr"),
  card(
    full_screen = T,
    card_header(
      markdown(
        "Emission factor list (in tCO2e ha<sup>-1</sup> yr<sup>-1</sup>)"
      ),
      class = "bg_theme d-flex justify-content-between"
    ),
    card_body(
      padding = 0,
      height = "100%",
      accordion(open = F, accordion_panel(
        title = HTML(other_emission_head), markdown(other_emission_desc)
      )),
      excelOutput("other_emission", height = "100%", width = "100%")
    )
  ),
  div(
    layout_column_wrap(
      box_emission(
        "other",
        "Net of emission from \"other\" sources",
        "fire",
        "danger"
      ),
      box_emission(
        "all",
        "Net of emission from \"all\" sources",
        "cloudversify",
        "dark"
      )
    ),
    layout_column_wrap(
      out_map_card("map_other", "Map of emission factor from \"other\" sources"),
      out_map_card("map_all", "Map of \"all combined\" emission factor")
      
    )
  )
)

please_input <- p("Please complete the required parameters in the input section.")

#### PROJECTION VALUE BOX ####
projection_value_box <- layout_column_wrap(
  width = "250px",
  fill = FALSE,
  value_box(
    id = "box_scenario",
    title = p("Number of modified conversion") ,
    value = textOutput("n_scenario"),
    showcase = icon("arrows-split-up-and-left"),
    theme = "success"
  ),
  value_box(
    id = "box_final_area",
    title = p("The largest projected area") ,
    value = textOutput("final_area1"),
    uiOutput("box_final_area_info"),
    showcase = plotlyOutput("final_plot"),
    showcase_layout = showcase_left_center(
      width = 0.4,
      width_full_screen = "1fr",
      max_height = "150px"
    ),
    full_screen = TRUE,
    theme = "info"
  ),
  value_box(
    id = "box_emission",
    title = "Net emission offset",
    value = textOutput("emission_margin"),
    uiOutput("box_emission_info"),
    showcase = plotlyOutput("emission_plot"),
    showcase_layout = showcase_left_center(width = 0.4, max_height = "150px"),
    full_screen = TRUE,
    theme = "dark"
  )
)

projection_scenario_selection <- layout_column_wrap(
  div(tags$b("Select scenario"), style = "text-align:right;"),
  selectInput("select_scenario", NULL, NULL),
  actionButton(
    "add_scenario",
    "Add new scenario",
    style = "height:38px; width:100%; padding:5px;",
    icon = icon("plus")
  ),
  actionButton(
    "remove_scenario",
    "Remove current scenario",
    style = "height:38px; width:100%; padding:5px;",
    icon = icon("trash-can")
  )
)

projection_scenario_modification <- layout_column_wrap(
  height = "100%",
  style = css(grid_template_columns = "3fr 2fr"),
  card(
    card_header("Projected land cover changes", class = "bg_theme"),
    card_body(
      padding = 5,
      accordion(
        open = F,
        accordion_panel(
          "How to modify the projection",
          tags$li(
            "Select the period by clicking the horizontal year axis on the area chart below"
          ),
          tags$li(
            "Click on the the land cover label or bar chart to edit the projection scenario"
          ),
          tags$li(
            "The input is on targeted hectare area by default.
                  Put '%' after the number to modify the percentage of projection rate"
          ),
          tags$li("Put a prefix '+' or '-' to add or reduce from the default value")
        )
      ),
      abacuslibOutput("edit_scenario", width = "100%")
    )
  ),
  card(
    card_header("Scenario details", class = "bg_theme"),
    card_body(# padding = 0,
      navset_underline(
        nav_panel(title = "Projection rate", reactableOutput("tpm_list")),
        nav_panel(
          title = "Land cover",
          reactableOutput("new_lc_list"),
          div(
            actionButton("remove_lc_scenario", "Remove"),
            actionButton("edit_lc_scenario", "Edit"),
            actionButton("add_lc_scenario", "Add"),
            style = "text-align:right;"
          )
        ),
        nav_panel(
          title = "Description",
          
          p(tags$b("Label"), textOutput("scenario_label")),
          p(tags$b("Description"), textOutput("scenario_desc")),
          div(actionButton("edit_scenario_label_btn", "Edit"), style = "text-align:right;")
          
        )
      ))
  )
)

download_panel <- layout_column_wrap(
  fill = F,
  card(
    card_header("Parameters data", class = "bg_theme"),
    p(
      "Download all the input parameters in one bundling (.zip) file.",
      tags$br(),
      "This file can be used later for the parameterization and uploaded at once."
    ),
    p(
      downloadButton("download_params", "Download all the data parameters")
    ),
    p("This bundling file contains the following data:"),
    generate_download_link(map_file_df, "Map data parameters"),
    generate_download_link(table_file_df, "Tabular data parameters"),
    p(p(tags$b(
      "Scenario parameters"
    )), uiOutput("scenario_output_params"))
  ),
  card(
    card_header("Output data", class = "bg_theme"),
    generate_download_link(output_map_file_df, "Map of emission factor"),
    # generate_download_link(json_file_df, "Projection data in JSON format"),
    generate_download_link(output_table_file_df, "Baseline projection"),
    p(p(tags$b(
      "Scenario projection"
    )), uiOutput("scenario_output_download"))
  )
)


#### UI MAIN PAGE ####
ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      primary = "#540b0e",
      dark = "#404040",
      success = "#47602B",
      info = "#335C67",
      warning = "#E09F3E",
      #"#FFF3B0",
      danger = "#9E2A2B",
      secondary = "#BE191E",
      font_scale = 0.9
    ),
    bg = "#FCE5E6",
    header =
      tags$head(
        tags$link(rel="shortcut icon", href="favicon.ico"),
        tags$style(
          HTML(
            ".bg_theme{background-color: #FCE5E6;}
            .bg_theme2{background-color: #FEF7F8;}
            .bg_warning{background-color: #F4DDBA;}

            .matrix_diagonal{background-color: #E6E6E6}
            .selectize-input{height:20px; font-size:1em;}
            .section{padding:10px; margin:0px; background-color: #FEF7F8;border: 1px solid #FCE5E6;border-radius: 5px;}
            .header_item{padding:3px 10px; margin-left:5px; background-color: #9E2A2B;
              border-radius: 8px;border: 1px solid #E6E6E6; color:white}
            .accordion {--bs-accordion-border-width: 0px;}
          "
          )
        ),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
      ),
    window_title = "REDD Abacus 2",
    title = 
    #   HTML(
    #   "<span style='color:#E42E34; background:#540b0e; padding:2px 20px 4px 20px; border-radius:15px'>
    #   <b>REDD<span style='color: white;'>Abacus</span>2</b></span>"
    # )
    span(tags$img(height = 18, src = "images/abacus2_logo_white.svg", style = "margin-right:5px"), 
         "REDD", span("Abacus", style = "color: white;", .noWS = c('before', "after") ), "2", 
         style = "color:#E42E34; background:#540b0e; padding:2px 20px 4px 20px; border-radius:15px; font-weight:bold"),
    
    underline = TRUE,
    
    nav_panel(
      #### HEADER ####
      title = "Input",
      icon = icon("arrow-right-long"),
      accordion(open = F, accordion_panel(title = HTML(main_head), markdown(main_desc))),
      
      #### MAP INPUT ####
      layout_column_wrap(
        fill = F,
        fillable = F,
        inp_map_card("map1", "1# Land cover map", "!output.is_map1"),
        conditionalPanel(
          condition = "output.is_map1",
          inp_map_card("map2", "2# Land cover map", "!output.is_map2")
        ),
        conditionalPanel(
          condition = "output.is_map2",
          inp_map_card("mapz", "Zonation map", "!output.is_mapz", F)
        )
      ),
      lc_input_value_box,
      lc_zone_table_input,
      conditionalPanel(
        condition = "output.is_matrix",
        lc_matrix_carbon_map,
        other_emission_selector,
        conditionalPanel(condition = "input.include_other", other_emission_input)
      )
    ),
    
    ### PROJECTION #############################
    nav_panel(
      title = "Projection",
      id = "panel_projection",
      icon = icon("arrow-trend-up"),
      conditionalPanel(condition = "!output.is_matrix", please_input),
      conditionalPanel(
        condition = "output.is_matrix",
        projection_value_box,
        projection_scenario_selection,
        projection_scenario_modification
      )
    ),
    
    ### OUTPUT #############################
    nav_panel(
      title = "Download",
      icon = icon("download"),
      conditionalPanel(condition = "!output.is_matrix", please_input),
      conditionalPanel(condition = "output.is_matrix", download_panel)
    ),
    
    ### MENU #############################
    nav_spacer(),
    nav_menu(
      title = "Options",
      align = "right",
      nav_item(actionLink(
        "upload_params", div(icon("upload"), "Load saved parameters (.zip)")
      )),
      nav_item(actionLink(
        "import_abacus_1",
        div(icon("file-import"), "Import REDD-Abacus-1 data (.car)")
      ))
    )
    
  )

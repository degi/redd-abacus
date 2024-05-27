library(shiny)
library(bslib)
library(bsicons)
library(abacuslib)
library(htmltools)
library(reactable)
library(plotly)
library(leaflet)
library(stars)
library(mapview)
library(excelR)

generate_download_link <- function(df, title) {
  card(card_header(title, class = "bg_theme2"),
       apply(df, 1, function(x) {
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

pop_inp_date <- function(..., inp_date_id) {
  popover(
    ...,
    title = div(icon("calendar"), "Input the map acquisition date"),
    id = paste0(inp_date_id, "_pop"),
    dateInput(inp_date_id, NULL,
              format = "d-M-yyyy")
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

inp_map_card <- function(map_id,
                         title,
                         cond_panel,
                         is_lc = T) {
  card(
    card_header(
      div(icon("layer-group"), title),
      class = "bg_theme d-flex justify-content-between",
      if_element(
        is_lc,
        div("Date:",
            textOutput(paste0(
              map_id, "_date_out"
            ), inline = T),
            class = "header_item")
        |>
          pop_inp_date(inp_date_id = paste0(map_id, "_date_b"))
      ),
      span(
        if_element(
          !is_lc,
          conditionalPanel(
            condition = paste0("!", cond_panel),
            span(icon("trash-can"), "Remove", style = "margin-right:20px;") |> pop_delete()
          )
        ),
        span(icon("upload"), "Upload") |> pop_inp_map(inp_map_id = paste0(map_id, "_file_b"))
      )
    ),
    
    id = paste0(map_id, "_card"),
    full_screen = TRUE,
    card_body(
      class = "bg_theme2",
      padding = 0,
      gap = 0,
      conditionalPanel(
        condition = cond_panel,
        div(
          style = "margin:auto; width:100%; padding:10px;",
          if_element(
            is_lc,
            dateInput(paste0(map_id, "_date_a"), "Map acquisition date",
                      format = "d-M-yyyy")
          ),
          fileInput(
            paste0(map_id, "_file_a"),
            NULL,
            accept = c("image/tiff"),
            placeholder = "Upload map file",
          )
        )
      ),
      conditionalPanel(
        condition = paste0("!", cond_panel),
        card_body(padding = 0, leafletOutput(paste0(map_id, "_plot")))
      )
    )
  )
}


out_map_card <- function(id, title) {
  card(
    card_header(div(
      title,
      co2_unit("(t"),
      per_ha_unit("", ")")
    ),
    class = "bg_theme"),
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
        tags$style(
          HTML(
            ".bg_theme{background-color: #FCE5E6;}
            .bg_theme2{background-color: #FEF7F8;}
            .bg_warning{background-color: #F4DDBA;}

            .matrix_diagonal{background-color: #E6E6E6}
            .selectize-input{height:20px; font-size:1em;}
            .section{padding:15px 0 0 20px; margin:0px; background-color: #FEF7F8;border: 1px solid #FCE5E6;border-radius: 5px;}
            .header_item{padding:3px 10px; margin-left:5px; background-color: white; border-radius: 5px;border: 1px solid #E6E6E6;}

            .accordion {
            	--bs-accordion-btn-color: black;
            	--bs-accordion-btn-bg: #FEF7F8;
            	--bs-accordion-active-color: black;
            	--bs-accordion-active-bg: #FCE5E6;
            }
          "
            
          )
        ),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css"),
      ),
    
    title = HTML(
      "<span style='color:#E42E34; background:#540b0e; padding:2px 20px 4px 20px; border-radius:15px'>
      <b>REDD<span style='color: white;'>Abacus</span>2</b></span>"
    ),
    underline = TRUE,
    
    nav_panel(
      #### INPUT OPTIONS ####
      title = "Input data",
      icon = icon("arrow-down"),
      accordion(
        open = F,
        accordion_panel(
          class = "bg_theme2",
          icon = icon("file-import"),
          "Other input options",
          actionButton("upload_params_btn", "Load bundling parameters (.zip) file")
          |> popover(
            title = div(icon("file-import"), "Load saved parameters"),
            id = "upload_params_pop",
            fileInput("upload_parameter", NULL, accept = ".zip")
          ),
          actionButton("import_abacus1_btn", "Import REDD-Abacus-1 data")
          |> popover(
            title = div(icon("file-import"), "Load REDD-Abacus-1 project file"),
            id = "import_abacus1_pop",
            fileInput("abacus_file", NULL, accept = c(".car"))
          ),
          actionButton("import_lumens_btn", "Import LUMENS data") |> popover("To be implemented..")
        )
      ),
      
      #### MAP INPUT ####
      layout_column_wrap(
        fill = F,
        fillable = F,
        inp_map_card("map1",
                     "1st land cover map",
                     "!output.is_map1"),
        inp_map_card("map2",
                     "2nd land cover map",
                     "!output.is_map2"),
        inp_map_card("mapz",
                     "Zonation map",
                     "!output.is_mapz", F)
      ),
      
      #### INFO INPUT ####
      layout_column_wrap(
        fill = F,
        conditionalPanel(condition = "output.is_lc_list",
                         card_body(
                           padding = 0,
                           value_box(
                             title = "Total land cover area",
                             value = textOutput("box_area_out"),
                             showcase = icon("draw-polygon"),
                             textOutput("box_area_info"),
                             theme = "info"
                           )
                         )),
        conditionalPanel(condition = "output.is_matrix",
                         card_body(
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
          box_emission("c", "Net of carbon emission",
                       "tree", "success")
        )
      ),
      
      #### LANCOVER AND ZONE TABLE ####
      layout_column_wrap(
        fill = F,
        conditionalPanel(condition = "output.is_lc_list",
                         card(
                           card_header(
                             "Land cover legend",
                             class = "bg_theme d-flex justify-content-between",
                             upload_file_button("lc_input", "Upload land cover data")
                             
                           ),
                           card_body(padding = 0,
                                     excelOutput("lc_map_list", height = "100%"))
                         ))
        ,
        conditionalPanel(condition = "output.is_zone_list",
                         card(
                           card_header(
                             "Zonation list and area size",
                             class = "bg_theme d-flex justify-content-between",
                             upload_file_button("zone_input", "Upload zonation data")
                           ),
                           card_body(padding = 0,
                                     excelOutput("zone_list", height = "100%"))
                         )),
        conditionalPanel(condition = "output.is_lc_list",
                         card(
                           card_header(
                             "Carbon stock of each land cover classes",
                             class = "bg_theme d-flex justify-content-between",
                             upload_file_button("c_input", "Upload carbon stock data")
                           ),
                           card_body(padding = 0,
                                     excelOutput("cstock_list", height = "100%"))
                         ))
      ),
      
      conditionalPanel(
        condition = "output.is_matrix",
        layout_column_wrap(
          height = "100%",
          style = css(grid_template_columns = "2fr 1fr"),
          card(
            card_header(
              "Land cover changes matrix (ha)",
              class = "bg_theme d-flex justify-content-between",
              conditionalPanel(condition = "output.is_zone_list",
                               div(
                                 span("Select the zone:", style = "float:left;margin: 0px 20px"),
                                 span(selectInput("zone_select1", NULL, NULL), style = "float:right")
                               ))
            ),
            full_screen = TRUE,
            card_body(padding = 0, reactableOutput("lc_changes1"))
          ),
          out_map_card("mapc", "Map of carbon emission factor")
        ),
        
        ### OTHER SOURCES OF EMISSION #######
        div(
          class = "section",
          span(
            checkboxInput("include_other", "Include emission from other sources", FALSE),
            style = "float:left;"
          ),
          conditionalPanel(
            condition = "input.include_other",
            span(icon("arrow-right-long"),
                 htmlOutput("other_source_list")) |> popover(
                   title = div(icon("check"), "Select the source of emission"),
                   id = "select_source_pop",
                   checkboxGroupInput("select_source", NULL, NULL)
                 )
          )
        ),
        conditionalPanel(
          condition = "input.include_other",
          layout_column_wrap(
            fill = F,
            fillable = F,
            style = css(grid_template_columns = "1fr 2fr"),
            card(
              card_header("Emission factor of land cover activities (tCO2e/ha/yr)",
                          class = "bg_theme d-flex justify-content-between"),
              htmlOutput("other_desc"),
              card_body(padding = 0, excelOutput("other_emission", height = "100%"))
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
        )
        
        
      ),
      
      p("Version: 2.0.0beta1", style = "text-align:right;")
    ),
    
    ### PROJECTION #############################
    
    nav_panel(
      title = "Land cover changes projection",
      id = "panel_projection",
      icon = icon("arrow-trend-up"),
      layout_column_wrap(
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
          id = "box_area",
          title = p("The largest projected area") ,
          value = textOutput("final_area1"),
          p("of", textOutput("final_lc1", inline = T)),
          p(
            "2nd",
            textOutput("final_lc2", inline = T),
            ":",
            textOutput("final_area2", inline = T)
          ),
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
          p(
            "Baseline:",
            textOutput("emission_baseline", inline = T),
            "tCO",
            tags$sub(2, .noWS = c("after", "before")),
            "-eq"
          ),
          p(
            "Scenario:",
            textOutput("emission_scenario", inline = T),
            "tCO",
            tags$sub(2, .noWS = c("after", "before")),
            "-eq"
          ),
          showcase = plotlyOutput("emission_plot"),
          showcase_layout = showcase_left_center(width = 0.4,
                                                 max_height = "150px"),
          full_screen = TRUE,
          theme = "dark"
        )
      ),
      layout_column_wrap(
        height = "100%",
        style = css(grid_template_columns = "3fr 2fr"),
        card(
          card_header("Projected land cover changes", class = "bg_theme"),
          abacuslibOutput("edit_scenario", width = "100%")
        ),
        card(
          card_header("Scenario", class = "bg_theme"),
          reactableOutput("scenario_list")
        )
      )
    ),
    
    
    ### OUTPUT #############################
    
    nav_panel(
      title = "Output data",
      icon = icon("arrow-up"),
      
      
      layout_column_wrap(
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
          generate_download_link(table_file_df, "Tabular data parameters")
        ),
        card(
          card_header("Output data", class = "bg_theme"),
          generate_download_link(output_map_file_df, "Emission factor map"),
          generate_download_link(json_file_df, "Projection data in JSON format"),
          generate_download_link(output_table_file_df, "Land cover change projection table")
        )
      )
    )
    
  )

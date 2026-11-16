


if (file.exists("R/abacus_info_docs.R")) source("R/abacus_info_docs.R")

#' Generate Group of Data Download Links
#' 
#' Builds a list of shiny downloadLink elements from a metadata data frame.
#' 
#' @param df Data frame containing `var` and `label` columns.
#' @param title Header title for the download section.
#' @return HTML paragraph tag containing download links.
generate_download_link <- function(df, title) {
  p(p(tags$b(title)), apply(df, 1, function(x) {
    downloadLink(paste0(x[["var"]], "_id"), (x[["label"]]))
  }))
}

#' Empty State Card Prompt
#' 
#' Rendered when no land cover maps have been uploaded yet across analysis tabs,
#' providing clear instructions and a direct one-click navigation link to the Land Cover Maps upload dropzone.
please_input <- tags$div(
  class = "d-flex justify-content-center align-items-center w-100 py-5",
  style = "min-height: 400px;",
  tags$div(
    class = "card border text-center p-4 p-md-5 shadow-sm",
    style = "max-width: 600px; border-radius: 12px; background: #ffffff; margin: 20px auto;",
    tags$div(
      class = "mb-3 d-inline-flex justify-content-center align-items-center rounded-circle",
      style = "width: 76px; height: 76px; margin: 0 auto; background-color: rgba(84, 11, 14, 0.08); color: #540b0e;",
      icon("map-location-dot", class = "fa-2x")
    ),
    tags$h4(
      class = "fw-bold mb-2",
      style = "color: #540b0e;",
      "No Land Cover Maps Uploaded"
    ),
    tags$p(
      class = "text-muted mb-4",
      style = "font-size: 0.95rem; line-height: 1.6;",
      "Please upload your land cover maps to begin analysis. Land cover maps are required to compute land use transitions, carbon stock changes, GHG emissions, forward projections, and economic abatement curves."
    ),
    tags$div(
      class = "d-flex flex-column align-items-center gap-2",
      tags$button(
        type = "button",
        class = "btn btn-primary px-4 py-2 d-inline-flex align-items-center gap-2 shadow-sm",
        style = "background-color: #540b0e; border-color: #540b0e; font-weight: 500;",
        onclick = "navigateToLandCoverUpload(); return false;",
        icon("upload"),
        "Go to Land Cover Maps upload"
      ),
      tags$a(
        href = "#",
        class = "text-decoration-none small mt-2 d-inline-flex align-items-center gap-1",
        style = "color: #BE191E; font-weight: 500;",
        onclick = "navigateToLandCoverUpload(); return false;",
        icon("arrow-right"),
        " Open Land Cover Maps upload page"
      )
    )
  )
)

#' Generate Generic Map Output Card
#' 
#' Constructs a standardized full-screen card for plotting spatial rasters with download toolbar.
#' 
#' @param id Identifier prefix for plot output and download handlers.
#' @param title Display title in card header.
#' @return A bslib card tag object.
out_map_card <- function(id, title) {
  card_id <- paste0(id, "_card")
  map_out <- plotOutput(paste0(id, "_out"), width = "100%", height = "240px")
  
  card(
    card_header(
      class = "bg_theme d-flex justify-content-between align-items-center",
      div(class = "d-flex align-items-center gap-2",
        icon("map"), " ", title, co2_unit("(t"), per_ha_unit("", ")")
      ),
      toolbar(
        card_download_button(paste0(id, "_stars_id"), title = paste0("Download ", title, " map (.tif)")),
        align = "right"
      )
    ),
    id = card_id,
    full_screen = TRUE,
    card_body(padding = 0, map_out)
  )
}

#' Generate GHG Emission Intensity Map Card
#' 
#' Constructs a full-screen card for rendering GHG emission intensity rasters with download toolbar.
#' 
#' @param id Identifier prefix for plot output and download handlers.
#' @param title Display title in card header.
#' @return A bslib card tag object.
ghg_map_card <- function(id, title) {
  card_id <- paste0(id, "_card")
  
  card(
    id = card_id,
    full_screen = TRUE,
    card_header(
      class = "bg_theme d-flex justify-content-between align-items-center",
      div(class = "d-flex align-items-center gap-2",
        icon("map"),
        div(title, co2_unit("(t"), per_ha_unit("", ")"))
      ),
      toolbar(
        card_download_button(paste0(id, "_stars_id"), title = paste0("Download ", title, " map (.tif)")),
        align = "right"
      )
    ),
    card_body(padding = 0, plotOutput(
      paste0(id, "_out"), width = "100%", height = "300px"
    ))
  )
}

#### LAND COVER LEGEND CARD ####
## Defined once and reused: it is the LEFT column of map_location_info_row, so
## it lines up with Relative location and General map info. Keeping the
## definition in one variable is what prevents the duplicate that arose when the
## card was written out twice with the same input ids.
lc_legend_card <- card(
  card_header(
    class = "bg_theme d-flex justify-content-between align-items-center",
    icon("palette"),
    "Land cover legend",
    toolbar(
      popover(
        trigger = toolbar_input_button(
          id      = "lc_menu_btn",
          label   = "Options",
          icon    = shiny::icon("ellipsis-vertical"),
          tooltip = FALSE
        ),
        title = "Land cover legend options",
        tags$strong("Color theme", style = "font-size: 0.85rem;"),
        selectInput(
          inputId  = "lc_palette_set",
          label    = NULL,
          choices  = legend_set_choices,
          selected = "gyr",
          width    = "100%"
        ),
        tags$hr(style = "margin: 6px 0;"),
        actionButton(
          "lc_reset_colors_btn",
          "Reset colors to theme",
          icon  = icon("rotate-left"),
          class = "btn-sm btn-light w-100 mb-2"
        ),
        downloadButton("download_lc_legend_csv", "Download CSV", class = "btn-sm btn-outline-secondary w-100 mb-2"),
        tags$hr(style = "margin: 6px 0;"),
        tags$strong("Import legend from CSV", style = "font-size: 0.85rem;"),
        tags$small(class = "text-muted d-block mb-1", "Columns: lc_id, color, label, description"),
        fileInput(
          inputId     = "lc_input_upload",
          label       = NULL,
          accept      = ".csv",
          buttonLabel = tagList(icon("upload"), " Import")
        ),
        id        = "lc_input_pop",
        placement = "bottom"
      ),
      align = "right"
    )
  ),
  card_body(padding = 0, excelOutput("lc_map_list", height = "430px"))
)

#### MAP INFO ROW (legend | relative location | general map info) ####
## One row of three columns. Neither card in it is defined here twice: each
## column renders exactly one card, and lc_legend_card is wrapped in a
## conditionalPanel that only has its content (see column_body()).
##
## The row appears as soon as ANY map is uploaded - a zonation map first
## included. When only zonation maps exist the legend column is empty, so
## Relative location and General map info keep the middle/right positions they
## have when a land cover map is present.
## A horizontal rule separates this last cards row from the uploaded map cards
## grid above it.
map_location_info_row <- conditionalPanel(
  condition = "output.is_mapz || output.is_lc_list",
  tags$hr(class = "uploaded-cards-separator"),
  layout_columns(
    col_widths = c(4, 4, 4),
    # LEFT: Land cover legend, empty until a land cover map exists
    conditionalPanel(condition = "output.is_lc_list", lc_legend_card),
    # MIDDLE: Map relative location card
    card(
      full_screen = TRUE,
      card_header(
        class = "bg_theme d-flex align-items-center justify-content-between",
        div(class = "d-flex align-items-center gap-2", icon("globe"), "Relative location"),
        toolbar(
          card_download_button("download_boundary_geojson", title = "Download study area boundary (GeoJSON)"),
          align = "right"
        )
      ),
      card_body(
        padding = 0,
        leafletOutput("map_location_leaflet", height = "430px")
      )
    ),
    # RIGHT: General map info card
    card(
      full_screen = TRUE,
      card_header(class = "bg_theme d-flex align-items-center gap-2", icon("circle-info"), "General map info"),
      card_body(uiOutput("map_general_info_ui"))
    )
  )
)

#### LANCOVER TABLE ####
## The legend card now lives in map_location_info_row (above); nothing is left
## to add here. It used to be a second copy of the same card, which rendered the
## legend twice and duplicated every input id.
lc_map_table_input <- NULL

carbon_emission_first_row <- tagList(
  conditionalPanel(condition = "output.is_lc_list", layout_columns(
    col_widths = c(4, 8),
    # COLUMN 1: Carbon stock of land cover classes
    card(
      id = "cstock_card",
      full_screen = TRUE,
      card_header(
        class = "bg_theme d-flex justify-content-between align-items-center",
        icon("database"),
        "Carbon stocks by land cover class (tC ha⁻¹)",
        toolbar(
          popover(
            trigger = toolbar_input_button(
              id      = "c_menu_btn",
              label   = "Options",
              icon    = shiny::icon("ellipsis-vertical"),
              tooltip = FALSE
            ),
            title = "Carbon stock options",
            actionButton(
              "cstock_reset_btn",
              "Reset all to 0",
              icon  = icon("rotate-left"),
              class = "btn-sm btn-light w-100 mb-2"
            ),
            downloadButton("download_cstock_csv", "Download CSV", class = "btn-sm btn-outline-secondary w-100 mb-2"),
            tags$hr(style = "margin: 6px 0;"),
            tags$strong("Import C-stock from CSV", style = "font-size: 0.85rem;"),
            tags$small(class = "text-muted d-block mb-1", "Columns: lc_id, c (tC ha⁻¹)"),
            fileInput(
              inputId     = "c_input_upload",
              label       = NULL,
              accept      = ".csv",
              buttonLabel = tagList(icon("upload"), " Import")
            ),
            id        = "c_input_pop",
            placement = "bottom"
          ),
          align = "right"
        )
      ),
      card_body(padding = 0, excelOutput("cstock_list", height = "auto"))
    ),
    
    # RIGHT SECTION (Width 8): Selector on top, then Map & Summary cards side-by-side
    div(
      class = "d-flex flex-column",
      div(
        class = "d-flex align-items-center matrix-header-controls flex-wrap bg_period",
        span(
          "Land cover changes period:",
          class = "matrix-header-label",
          style = "margin-left: 0;"
        ),
        shiny::selectizeInput(
          "analysis_map1",
          NULL,
          NULL,
          width = "160px",
          options = list(placeholder = "Before map")
        ),
        span(icon("arrow-right-long"), class = "matrix-header-label"),
        shiny::selectizeInput(
          "analysis_map2",
          NULL,
          NULL,
          width = "160px",
          options = list(placeholder = "After map")
        ),
      ),
      layout_columns(
        style = "margin-top: 20px;",
        col_widths = c(6, 6),
        card(
          id = "mapc_card",
          full_screen = TRUE,
          card_header(
            class = "bg_theme d-flex align-items-center justify-content-between",
            div(class = "d-flex align-items-center gap-2",
              icon("map"),
              div("Emission intensity", co2_unit("(t"), per_ha_unit("", ")"))
            ),
            toolbar(
              card_download_button("mapc_stars_id", title = "Download carbon emission map (.tif)"),
              align = "right"
            )
          ),
          card_body(
            padding = 0,
            plotOutput("mapc_out", width = "100%", height = "300px")
          )
        ),
        card(
          id = "c_emission_summary_card",
          full_screen = TRUE,
          card_header(class = "bg_theme d-flex align-items-center gap-2", icon("list"), "Carbon emission summary"),
          card_body(uiOutput("c_emission_summary_ui"))
        )
      )
    )
  )),
  conditionalPanel(condition = "!output.is_lc_list", please_input)
)

emission_factor_table_input <- carbon_emission_first_row

#### LC MATRIX AND EMISSION MAP ####
lc_changes_matrix_card <- card(
  card_header(
    tags$div(
      class = "d-flex justify-content-between align-items-center w-100 matrix-header",
      span(
        icon("table-cells-large"),
        " ",
        uiOutput("matrix_card_title", inline = TRUE)
      ),
      chart_download_toolbar(
        button_id                = "lc_changes_download_btn",
        chart_container_selector = "#lc_area_bar, #lc_changes_scenario",
        base_filename            = "landcover_changes",
        html_download_id         = "download_lc_changes_html",
        csv_download_id          = "download_lc_changes_csv",
        title                    = "Download Land Cover Changes"
      )
    )
  ),
  full_screen = TRUE,
  card_body(
    padding = 0,
    div(
      class = "bg_period",
      style = "margin:20px;",
      conditionalPanel(
        condition = "output.is_lc_list",
        div(
          class = "d-flex align-items-center matrix-header-controls mb-3",
          style = "margin-top:10px;",
          span("Period:", class = "matrix-header-label"),
          shiny::selectizeInput(
            "matrix_map1",
            NULL,
            NULL,
            width = "200px",
            options = list(placeholder = "Before map")
          ),
          span(icon("arrow-right-long"), class = "matrix-header-label"),
          shiny::selectizeInput(
            "matrix_map2",
            NULL,
            NULL,
            width = "200px",
            options = list(placeholder = "After map (none)")
          ),
          span(icon("circle-info"), title = "Select a single map to show its land cover area instead of the changes matrix", class = "matrix-header-label")
        )
      ),
      conditionalPanel(
        condition = "output.is_zone_available",
        div(
          class = "d-flex align-items-center matrix-header-controls mb-3",
          span("Zonation maps:", class = "matrix-header-label"),
          ## Multi-select: several zonation maps can be overlaid on the same land
          ## cover pair at once. Clearing it removes the overlay entirely (and the
          ## zone selectors with it).
          shiny::selectizeInput(
            "matrix_zone_map",
            NULL,
            NULL,
            multiple = TRUE,
            width = "280px",
            options = list(placeholder = "No zonation map", plugins = list("remove_button"))
          ),
          ## One zone selector per selected zonation map, built server-side: two
          ## maps give two selectors, three give three, and so on. Each filters the
          ## zones of its own map, so the pair of selections is a combination filter
          ## (e.g. Zonation A -> Zone 3 AND Zonation B -> Zone 7).
          ##
          ## Renders nothing when the zonation map list is empty, so the zone
          ## controls disappear rather than sitting there as idle "All zones" boxes.
          uiOutput("zone_filters", inline = TRUE)
        )
      )
    ),
    conditionalPanel(condition = "output.is_matrix_pair", scenario_editor_output("lc_changes_scenario", width = "100%", height = "560px")),
    conditionalPanel(condition = "output.is_matrix_single", plotlyOutput("lc_area_bar", height = "420px"))
  )
)

#### OTHER EMISSION SELECTOR ####
other_emission_selector <- NULL

#### OTHER EMISSION INPUT ####
## Same two-column layout as the carbon emission tab: the GHG factor table on
## the left (4/12), and on the right (8/12) the period selector with the map and
## summary cards side by side underneath.
other_emission_input <- layout_columns(
  col_widths = c(4, 8),
  # COLUMN 1: GHG emission factor table
  card(
    id = "other_emission_card",
    full_screen = T,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      div(
        icon("arrow-up-from-bracket"),
        span(
          style = "font-weight: 600; margin-left: 6px;",
          HTML(
            "Flux emission factors (tCO<sub>2</sub>e ha<sup>-1</sup> yr<sup>-1</sup>)"
          )
        )
      ),
      div(
        class = "d-flex align-items-center gap-2",
        toolbar(
          popover(
            trigger = toolbar_input_button(
              id      = "other_emission_menu_btn",
              label   = "Options",
              icon    = shiny::icon("ellipsis-vertical"),
              tooltip = FALSE
            ),
            title = "Flux Emission Factor Options",
            actionButton(
              "clear_other_emission_btn",
              "Clear all factors",
              icon  = icon("trash"),
              class = "btn-sm btn-light w-100 mb-2"
            ),
            downloadButton("download_other_emission_csv", "Download CSV", class = "btn-sm btn-outline-secondary w-100 mb-2"),
            tags$hr(style = "margin: 6px 0;"),
            tags$strong("Import from CSV", style = "font-size: 0.85rem;"),
            tags$small(class = "text-muted d-block mb-1", "Columns: applies_to, efactor, source"),
            fileInput(
              inputId     = "upload_other_emission_csv",
              label       = NULL,
              accept      = ".csv",
              buttonLabel = tagList(icon("upload"), " Import")
            ),
            id        = "other_emission_menu_pop",
            placement = "bottom"
          ),
          align = "right"
        )
      )
    ),
    card_body(
      padding = 0,
      height = "100%",
      actionButton(
        "add_other_emission_btn",
        "Add Factor",
        icon = icon("plus"),
        class = "btn-sm btn-primary"
      ),
      reactableOutput("other_emission_table")
    )
  ),
  
  # RIGHT SECTION (Width 8): Selector on top, then Map & Summary cards side-by-side
  div(
    class = "d-flex flex-column",
    div(
      class = "matrix-header-controls bg_period",
      div(
        class = "d-flex align-items-center flex-wrap mb-2",
        span(
          "Land cover changes period:",
          class = "matrix-header-label",
          style = "margin-left: 0; min-width: 195px;"
        ),
        shiny::selectizeInput(
          "ghg_analysis_map1",
          NULL,
          NULL,
          width = "160px",
          options = list(placeholder = "Before map")
        ),
        span(icon("arrow-right-long"), class = "matrix-header-label"),
        shiny::selectizeInput(
          "ghg_analysis_map2",
          NULL,
          NULL,
          width = "160px",
          options = list(placeholder = "After map")
        )
      ),
      div(
        class = "d-flex align-items-center flex-wrap",
        span("Flux emission sources:", class = "matrix-header-label", style = "margin-left: 0; min-width: 195px;"),
        shiny::selectizeInput(
          "select_source",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          width = "380px",
          options = list(placeholder = "No flux emission source selected", plugins = list("remove_button"))
        )
      )
    ),
    layout_columns(
      style = "margin-top: 20px;",
      col_widths = c(6, 6),
      ghg_map_card("map_ghg", "Emission intensity"),
      card(
        id = "ghg_emission_summary_card",
        full_screen = TRUE,
        card_header(class = "bg_theme d-flex align-items-center gap-2", icon("list"), "Flux emission summary"),
        card_body(uiOutput("ghg_emission_summary_ui"))
      )
    )
  )
)

#### COMBINED EMISSION SUMMARY ####
## Carbon stock-change plus GHG flux: period selector, combined intensity map,
## and a summary that adds the two sources.
combined_emission_summary_input <- tagList(
  conditionalPanel(
    condition = "output.is_lc_list",
    div(
      class = "d-flex flex-column",
      div(
        class = "matrix-header-controls bg_period",
        div(
          class = "d-flex align-items-center flex-wrap",
          span(
            "Land cover changes period:",
            class = "matrix-header-label",
            style = "margin-left: 0; min-width: 195px;"
          ),
          shiny::selectizeInput(
            "summary_analysis_map1",
            NULL,
            NULL,
            width = "160px",
            options = list(placeholder = "Before map")
          ),
          span(icon("arrow-right-long"), class = "matrix-header-label"),
          shiny::selectizeInput(
            "summary_analysis_map2",
            NULL,
            NULL,
            width = "160px",
            options = list(placeholder = "After map")
          )
        ),
        conditionalPanel(
          condition = "output.has_other_emission_source",
          div(
            class = "d-flex align-items-center flex-wrap",
            style = "margin-top: 8px;",
            span("Flux emission sources:", class = "matrix-header-label", style = "margin-left: 0; min-width: 195px;"),
            shiny::selectizeInput(
              "summary_select_source",
              label = NULL,
              choices = NULL,
              multiple = TRUE,
              width = "380px",
              options = list(placeholder = "No flux emission source selected", plugins = list("remove_button"))
            )
          )
        )
      ),
      layout_columns(
        style = "margin-top: 20px;",
        col_widths = c(6, 6),
        ghg_map_card("map_all", "Combined emission intensity"),
        card(
          id = "combined_emission_summary_card",
          full_screen = TRUE,
          card_header(class = "bg_theme d-flex align-items-center gap-2", icon("list"), "Combined emission summary"),
          card_body(uiOutput("combined_emission_summary_ui"))
        )
      )
    )
  ),
  conditionalPanel(condition = "!output.is_lc_list", please_input)
)

#### PROJECTION DEMO SCENARIO EDITOR ####
projection_panel_ui <- card_body(
  padding = 10,
  conditionalPanel(condition = "!output.has_scenario", div(
    class = "p-3",
    div(
      class = "d-flex flex-column align-items-center justify-content-center text-center p-5 bg-white rounded-3 border shadow-sm",
      style = "min-height: 480px;",
      div(class = "mb-3 p-3 bg-light rounded-circle text-primary border", icon("layer-group", class = "fa-3x")),
      tags$h4("No Scenarios Defined", class = "fw-bold text-dark mb-2"),
      tags$p(
        "Start by creating your first scenario. A scenario allows you to project land cover changes, see how emissions deviate from the baseline, and simulate interventions.",
        class = "text-muted mb-4",
        style = "max-width: 560px; font-size: 1rem;"
      ),
      uiOutput("empty_scenario_inputs_summary"),
      div(
        class = "mt-3",
        actionButton(
          "btn_add_first_scenario",
          "Add First Scenario",
          icon = icon("plus"),
          class = "btn-primary btn-lg shadow-sm px-4"
        )
      )
    )
  )),
  conditionalPanel(
    condition = "output.has_scenario",
    card_body(fill = FALSE, layout_column_wrap(
      height = 250,
      ## Scenario card: body and header share the same background.
      card(
        id = "scenario_card",
        class = "border-0",
        style = "background-color: #99a88c;",
        card_header(
          class = "text-black d-flex align-items-center gap-2",
          style = "background-color: #99a88c; color: #000000;",
          toolbar(
            align = "right",
            toolbar_input_select(
              "sel_scenario",
              "Scenario",
              choices = c("(No scenarios)" = ""),
              width = "100%"
            ),
            toolbar_divider(),
            toolbar_input_button("add_scenario", "Add", icon = shiny::icon("plus")),
            toolbar_input_button("edit_scenario", "Edit", icon = shiny::icon("pen")),
            toolbar_input_button("remove_scenario", "Remove", icon = shiny::icon("trash"))
          )
        ),
        card_body(padding = 10, uiOutput("scenario_card_info_ui"))
      ),
      ## Headerless, borderless card: the plot heading + chart carry their own
      ## layout, so the card is just a transparent, unpadded container.
      card(
        class = "border-0 shadow-none bg-transparent",
        card_header(
          class = "bg-transparent border-0 p-0 d-flex justify-content-between align-items-center mb-1",
          div(
            class = "area-plot-heading mb-0",
            div("Projected Land Cover Area (ha)", class = "area-plot-title"),
            div(class = "area-plot-subtitle", textOutput("area_plot_zone"))
          ),
          chart_download_toolbar(
            button_id                = "area_plot_download_btn",
            chart_container_selector = "#area_plot",
            base_filename            = "projected_landcover_area",
            html_download_id         = "download_area_proj_html",
            csv_download_id          = "download_area_proj_csv",
            title                    = "Download Projected Area"
          )
        ),
        card_body(
          padding = 0,
          fill = TRUE,
          class = "d-flex flex-column gap-1",
          lc_area_chart_output("area_plot", width = "100%", height = "190px")
        )
      ),
      ## Headerless, borderless card: the content (plot + summary box) carries
      ## its own frame, so the card itself is just a transparent container.
      ## Title/flag span the full row width, above the plot + summary row.
      card(
        id = "box_emission",
        full_screen = FALSE,
        card_header(
          class = "bg-transparent border-0 p-0 d-flex justify-content-between align-items-center mb-1",
          div(class = "d-flex align-items-center", uiOutput("box_emission_header")),
          chart_download_toolbar(
            button_id                = "emission_plot_download_btn",
            chart_container_selector = "#emission_plot",
            base_filename            = "projected_emissions",
            html_download_id         = "download_emission_proj_html",
            csv_download_id          = "download_emission_proj_csv",
            title                    = "Download Emission Projection"
          )
        ),
        card_body(
          padding = 0,
          layout_column_wrap(
            width = 1 / 2,
            card_body(
              padding = 0,
              plotlyOutput("emission_plot", width = "100%", height = "100%")
            ),
            card_body(padding = 0, uiOutput("box_emission_info", width = "100%"))
          )
        )
      )
    )),
    
    
    
    navset_card_tab(
      nav_panel(
        "Scenario Editor",
        div(
          class = "scenario-editor-wrapper",
          div(
            class = "selector-toolbar",
            conditionalPanel(
              condition = "output.has_scenario_zones",
              style = "display: contents;",
              div(
                class = "selector-group selector-zone",
                tags$label("Zone", `for` = "sel_zone", class = "form-label text-muted small mb-1"),
                selectInput("sel_zone", NULL, choices = NULL, width = "100%")
              ),
              actionButton(
                "show_zonation_map",
                "Map",
                icon = icon("map"),
                class = "btn-outline-secondary btn-action",
                title = "Show zonation map"
              )
            ),
            div(
              class = "selector-group selector-period",
              tags$label("Period", `for` = "sel_iteration", class = "form-label text-muted small mb-1"),
              selectInput("sel_iteration", NULL, choices = NULL, width = "100%")
            )
          ),
          div(
            class = "flex-grow-1 p-2 d-flex flex-column",
            scenario_editor_output("widget_out", width = "100%", height = "560px")
          )
        )
      ),
      nav_panel("Edited Items", div(
        class = "scenario-editor-wrapper",
        div(
          class = "d-flex justify-content-end align-items-center gap-2 px-2 pt-2",
          actionButton(
            "reset_scenario_edits_btn",
            "Reset all edits",
            icon  = icon("rotate-left"),
            class = "btn-sm btn-outline-warning"
          ),
          downloadButton("download_scenario_edits_csv", "Download CSV", class = "btn-sm btn-outline-secondary")
        ),
        div(
          class = "p-2 h-100",
          reactableOutput("scenario_table", width = "100%", height = "100%")
        )
      ))
    )
  )
)

#### PROJECTION SUMMARY UI ####
projection_summary_ui <- card_body(
  padding = 15,
  conditionalPanel(
    condition = "!output.has_scenario",
    div(
      class = "d-flex flex-column align-items-center justify-content-center text-center p-5 bg-white rounded-3 border shadow-sm",
      style = "min-height: 480px;",
      div(class = "mb-3 p-3 bg-light rounded-circle text-primary border", icon("chart-line", class = "fa-3x")),
      tags$h4("No Scenarios Defined", class = "fw-bold text-dark mb-2"),
      tags$p("Define intervention scenarios first under 'Intervention scenarios' to view and compare cumulative projected emissions across all scenarios.", class = "text-muted mb-4", style = "max-width: 540px; font-size: 1rem;")
    )
  ),
  conditionalPanel(
    condition = "output.has_scenario",
    card(
      id = "scenarios_emission_summary_card",
      full_screen = TRUE,
      card_header(
        class = "bg_theme d-flex justify-content-between align-items-center",
        div(class = "d-flex align-items-center gap-2", icon("chart-line"), span(style = "font-weight: 600;", "Cumulative Projected Emissions (All Scenarios)")),
        chart_download_toolbar(
          button_id                = "scenarios_emission_summary_download_btn",
          chart_container_selector = "#scenarios_emission_summary_plot",
          base_filename            = "cumulative_emissions_all_scenarios",
          html_download_id         = "download_scenarios_emission_summary_html",
          csv_download_id          = "download_scenarios_emission_summary_csv",
          title                    = "Download Summary Chart"
        )
      ),
      card_body(
        padding = 10,
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-2 pb-2 border-bottom",
          div(
            class = "d-flex align-items-center gap-2",
            span(class = "text-muted small fw-semibold text-uppercase", style = "font-size: 0.75rem; letter-spacing: 0.5px;",
                 icon("filter", class = "me-1 text-secondary"), "Emission type:"),
            shinyWidgets::radioGroupButtons(
              inputId = "projection_summary_emission_type",
              label = NULL,
              choices = c(
                "Net emission" = "net",
                "Positive emission" = "positive",
                "Negative emission (sequestration)" = "negative"
              ),
              selected = "net",
              status = "outline-primary",
              size = "sm",
              individual = FALSE
            )
          ),
          div(
            class = "text-muted small fst-italic", style = "font-size: 0.75rem;",
            uiOutput("projection_summary_emission_type_hint", inline = TRUE)
          )
        ),
        tags$style(HTML("
          #scenarios_emission_summary_plot .hoverlayer .hovertext text:first-of-type:not(:last-of-type),
          #scenarios_emission_summary_plot .hoverlayer .hovertext .hover-title,
          #scenarios_emission_summary_plot .hoverlayer .hovertext text.hover-title,
          #scenarios_emission_summary_plot .hoverlayer .hovertext text.name,
          #scenarios_emission_summary_plot .hoverlayer .hovertext text tspan:first-of-type:not(:last-of-type),
          #scenarios_emission_summary_plot .hoverlayer .hovertext text tspan.line:first-of-type:not(:last-of-type),
          #scenarios_emission_summary_plot .hoverlayer .axistext,
          #scenarios_emission_summary_plot .hoverlayer .xhovertext,
          #scenarios_emission_summary_plot .hoverlayer .yhovertext,
          #scenarios_emission_summary_plot .hoverlayer .axistitle {
            display: none !important;
            opacity: 0 !important;
            visibility: hidden !important;
            font-size: 0 !important;
            line-height: 0 !important;
          }
        ")),
        tags$script(HTML("
          (function() {
            function cleanHover() {
              var plot = document.getElementById('scenarios_emission_summary_plot');
              if (!plot) return;
              var hoverlayer = plot.querySelector('.hoverlayer');
              if (!hoverlayer) return;

              // Hide/clear any text or tspan inside hoverlayer that is an x-tick/header
              // (does not contain ':' or 'CO' or '[', which all our trace lines have)
              var allTexts = hoverlayer.querySelectorAll('text, tspan');
              allTexts.forEach(function(el) {
                if (el.children && el.children.length > 0) return;
                var txt = el.textContent ? el.textContent.trim() : '';
                if (!txt) return;
                if (!txt.includes(':') && !txt.includes('CO') && !txt.includes('[')) {
                  el.textContent = '';
                  el.style.display = 'none';
                  el.style.visibility = 'hidden';
                  el.style.fontSize = '0px';
                }
              });

              var ax = hoverlayer.querySelectorAll('.axistext, .xhovertext, .hover-title, .axistitle');
              ax.forEach(function(el) {
                el.style.display = 'none';
                el.textContent = '';
              });
            }

            var hoverObserver = null;
            function initHoverObserver() {
              var target = document.getElementById('scenarios_emission_summary_plot');
              if (!target) return;
              if (target._hoverObsAttached) return;
              target._hoverObsAttached = true;
              if (hoverObserver) hoverObserver.disconnect();
              hoverObserver = new MutationObserver(cleanHover);
              hoverObserver.observe(target, { childList: true, subtree: true, characterData: true });
              target.addEventListener('mousemove', cleanHover, { passive: true });
            }

            $(document).on('shiny:value shiny:connected', function() {
              setTimeout(initHoverObserver, 200);
              setTimeout(initHoverObserver, 800);
            });
            $(document).ready(function() {
              initHoverObserver();
              setTimeout(initHoverObserver, 500);
            });
          })();
        ")),
        plotlyOutput("scenarios_emission_summary_plot", height = "520px")
      )
    ),
    # Summary table below the chart
    card(
      id = "scenarios_emission_summary_table_card",
      class = "mt-3",
      card_header(
        class = "bg_theme d-flex align-items-center gap-2",
        icon("table"), span(style = "font-weight: 600;", uiOutput("scenarios_emission_summary_table_title", inline = TRUE))
      ),
      card_body(
        padding = 10,
        uiOutput("scenarios_emission_summary_table")
      )
    )
  )
)


#### NPV INPUT DATA UI ####
## The NPV spreadsheet card, shared by the Economic Trade-Off tab (left column)
## and the standalone "NPV Input Data" tab. Defined before economic_tradeoff_ui
## because that UI references it.
npv_input_card <- card(
  id = "npv_card",
  full_screen = TRUE,
  card_header(
    class = "bg_theme d-flex justify-content-between align-items-center",
    div(
      icon("coins"),
      span(style = "font-weight: 600; margin-left: 6px;", "Net Present Value")
    ),
    toolbar(
      popover(
        trigger = toolbar_input_button(
          id      = "npv_menu_btn",
          label   = "Options",
          icon    = shiny::icon("ellipsis-vertical"),
          tooltip = FALSE
        ),
        title = "NPV Options",
        actionButton("npv_set_zero_btn", "Reset All to $0", icon = icon("rotate-left"), class = "btn-sm btn-light w-100 mb-2"),
        downloadButton("download_npv_csv", "Download CSV", class = "btn-sm btn-outline-secondary w-100 mb-2"),
        tags$hr(style = "margin: 6px 0;"),
        tags$strong("Import NPV from CSV", style = "font-size: 0.85rem;"),
        fileInput("upload_npv_csv", NULL, accept = ".csv", width = "100%",
                  buttonLabel = tagList(icon("upload"), " Import")),
        id        = "npv_menu_pop",
        placement = "bottom"
      ),
      align = "right"
    )
  ),
  card_body(
    padding = 0,
    excelOutput("npv_list", height = "auto")
  )
)

#### ECONOMIC TRADE-OFF UI ####
economic_tradeoff_ui <- tagList(
  tags$script(HTML("
    function isNoZoneSelected() {
      var zone = $('#abacost_zone_map').val();
      return !zone || zone === '' || zone === 'No zonation map';
    }

    function findChartTarget(selector) {
      if (!selector) return null;
      var elList = document.querySelectorAll(selector);
      if (!elList || elList.length === 0) return null;
      for (var i = 0; i < elList.length; i++) {
        var el = elList[i];
        if (el.offsetParent === null && el.offsetWidth === 0 && el.offsetHeight === 0 && !el.querySelector('svg, .js-plotly-plot')) {
          continue;
        }
        var plotly = (el.classList && el.classList.contains('js-plotly-plot')) ? el : el.querySelector('.js-plotly-plot');
        if (plotly) return { type: 'plotly', element: plotly };
        var svg = (el.tagName && el.tagName.toLowerCase() === 'svg') ? el : el.querySelector('svg');
        if (svg) return { type: 'svg', element: svg };
      }
      return null;
    }

    function downloadChartAsPng(containerSelector, filename) {
      filename = filename || 'chart.png';
      var target = findChartTarget(containerSelector);
      if (!target) return;

      if (target.type === 'plotly' && window.Plotly && window.Plotly.downloadImage) {
        Plotly.downloadImage(target.element, {
          format: 'png',
          filename: filename.replace(/\\.png$/i, ''),
          height: target.element.clientHeight || 500,
          width: target.element.clientWidth || 800
        });
        return;
      }

      var svg = target.element;
      if (containerSelector.indexOf('#curve') !== -1 && isNoZoneSelected()) {
        var col = $('#abacost_bar_color').val() || '#2b8cbe';
        var bars = svg.querySelectorAll('.bar');
        for (var i = 0; i < bars.length; i++) {
          bars[i].setAttribute('fill', col);
        }
      }

      var serializer = new XMLSerializer();
      var svgStr = serializer.serializeToString(svg);
      var svgBlob = new Blob([svgStr], {type: 'image/svg+xml;charset=utf-8'});
      var url = URL.createObjectURL(svgBlob);
      var img = new Image();
      img.onload = function() {
        var canvas = document.createElement('canvas');
        var bbox = svg.getBoundingClientRect();
        var width = bbox.width || 1100;
        var height = bbox.height || 550;
        canvas.width = width * 2;
        canvas.height = height * 2;
        var ctx = canvas.getContext('2d');
        ctx.scale(2, 2);
        ctx.fillStyle = '#ffffff';
        ctx.fillRect(0, 0, width, height);
        ctx.drawImage(img, 0, 0, width, height);
        URL.revokeObjectURL(url);

        var a = document.createElement('a');
        a.href = canvas.toDataURL('image/png');
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
      };
      img.src = url;
    }

    function downloadChartAsSvg(containerSelector, filename) {
      filename = filename || 'chart.svg';
      var target = findChartTarget(containerSelector);
      if (!target) return;

      if (target.type === 'plotly' && window.Plotly && window.Plotly.downloadImage) {
        Plotly.downloadImage(target.element, {
          format: 'svg',
          filename: filename.replace(/\\.svg$/i, ''),
          height: target.element.clientHeight || 500,
          width: target.element.clientWidth || 800
        });
        return;
      }

      var svg = target.element;
      if (containerSelector.indexOf('#curve') !== -1 && isNoZoneSelected()) {
        var col = $('#abacost_bar_color').val() || '#2b8cbe';
        var bars = svg.querySelectorAll('.bar');
        for (var i = 0; i < bars.length; i++) {
          bars[i].setAttribute('fill', col);
        }
      }

      var serializer = new XMLSerializer();
      var source = serializer.serializeToString(svg);
      if (!source.match(/^<svg[^>]+xmlns=/)) {
        source = source.replace(/^<svg/, \"<svg xmlns='http://www.w3.org/2000/svg'\");
      }
      var blob = new Blob([source], {type: 'image/svg+xml;charset=utf-8'});
      var url = URL.createObjectURL(blob);
      var a = document.createElement('a');
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }

    $(document).on('click', '#btn_download_png', function() {
      downloadChartAsPng('#curve', 'abatement_cost_curve.png');
    });

    $(document).on('click', '#btn_download_svg', function() {
      downloadChartAsSvg('#curve', 'abatement_cost_curve.svg');
    });

    // Real-time preview while dragging color picker in no-zonation mode
    $(document).on('input', '#abacost_bar_color', function() {
      if (isNoZoneSelected()) {
        var col = $(this).val();
        var svgBars = document.querySelectorAll('#curve svg .bar');
        for (var i = 0; i < svgBars.length; i++) {
          svgBars[i].setAttribute('fill', col);
        }
      }
    });

    // Commit change to Shiny once dialog closes / selection finalized
    $(document).on('change', '#abacost_bar_color', function() {
      var col = $(this).val();
      if (isNoZoneSelected()) {
        var svgBars = document.querySelectorAll('#curve svg .bar');
        for (var i = 0; i < svgBars.length; i++) {
          svgBars[i].setAttribute('fill', col);
        }
      }
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('abacost_bar_color', col);
      }
    });

    $(document).on('shiny:connected', function() {
      var el = document.getElementById('abacost_bar_color');
      if (el && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('abacost_bar_color', el.value);
      }
    });

    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('set_bar_color', function(color) {
        var el = document.getElementById('abacost_bar_color');
        if (el) {
          el.value = color;
        }
        if (isNoZoneSelected()) {
          var svgBars = document.querySelectorAll('#curve svg .bar');
          for (var i = 0; i < svgBars.length; i++) {
            svgBars[i].setAttribute('fill', color);
          }
        }
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('abacost_bar_color', color);
        }
      });
    }
  ")),
  ## Two-column layout: the NPV input table on the left, and on the right the
  ## period / zone selectors above the abatement cost curve.
  layout_columns(
    col_widths = c(4, 8),
    npv_input_card,
    div(
      class = "abacost-right-col d-flex flex-column",
      ## Period & zone selectors toolbar in colored banner box
      div(
        class = "matrix-header-controls bg_period mb-3",
        div(
          class = "d-flex align-items-center flex-wrap gap-2",
          span(
            "Period:",
            class = "matrix-header-label",
            style = "margin-left: 0;"
          ),
          shiny::selectizeInput(
            "abacost_map1",
            NULL,
            NULL,
            width = "140px",
            options = list(placeholder = "Before map")
          ),
          span(icon("arrow-right-long"), class = "matrix-header-label"),
          shiny::selectizeInput(
            "abacost_map2",
            NULL,
            NULL,
            width = "140px",
            options = list(placeholder = "After map")
          ),
          span(
            "Zone map:",
            class = "matrix-header-label",
            style = "margin-left: 12px;"
          ),
          shiny::selectizeInput(
            "abacost_zone_map",
            NULL,
            choices = c("No zonation map" = ""),
            width = "200px",
            options = list(placeholder = "No zonation map")
          )
        )
      ),
      card(
        id = "abacost_curve_card",
        full_screen = TRUE,
        ## Fixed height so the configuration sidebar scrolls inside the card
        ## instead of stretching it when opened.
        height = "620px",
        card_header(
          class = "bg_theme d-flex justify-content-between align-items-center",
          div(
            icon("chart-column"),
            span(style = "font-weight: 600; margin-left: 6px;", "Abatement Cost Curve")
          ),
          div(
            class = "d-flex align-items-center gap-2",
            chart_download_toolbar(
              button_id                = "abacost_hdr_download_btn",
              chart_container_selector = "#curve",
              base_filename            = "abatement_cost_curve",
              html_download_id         = "download_html_hdr",
              csv_download_id          = "download_abacost_csv",
              title                    = "Download Curve"
            )
          )
        ),
        card_body(
          class = "p-0 m-0",
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
              id = "abacost_config_sidebar",
              width = 300,
              padding = 10,
              gap = 8,
              class = "abacost-config-sidebar",
              open = "closed",
              h5(class = "fw-bold mb-2", icon("sliders"), " Configuration"),
              checkboxInput("show_negatives", "Show Negative Emissions", value = FALSE),
              checkboxInput("show_reference", "Show Cost Threshold Line", value = TRUE),
              numericInput("reference_cost", "Initial Threshold ($/Mg CO2-eq):", value = 0.005, step = 0.001),
              selectInput("color_theme", "Color Theme:", choices = c("viridis", "rainbow", "turbo", "cool"), selected = "viridis"),
              selectInput("y_scale", "Y-Axis Scale:", choices = c("logarithmic", "linear", "square-root"), selected = "logarithmic"),
              conditionalPanel(
                condition = "input.abacost_zone_map && input.abacost_zone_map != ''",
                checkboxInput("show_legend", "Show Legend", value = TRUE)
              ),
              conditionalPanel(
                condition = "!input.abacost_zone_map || input.abacost_zone_map == ''",
                tags$div(
                  class = "form-group shiny-input-container",
                  style = "margin-bottom: 15px;",
                  tags$label(class = "control-label", style = "font-weight: 600;", "Bar Color (no zonation):"),
                  tags$div(
                    class = "d-flex align-items-center gap-2",
                    tags$input(
                      id = "abacost_bar_color",
                      type = "color",
                      class = "form-control form-control-color",
                      value = "#2b8cbe",
                      style = "width: 50px; height: 38px; padding: 3px; cursor: pointer; flex-shrink: 0;"
                    ),
                    actionButton("abacost_reset_color_btn", "Reset to theme", class = "btn btn-sm btn-outline-secondary", style = "font-size: 0.8rem;")
                  )
                )
              ),
              sliderInput("font_size", "Base Font Size (px):", min = 8, max = 24, value = 12, step = 1),
              tags$details(
                style = "margin-bottom: 15px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 8px 10px;",
                tags$summary(
                  style = "font-weight: 600; cursor: pointer; color: #1e293b;",
                  "Custom Font Sizes (by Part)"
                ),
                tags$div(
                  style = "margin-top: 10px;",
                  numericInput("x_title_font_size", "X-Axis Title (px):", value = NA, min = 6, max = 32, step = 1),
                  numericInput("y_title_font_size", "Y-Axis Title (px):", value = NA, min = 6, max = 32, step = 1),
                  numericInput("x_tick_font_size", "X-Axis Ticks (px):", value = NA, min = 6, max = 24, step = 1),
                  numericInput("y_tick_font_size", "Y-Axis Ticks (px):", value = NA, min = 6, max = 24, step = 1),
                  numericInput("legend_title_font_size", "Legend Title (px):", value = NA, min = 6, max = 32, step = 1),
                  numericInput("legend_text_font_size", "Legend Labels (px):", value = NA, min = 6, max = 24, step = 1),
                  numericInput("tooltip_font_size", "Hover Tooltip (px):", value = NA, min = 6, max = 24, step = 1),
                  numericInput("annotation_font_size", "Annotation Balloon (px):", value = NA, min = 6, max = 28, step = 1),
                  numericInput("threshold_text_font_size", "Threshold Box (px):", value = NA, min = 6, max = 24, step = 1)
                )
              ),
              hr(),
              h5(class = "fw-bold mb-2", icon("download"), " Download Chart"),
              actionButton("btn_download_png", "Download PNG", icon = icon("image"), class = "btn-primary", style = "width: 100%; margin-bottom: 8px;"),
              actionButton("btn_download_svg", "Download SVG", icon = icon("file-code"), class = "btn-outline-secondary", style = "width: 100%; margin-bottom: 8px;"),
              downloadButton("download_html", "Download HTML Widget", class = "btn-outline-primary", style = "width: 100%; margin-bottom: 8px;"),
              downloadButton("download_abacost_csv_sb", "Download CSV", class = "btn-secondary", style = "width: 100%;")
            ),
            div(
              class = "abacost-curve-plot",
              style = "align-self: stretch; height: 100%; padding: 10px;",
              abacostCurveOutput("curve", height = "560px")
            )
          )
        )
      )
    )
  )
)


npv_info_ui <- if (exists("economic_info_ui")) {
  economic_info_ui
} else {
  card(
    card_header(
      span(style = "font-weight: 600; margin-left: 6px;", "About Net Present Value (NPV) & Economic Trade-Off")
    ),
    card_body(
      p("Net Present Value (NPV) measures the profitability of each land use system over time, expressed in ", tags$strong("$/ha"), ". It discounts future streams of revenues and costs back to present-day value."),
      tags$ul(
        tags$li(tags$strong("Opportunity Cost:"), " When a high-carbon land cover (e.g., natural forest) is converted to an agricultural or commercial system, the trade-off is between the carbon emitted and the economic gain (NPV difference)."),
        tags$li(tags$strong("Abatement Cost ($/tCO2e):"), " Calculated as the difference in NPV divided by the difference in time-averaged carbon stocks between land use systems."),
        tags$li(tags$strong("Abatement Cost Curve:"), " Ranks land use transitions from lowest to highest cost per unit of emission reduction, helping identify cost-effective emissions reduction interventions.")
      ),
      hr(),
      div(
        class = "alert alert-info py-2 px-3 mb-0",
        icon("lightbulb"),
        " Edit the values directly in the spreadsheet table on the left. Values update reactively."
      )
    )
  )
}

#### UI MAIN PAGE ####
ui <- page_navbar(
    id = "main_page",
    theme = bs_theme(
      version = 5,
      primary = "#540b0e",
      secondary = "#BE191E",
      success = "#66827a",
      info = "#540b0e",
      warning = "#e09f3e",
      danger = "#BE191E",
      bg = "#fbfcfd",
      fg = "#1e293b",
      font_scale = 0.9
    ),
    navbar_options = navbar_options(bg = "#540b0e", underline = FALSE),
    header =
      tags$head(
        tags$link(rel = "shortcut icon", href = "favicon.ico"),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css"),
        tags$style(
          HTML(
            "
            /* =================================================================
               REDD Abacus 2 — Design System & Theme Styles
               Color Palette:
                 #540b0e (Mahogany)    - Primary brand, active tabs, card navbars
                 #BE191E (Carmine Red) - Secondary actions, hover accents, alerts
                 #335c67 (Deep Teal)   - Baseline data series, analytic curves
                 #66827a (Sage Teal)   - Controls, panels, secondary surfaces
                 #99a88c (Olive Sage)  - Grouping surfaces, period badges
                 #fff3b0 (Pale Cream)  - Subtle highlights, icon contrast
                 #f0c977 (Amber Sand)  - Land cover cards, warm notices
                 #e09f3e (Ochre Gold)  - Warnings, scenario curves & param icons
                 #bf6535 (Terracotta)  - Zonation cards, earth accents
               ================================================================= */

            :root {
              --aba-primary: #540b0e;
              --aba-primary-hover: #3d070a;
              --aba-secondary: #BE191E;
              --aba-secondary-hover: #9e1418;
              --aba-teal-dark: #335c67;
              --aba-teal-hover: #26464e;
              --aba-sage: #66827a;
              --aba-sage-hover: #556e67;
              --aba-olive: #99a88c;
              --aba-cream: #fff3b0;
              --aba-amber: #f0c977;
              --aba-ochre: #e09f3e;
              --aba-terracotta: #bf6535;
              --aba-crimson: #BE191E;
              --aba-crimson-hover: #9e1418;
              --aba-mahogany: #540b0e;

              --aba-surface: #ffffff;
              --aba-surface-subtle: #f8fafc;
              --aba-surface-muted: #f1f5f9;
              --aba-border: #e2e8f0;
              --aba-border-dark: #cbd5e1;
              --aba-text-main: #1e293b;
              --aba-text-muted: #64748b;
              --aba-radius: 8px;
              --aba-radius-sm: 6px;
              --aba-shadow-sm: 0 1px 3px rgba(0, 0, 0, 0.05), 0 1px 2px rgba(0, 0, 0, 0.03);
              --aba-shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.07), 0 2px 4px -2px rgba(0, 0, 0, 0.05);
            }

            /* ── Global Typography & Foundations ── */
            body, button, input, select, textarea {
              font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
            }
            body {
              color: var(--aba-text-main);
              background-color: #fbfcfd;
              -webkit-font-smoothing: antialiased;
            }

            /* ── Consistent Card Styling ── */
            .card {
              border: 1px solid var(--aba-border);
              border-radius: var(--aba-radius);
              background-color: var(--aba-surface);
              box-shadow: var(--aba-shadow-sm);
              transition: border-color 0.15s ease, box-shadow 0.15s ease;
            }
            .card:hover {
              border-color: var(--aba-border-dark);
            }
            .card-header {
              padding: 9px 14px !important;
              font-weight: 600;
              font-size: 0.88rem;
              letter-spacing: 0.01em;
              border-top-left-radius: calc(var(--aba-radius) - 1px) !important;
              border-top-right-radius: calc(var(--aba-radius) - 1px) !important;
            }

            /* ── Header Themes ── */
            .bg_theme {
              background-color: var(--aba-surface-subtle) !important;
              color: var(--aba-text-main) !important;
              border-bottom: 1px solid var(--aba-border) !important;
            }
            .bg_theme i, .bg_theme .fa, .bg_theme .fa-solid {
              color: var(--aba-primary);
            }

            .bg_lc_theme {
              background-color: var(--aba-amber) !important;
              color: #2c1a02 !important;
              border-bottom: 1px solid #dfb562 !important;
              font-weight: 600;
            }
            .bg_lc_theme i, .bg_lc_theme .fa, .bg_lc_theme .fa-solid {
              color: var(--aba-mahogany);
            }

            .bg_zone_theme {
              background-color: var(--aba-terracotta) !important;
              color: #ffffff !important;
              border-bottom: 1px solid #a8562a !important;
              font-weight: 600;
            }
            .bg_zone_theme i, .bg_zone_theme .fa, .bg_zone_theme .fa-solid {
              color: var(--aba-cream);
            }

            .bg_theme2 {
              background-color: var(--aba-surface-subtle) !important;
              border: 1px solid var(--aba-border) !important;
              border-radius: var(--aba-radius-sm);
            }
            .bg_warning {
              background-color: #fef3c7 !important;
              border: 1px solid #fde68a !important;
              color: #92400e !important;
            }

            /* ── Sub-navigation: Underline Tabs ── */
            .nav-underline {
              border-bottom: 1px solid var(--aba-border) !important;
              gap: 4px;
              margin-bottom: 14px;
              padding-left: 2px;
            }
            .nav-underline .nav-link {
              color: var(--aba-text-muted) !important;
              font-weight: 500;
              font-size: 0.88rem;
              padding: 8px 16px !important;
              border-bottom: 2.5px solid transparent !important;
              border-radius: 0;
              transition: color 0.15s ease, border-color 0.15s ease;
            }
            .nav-underline .nav-link:hover {
              color: var(--aba-secondary) !important;
              border-bottom-color: rgba(190, 25, 30, 0.4) !important;
            }
            .nav-underline .nav-link.active {
              color: var(--aba-primary) !important;
              font-weight: 600;
              border-bottom-color: var(--aba-primary) !important;
              background: transparent !important;
            }

            /* ── Card Navset Tabs (e.g. Scenario Editor) ── */
            .card-header.bslib-navs-card-title {
              background-color: var(--aba-primary) !important;
              border-bottom: none !important;
              padding: 4px 10px 0 !important;
            }
            .card-header.bslib-navs-card-title .nav-link {
              color: rgba(255, 255, 255, 0.85) !important;
              font-weight: 500;
              font-size: 0.86rem;
              border: none !important;
              border-radius: 6px 6px 0 0 !important;
              padding: 7px 14px !important;
              transition: all 0.15s ease;
            }
            .card-header.bslib-navs-card-title .nav-link:hover {
              color: #ffffff !important;
              background-color: rgba(255, 255, 255, 0.15) !important;
            }
            .card-header.bslib-navs-card-title .nav-link.active {
              background-color: #ffffff !important;
              color: var(--aba-primary) !important;
              font-weight: 600;
            }

            /* ── Control Banners & Period Bars ── */
            .bg_period {
              background-color: var(--aba-sage);
              color: #ffffff;
              border: 1px solid #526f68;
              border-radius: var(--aba-radius);
              padding: 10px 16px;
              box-shadow: var(--aba-shadow-sm);
            }
            .bg_period .matrix-header-label {
              color: #ffffff !important;
              font-weight: 600;
              font-size: 0.88rem;
              margin: 0 8px 0 0;
              white-space: nowrap;
            }
            .bg_period .matrix-header-label i {
              color: var(--aba-cream);
            }
            .matrix-header-label {
              color: var(--aba-mahogany);
              font-weight: 600;
              margin: 0 6px 0 10px;
              white-space: nowrap;
            }
            .matrix-header-controls .form-group { margin-bottom: 0; }
            .matrix-header-controls .selectize-control { margin-bottom: 0; }
            .matrix-header-controls .selectize-control.multi .selectize-input {
              min-height: 28px; height: auto; padding: 2px 6px;
              max-height: 76px; overflow-y: auto;
            }
            .matrix-header-controls .selectize-control.multi .selectize-input > div {
              padding: 0 5px; margin: 0 3px 1px 0; font-size: 0.86em;
            }
            .matrix-header-controls .selectize-control.multi .selectize-input.has-items {
              display: flex; flex-wrap: wrap; align-items: center;
            }
            .matrix-header-controls .selectize-control.multi .selectize-input > input {
              flex: 1 1 40px; min-width: 40px;
            }
            .bg_period .selectize-input {
              background-color: #ffffff !important;
              color: var(--aba-text-main) !important;
              border: 1px solid var(--aba-primary) !important;
              border-radius: var(--aba-radius-sm);
              box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
            }
            .bg_period .selectize-input > input {
              color: var(--aba-text-main) !important;
              background-color: transparent !important;
            }
            .bg_period .selectize-input > input::placeholder {
              color: var(--aba-text-muted) !important;
              opacity: 0.75;
            }
            .bg_period .selectize-dropdown {
              border: 1px solid var(--aba-primary);
              border-radius: var(--aba-radius-sm);
              box-shadow: var(--aba-shadow-md);
              color: var(--aba-text-main) !important;
            }
            .bg_period .selectize-dropdown .option.active,
            .bg_period .selectize-dropdown .option:hover {
              background-color: var(--aba-primary) !important;
              color: #ffffff !important;
            }
            .bg_period .selectize-dropdown .option.selected {
              background-color: rgba(84, 11, 14, 0.12) !important;
              color: var(--aba-primary) !important;
            }
            .bg_period .selectize-control.single .selectize-input::after {
              border-top-color: var(--aba-primary) !important;
            }
            .bg_period .form-label {
              background-color: #ffffff !important;
              border: 1px solid var(--aba-primary) !important;
              color: var(--aba-primary) !important;
              padding: 1px 6px;
              border-radius: 4px;
            }
            .bg_period .form-select {
              background-color: #ffffff !important;
              color: var(--aba-text-main) !important;
              border: 1px solid var(--aba-primary) !important;
              border-radius: var(--aba-radius-sm);
            }

            /* ── Buttons, Controls & Toolbars ── */
            .btn {
              border-radius: var(--aba-radius-sm);
              font-weight: 500;
              font-size: 0.86rem;
              transition: all 0.15s ease;
            }
            .btn-primary {
              background-color: var(--aba-primary) !important;
              border-color: var(--aba-primary) !important;
              color: #ffffff !important;
            }
            .btn-primary:hover, .btn-primary:focus {
              background-color: var(--aba-primary-hover) !important;
              border-color: var(--aba-primary-hover) !important;
              color: #ffffff !important;
              box-shadow: 0 2px 4px rgba(84, 11, 14, 0.28);
            }
            .btn-secondary {
              background-color: var(--aba-secondary) !important;
              border-color: var(--aba-secondary) !important;
              color: #ffffff !important;
            }
            .btn-secondary:hover, .btn-secondary:focus {
              background-color: var(--aba-secondary-hover) !important;
              border-color: var(--aba-secondary-hover) !important;
              color: #ffffff !important;
              box-shadow: 0 2px 4px rgba(190, 25, 30, 0.28);
            }
            .btn-outline-primary {
              border-color: var(--aba-primary) !important;
              color: var(--aba-primary) !important;
            }
            .btn-outline-primary:hover,
            .btn-outline-primary:focus,
            .btn-outline-primary:active,
            .btn-outline-primary.active,
            .btn-outline-primary.active:hover,
            .btn-outline-primary.active:focus,
            .show > .btn-outline-primary.dropdown-toggle,
            .btn-check:checked + .btn-outline-primary,
            .btn-check:checked + label.btn-outline-primary,
            .btn-check:active + .btn-outline-primary,
            .btn-check:active + label.btn-outline-primary,
            .btn-group > .btn-outline-primary.active,
            .btn-group-toggle > .btn-outline-primary.active,
            .btn-group-toggle > label.btn-outline-primary.active,
            .btn-group-container-sw .btn-outline-primary.active,
            #projection_summary_emission_type .btn.active,
            #projection_summary_emission_type .btn-check:checked + .btn,
            #projection_summary_emission_type label.btn.active {
              background-color: var(--aba-primary) !important;
              border-color: var(--aba-primary) !important;
              color: #ffffff !important;
            }
            .btn-outline-secondary {
              border-color: var(--aba-secondary) !important;
              color: var(--aba-secondary) !important;
            }
            .btn-outline-secondary:hover,
            .btn-outline-secondary:focus,
            .btn-outline-secondary:active,
            .btn-outline-secondary.active,
            .btn-outline-secondary.active:hover,
            .btn-outline-secondary.active:focus,
            .show > .btn-outline-secondary.dropdown-toggle,
            .btn-check:checked + .btn-outline-secondary,
            .btn-check:checked + label.btn-outline-secondary,
            .btn-check:active + .btn-outline-secondary,
            .btn-check:active + label.btn-outline-secondary,
            .btn-group > .btn-outline-secondary.active,
            .btn-group-toggle > .btn-outline-secondary.active,
            .btn-group-toggle > label.btn-outline-secondary.active,
            .btn-group-container-sw .btn-outline-secondary.active {
              background-color: var(--aba-secondary) !important;
              border-color: var(--aba-secondary) !important;
              color: #ffffff !important;
            }
            .btn-danger {
              background-color: var(--aba-crimson) !important;
              border-color: var(--aba-crimson) !important;
              color: #ffffff !important;
            }
            .btn-danger:hover, .btn-danger:focus {
              background-color: var(--aba-crimson-hover) !important;
              border-color: var(--aba-crimson-hover) !important;
              color: #ffffff !important;
            }
            .btn-action {
              height: 36px;
              display: inline-flex;
              align-items: center;
              justify-content: center;
            }
            .card-toolbar-btn {
              background: transparent !important;
              border: none !important;
              color: inherit !important;
              opacity: 0.82;
              padding: 3px 6px !important;
              border-radius: var(--aba-radius-sm);
              line-height: 1;
              cursor: pointer;
              display: inline-flex;
              align-items: center;
              gap: 4px;
              text-decoration: none !important;
              transition: all 0.15s ease-in-out;
            }
            .card-toolbar-btn:hover, .card-toolbar-btn:focus {
              opacity: 1 !important;
              background-color: rgba(0, 0, 0, 0.08) !important;
              transform: scale(1.08);
              color: inherit !important;
            }
            .bg_theme .card-toolbar-btn { color: var(--aba-text-main) !important; }
            .bg_theme .card-toolbar-btn:hover { background-color: rgba(84, 11, 14, 0.08) !important; color: var(--aba-primary) !important; }
            .bg_zone_theme .card-toolbar-btn { color: rgba(255, 255, 255, 0.9) !important; }
            .bg_zone_theme .card-toolbar-btn:hover { color: #ffffff !important; background-color: rgba(255, 255, 255, 0.22) !important; }
            .btn-map-edit, .btn-map-remove {
              opacity: 0.75;
              transition: all 0.15s ease-in-out;
              cursor: pointer;
              border: none !important;
              background: transparent !important;
              padding: 3px 6px !important;
              border-radius: 4px;
              line-height: 1;
            }
            .btn-map-edit:hover, .btn-map-edit:focus, .btn-map-remove:hover, .btn-map-remove:focus {
              opacity: 1 !important;
              transform: scale(1.12);
            }
            .bg_zone_theme .btn-map-edit { color: rgba(255, 255, 255, 0.85) !important; }
            .bg_zone_theme .btn-map-edit:hover { color: #ffffff !important; background-color: rgba(255, 255, 255, 0.2) !important; }
            .bg_lc_theme .btn-map-edit { color: rgba(44, 26, 2, 0.8) !important; }
            .bg_lc_theme .btn-map-edit:hover { color: #000000 !important; background-color: rgba(0, 0, 0, 0.08) !important; }
            .bg_zone_theme .btn-map-remove { color: rgba(255, 255, 255, 0.85) !important; }
            .bg_zone_theme .btn-map-remove:hover { color: #ffffff !important; background-color: rgba(255, 255, 255, 0.2) !important; }
            .bg_theme .btn-map-remove { color: var(--aba-crimson) !important; }
            .bg_theme .btn-map-remove:hover { color: var(--aba-crimson-hover) !important; background-color: rgba(190, 25, 30, 0.12) !important; }

            /* ── Badges ── */
            .badge {
              border-radius: 4px;
              font-weight: 500;
              font-size: 0.72rem;
              padding: 3px 7px;
            }

            /* ── Form Inputs & Selectize ── */
            .selectize-input {
              border: 1px solid var(--aba-border-dark);
              border-radius: var(--aba-radius-sm);
              font-size: 0.88rem;
              transition: border-color 0.15s ease, box-shadow 0.15s ease;
            }
            .selectize-input.focus {
              border-color: var(--aba-primary) !important;
              box-shadow: 0 0 0 2px rgba(84, 11, 14, 0.18) !important;
            }
            .form-control:focus, .form-select:focus {
              border-color: var(--aba-primary) !important;
              box-shadow: 0 0 0 2px rgba(84, 11, 14, 0.18) !important;
            }

            /* ── Card Heights & Layout Constrains ── */
            #cstock_list { height: auto !important; }
            #cstock_list .jexcel_content { max-height: calc(100vh - 240px) !important; overflow-y: auto !important; }
            #cstock_card { max-height: calc(100vh - 170px) !important; }
            #npv_list { height: auto !important; }
            #npv_list .jexcel_content { max-height: calc(100vh - 240px) !important; overflow-y: auto !important; }
            #npv_card { max-height: calc(100vh - 170px) !important; }
            #npv_card:has(+ .abacost-right-col) { max-height: none !important; }
            #npv_card:has(+ .abacost-right-col) #npv_list .jexcel_content { max-height: none !important; }
            #cstock_card, #mapc_card, #c_emission_summary_card { margin-bottom: 0 !important; }
            #other_emission_card { max-height: calc(100vh - 170px) !important; }
            #other_emission_card, #map_ghg_card, #ghg_emission_summary_card { margin-bottom: 0 !important; }
            #map_all_card, #combined_emission_summary_card { margin-bottom: 0 !important; }
            #other_emission .jexcel_content { max-height: calc(100vh - 240px) !important; overflow-y: auto !important; }
            .tab-pane[data-value='Carbon emission'],
            .tab-pane[data-value='GHG emission'] { overflow: hidden !important; }

            /* ── Uploaded Map Cards Grid ── */
            .uploaded-map-cards-grid {
              display: grid;
              grid-template-columns: repeat(3, minmax(0, 1fr));
              gap: 14px;
              align-items: start;
              width: 100%;
            }
            .uploaded-cards-separator {
              border-top: 2px solid var(--aba-border);
              opacity: 1;
              margin: 14px 0 16px 0;
            }
            @media (max-width: 900px) { .uploaded-map-cards-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
            @media (max-width: 600px) { .uploaded-map-cards-grid { grid-template-columns: minmax(0, 1fr); } }

            .map-card-wrap { display: flex; }
            .map-card { flex: 1 1 auto; min-width: 0; }
            .map-card .map-card-body { min-height: 0; }
            .map-zone-map-plot { min-width: 0; }
            .map-zone-map-plot .shiny-plot-output { width: 100%; height: 100%; }

            /* ── Zonation Legend Sidebar ── */
            .map-zone-sidebar {
              background-color: #fdfaf7;
              border-right: 1px solid #ebd9c8;
              overflow-x: auto;
              overflow-y: hidden;
            }
            .map-zone-sidebar > .sidebar-content {
              padding: 4px;
              display: flex;
              flex-direction: column;
              min-height: 0;
              overflow: hidden;
            }
            .map-zone-sidebar .sidebar-header { min-height: 0; padding: 2px 4px 0; }
            .map-zone-sidebar .collapse-toggle { width: 22px; min-height: 22px; flex: 0 0 auto; }
            .map-zone-sidebar .collapse-toggle .collapse-icon { font-size: 0.7rem; padding: 0; }
            .map-zone-sidebar-table { flex: 1 1 auto; min-height: 0; overflow: auto; }
            .map-zone-sidebar-table > * { max-width: 100%; }
            .map-zone-sidebar-table .jexcel_container { width: 100% !important; }
            .map-zone-sidebar .jexcel > tbody > tr > td,
            .map-zone-sidebar .jexcel > thead > tr > td { font-size: 0.70rem !important; line-height: 1.25 !important; }
            .map-zone-sidebar-table .jexcel > tbody > tr > td { padding-top: 2px !important; padding-bottom: 2px !important; }
            .map-zone-sidebar-table .handsontable,
            .map-zone-sidebar-table table { margin-bottom: 0 !important; }
            .bslib-sidebar-layout.sidebar-collapsed { position: relative; grid-template-columns: 0 1fr; }
            .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle {
              position: absolute; top: 4px; left: 4px; z-index: 5;
              align-self: auto; width: 26px; min-height: 0; height: 22px;
              display: inline-flex; align-items: center; justify-content: center;
              padding: 0; border-radius: 4px; opacity: 1;
              background: #fdf6f0; border: 1px solid #f0dcc9; color: #8a4b22;
              box-shadow: 0 1px 2px rgba(0, 0, 0, 0.07);
            }
            .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle:hover,
            .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle:focus {
              background: #f7e8d8; color: #6d3a17;
            }
            .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle .collapse-icon { font-size: 0.7rem; padding: 0; }
            .bslib-sidebar-layout.sidebar-collapsed > .main { min-width: 0; }
            .map-zone-sidebar > .sidebar-content { overflow-y: auto; }

            /* ── Abatement Cost Curve Sidebar & Layout ── */
            .abacost-config-sidebar {
              background-color: var(--aba-surface-subtle);
              border-right: 1px solid var(--aba-border);
              min-height: 0;
              overflow: hidden;
            }
            .bslib-sidebar-layout:has(> .abacost-config-sidebar) { min-height: 0; }
            .card-body:has(> .bslib-sidebar-layout > .abacost-config-sidebar) { min-height: 0; }
            .card:has(> .card-body > .bslib-sidebar-layout > .abacost-config-sidebar) { min-height: 0; }
            .tab-pane:has(> .card > .card-body > .bslib-sidebar-layout > .abacost-config-sidebar) { min-height: 0; }
            .abacost-config-sidebar > .sidebar-content { padding: 10px; overflow-y: auto; min-height: 0; }
            .abacost-config-sidebar .form-group,
            .abacost-config-sidebar .shiny-input-container { margin-bottom: 10px; }
            .abacost-config-sidebar .collapse-toggle { width: 22px; min-height: 22px; flex: 0 0 auto; }
            .abacost-config-sidebar .collapse-toggle .collapse-icon { font-size: 0.7rem; padding: 0; }
            .bslib-sidebar-layout.sidebar-collapsed:has(> .abacost-config-sidebar) {
              position: relative; grid-template-columns: 0 1fr;
            }
            .bslib-sidebar-layout.sidebar-collapsed:has(> .abacost-config-sidebar) > .collapse-toggle {
              position: absolute; top: 4px; left: 4px; z-index: 5;
              align-self: auto; width: 26px; min-height: 0; height: 22px;
              display: inline-flex; align-items: center; justify-content: center;
              padding: 0; border-radius: 4px; opacity: 1;
              background: var(--aba-surface-subtle); border: 1px solid var(--aba-border); color: var(--aba-primary);
              box-shadow: 0 1px 2px rgba(0, 0, 0, 0.07);
            }
            .bslib-sidebar-layout.sidebar-collapsed:has(> .abacost-config-sidebar) > .collapse-toggle:hover,
            .bslib-sidebar-layout.sidebar-collapsed:has(> .abacost-config-sidebar) > .collapse-toggle:focus {
              background: var(--aba-surface-muted); color: var(--aba-primary-hover);
            }
            .bslib-sidebar-layout.sidebar-collapsed:has(> .abacost-config-sidebar) > .main { min-width: 0; }
            .abacost-curve-plot { min-width: 0; }
            .abacost-curve-plot .html-widget,
            .abacost-curve-plot .abacost-curve-widget { width: 100%; height: 100%; }
            .abacost-right-col { min-width: 0; }
            .abacost-right-col > .card { margin-bottom: 0; }
            #npv_card:has(+ .abacost-right-col) { height: 100%; min-height: 620px; }
            #npv_card:has(+ .abacost-right-col) > .card-body { min-height: 0; overflow: auto; }

            /* ── Scenario Projection & Parameter Card ── */
            #box_emission {
              background-color: transparent; border: none;
              border-radius: 0; box-shadow: none; overflow: visible;
            }
            #scenario_card, #scenario_card .card-header, #scenario_card .card-body {
              background-color: var(--aba-olive) !important;
            }
            #scenario_card .card-header, #scenario_card .card-header * {
              color: #000000 !important;
            }
            #scenario_card .scenario-param-icon { color: var(--aba-ochre) !important; }
            #box_emission .info_icon_red { color: #e5484d !important; }

            /* ── Scenario Editor Toolbar & Sizing ── */
            .selector-toolbar {
              display: flex; flex: 0 0 auto; flex-wrap: wrap; align-items: flex-end; gap: 0.5rem;
              padding: 0.5rem 0.75rem 0.25rem;
            }
            .selector-toolbar .shiny-input-container { margin-bottom: 0; }
            .selector-group { min-width: 0; }
            .selector-zone { flex: 1 1 300px; max-width: 400px; }
            .selector-period { flex: 0 1 180px; min-width: 150px; }
            @media (max-width: 575.98px) {
              .selector-zone { flex: 1 1 100%; max-width: none; }
              .selector-period { flex: 1 1 140px; min-width: 140px; }
            }
            .area-plot-heading { padding: 0.3rem 0.5rem 0; margin-bottom: 0.5rem; line-height: 1.1; }
            .area-plot-title { font-size: 0.9rem; line-height: 1.15; font-weight: 600; }
            .area-plot-subtitle { margin-top: 1px; font-size: 0.75rem; line-height: 1.1; color: var(--aba-text-muted); }
            .scenario-editor-wrapper { height: 100%; min-height: 580px; display: flex; flex-direction: column; }
            .selector-toolbar > .shiny-conditional-panel[style*='display: block'],
            .selector-toolbar > .shiny-conditional-panel:not([style*='display: none']) { display: contents !important; }
            #widget_out { min-height: 520px; width: 100%; overflow: visible; }
            #area_plot { min-height: 180px; width: 100%; }
            .selectize-dropdown-content .option:not(.selected)[data-value]:before { content: ''; }
            .selectize-dropdown-content .option.has-edits,
            .selectize-control .item.has-edits { color: var(--aba-crimson); font-weight: 600; }

            /* ── Modals & Popovers ── */
            .modal-content {
              border-radius: 12px;
              border: 1px solid var(--aba-border);
              box-shadow: 0 12px 30px rgba(0, 0, 0, 0.15);
              overflow: hidden;
            }
            .modal-header {
              background-color: var(--aba-surface-subtle);
              border-bottom: 1px solid var(--aba-border);
              padding: 12px 20px;
              font-weight: 600;
              color: var(--aba-text-main);
            }
            .modal-title { font-weight: 600; font-size: 1.05rem; }
            .modal-footer {
              background-color: var(--aba-surface-subtle);
              border-top: 1px solid var(--aba-border);
              padding: 12px 20px;
            }
            .popover {
              border: 1px solid var(--aba-border);
              border-radius: var(--aba-radius);
              box-shadow: var(--aba-shadow-md);
            }
            .popover-header {
              background-color: var(--aba-surface-subtle);
              border-bottom: 1px solid var(--aba-border);
              font-weight: 600;
              font-size: 0.88rem;
              color: var(--aba-text-main);
            }

            /* ── Reactable ── */
            .Reactable .rt-no-data {
              position: absolute !important; top: 50% !important; left: 50% !important;
              transform: translate(-50%, -50%) !important; line-height: 1.5 !important;
              width: 90% !important; max-width: 320px !important; text-align: center !important;
              color: var(--aba-text-muted) !important; font-size: 0.875rem !important; white-space: normal !important;
            }
            .Reactable .rt-tbody-no-data { min-height: 120px !important; position: relative !important; }
            .Reactable .rt-thead .rt-th {
              background-color: var(--aba-surface-subtle) !important;
              color: #334155 !important;
              font-weight: 600 !important;
              border-bottom: 1px solid var(--aba-border) !important;
              font-size: 0.84rem;
            }
            .Reactable .rt-tr:hover { background-color: var(--aba-surface-subtle) !important; }

            /* ── Navbar Layout & Options / About Actions ── */
            @media (min-width: 992px) {
              nav.navbar > .container-fluid {
                display: flex !important; align-items: center !important;
                justify-content: space-between !important; width: 100% !important;
                padding-right: 12px !important;
              }
              nav.navbar .navbar-collapse { display: flex !important; flex: 1 1 auto !important; width: 100% !important; }
              nav.navbar .navbar-nav {
                display: flex !important; flex-direction: row !important;
                align-items: center !important; flex: 1 1 auto !important; width: 100% !important;
              }
              nav.navbar .navbar-nav > .ms-auto,
              nav.navbar .navbar-nav > .bslib-nav-spacer { margin-left: auto !important; }
              .navbar .nav-link[data-value='About'] {
                border-radius: 50% !important; width: 2rem !important; height: 2rem !important;
                padding: 0 !important; margin: 0 2px !important; justify-content: center !important;
              }
              .navbar .nav-link[data-value='About']::after { content: none !important; }
            }

            @media (max-width: 991.98px) {
              nav.navbar .navbar-collapse.show { padding: 10px 0 !important; }
              nav.navbar .navbar-collapse.show .navbar-nav {
                display: flex !important; flex-direction: column !important; align-items: stretch !important; gap: 4px !important;
              }
              nav.navbar .navbar-nav > .bslib-nav-spacer { display: none !important; }
              nav.navbar .navbar-nav > .ms-auto { margin-left: 0 !important; margin-top: 8px !important; margin-bottom: 4px !important; }
              .btn-options-navbar { width: 100% !important; justify-content: flex-start !important; padding: 8px 14px !important; }
              .navbar .nav-link[data-value='About'] {
                width: 100% !important; height: auto !important; border-radius: 6px !important;
                padding: 8px 14px !important; margin: 2px 0 !important; justify-content: flex-start !important; gap: 8px !important;
              }
              .navbar .nav-link[data-value='About']::after {
                content: 'About REDD Abacus'; font-size: 0.9rem; font-weight: 500;
              }
            }

            .btn-options-navbar {
              color: #fff !important;
              background-color: rgba(255, 255, 255, 0.15) !important;
              border: 1px solid rgba(255, 255, 255, 0.35) !important;
              border-radius: 6px !important;
              padding: 4px 12px;
              font-size: 0.88rem !important;
              font-weight: 500 !important;
              display: inline-flex !important;
              align-items: center !important;
              gap: 6px !important;
              line-height: 1.4 !important;
              transition: all 0.2s ease !important;
              cursor: pointer !important;
            }
            .btn-options-navbar:hover, .btn-options-navbar:focus {
              background-color: rgba(255, 255, 255, 0.3) !important;
              border-color: rgba(255, 255, 255, 0.6) !important;
              color: #fff !important;
              box-shadow: 0 2px 6px rgba(0, 0, 0, 0.2) !important;
            }

            .navbar .nav-link[data-value='About'] {
              color: rgba(255, 255, 255, 0.75) !important;
              background-color: transparent !important;
              display: flex !important;
              align-items: center !important;
              font-size: 1.15rem !important;
              transition: all 0.2s ease !important;
            }
            .navbar .nav-link[data-value='About']:hover, .navbar .nav-link[data-value='About']:focus {
              color: #ffffff !important;
              background-color: rgba(255, 255, 255, 0.15) !important;
              outline: none !important;
            }
            .navbar .nav-link[data-value='About'].active {
              color: #ffffff !important;
              background-color: rgba(255, 255, 255, 0.25) !important;
              box-shadow: none !important;
            }
            "
          )
        ),
        tags$script(
          HTML(
            "
          $(document).on('shiny:value', function(event, binding) {
            if (event.binding) return;
            event.binding = binding || {
              el: event.target,
              onResize: function() {}
            };
          });
        "
          )
        ),
        ## Zonation cards start with their legend sidebar CLOSED, so the only
        ## affordance is a small chevron. bslib labels it with a generic
        ## "Toggle sidebar"; replace that with a tooltip saying WHERE the zone
        ## legend is, on both the closed control and the sidebar's own chevron.
        ## Cards are inserted dynamically, so re-apply on every insertion.
        tags$script(
          HTML(
            "
          (function() {
            var CLOSED_TIP = 'Zone legend is hidden: click to show the zone legend';
            var OPEN_TIP   = 'Hide the zone legend';
            function labelToggles(root, collapsed) {
              (root || document).querySelectorAll('.map-zone-sidebar .collapse-toggle, .bslib-sidebar-layout > .collapse-toggle').forEach(function(btn) {
                var inZone = btn.closest('.bslib-sidebar-layout') &&
                             btn.closest('.bslib-sidebar-layout').querySelector('.map-zone-sidebar');
                if (!inZone) return;
                var isCollapsed = collapsed !== undefined ? collapsed
                  : (btn.closest('.bslib-sidebar-layout') || {classList: {contains: function(){ return false; }}})
                      .classList.contains('sidebar-collapsed');
                btn.setAttribute('title', isCollapsed ? CLOSED_TIP : OPEN_TIP);
                btn.setAttribute('aria-label', isCollapsed ? CLOSED_TIP : OPEN_TIP);
                btn.setAttribute('data-bs-toggle', 'tooltip');
                if (window.bootstrap && window.bootstrap.Tooltip) {
                  var inst = window.bootstrap.Tooltip.getInstance(btn);
                  if (inst) inst.setContent({'.tooltip-inner': btn.getAttribute('title')});
                }
              });
            }
            function refresh() { labelToggles(document); }
            $(document).on('shiny:value', refresh);
            document.addEventListener('click', function(e) {
              if (e.target.closest && e.target.closest('.collapse-toggle')) {
                setTimeout(refresh, 30);
              }
            }, true);
            var mo = new MutationObserver(function() { refresh(); });
            document.addEventListener('DOMContentLoaded', function() {
              mo.observe(document.body, { childList: true, subtree: true });
              refresh();
            });
            if (document.readyState !== 'loading') {
              mo.observe(document.body, { childList: true, subtree: true });
              refresh();
            }
          })();
        "
          )
        ),
        tags$script(
          HTML(
            "
          (function() {
            var DOT = '\\u25cf';
            function colorDots(root) {
              var els = (root || document).querySelectorAll(
                '.selectize-input .item, .selectize-dropdown-content .option'
              );
              for (var i = 0; i < els.length; i++) {
                var isEdited = (els[i].textContent || '').trimStart().charAt(0) === DOT;
                els[i].classList.toggle('has-edits', isEdited);
              }
            }
            colorDots(document);
            new MutationObserver(function() {
              colorDots(document);
            }).observe(document.documentElement, { childList: true, subtree: true, characterData: true });
          })();

          /* Navbar right-edge fix: ensure Options and About stay grouped at the right edge on desktop */
          function alignNavbarRight() {
            var opt = document.querySelector('nav.navbar .navbar-nav > .ms-auto');
            if (opt) {
              if (window.innerWidth >= 992) {
                opt.style.setProperty('margin-left', 'auto', 'important');
              } else {
                opt.style.removeProperty('margin-left');
              }
            }
          }
          window.addEventListener('load', alignNavbarRight);
          document.addEventListener('DOMContentLoaded', alignNavbarRight);
          window.addEventListener('resize', alignNavbarRight);

          /* Navigate to Land Cover Maps upload sub-tab from empty state */
          function navigateToLandCoverUpload() {
            var mainTab = document.querySelector('#main_page a[data-value=\\'Land Cover\\']');
            if (mainTab) mainTab.click();
            setTimeout(function() {
              var subTab = document.querySelector('#input_subtabs a[data-value=\\'Land Cover Maps\\']');
              if (subTab) subTab.click();
              window.scrollTo({ top: 0, behavior: 'smooth' });
            }, 80);
          }
        "
          )
        ),
      ),
    window_title = "REDD Abacus 2",
    title =
      span(
        tags$img(
          height = 32,
          src = "images/abacus_logo.svg",
          style = "margin-right:5px;"
        ),
        "REDD",
        span(
          "Abacus",
          style = "color:white;",
          .noWS = c('before', "after")
        ),
        "2",
        style = "color:#ED2839; font-weight:bold"
      ),
    padding = 10,
    nav_panel(
      #### HEADER ####
      title = "Land Cover",
      icon = icon("earth-asia"),
      navset_underline(
        id = "input_subtabs",
        nav_panel(
          title = "Land Cover Maps",
          icon = icon("map"),
          tags$script(
            HTML(
              "Shiny.addCustomMessageHandler('resetFileInput', function(id) { var input = document.getElementById(id); if (input) input.value = ''; });"
            )
          ),
          tags$div(
            class = "uploaded-map-cards-grid",
            id = "uploaded_map_cards_grid",
            tags$div(id = "uploaded_cards_container", style = "display: contents;"),
            div(
              id = "map_upload_dropzone_card",
              class = "card-upload-dropzone-wrap map-upload-layout",
              drag_drop_file_input(
                input_id = "map_upload_file",
                accept = c("image/tiff", ".tif", ".tiff", ".zip", ".shp"),
                helper_text = "Drag and drop a map (.tif, .shp, or .zip shapefile) here"
              )
            )
          ),
          ## Relative location + General map info: visible for any uploaded map,
          ## land cover or zonation (see map_location_info_row).
          map_location_info_row,
          lc_map_table_input
        ),
        nav_panel(
          title = "Land Cover Changes",
          icon = icon("code-compare"),
          conditionalPanel(condition = "output.is_lc_trend", card(
            card_header(
              class = "bg_theme d-flex justify-content-between align-items-center",
              div(class = "d-flex align-items-center gap-2", icon("chart-line"), "Land cover area trend"),
              chart_download_toolbar(
                button_id                = "lc_trend_download_btn",
                chart_container_selector = "#lc_trend_chart",
                base_filename            = "landcover_area_trend",
                html_download_id         = "download_lc_trend_html",
                csv_download_id          = "download_lc_trend_csv",
                title                    = "Download Trend Chart"
              )
            ),
            full_screen = FALSE,
            card_body(padding = 10, lc_area_chart_output("lc_trend_chart", width = "100%", height = "250px"))
          )),
          ## Shown as soon as one land cover map exists: the card itself
          ## switches between the changes matrix and the single-map area chart.
          conditionalPanel(condition = "output.is_matrix_pair || output.is_matrix_single", lc_changes_matrix_card),
          conditionalPanel(condition = "!output.is_matrix_pair && !output.is_matrix_single", please_input)
        ),
        nav_panel(
          title = "",
          value = "landcover_info",
          icon = icon("circle-info"),
          landcover_info_ui
        )
      )
    ),
    
    ### EMISSION #############################
    nav_panel(
      title = "Emission",
      id = "panel_emission",
      icon = icon("industry"),
      
      navset_underline(
        id = "emission_subtabs",
        nav_panel(
          title = "Carbon stock change emissions",
          icon = icon("coins"),
          carbon_emission_first_row
        ),
        nav_panel(
          title = "GHG flux emissions",
          icon = icon("wind"),
          conditionalPanel(condition = "output.is_lc_list", other_emission_input),
          conditionalPanel(condition = "!output.is_lc_list", please_input)
        ),
        nav_panel(
          title = "Emissions summary",
          icon = icon("chart-simple"),
          combined_emission_summary_input
        ),
        nav_panel(
          title = "",
          value = "emission_info",
          icon = icon("circle-info"),
          emission_info_ui
        )
      )
    ),
    
    ### PROJECTION #############################
    nav_panel(
      title = "Projection",
      id = "panel_projection",
      icon = icon("arrow-trend-up"),
      navset_underline(
        nav_panel(
          title = "Intervention scenarios",
          icon = icon("arrows-split-up-and-left"),
          conditionalPanel(condition = "!output.is_matrix", please_input),
          conditionalPanel(condition = "output.is_matrix", projection_panel_ui)
        ),
        nav_panel(
          title = "Projection summary",
          icon = icon("chart-simple"),
          conditionalPanel(condition = "!output.is_matrix", please_input),
          conditionalPanel(condition = "output.is_matrix", projection_summary_ui)
        ),
        nav_panel(
          title = "",
          value = "projection_info",
          icon = icon("circle-info"),
          projection_info_ui
        )
      )
    ),
    
    nav_panel(
      title = "Economic Trade-Off",
      icon = icon("dollar-sign"),
      navset_underline(
        id = "economic_subtabs",
        nav_panel(
          title = "Abatement Cost Curve",
          icon = icon("chart-column"),
          conditionalPanel(condition = "output.is_lc_list", economic_tradeoff_ui),
          conditionalPanel(condition = "!output.is_lc_list", please_input)
        ),
        nav_panel(
          title = "",
          value = "economic_info",
          icon = icon("circle-info"),
          economic_info_ui
        )
      )
    ),

    
    ### RIGHT SIDE ITEMS #############################
    nav_spacer(),
    nav_item(
      actionButton(
        "btn_options",
        label = "Options",
        icon = icon("gear"),
        class = "btn-options-navbar"
      ),
      class = "ms-auto"
    ),
    nav_panel(
      title = "",
      value = "About",
      icon = icon("circle-question"),
      about_ui
    )
    
  )

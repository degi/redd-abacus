install_load <- function (package1, ...)  {

  # convert arguments to vector
  packages <- c(package1, ...)

  # start loop to determine if each package is installed
  for(package in packages){

    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))

    # if package is not installed locally, download, then load
    else {

      install.packages(package)
      do.call("library", list(package))
    }
  }
}

install_load("shiny", "devtools",
             "bslib", "bsicons", "htmltools", "reactable", "plotly", "leaflet",
             "stars", "mapview", "excelR", "RColorBrewer", "jsonlite", "reshape2",
             "leafem", "yaml")

if(!("abacuslib" %in% rownames(installed.packages()))) {
  install_github("degi/abacuslib")
  do.call("library", list("abacuslib"))
}

library(abacuslib)

table_file_df <- data.frame(
  var = c(
    "lc_list_df",
    "cstock_list",
    "zone_list_df",
    "other_emission_df",
    "scenario.tpm",
    "scenario.baseline_tpm",
    "scenario.baseline_area"
  ),
  file = c(
    "landcover.csv",
    "carbonstock.csv",
    "zonation.csv",
    "other_emission.csv",
    "scenario_activities.csv",
    "scenario_tpm.csv",
    "baseline_lc_area.csv"
  ),
  label = c(
    "Land cover legend",
    "Carbon stock data",
    "Zonation legend",
    "Other emission data",
    "Scenario activities",
    "Transition projection matrix of the scenario",
    "Baseline land cover area"
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
    "abacus_baseline.emission.lc_emission",
    "abacus_scenario.projection.lc_area",
    "abacus_scenario.emission.lc_emission"
  ),
  file = c(
    "baseline_projection.csv",
    "baseline_emission_projection.csv",
    "scenario_projection.csv",
    "scenario_emission_projection.csv"
  ),
  label = c(
    "Baseline land cover change projection",
    "Baseline emission projection",
    "Scenario land cover change projection",
    "Scenario emission projection"
  )
)

output_map_file_df <- data.frame(
  var = c("mapc_stars", "map_other_stars", "map_all_stars"),
  file = c("map_c_emission.tif", "map_other_emission.tif", "map_all_emission.tif"),
  label = c("Map of carbon emission factor", "Map of additional emission factor", "Map of all combined emission factor")
)

json_file_df <- data.frame(
  var = c(
    "abacus_baseline",
    "abacus_scenario"
  ),
  file = c(
    "baseline_projection.json",
    "scenario_projection.json"
  ),
  label = c(
    "Baseline data",
    "Scenario data"
  )
)

list_to_df <- function(x) {
  df <- dcast(melt(x), L1 ~ L2)
  df$L1 = NULL
  return(df)
}


### abacuslib handler
jslist_to_df <- function(x, session, inputname) {
  return(fromJSON(x))
}
removeInputHandler("js_to_df")
registerInputHandler("js_to_df", jslist_to_df)

### map 
map_factor_to_numeric <- function(map) {
  v <- unlist(map[[1]])
  nr <- nrow(map[[1]])
  map[[1]] <- matrix(as.numeric(levels(v))[v], nr)
  return(map)
}

reclassify_map <- function(map, fromto_df) {
  fromto_df <- fromto_df[order(fromto_df[,1]),]
  from <- fromto_df[[1]]
  to <- fromto_df[[2]]
  map2 <- cut(map, c(min(from)-1, from), labels = to)
  map2 <- map_factor_to_numeric(map2)
  return(map2)
} 

co2_unit <- function(prefix = "", suffix = "") {
  tags$html(paste0(prefix, "CO"), tags$sub(2, .noWS = c("after", "before")), 
            paste0("e", suffix), .noWS = c("after", "before"))
}

per_ha_unit <- function(prefix = "", suffix = "") {
  tags$html(paste0(prefix, "ha"), tags$sup(-1, .noWS = c("after", "before")), 
            suffix, .noWS = c("after", "before"))
}

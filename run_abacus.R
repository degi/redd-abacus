library(shiny)

resolve_app_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    if (file.exists(script_path)) {
      return(normalizePath(dirname(script_path), winslash = "/"))
    }
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(dirname(sys.frames()[[1]]$ofile), winslash = "/"))
  }

  getwd()
}

app_dir <- resolve_app_dir()

if (!file.exists(file.path(app_dir, "ui.R")) || !file.exists(file.path(app_dir, "server.R"))) {
  stop("REDD-Abacus app files were not found in: ", app_dir)
}

setwd(app_dir)
options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)
runApp(app_dir, launch.browser = FALSE)

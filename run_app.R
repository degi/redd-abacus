shiny::runApp(
  appDir = normalizePath(getwd(), winslash = "/"),
  port = 3838,
  host = "127.0.0.1",
  launch.browser = FALSE
)

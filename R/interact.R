interact <-
function(app) {

# Thanks to
#   Dean Attali at https://deanattali.com/2015/04/21/r-package-shiny-app/

  if (!missing(app)) {
    app <- tolower(app)
    if (app == "barchart") app <- "BarChart"
    if (app == "bc") app <- "BarChart"
    if (grepl("bar", app)) app <- "BarChart"
    if (app == "piechart") app <- "PieChart"
    if (app == "pc") app <- "PieChart"
    if (grepl("pie", app)) app <- "PieChart"
    if (app == "histogram") app <- "Histogram"
    if (app == "hs") app <- "Histogram"
    if (grepl("hist", app)) app <- "Histogram"
    if (app == "scatterplot") app <- "ScatterPlot"
    if (app == "plot") app <- "ScatterPlot"
    if (app == "trellis") app <- "Trellis"
    if (app == "VBS") app <- "Trellis"
  }

  # locate all shiny app examples that exist, as folders in shiny_apps
# validApps <- list.files(system.file("shiny_apps", package="lessR"))
  validApps <- c("BarChart", "PieChart", "Histogram", "ScatterPlot", "Trellis")
  validAppsMsg <-
    paste0("Valid names (enclose in quotes):\n '",
           paste(validApps, collapse = "', '"), "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) || !app %in% validApps) {
    message(
      "Run  interact()  with a one of the following app names, such as: ",
      "interact(\"BarChart\")\n\n",
      validAppsMsg, "\n")
  }

  else { # find and launch the app
    #first save original settings to restore in the regular apps
    options(l.cex=getOption("lab_cex"))
    options(l.axc=getOption("axis_cex"))

    style(lab_cex=1.201, axis_cex=1.011, suggest=FALSE)
    appDir <- system.file("shiny_apps", app, package="lessR")
    shiny::runApp(appDir, display.mode="normal")
    # no return here to process any info after runApp() runs
    # Shiny now loaded
  }

}


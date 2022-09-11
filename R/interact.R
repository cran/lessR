interact <-
function(app) {

# Thanks to
#   Dean Attali at https://deanattali.com/2015/04/21/r-package-shiny-app/

  # locate all shiny app examples that exist, as folders in shiny_examples
  validApps <- list.files(system.file("shiny_apps", package="lessR"))

  validAppsMsg <-
    paste0("Valid names (enclose in quotes):\n '",
           paste(validApps, collapse = "', '"), "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) || !app %in% validApps) {
    message(
      "Run  interact()  with a one of the following app names, such as: ",
      "interact(\"BarChart\")\n\n", validAppsMsg, "\n")
  }
  else { # find and launch the app
    appDir <- system.file("shiny_apps", app, package="lessR")
    shiny::runApp(appDir, display.mode="normal")
    # no return here to process any info after runApp() runs
    # Shiny now loaded including after it quits running
  }

}


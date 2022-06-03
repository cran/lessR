interact <-
function(app) {

# Thanks to
#   Dean Attali at https://deanattali.com/2015/04/21/r-package-shiny-app/

  # locate all the shiny app examples that exist
  validApps <- list.files(system.file("shiny_examples", package="lessR"))

  validAppsMsg <-
    paste0("Valid names:\n '", paste(validApps, collapse = "', '"), "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) || !app %in% validApps) {
    message(
      "Run `interact()` with a valid app name, such as: ",
      "interact(\"BarChart1\")\n\n", validAppsMsg, "\n")
  }
  else { # find and launch the app
    appDir <- system.file("shiny_examples", app, package="lessR")
    shiny::runApp(appDir, display.mode="normal")
    # no return here to process any info after runApp() runs
    # Shiny now loaded including after it quits running
  }

}


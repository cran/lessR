# -----------
# Scatterplot
# -----------

library(shiny)
library(lessR)

clr.one <- list(
  "#324E5C", "dodgerblue3", "cornflowerblue", "steelblue", "darkblue",
  "pink2", "red3", "firebrick2", "darkred",
  "violetred", "mediumorchid", "purple3",
  "darkorange2", "salmon", "orange3", "sienna", "rosybrown", 
  "wheat3", "goldenrod2", "khaki", "yellow2",
  "darkseagreen2", "springgreen3", "seagreen4", "darkgreen",
  "black", "gray45", "slategray4", "gray75", "snow3", "gray95",
  "lavender", "ivory2", "aliceblue", "white")

clr.edge <- list("off", "black", "gray50", "gray75", "white", "ivory", 
  "darkblue", "darkred", "darkgreen", "rosybrown2", "bisque", 
  "slategray2", "aliceblue", "thistle1", "coral", "gold")

clr.qual <- c("hues", "Okabe-Ito", "viridis")

clr.seq <- list("reds", "rusts", "browns", "olives", "greens",
  "emeralds", "turquoises", "aquas", "blues", "purples", "violets",
  "magentas", "grays")

clr.fit <- c("#5C4032", clr.one[2:length(clr.one)])

addResourcePath("shiny_dir", system.file("shiny_apps", package="lessR"))


ui <- fluidPage(
tags$head(tags$link(rel="stylesheet", href="shiny_dir/styles.css")),

  tabsetPanel(

    tabPanel("Data",
      titlePanel(div("Upload a text (.csv, .txt) or Excel file", id="hp")),

      sidebarLayout(
        sidebarPanel(

          radioButtons("fType", HTML("<h5 class='soft'>Format</h5>"), 
                       c("Excel"="Excel", "Text"="Text")),
          conditionalPanel(condition="input.fType == 'Text'",
            radioButtons("sep", HTML("<h5 class='soft'>Separator</h5>"),
                         c(Comma=",", Semicolon=";", Tab="\t"), ","),
            radioButtons("decimal", HTML("<h5 class='soft'>Decimal</h5>"),
                         c("Point"=".", "Comma"=",")),
          ),

          radioButtons("fSource", HTML("<h5 class='soft'>Source</h5>"), 
                       c("Local"="local", "Web"="web")),
          conditionalPanel(condition="input.fSource == 'local'",
            fileInput("myFile", "Locate your data file",
                      accept=c(".csv", ".txt", ".xlsx", ".xlsm")),
          ),
          conditionalPanel(condition="input.fSource == 'web'",
            textInput("myURL", "Web address of data file"),
            actionButton("submitURL", "Submit")
          ),

          textOutput("nrows"),
          textOutput("ncols"),
          uiOutput("d.show"),

        ),  # end sidbarPanel

        mainPanel(
          tableOutput("d.table"),
          tags$style(type="text/css", "#d.table {font-size: .95em;}")
        )

      )  # end sidbarLayout
    ),  # end tabPanel 1


    tabPanel("Scatterplot",
      pageWithSidebar(
        titlePanel(""),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),
          selectInput('y.col', 'y Variable', "", selected=""),

          checkboxInput("do_by", div("by variable", class="view"), FALSE),
          conditionalPanel(condition="input.do_by == true",
            selectInput("by.col", "by Variable", "", selected=""),
            selectInput("myFill_by", "fill",
              choices=list("Qualitative"=clr.qual, "Sequential"=clr.seq)),
          ),

          checkboxInput("do_size", div("size variable", class="view"), FALSE),
          conditionalPanel(condition="input.do_size == true",
            selectInput('size.col', 'size Variable', "", selected=""),
          ),

          tags$hr(),
          checkboxInput("do_geom", div("Points", class="view"), FALSE),
          conditionalPanel(condition="input.do_geom == true",
            conditionalPanel(condition="input.do_by == false",
              selectInput("myFill", "fill", choices=clr.one),
              selectInput("myColor", label="color", choices=clr.edge),
            ),
            sliderInput("myTrans", "transparency", min=0, max=1, value=0),
            conditionalPanel(condition="input.do_size == false",
              sliderInput("mySize", "size", min=0.0, max=5, value=1.25,
                          step=0.25)),
            conditionalPanel(condition="input.do_size == false",
              selectInput("myShape", "shape",
              choices=list("circle", "square", "diamond", "triup", "tridown")),
            ),
          ),

          tags$hr(),
          checkboxInput("do_FlEO", 
                        div("Line, Ellipse, Outliers", class="view"), FALSE),
          conditionalPanel(condition="input.do_FlEO == true",
            conditionalPanel(condition="input.do_by == false",
              checkboxInput("myEnhance", "enhance", value=FALSE),
              checkboxInput("myAddMeans", "add=\"means\"", value=FALSE),
            ),
            selectInput("myFit", "fit",
              choices=list("off", "loess", "lm", "exp", "quad", "null")),
            conditionalPanel(condition="input.myFit != 'off'",
              checkboxInput("myErrors", "plot_errors", value=FALSE),
              conditionalPanel(condition="input.do_by == false",
                selectInput("myFitClr", "fit_color", choices=clr.fit),
              ),
              sliderInput("myFitSE", "fit_se", min=0, max=0.99, value=0.95),
            ),
            sliderInput("myEllipse", "ellipse", min=0, max=0.99, value=0),
            conditionalPanel(condition="input.do_by == false",
              sliderInput("myMDcut", "MD_cut", min=0, max=15, value=0),
              conditionalPanel(condition="input.myMDcut > 0",
                selectInput('ID.col', 'ID variable', "", selected=""),
              ),
            ),
          ),

          tags$hr(),
          checkboxInput("do_jitter", div("Jitter", class="view"), FALSE),
          conditionalPanel(condition="input.do_jitter == true",
            sliderInput("myJitx", "jitter_x", min=0, max=1000, value=0,
                        step=0.5),  # need the max value set from data
            sliderInput("myJity", "jitter_y", min=0, max=1000, value=0,
                        step=0.5),
          ),

          tags$hr(),
          checkboxInput("do_rotate", div("Rotate Labels", class="view"), FALSE),
          conditionalPanel(condition="input.do_rotate == true",
            sliderInput("myRttx", "rotate_x", min=0, max=90, value=0, step=10),
            sliderInput("myRtty", "rotate_y", min=0, max=90, value=0, step=10),
            sliderInput("myOff", "offset", min=0, max=4, value=0.5, step=0.25),
          ),

          tags$hr(),
          checkboxInput("do_pdf", div("Save", class="view"), FALSE),
          conditionalPanel(condition="input.do_pdf == true",
            sliderInput("w", "width (inches):", min=3, max=20, value=8),
            sliderInput("h", "height (inches):", min=3, max=20, value=6),
            checkboxInput("do_cmt", "include comments in R file", TRUE),
            actionButton(inputId="btn_pdf", "Save"),
            tags$p(div("Save pdf file and R code file",
                  style="margin-top:.25em; margin-bottom:0em;")),
          ),

          tags$hr(),
          checkboxInput("do_help", div("Help", class="view"), FALSE),

        ),  # end sidebarPanel

      mainPanel(
        plotOutput('myPlot'),
        verbatimTextOutput("summary"),
        plotOutput("saved_plot"), 
        textOutput("help")
      )

      )  # end pageWithSidebar
  )  # end tabPanel 2
  )  # end tabsetPanel
)  # end fluidPage 


server <- function(input, output, session) {

  v <- reactiveValues()
  v$add <- NULL


# ------- Read and Display Data -----------
# -----------------------------------------

  # process the URL for reading from the web
  theURL <- eventReactive(input$submitURL, {
    input$myURL
  })

  data <- reactive({
    if (input$fSource == "local") {
      shiny::req("input$myFile")
      myPath <- input$myFile$datapath
      theRead <- input$myFile$name
    }
    if (input$fSource == "web") {
      url <- theURL()
      if (!(grepl("http://", url)))
        url <- paste("http://", url, sep="")
      myPath <- url
       theRead <- myPath
    }
      
    shiny::req(myPath)
    if (input$fType == "Excel") { 
      library(openxlsx)
      if (grepl(".xlsx", myPath, fixed=TRUE)) {
        d <- read.xlsx(myPath, detectDates=TRUE)
      }
      else {
        message("\n>>> Excel file must have file type of .xlsx <<<\n\n")
        stopApp()
      }
    }
      if (input$fType == "Text") { 
        if ((grepl(".csv", myPath, fixed=TRUE)) ||
            (grepl(".txt", myPath, fixed=TRUE))) {
            d <- read.csv(myPath, sep=input$sep, dec=input$decimal,
                          na.strings="")  # default is NOT a blank char missing
        }
        else {
          message("\n>>> Text file must have file type of .csv or .txt <<<\n\n")
          stopApp()
        }       
      }  # end fType is "Text"

    shiny::updateSelectInput(session, inputId="x.col", label="x variable",
                      choices=c("Select a variable"="", names(d)))
    shiny::updateSelectInput(session, inputId="y.col", label="y variable",
                      choices=c("Select a variable"="", names(d)))
    
    return(d)
  })  # end reactive()


  output$d.show <- renderUI({
    shiny::req(data())
    output$nrows <- renderText({paste("Number of data rows:", nrow(data()))})
    output$ncols <- renderText({paste("Number of variables:", ncol(data()))})
    if (nrow(data()) > 10)
      radioButtons("d.show", HTML("<h5 class='soft'>Rows to display</h5>"),
                   c("First 10"="head", "Last 10"="tail", "Random 10"="random",
                     "All"="all"))
  })


  output$d.table <- renderTable({
    if (is.null(input$d.show)) 
      data()
    else {
      nr <- min(11, nrow(data()))
      if (nr == 11) {
        if (input$d.show == "all")
          data()
        else if (input$d.show == "head")
          head(data(), n=10)
        else if (input$d.show == "tail")
          tail(data(), n=10)
        else if (input$d.show == "random") { 
          dd <- data()
          dd[.(random(10)), ]
        }
      }
    }
  }, striped=TRUE)  # end renderTable

# -----------------------------------------


  observeEvent(input$do_by, {

    d <- data()
    is.cat <- sapply(d, function(x) {
      lu.x <- length(unique(x))
      x.char <- (is.character(x) || is.factor(x) || lu.x<11) && (lu.x<nrow(d))
    })

    if (all(!is.cat)) {
      message("A  by  variable partitions values of a categorical variable.\n",
              "Categorical variables have non-numeric values or, if numeric,\n",
              "  defined here as 10 or fewer unique values.\n\n",
              "No categorical variables are present in this data set.")
      stopApp()
    }

    the.cats <- names(d)[is.cat]
    shiny::updateSelectInput(session, inputId="by.col", label="by variable",
                        choices=c("Select a categorical variable"="", the.cats))
  })


  observeEvent(input$do_size, {
    d <- data()
    shiny::updateSelectInput(session, inputId="size.col", label="size variable",
                        choices=c("Select a numerical variable"="",
                        names(d)[sapply(d, is.numeric)]))
  })


  observeEvent(input$myMDcut, {
    shiny::req(input$myMDcut > 0)

    d <- data()
    is.unique <- sapply(d, function(x) {
      l.x <- length(x)
      lu.x <- length(unique(x))
      x.unique <- ifelse (l.x == lu.x, TRUE, FALSE)      
    })

    if (all(!is.unique)) {
      message("An  ID  variable must have unique values.\n",
              "No variables have unique values in this data set.")
      stopApp()
    }

    the.unique <- c("row.name", names(d)[is.unique])
    shiny::updateSelectInput(session, inputId="ID.col", label="ID variable",
                        choices=c("Var with unique values"="", the.unique),
                        selected="row.name")
  })


# ------------ Enhance and Add ------------
# -----------------------------------------

  observeEvent(input$myAddMeans, {
    if (input$myAddMeans)
     v$add <- "means"
    else
     v$add <- NULL    
  })


  observeEvent(input$myEnhance, {
    if (input$myEnhance) {
     v$add <- "means"
     add.val <- TRUE
    }
    else {
     v$add <- NULL
     add.val <- FALSE
    }
    updateCheckboxInput(session, inputId="myAddMeans", "add=\"means\"",
        value=add.val)

    elp.val <- ifelse (input$myEnhance, 0.95, 0)
    updateSliderInput(session, inputId="myEllipse", "ellipse",
         min=0, max=0.99, value=elp.val)

    fit.val <- ifelse (input$myEnhance, "lm", "off")
    updateSelectInput(session, inputId="myFit", "fit", 
         choices=list("off", "loess", "lm", "exp", "quad", "null"),
         selected=fit.val)
    if (!input$do_by) {
      MDc.val <- ifelse (input$myEnhance, 6, 0)
      updateSliderInput(session, inputId="myMDcut", "MD_cut",
           min=0, max=15, value=MDc.val)
    }
  })
 


# ------------ The Scatterplot ------------
# -----------------------------------------

  output$myPlot <- renderPlot({
    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]

    y.name <- input$y.col
    shiny::req(y.name)
    y <- data()[, y.name]

    in.fill <- input$myFill
    in.shp  <- input$myShape

    # shiny only plots shapes 21:25, everything else blank
    # so if more than 5 levels, then shiny plots different shapes
    by <- NULL
    lt <- NULL  # legend_title
    if (input$do_by) {  # a by variable
      by.name <- input$by.col
      shiny::req(by.name)
      by <- data()[, by.name]
      by.unq <- length(unique(by))
      in.fill <- input$myFill_by  # color range selection
#     shapes <-  c(24,25,21,22,23,24,25,21,22,23,24,25,21,22,23)
#     in.shp  <- shapes[1:by.unq]  # with shiny, input$myShape never missing 
      if (in.shp == "circle") in.shp=21
      if (in.shp == "square") in.shp=22
      if (in.shp == "diamond") in.shp=23
      if (in.shp == "triup") in.shp=24
      if (in.shp == "tridown") in.shp=25
      lt <- by.name
    }

    in.size <- input$mySize   # constant value of size from slider
    if (input$do_size) {  # see if proceed with size is a variable
      shiny::req(input$size.col)
      in.size <- data()[, input$size.col]
      size.name <- input$size.col
    }
    if (!input$do_size) in.size <- 1.4 * in.size  # scale up, as with axes

    in.fit <- input$myFit
    in.elp <- input$myEllipse
    in.add <- v$add 
    if (input$do_by) {  # choose MD then by var
      in.MD <- 0
      in.ID <- row.names(data())
      if (input$myEnhance) {
        in.fit <- "off" 
        in.elp <- 0
        in.add <- NULL
      }
    }
    else {
      in.MD <- input$myMDcut 
      if (nchar(input$ID.col) != 0) {
        if (input$ID.col != "row.name")
          in.ID <- data()[, input$ID.col]
        else
          in.ID <- row.names(data())
      }
      else
        in.ID <- row.names(data())
    }

     v$p <- Plot(x, y, data=NULL, by=by,
          fill=in.fill, color=input$myColor, transparency=input$myTrans,
          size=in.size, shape=in.shp, plot_errors=input$myErrors,
          fit=in.fit, fit_color=input$myFitClr, fit_se=input$myFitSE, 
          ellipse=in.elp, MD_cut=in.MD, ID=in.ID, add=in.add, 
          enhance=input$myEnhance, jitter_x=input$myJitx, jitter_y=input$myJity,
          rotate_x=input$myRttx, rotate_y=input$myRtty, offset=input$myOff,  
          xlab=x.name, ylab=y.name, legend_title=lt, quiet=FALSE)

      p_fill <- ifelse (nchar(input$by.col)==0, in.fill == "#324E5C",
                                                in.fill == "hues") 
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_by <- input$do_by == FALSE
      p_size <- input$mySize==1.25 && length(in.size)==1
      p_shape <- input$myShape == "circle"
      p_errors <- input$myErrors == FALSE
      p_fit <- in.fit == "off"
      p_fitclr <- input$myFitClr == getOption("fit_color")
      p_fitse <- input$myFitSE == 0.95
      p_ellipse <- in.elp == 0
      p_MDcut <- in.MD == 0
      p_ID <- nchar(input$ID.col)==0 || input$ID.col=="row.name" || in.MD==0
      p_jitx <- input$myJitx == 0
      p_jity <- input$myJity == 0
      p_add <- is.null(in.add)

      # all enhance default settings must be TRUE for enhance to be TRUE
      p_enhance <- FALSE
      if (input$myEnhance && in.MD > 0 && !is.null(v$add)) {
        if (input$myFit=="lm" && input$myEllipse==0.95
            && input$myMDcut==6 && v$add=="means")
          p_enhance <- TRUE
        else
          p_enhance <- FALSE
      }

      out <- paste("Plot(", x.name, ", ", y.name, sep="")

      if (!p_fill) out <- paste(out, ", fill=\"", in.fill, "\"", sep="")
      if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
      if (!p_trans) out <- paste(out, ", transparency=", input$myTrans, sep="")
      if (!p_by) out <- paste(out, ", by=", by.name, sep="")
      if (!p_size) {
        sz.out <- ifelse (length(in.size)==1, input$mySize, input$size.col)
        out <- paste(out, ", size=", sz.out, sep="")
      }
      if (!p_shape) out <- paste(out, ", shape=\"", input$myShape, "\"", sep="")
      if (!p_errors) out <- paste(out, ", errors=", input$myErrors, sep="")
      if (!p_fitclr) out <- paste(out, ", fit_color=\"", input$myFitClr,
                                  "\"", sep="")
      if (!p_fitse) out <- paste(out, ", fit_se=", input$myFitSE, sep="")
      if (!p_jitx) out <- paste(out, ", jitter_x=", input$myJitx, sep="")
      if (!p_jity) out <- paste(out, ", jitter_y=", input$myJity, sep="")

      if (!p_enhance) {
        if (!p_fit) out <- paste(out, ", fit=\"", in.fit, "\"", sep="")
        if (!p_ellipse) out <- paste(out, ", ellipse=", in.elp, sep="")
        if (!p_MDcut) out <- paste(out, ", MD_cut=", in.MD, sep="")
        if (!p_add) out <- paste(out, ", add=\"", in.add, "\"", sep="")
      }
      else
        out <- paste(out, ", enhance=", p_enhance, sep="")

      if (!p_ID) out <- paste(out, ", ID=\"", input$ID.col, "\"", sep="")

      out <- paste(out, ")", sep="")
      cat(out, "\n")
      v$code <- out  # save the code for a pdf file
  })  # end renderPlot


  # print stats
  output$summary <- renderPrint({
    shiny::req(v$p)
    print(v$p)
  })


  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- v$code

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]

    y.name <- input$y.col
    shiny::req(y.name)
    y <- data()[, y.name]

    in.fill <- input$myFill

    by <- NULL
    lt <- NULL  # legend_title
    if (input$do_by) {  # a by variable
      by.name <- input$by.col
      shiny::req(by.name)
      by <- data()[, by.name]
      in.fill <- input$myFill_by  # color range selection
      lt <- by.name
    }

    in.size <- input$mySize   # constant value of size from slider
    if (input$do_size) {  # see if proceed with size is a variable
      shiny::req(input$size.col)
      in.size <- data()[, input$size.col]
    }
     
    pdf.fname <- paste("plot_", x.name, y.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)

    in.fit <- input$myFit
    in.elp <- input$myEllipse
    in.add <- v$add 
    if (input$do_by) {  # choose MD then by var
      in.MD <- 0
      in.ID <- row.names(data())
      if (input$myEnhance) {
        in.fit <- "off" 
        in.elp <- 0
        in.add <- NULL
      }
    }
    else {
      in.MD <- input$myMDcut 
      if (nchar(input$ID.col) != 0) {
        if (input$ID.col != "row.name")
          in.ID <- data()[, input$ID.col]
        else
          in.ID <- row.names(data())
      }
      else
        in.ID <- row.names(data())
    }

    # styles before re-set in interact() were saved
    style(lab_cex=getOption("l.cex"))
    style(axis_cex=getOption("l.axc"))

    Plot(x, y, data=NULL, by=by,
         fill=in.fill, color=input$myColor, transparency=input$myTrans,
         size=in.size, shape=input$myShape, plot_errors=input$myErrors,
         fit=in.fit, fit_color=input$myFitClr, fit_se=input$myFitSE, 
         ellipse=in.elp, MD_cut=in.MD, ID=in.ID, add=in.add,
         enhance=input$myEnhance, jitter_x=input$myJitx, jitter_y=input$myJity,
         rotate_x=input$myRttx, rotate_y=input$myRtty, offset=input$myOff,  
         xlab=x.name, ylab=y.name, legend_title=lt, quiet=TRUE,
         pdf_file=pdf.path,
         width=as.numeric(input$w), height=as.numeric(input$h))

    # reset back to shiny setting
    style(lab_cex=1.201, axis_cex=1.011, suggest=FALSE)

    # R code
    r.fname <- paste("plot_", x.name, y.name, ".r", sep="")
    r.path <- file.path(path.expand("~"), r.fname)
    cat("\n")
    message("---------------------------------------------")
    cat("Files written to folder:", path.expand("~"), "\n")
    message("---------------------------------------------")
    cat("pdf file: ", pdf.fname, "\n")
    cat("R code file: ", r.fname, "\n")
    message("---------------------------------------------")
    cat("\n")

    if (input$fSource == "web") {
      url <- theURL()
      if (!(grepl("http://", url)))
        url <- paste("http://", url, sep="")
    }

    read.path <- ifelse (input$fSource == "local", input$myFile$name, url)
    read.code <- paste("d <- Read(\"", read.path, "\")", sep="")
    is.local <- !grepl("http://", read.path, fixed=TRUE)

    if (input$do_cmt)
      cat("# The # symbol indicates a comment rather than an R instruction\n\n",
          "# Begin the R session by loading the lessR functions ",
          "from the library\n", sep="", file=r.path)
      cat("library(\"lessR\")\n\n", file=r.path, append=TRUE)

    if (input$do_cmt) {
      cat("# Read your data into an R data table, the data frame, here d",
          "\n", sep="", file=r.path, append=TRUE)
      if (is.local)
        cat("# To browse for the data file, include nothing between the quotes",
            "\n", sep="", file=r.path, append=TRUE)
    }
    if (is.local && input$do_cmt)
      cat("d <- Read(\"\")\n\n", file=r.path, append=TRUE)

    if (is.local && input$do_cmt) {
      cat("# For security, the path to your data file is not available\n",
          "# You can replace PATHtoFILE in the following with the path\n",
          "# Remove the # sign in the first column and delete the previous ",
          "Read()\n", sep="", file=r.path, append=TRUE)
      read.path <- file.path("PATHtoFILE", read.path) 
      read.code <- paste("# d <- Read(\"", read.path, "\")", sep="")
    }
    cat(read.code, "\n\n", file=r.path, append=TRUE)

    if (input$do_cmt)
      cat("# When you have your data table, do the scatterplot analysis of\n",
          "#   two variables in the data table\n",
          "# d is the default data frame name, so no need to specify\n",
          sep="", file=r.path, append=TRUE)
    cat(code, "\n\n", file=r.path, append=TRUE)

    anlys <- "ScatterPlot()"
    if (input$do_cmt)
      cat("# If accessing data with a name other than d, must add  data=NAME\n",
          paste("#   to the", anlys, "call, where NAME is the name of your",
          "data frame"), "\n", sep="", file=r.path, append=TRUE)

  })
  output$saved_plot <- renderPlot({ plotInput() })


  # access web page help file
  output$help <- eventReactive(input$do_help, {
    shiny::req(input$do_help)
    fp <- system.file("shiny_apps/help/ScatterPlot.html", package="lessR")
    browseURL(fp)
  })

}  # end server

shinyApp(ui, server)

# -----------
# Scatterplot
# -----------

library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=1, suggest=FALSE)

clr.one <- list(
  "#324E5C", "dodgerblue3", "cornflowerblue", "darkblue", "pink2", "red3",
  "darkred", "darkorange2", "lightcoral", "salmon", "ivory", "wheat3",
  "burlywood4", "sienna", "goldenrod2", "yellow2", "darkseagreen2",
  "springgreen3", "seagreen4", "violetred", "lavender", "thistle3",
  "lightcoral", "magenta3", "mediumorchid",
  "black", "gray45", "gray75", "gray95", "white")

clr.fit <- c("#5C4032", clr.one[2:length(clr.one)])

clr.edge <- list("off", "#8496AF", "black", "gray50", "gray75", "white", 
  "darkblue", "darkred", "darkgreen", "rosybrown2", "slategray2", "thistle1",
  "coral", "gold", "ivory")

clr.qual <- list("reds", "rusts", "browns", "olives", "greens",
  "emeralds", "turquoises", "aquas", "blues", "purples", "violets",
  "magentas", "grays")


ui <- fluidPage(
  tags$style(type='text/css', "
             label {font-size: 1em; font-weight: bold;}           
             hr {border-top: .10em solid #8f8f8f;}
             .nav-tabs {font-size: 1.1em;}
             .checkbox label {font-weight: bold;}
             .soft {margin-bottom:.25em; font-size:1em; font-style:italic;}
             .hp {color:#808080; font-size: .5em;}
             .head {font-size:.85em; margin-top: -1em; color:#8f8f8f}
             "),

  titlePanel("Analysis"),

  tabsetPanel(
    tabPanel("Data",
      headerPanel(div("Upload a text (.csv, .txt) or Excel file", class="hp")),

      sidebarLayout(
        sidebarPanel(
          radioButtons("fType", "Format", c("Excel"="Excel", "Text"="Text")),
          conditionalPanel(condition="input.fType == 'Text'",
            radioButtons("sep", HTML("<h5 class='soft'>Separator</h5>"),
                         c(Comma=",", Semicolon=";", Tab="\t"), ","),
            radioButtons("decimal", HTML("<h5 class='soft'>Decimal</h5>"),
                         c("Point"=".", "Comma"=",")),
            tags$br()
          ),

          radioButtons("fSource", "Source", c("local"="local", "web"="web")),
          conditionalPanel(condition="input.fSource == 'local'",
            fileInput("myFile", "Locate your data file",
                      accept=c(".csv", ".txt", ".xlsx", ".xlsm"))
          ),
          conditionalPanel(condition="input.fSource == 'web'",
            textInput("myURL", "web address"),
            actionButton("submitURL", "Submit")
          ),

          textOutput("nrows"),
          textOutput("ncols"),
          tags$br(),
          radioButtons("show", "Rows to display",
                       c("First 10"="head", "Last 10"="tail", "All"="all"))

        ),  # end sidbarPanel

        mainPanel(
          tableOutput("contents"),
          tags$style(type="text/css", "#contents {font-size: .95em;}")
        )

      )  # end sidbarLayout
    ),  # end tabPanel 1


    tabPanel("Scatterplot",
      pageWithSidebar(
        headerPanel(div("2-Dimensional Scatterplot", class="hp")),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),
          selectInput('y.col', 'y Variable', "", selected=""),

          checkboxInput("do_by", "add a by variable", FALSE),
          conditionalPanel(condition="input.do_by == true",
            selectInput("by.col", "by Variable", "", selected=""),
            selectInput("myFill2", "fill",
              choices=list("Qualitative"=list("hues"), "Sequential"=clr.qual)),
          ),

          checkboxInput("do_size", "add a size variable", FALSE),
          conditionalPanel(condition="input.do_size == true",
            selectInput('size.col', 'size Variable', "", selected=""),
          ),

          tags$hr(),
          h4(div("Colors, Size, & Shape", class="head")),
          checkboxInput("do_geom", "view options", FALSE),
          conditionalPanel(condition="input.do_geom == true",
            conditionalPanel(condition="input.do_by == false",
              selectInput("myFill", "fill", choices=clr.one),
              selectInput("myColor", label="color", choices=clr.edge),
            ),
            sliderInput("myTrans", label="trans", min=0, max=1, value=0),
            conditionalPanel(condition="input.do_size == false",
              sliderInput("mySize", "size", min=0.0, max=5, value=1.25,
                          step=0.25)),
            selectInput("myShape", "shape",
              choices=list("circle", "square", "diamond", "triup", "tridown")),
          ),

          tags$hr(),
          h4(div("Fit Line, Ellipse, & Outliers", class="head")),
          checkboxInput("do_FlEO", "view options", FALSE),
          conditionalPanel(condition="input.do_FlEO == true",
            conditionalPanel(condition="input.do_by == false",
              checkboxInput("myEnhance", "enhance", value=FALSE),
            ),
            checkboxInput("myErrors", "plot_errors", value=FALSE),
            selectInput("myFit", "fit",
              choices=list("off", "loess", "lm", "exp", "quad", "null")),
            conditionalPanel(condition="input.myFit != 'off'",
              selectInput("myFitClr", "fit_color", choices=clr.fit),
              sliderInput("myFitSE", "fit_se", min=0, max=0.99, value=0.95),
            ),
            sliderInput("myEllipse", "ellipse", min=0, max=0.99, value=0),
            conditionalPanel(condition="input.do_by == false",
              sliderInput("myMDcut", "MD_cut", min=0, max=12, value=0),
            ),
            checkboxInput("myAddMeans", "add=\"means\"", value=FALSE),
          ),

          tags$hr(),
          h4(div("Jitter Points", class="head")),
          checkboxInput("do_jitter", "view options", FALSE),
          conditionalPanel(condition="input.do_jitter == true",
            sliderInput("myJitx", "jitter_x", min=0, max=2.5, value=0, step=0.10),
            sliderInput("myJity", "jitter_y", min=0, max=2.5, value=0, step=0.10),
          ),

          tags$hr(),
          h4(div("Rotate Axis Labels", class="head")),
          checkboxInput("do_rotate", "view options", FALSE),
          conditionalPanel(condition="input.do_rotate == true",
            sliderInput("myRttx", "rotate_x", min=0, max=90, value=0, step=10),
            sliderInput("myRtty", "rotate_y", min=0, max=90, value=0, step=10),
            sliderInput("myOff", "offset", min=0, max=4, value=0.5, step=0.25),
          ),

          tags$hr(),
          h4(div("Save", class="head")),
          checkboxInput("do_pdf", "view options", FALSE),
          conditionalPanel(condition="input.do_pdf == true",
            sliderInput("w", "width (inches):", min=3, max=20, value=8),
            sliderInput("h", "height (inches):", min=3, max=20, value=6),
            checkboxInput("do_cmt", "include comments in R file", TRUE),
            actionButton(inputId="btn_pdf", "Save"),
            tags$p(div("Save pdf file and R code file",
                  style="margin-top:.25em; margin-bottom:0em;")),
          )
        ),  # end sidebarPanel

        mainPanel(
          plotOutput('myPlot'),
          verbatimTextOutput("summary"),
          plotOutput("saved_plot") 
        )

      )  # end pageWithSidebar
  )  # end tabPanel 2
  )  # end tabsetPanel
)  # end fluidPage 


server <- function(input, output, session) {

  vals <- reactiveValues()
  vals$add <- NULL


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
      vals$theRead <- input$myFile$name
    }
    if (input$fSource == "web") {
      url <- theURL()
      if (!(grepl("http://", url)))
        url <- paste("http://", url, sep="")
      myPath <- url
       vals$theRead <- myPath
    }
      
    shiny::req(myPath)
    if (input$fType == "Excel") { 
      library(openxlsx)
      if (grepl(".xlsx", myPath, fixed=TRUE)) {
        d <- read.xlsx(myPath, detectDates=TRUE)
      }
      else {
        message("Excel file must have file type of .xlsx")
        stopApp()
      }
    }
      if (input$fType == "Text") { 
        if ((grepl(".csv", myPath, fixed=TRUE)) ||
            (grepl(".txt", myPath, fixed=TRUE))) {
            d <- read.csv(myPath, sep=input$sep, dec=input$decimal)
        }
        else {
          message("Text file must have file type of .csv or .txt")
          stopApp()
        }       
      }  # end fType is "Text"

    shiny::updateSelectInput(session, inputId="x.col", label="x variable",
                      choices=c("Select a variable"="", names(d)))
    shiny::updateSelectInput(session, inputId="y.col", label="y variable",
                      choices=c("Select a variable"="", names(d)))
    
    output$nrows <- shiny::renderText({paste("Number of rows of data:", nrow(d))})
    output$ncols <- shiny::renderText({paste("Number of variables:", ncol(d))})

    return(d)
  })  # end reactive()


  output$contents <- renderTable({
    if (input$show == "all")
      data()
    else if (input$show == "head")
      head(data(), n=10)
    else if (input$show == "tail")
      tail(data(), n=10)
  })  # end renderTable


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
              "There are no categorical variables in this data set.")
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


# ------------ Enhance and Add ------------
# -----------------------------------------

  observeEvent(input$myAddMeans, {
    if (input$myAddMeans)
     vals$add <- "means"
    else
     vals$add <- NULL    
  })


  observeEvent(input$myEnhance, {
    if (input$myEnhance) {
     vals$add <- "means"
     add.val <- TRUE
    }
    else {
     vals$add <- NULL
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
           min=0, max=12, value=MDc.val)
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

    in.fill <- ifelse (nchar(input$by.col)==0, input$myFill, input$myFill2)

    by <- NULL
    lt <- NULL  # legend_title
    if (input$do_by) {  # a by variable
      by.name <- input$by.col
      shiny::req(by.name)
      by <- data()[, by.name]
      in.fill <- input$myFill2  # color range selection
      lt <- by.name
    }

    in.size <- input$mySize   # constant value of size from slider
    if (input$do_size) {  # see if proceed with size is a variable
      shiny::req(input$size.col)
      in.size <- data()[, input$size.col]
      size.name <- input$size.col
    }

    if (!input$do_size) in.size <- 1.4 * in.size  # scale up, as with axes

     vals$p <- Plot(x, y, data=NULL, by=by,
          fill=in.fill, color=input$myColor, trans=input$myTrans,
          size=in.size, shape=input$myShape, plot_errors=input$myErrors,
          fit=input$myFit, fit_color=input$myFitClr, fit_se=input$myFitSE, 
          ellipse=input$myEllipse, MD_cut=input$myMDcut, add=vals$add,
          enhance=input$myEnhance, jitter_x=input$myJitx, jitter_y=input$myJity,
          rotate_x=input$myRttx, rotate_y=input$myRtty, offset=input$myOff,  
          xlab=x.name, ylab=y.name, legend_title=lt, quiet=FALSE)

      p_fill <- ifelse (nchar(input$by.col)==0, in.fill == "#324E5C",
                                                in.fill == "hues") 
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_size <- input$mySize == 1.25
      p_shape <- input$myShape == "circle"
      p_errors <- input$myErrors == FALSE
      p_fit <- input$myFit == "off"
      p_fitclr <- input$myFitClr == getOption("fit_color")
      p_fitse <- input$myFitSE == 0.95
      p_ellipse <- input$myEllipse == 0
      p_MDcut <- input$myMDcut == 0
      p_jitx <- input$myJitx == 0
      p_jity <- input$myJity == 0
      p_add <- is.null(vals$add)

      # all enhance default settings must be TRUE for enhance to be TRUE
      p_enhance <- FALSE
      if (input$myEnhance) {
        if (input$myFit=="lm" && input$myEllipse==0.95
            && input$myMDcut==6 && vals$add=="means")
          p_enhance <- TRUE
        else
          p_enhance <- FALSE
      }

      # can have by or size but not both
      if (input$do_by)
        out <- paste("Plot(", x.name, ", ", y.name, ", by=", by.name, sep="")
      else if (input$do_size)
        out <- paste("Plot(", x.name, ", ", y.name, ", size=", size.name, sep="")
      else
        out <- paste("Plot(", x.name, ", ", y.name, sep="")

      if (!p_fill) out <- paste(out, ", fill=\"", in.fill, "\"", sep="")
      if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
      if (!p_trans) out <- paste(out, ", trans=", input$myTrans, sep="")
      if (!p_size) out <- paste(out, ", size=", input$mySize, sep="")
      if (!p_shape) out <- paste(out, ", shape=\"", input$myShape, "\"", sep="")
      if (!p_errors) out <- paste(out, ", errors=", input$myErrors, sep="")
      if (!p_fitclr) out <- paste(out, ", fit_color=\"", input$myFitClr,
                                  "\"", sep="")
      if (!p_fitse) out <- paste(out, ", fit_se=", input$myFitSE, sep="")
      if (!p_jitx) out <- paste(out, ", jitter_x=", input$myJitx, sep="")
      if (!p_jity) out <- paste(out, ", jitter_y=", input$myJity, sep="")

      if (!p_enhance) {
        if (!p_fit) out <- paste(out, ", fit=\"", input$myFit, "\"", sep="")
        if (!p_ellipse) out <- paste(out, ", ellipse=", input$myEllipse, sep="")
        if (!p_MDcut) out <- paste(out, ", MDcut=", input$myMDcut, sep="")
        if (!p_add) out <- paste(out, ", add=\"", vals$add, "\"", sep="")
      }
      else
        out <- paste(out, ", enhance=", p_enhance, sep="")

      out <- paste(out, ")", sep="")
      cat(out, "\n")
      vals$code <- out  # save the code for a pdf file
  })  # end renderPlot


  # print stats
  output$summary <- renderPrint({
    shiny::req(vals$p)
    print(vals$p)
  })


  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- vals$code

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
      in.fill <- input$myFill2  # color range selection
      lt <- by.name
    }

    in.size <- input$mySize   # constant value of size from slider
    if (input$do_size) {  # see if proceed with size is a variable
      shiny::req(input$size.col)
      in.size <- data()[, input$size.col]
    }
     
    pdf.fname <- paste("plot_", x.name, y.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)

    Plot(x, y, data=NULL, by=by,
         fill=in.fill, color=input$myColor, trans=input$myTrans,
         size=in.size, shape=input$myShape, plot_errors=input$myErrors,
         fit=input$myFit, fit_color=input$myFitClr, fit_se=input$myFitSE, 
         ellipse=input$myEllipse, MD_cut=input$myMDcut, add=vals$add,
         enhance=input$myEnhance, jitter_x=input$myJitx, jitter_y=input$myJity,
         rotate_x=input$myRttx, rotate_y=input$myRtty, offset=input$myOff,  
         xlab=x.name, ylab=y.name, legend_title=lt, quiet=TRUE,
         pdf_file=pdf.path,
         width=as.numeric(input$w), height=as.numeric(input$h))

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

    if (input$do_cmt) {
      cat("# The first line of any R/lessR session is always\n",
          "library(\"lessR\")\n# Next read your data into R, then analysis\n\n",
          file=r.path)
    }

    read.path <- vals$theRead
    read.code <- paste("d <- Read(\"", read.path, "\"", ")", sep="")
    is.local <- !grepl("http://", read.path, fixed=TRUE)
    if (is.local && input$do_cmt) {
      read.path <- file.path("PATHtoFILE", read.path) 
      read.code <- paste("d <- Read(\"", read.path, "\"", ")", sep="")
      cat("# For security, the path to your data file is",
          "not available here\n", file=r.path, append=TRUE)
      cat("# Either browse for the data file, with nothing",
          "between the quotes,\n", file=r.path, append=TRUE)
      cat("# or replace PATHtoFILE with the actual path\n\n",
          file=r.path, append=TRUE)
    }
    cat("d <- Read(\"\"", ")\n",
         file=r.path, append=(is.local && input$do_cmt), sep="")
    if (is.local  &&  input$do_cmt) {
      cat("#       or\n", file=r.path, append=TRUE)
      cat(read.code, "\n\n", file=r.path, append=TRUE)
    }
    if (input$do_cmt) {
      cat("# d is the default data frame name, otherwise\n", 
          file=r.path, append=TRUE)
      cat("# if reading into a data frame, with the specified name, add: ",
          "data=\n\n", file=r.path, append=TRUE)
    }
    cat(code, "\n", file=r.path, append=TRUE)
    if (input$do_cmt) {
      cat("#       or\n", file=r.path, append=TRUE)
      new.code <- sub(")", "", code, fixed=TRUE)
      new.code <- paste(new.code, ", data=NAME)\n", sep="")
      cat(new.code, file=r.path, append=TRUE)
    }

  })
  output$saved_plot <- renderPlot({ plotInput() })

}  # end server

shinyApp(ui, server)

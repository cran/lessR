# --------
# BarChart
# --------

library(shiny)
library(lessR)

clr.one <- list(
  "slategray3", "dodgerblue3", "cornflowerblue", "steelblue", "darkblue",
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

          textOutput("ncols"),
          textOutput("nrows"),
          uiOutput("d.radio"),

        ),  # end sidbarPanel

        mainPanel(
          tableOutput("d.table"),
          tags$style(type="text/css", "#d.table {font-size: .95em;}")
        )

      )  # end sidbarLayout
    ),  # end tabPanel 1


    tabPanel("BarChart",
      pageWithSidebar(
        titlePanel(""),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),

          checkboxInput("do_by", div("by variable", class="view"), FALSE),
          conditionalPanel(condition="input.do_by == true",
            selectInput('by.col', 'by Variable', "", selected=""),
            selectInput("myFill2", "fill",
              choices=list("Qualitative"=clr.qual, "Sequential"=clr.seq)),
            checkboxInput("myBeside", "beside", value = FALSE),
            checkboxInput("my100", "stack100", value = FALSE),
          ),

          tags$hr(),
          checkboxInput("do_y", div("y variable", class="view"), FALSE),
          conditionalPanel(condition="input.do_y == true",
            selectInput('y.col', 'y Variable', "", selected=""),
            uiOutput("radio_stats")  # only if not a summary table
          ),

          tags$hr(),
          checkboxInput("do_geom", div("Bars", class="view"), FALSE),
          conditionalPanel(condition="input.do_geom == true",
            conditionalPanel(condition="input.do_by == false",
              selectInput("myFill", "fill",
                choices=list("Qualitative"=clr.qual,
                             "Constant"=clr.one, "Sequential"=clr.seq))),
            selectInput("myColor", label="color", choices=clr.edge),
            sliderInput("myTrans", label="transparency", min=0, max=1, value=0),
            selectInput("mySort", "sort", choices=list("0", "+", "-")),
            checkboxInput("myHoriz", "horiz", value=FALSE)
          ),

          tags$hr(),
          checkboxInput("do_values", div("Values", class="view"), FALSE),
          conditionalPanel(condition="input.do_values == true",
            selectInput("myValues", "values", choices=list("%","input","off")),
            selectInput("myValuesColor", "values_color",
               choices=list("white", "gray", "darkgray", "black",
                            "red", "green3")),
            conditionalPanel(condition="input.do_by == false",
              selectInput("myValuesPos", "values_position",
                 choices=list("in", "out")),
            ),
            sliderInput("myValuesSize", "values_size",
                        min=0, max=2, value=0.9, step=0.1),
          ),

          tags$hr(),
          checkboxInput("do_pdf", div("Save", class="view"), FALSE),
          conditionalPanel(condition="input.do_pdf == true",
            sliderInput("w", "width (inches):", min=3, max=20, value=8),
            sliderInput("h", "height (inches):", min=3, max=20, value=6),
            checkboxInput("do_cmt", "include comments in R file", TRUE),
            actionButton(inputId="btn_pdf", "Save"),
            tags$p(div("Save pdf file and R code file",
                  style="margin-top:.25em; margin-bottom"))
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

  # select categorical variables from read data frame
  # unlike the other interactive apps, need to adapt to input summary table
  the.vars <- function(data, cats=TRUE) {

    lu.x <- sapply(data, function(x) { length(unique(x)) }) 
    is.cat <- logical(length=ncol(data))
    nr <- min(nrow(data), 500)
    for (i in 1:ncol(data)) {
      is.cat[i] <- is.character(data[1:nr,i]) || is.factor(data[1:nr,i])
      # numeric var is.cat if 10 or less unique values but not a summary tbl
      if (is.numeric(data[1:nr,i]) && lu.x[i]<11)
        if (nrow(data)>lu.x[i] && ncol(data)>2) is.cat[i] <- TRUE 
    }

    if (all(!is.cat)) {
      message("A bar chart displays the values of a categorical variable.\n",
              "Categorical variables have non-numeric values or, if numeric,\n",
              "  defined here as 10 or fewer unique values.\n\n",
              "There are no categorical variables in this data set.")
      stopApp()
    }

    if (cats)
      return(names(data)[is.cat])
    else
      return(names(data)[!is.cat])
  }


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
        d <- read.xlsx(myPath)
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

    updateSelectInput(session, inputId="x.col", label="x variable",
                      choices=c("Select a categorical variable" = "",
                              the.vars(d, cats=TRUE)))
    
    return(d)
  })  # end reactive()


  output$d.radio <- renderUI({
    shiny::req(data())
    output$nrows <- renderText({paste("Number of data rows:", nrow(data()))})
    output$ncols <- renderText({paste("Number of variables:", ncol(data()))})
    if (nrow(data()) > 10)
      radioButtons("d.radio", HTML("<h5 class='soft'>Rows to display</h5>"),
                   c("First 10"="head", "Last 10"="tail", "Random 10"="random",
                     "All"="all"))
  })

  output$d.table <- renderTable({
    if (is.null(input$d.radio)) 
      data()
    else {
      nr <- min(11, nrow(data()))
      if (nr == 11) {
        if (input$d.radio == "all")
          data()
        else if (input$d.radio == "head")
          head(data(), n=10)
        else if (input$d.radio == "tail")
          tail(data(), n=10)
        else if (input$d.radio == "random") { 
          dd <- data()
          dd[.(random(10)), ]
        }
      }
    }
  }, striped=TRUE)  # end renderTable

# -----------------------------------------


  observeEvent(input$do_by, {
    shiny::updateSelectInput(session, inputId="by.col", label="by variable",
                             choices=c("Select a categorical variable" = "",
                             the.vars(data(), cats=TRUE)))
  })


  observeEvent(input$do_y, {
    shiny::updateSelectInput(session, inputId="y.col", label="y variable",
                      choices=c("Select a numerical variable" = "",
                              names(data())[sapply(data(), is.numeric)]))
  })

  output$radio_stats <- renderUI ({
    shiny::req(input$do_y)

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
    unq.x <- length(unique(x))
    smry.tbl <- ifelse (unq.x==length(x) && ncol(data())==2, TRUE, FALSE)

    shiny::req(!smry.tbl)  # stat makes no sense for summary table data
    radioButtons("statType", "stat", 
               c("sum"="sum", "mean"="mean", "deviation"="dev",
                 "sd"="sd", "min"="min", "median"="median",
                 "max"="max"), select="mean")
    })

# ----------------------------------------

  get.ax.nm <- function (nm, in.stat, smry.tbl) {

    a <- ""
    if (!is.null(in.stat)) {
      if (in.stat == "sum") a <- "Sum"
      if (in.stat == "mean") a <- "Mean"
      if (in.stat == "sd") a <- "Standard Deviation"
      if (in.stat == "dev") a <- "Mean Deviation"
      if (in.stat == "min") a <- "Minimum"
      if (in.stat == "median") a <- "Median"
      if (in.stat == "max") a <- "Maximum"
    }

    axis.name <- " "
    if (!smry.tbl) {
      axis.name <- ifelse (nchar(a) > 0, paste(a, "of", nm), " ")
    }
    else
      axis.name <- nm  # summary table

    return(axis.name)
  }


# ------------- The BarChart --------------
# -----------------------------------------

  output$myPlot <- renderPlot({

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
    unq.x <- length(unique(x))
    smry.tbl <- ifelse (unq.x==length(x) && ncol(data())==2, TRUE, FALSE)

    y <- NULL
    in.stat <- NULL
    y.name <- ""
    if (input$do_y) {
      y.name <- input$y.col
      shiny::req(y.name)
      y <- data()[, y.name]
      in.stat <- input$statType
    }

    if (nchar(y.name) == 0) 
      axis.name <- paste("Count of", x.name)
    else
      axis.name <- get.ax.nm(y.name, in.stat, smry.tbl)

    in.fill <- input$myFill
    by.name <- ""
    by <- NULL
    lt <- NULL  # legend_title
    if (input$do_by) {  # a by variable
      by.name <- input$by.col
      shiny::req(by.name)
      by <- data()[, by.name]
      in.fill <- input$myFill2  # color range selection
      lt <- by.name
    }
    v$by.name <- by.name
     
    # analysis has variables in global env, not a data frame
    v$b <- BarChart(x, y, by=by, data=NULL, stat=in.stat,
             fill=in.fill, color=input$myColor, transparency=input$myTrans,
             sort=input$mySort, horiz=input$myHoriz,
             stack100=input$my100, beside=input$myBeside,
             values=input$myValues, values_color=input$myValuesColor,
             values_size=input$myValuesSize, values_position=input$myValuesPos,
             xlab=x.name, ylab=axis.name, legend_title=lt, quiet=TRUE)

      p_fill <- in.fill == "hues"
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_horiz <- input$myHoriz == FALSE
      p_sort <- input$mySort == "0"
      p_100 <- input$my100 == FALSE
      p_beside <- input$myBeside == FALSE
      p_values <- input$myValues == "%"
      p_values_color <- input$myValuesColor == "white"
      p_values_position <- input$myValuesPos == "in"
      p_values_size <- input$myValuesSize == 0.9
      p_stat <- is.null(in.stat)
      
      txt <- ifelse (input$do_y, ", y=", "")
      out <- paste("BarChart(", x.name, txt, y.name, sep="")
      
      txt <- ifelse (input$do_by, ", by=", "")
      out <- paste(out, txt, by.name, sep="")

      if (!p_fill) out <- paste(out, ", fill=\"", in.fill, "\"", sep="")
      if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
      if (!p_trans) out <- paste(out, ", transparency=", input$myTrans, sep="")
      if (!p_horiz) out <- paste(out, ", horiz=", input$myHoriz, sep="")
      if (!p_sort) out <- paste(out, ", sort=\"", input$mySort, "\"", sep="")
      if (!p_100) out <- paste(out, ", stack100=", input$my100, sep="")
      if (!p_beside) out <- paste(out, ", beside=", input$myBeside, sep="")
      if (!p_values) out <- paste(out, ", values=\"", input$myValues, "\"",
                                  sep="")
      if (!p_values_color) out <- paste(out, ", values_color=\"", 
                                        input$myValuesColor, "\"", sep="")
      if (!p_values_position) out <- paste(out, ", values_position=\"", 
                                        input$myValuesPos, "\"", sep="")
      if (!p_values_size) out <- paste(out, ", value_size=", 
                                       input$myValuesSize, sep="")
      if (!p_stat) out <- paste(out, ", stat=\"", in.stat, "\"", sep="")

      out <- paste(out, ")", sep="")
      cat(out)
      v$code <- out  # save the code for a pdf file
  })  # end renderPlot

  # print stats
  output$summary <- renderPrint({

    x.name <- input$x.col
    shiny::req(x.name)
    shiny::req(v$b)

    x <- data()[, x.name]
    unq.x <- length(unique(x))
    smry.tbl <- ifelse (unq.x==length(x) && ncol(data())==2, TRUE, FALSE)
    shiny::req(!smry.tbl)  # otherwise a summary table, no stats to do
    
    b <- v$b
    if (!input$do_y) {
      out2 <- c(b$out_miss, " ", b$out_count, " ", b$out_chi)
      if (input$my100) out2 <- c(out2, " ", b$out_col)
     }
     else {
       y.name <- input$y.col
       shiny::req(y.name)
       cat("Summary Statistics\n", "------------------\n", sep="")
       pv <- pivot(data(), c(mean, sd, min, median, max), y.name, by=x.name)
       print(pv, print.gap=2)
       out2 <- b$out_y
     }
     cat("\n"); for (i in 1:length(out2)) cat(out2[i], "\n")
  })


  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- v$code

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
    unq.x <- length(unique(x))
    smry.tbl <- ifelse (unq.x==length(x) && ncol(data())==2, TRUE, FALSE)

    y <- NULL
    in.stat <- NULL
    y.name <- ""
    if (input$do_y) {
      y.name <- input$y.col
      shiny::req(y.name)
      y <- data()[, y.name]
      in.stat <- input$statType
    }

    if (nchar(y.name) == 0) 
      axis.name <- paste("Count of", x.name)
    else
      axis.name <- get.ax.nm(y.name, in.stat, smry.tbl)

    in.fill <- input$myFill
    by.name <- ""

    by <- NULL
    lt <- NULL  # legend_title
    if (input$do_by) {  # a by variable
      by.name <- input$by.col
      shiny::req(by.name)
      by <- data()[, by.name]
      in.fill <- input$myFill2  # color range selection
      lt <- by.name
    }

    pdf.fname <- paste("bc_", x.name, by.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)

    # styles before re-set in interact() were saved
    style(lab_cex=getOption("l.cex"))
    style(axis_cex=getOption("l.axc"))

    BarChart(x, y, by=by, data=NULL, stat=in.stat,
            fill=in.fill, color=input$myColor, transparency=input$myTrans,
            sort=input$mySort, horiz=input$myHoriz,
            stack100=input$my100, beside=input$myBeside,
            values=input$myValues, values_color=input$myValuesColor,
            values_size=input$myValuesSize, values_position=input$myValuesPos,
            xlab=x.name, ylab=axis.name, legend_title=lt, quiet=TRUE,
            pdf_file=pdf.path,
            width=as.numeric(input$w), height=as.numeric(input$h))

    # reset back to shiny setting
    style(lab_cex=1.201, axis_cex=1.011, suggest=FALSE)

    # R code
    r.fname <- paste("bc_", x.name, by.name, ".r", sep="")
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
          "# Can replace PATHtoFILE in the following with the path\n",
          "# Remove the # sign in the first column and delete the previous ",
          "Read()\n", sep="", file=r.path, append=TRUE)
      read.path <- file.path("PATHtoFILE", read.path) 
      read.code <- paste("# d <- Read(\"", read.path, "\")", sep="")
    }
    cat(read.code, "\n\n", file=r.path, append=TRUE)

    if (input$do_cmt)
      cat("# When you have your data table, do the bar chart analysis of a\n",
          "#   categorical variable in the data table\n",
          "# d is the default data frame name, so no need to specify\n",
          sep="", file=r.path, append=TRUE)
    cat(code, "\n\n", file=r.path, append=TRUE)

    anlys <- "BarChart()"
    if (input$do_cmt)
      cat("# If accessing data with a name other than d, must add  data=NAME\n",
          paste("#   to the", anlys, "call, where NAME is the name of your",
          "data frame"), "\n", sep="", file=r.path, append=TRUE)
  })
  output$saved_plot <- renderPlot({ plotInput() })


  # access web page help file
  output$help <- eventReactive(input$do_help, {
    shiny::req(input$do_help)
    fp <- system.file("shiny_apps/help/BarChart.html", package="lessR")
    browseURL(fp)
  })

}  # end server

shinyApp(ui, server)

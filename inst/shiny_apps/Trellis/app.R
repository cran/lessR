# -------
# Trellis
# -------

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


    tabPanel("VBS Plots",
      pageWithSidebar(
        headerPanel(div("ViolinBoxScatter Plot", class="hp")),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),

          checkboxInput("do_by1", "add a by1 variable", FALSE),
          conditionalPanel(condition="input.do_by1 == true",
            selectInput('by1.col', 'by1 Variable', "", selected="")
          ),

          tags$hr(),
          h4(div("Point Colors, Size, & Shape", class="head")),
          checkboxInput("do_geom", "view options", FALSE),
          conditionalPanel(condition="input.do_geom == true",
            selectInput("myFill", "fill", choices=clr.one),
            selectInput("myColor", label="color", choices=clr.edge),
            sliderInput("myTrans", label="trans", min=0, max=1, value=0),
            sliderInput("mySize", "size", min=0.0, max=5, value=1.25, step=0.25),
            sliderInput("myOutSize", "out_size", min=0.5, max=5, value=1.25),
            selectInput("myShape", "shape",
              choices=list("circle", "square", "diamond", "triup", "tridown")),
          ),

          tags$hr(),
          h4(div("Object Colors, Properties", class="head")),
          checkboxInput("do_obj", "view options", FALSE),
          conditionalPanel(condition="input.do_obj == true",
            selectInput("myVBS", "vbs_plot",
                        choices=list("vbs", "vb", "vs", "v", "bs", "b", "s")),
            selectInput("myViolin", "violin fill", choices=list(
                        "Constant"=c("#7485975A", clr.one[2:length(clr.one)]),
                        "Qualitative"=list("hues"), "Sequential"=clr.qual)),
            selectInput("myBox", "box fill",
             choices=list("Qualitative"=list("hues"),
                          "Sequential"=clr.qual, "Constant"=clr.one)),
            checkboxInput("myMean", "vbs_mean", value=FALSE),
            checkboxInput("myFences", "fences", value=FALSE),
          ),

          tags$hr(),
          h4(div("Jitter Points", class="head")),
          checkboxInput("do_jitter", "view options", FALSE),
          conditionalPanel(condition="input.do_jitter == true",
            sliderInput("myJity", "jitter_y", min=0, max=4, value=0.01,
                        step=0.10),
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
        d <- read.xlsx(myPath)
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

    updateSelectInput(session, inputId="x.col", label="x variable",
                      choices=c("Select a numerical variable" = "",
                              names(d)[sapply(d, is.numeric)]))
    
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


  observeEvent(input$do_by1, {

    d <- data()
    is.cat <- sapply(d, function(x) {
      lu.x <- length(unique(x))
      x.char <- (is.character(x) || is.factor(x) || lu.x<11) && (lu.x<nrow(d))
    })

    if (all(!is.cat)) {
      message("A  by1  variable partitions values of a categorical variable.\n",
              "Categorical variables have non-numeric values or, if numeric,\n",
              "  defined here as 10 or fewer unique values.\n\n",
              "There are no categorical variables in this data set.")
      stopApp()
    }

    the.cats <- names(d)[is.cat]
    shiny::updateSelectInput(session, inputId="by1.col", label="by1 variable",
                        choices=c("Select a categorical variable"="", the.cats))
  })

 

# ------------ The VBS Plot ------------
# --------------------------------------

  output$myPlot <- renderPlot({

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]

    by1 <- NULL
    lt <- NULL  # legend_title
    by1.name <- ""
    if (input$do_by1) {  # a by variable
      by1.name <- input$by1.col
      shiny::req(by1.name)
      by1 <- data()[, by1.name]
      lt <- by1.name
    }

     vals$p <- Plot(x, data=NULL, by1=by1,
          fill=input$myFill, color=input$myColor, trans=input$myTrans,
          size=input$mySize, shape=input$myShape,
          vbs_plot=input$myVBS, vbs_mean=input$myMean,
          fences=input$myFences, out_size=input$myOutSize,
          jitter_y=input$myJity,
          violin_fill=input$myViolin, box_fill=input$myBox,
          xlab=x.name, ylab=by1.name, quiet=TRUE)

      p_fill <- input$myFill == "#324E5C"
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_size <- input$mySize == 1.25
      p_shape <- input$myShape == "circle"
      p_VBS <- input$myVBS == "vbs" 
      p_mean <- input$myMean == FALSE
      p_fences <- input$myFences == FALSE 
      p_out_size <- input$myOutSize == 1.25 
      p_jity <- input$myJity == 0.01
      p_vfill <- input$myViolin == "#7485975A"
      p_bfill <- input$myBox == "#419BD2"

      if (input$do_by1)
        out <- paste("Plot(", x.name, ", by1=", by1.name, sep="")
      else
        out <- paste("Plot(", x.name, sep="")

      if (!p_fill) out <- paste(out, ", fill=\"", input$myFill, "\"", sep="")
      if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
      if (!p_trans) out <- paste(out, ", trans=", input$myTrans, sep="")
      if (!p_size) out <- paste(out, ", size=", input$mySize, sep="")
      if (!p_shape) out <- paste(out, ", shape=\"", input$myShape, "\"", sep="")
      if (!p_VBS) out <- paste(out, ", vbs_plot=\"", input$myVBS, "\"", sep="")
      if (!p_mean) out <- paste(out, ", vbs_mean=", input$myMean, sep="")
      if (!p_fences) out <- paste(out, ", fences=", input$myFences, sep="")
      if (!p_out_size) out <- paste(out, ", out_size=", input$myOutSize, sep="")
      if (!p_jity) out <- paste(out, ", jitter_y=", input$myJity, sep="")

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


    by1 <- NULL
    lt <- NULL  # legend_title
    by1.name <- ""
    if (input$do_by1) {  # a by variable
      by1.name <- input$by1.col
      shiny::req(by1.name)
      by1 <- data()[, by1.name]
      lt <- by1.name
    }

    pdf.fname <- paste("plot_", x.name, by1.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)

    Plot(x, data=NULL, by1=by1,
        fill=input$myFill, color=input$myColor, trans=input$myTrans,
        size=input$mySize, shape=input$myShape,
        vbs_plot=input$myVBS, vbs_mean=input$myMean,
        fences=input$myFences, out_size=input$myOutSize,
        jitter_y=input$myJity,
        violin_fill=input$myViolin, box_fill=input$myBox,
        xlab=x.name, ylab=by1.name, quiet=TRUE,
        pdf_file=pdf.path,
        width=as.numeric(input$w), height=as.numeric(input$h))

    # R code
    r.fname <- paste("plot_", x.name, by1.name, ".r", sep="")
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

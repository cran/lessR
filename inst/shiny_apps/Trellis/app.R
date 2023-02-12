# -------
# Trellis
# -------

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


    tabPanel("VBS Plots",
      pageWithSidebar(
        titlePanel(""),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),

          checkboxInput("do_by1", div("by1 variable", class="view"), FALSE),
          conditionalPanel(condition="input.do_by1 == true",
            selectInput('by1.col', 'by1 Variable', "", selected="")
          ),

          tags$hr(),
          checkboxInput("do_geom", div("Points", class="view"), FALSE),
          conditionalPanel(condition="input.do_geom == true",
            selectInput("myFill", "fill", choices=clr.one),
            selectInput("myColor", label="color", choices=clr.edge),
            sliderInput("myTrans", "transparency", min=0, max=1, value=0),
            sliderInput("mySize", "size", min=0.0, max=5, value=1.25, step=0.25),
            selectInput("myShape", "shape",
              choices=list("circle", "square", "diamond", "triup", "tridown")),
            sliderInput("myOutSize", "out_size", min=0.5, max=5, value=1.25),
          ),

          tags$hr(),
          checkboxInput("do_obj", div("Objects", class="view"), FALSE),
          conditionalPanel(condition="input.do_obj == true",
            selectInput("myVBS", "vbs_plot",
                        choices=list("vbs", "vb", "vs", "v", "bs", "b", "s")),
            selectInput("myViolin", "violin fill", choices=list(
                        "Constant"=c("#7485975A", clr.one[2:length(clr.one)]),
                        "Qualitative"=clr.qual, "Sequential"=clr.seq)),
            selectInput("myBox", "box fill",
             choices=list("Qualitative"=clr.qual, "Sequential"=clr.seq, 
                          "Constant"=clr.one)),
            checkboxInput("myMean", "vbs_mean", value=FALSE),
            checkboxInput("myFences", "fences", value=FALSE),
          ),

          tags$hr(),
          checkboxInput("do_jitter", div("Jitter", class="view"), FALSE),
          conditionalPanel(condition="input.do_jitter == true",
            sliderInput("myJity", "jitter_y", min=0, max=4, value=0.01,
                        step=0.10),
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
        plotOutput("myPlot"),
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
                      choices=c("Select a numerical variable" = "",
                              names(d)[sapply(d, is.numeric)]))
    
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
    by1.name <- NULL
    if (input$do_by1) {  # a by variable
      by1.name <- input$by1.col
      shiny::req(by1.name)
      by1 <- data()[, by1.name]
      lt <- by1.name
    }

     v$p <- Plot(x, data=NULL, by1=by1,
          fill=input$myFill, color=input$myColor, transparency=input$myTrans,
          size=input$mySize, shape=input$myShape,
          vbs_plot=input$myVBS, vbs_mean=input$myMean,
          fences=input$myFences, out_size=input$myOutSize,
          jitter_y=input$myJity,
          violin_fill=input$myViolin, box_fill=input$myBox,
          xlab=x.name, ylab=by1.name, quiet=FALSE)

      p_fill <- input$myFill == "#324E5C"
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_size <- input$mySize == 1.25
      p_shape <- input$myShape == "circle"
      p_VBS <- input$myVBS == "vbs" 
      p_mean <- input$myMean == FALSE
      p_fences <- input$myFences == FALSE 
      p_out_size <- input$myOutSize == 1.25 
      p_jity <- input$myJity <= 0.01
      p_vfill <- input$myViolin == "#7485975A"
      p_bfill <- input$myBox == "hues"   # "#419BD2"

      if (input$do_by1)
        out <- paste("Plot(", x.name, ", by1=", by1.name, sep="")
      else
        out <- paste("Plot(", x.name, sep="")

      if (!p_fill) out <- paste(out, ", fill=\"", input$myFill, "\"", sep="")
      if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
      if (!p_trans) out <- paste(out, ", transparency=", input$myTrans, sep="")
      if (!p_size) out <- paste(out, ", size=", input$mySize, sep="")
      if (!p_shape) out <- paste(out, ", shape=\"", input$myShape, "\"", sep="")
      if (!p_VBS) out <- paste(out, ", vbs_plot=\"", input$myVBS, "\"", sep="")
      if (!p_mean) out <- paste(out, ", vbs_mean=", input$myMean, sep="")
      if (!p_fences) out <- paste(out, ", fences=", input$myFences, sep="")
      if (!p_out_size) out <- paste(out, ", out_size=", input$myOutSize, sep="")
      if (!p_jity) out <- paste(out, ", jitter_y=", input$myJity, sep="")
      if (!p_bfill) out <- paste(out, ", box_fill=", input$myBox, sep="")
      if (!p_vfill) out <- paste(out, ", violin_fill=", input$myViolin, sep="")

      out <- paste(out, ")", sep="")
      cat(out, "\n")
      v$code <- out  # save the code for a pdf file
  })  # end renderPlot


  # print stats
  output$summary <- renderPrint({
    shiny::req(v$p)
    p <- v$p
    out2 <- c(p$out_grp, " ", p$out_rep, " ", p$out_parm)
    for (i in 1:length(out2)) cat(out2[i], "\n")
  })


  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- v$code

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
        fill=input$myFill, color=input$myColor, transparency=input$myTrans,
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
      cat("# When you have your data table, do the VBS analysis of a\n",
          "#   continuous variable in the data table\n",
          "# d is the default data frame name, so no need to specify\n",
          sep="", file=r.path, append=TRUE)
    cat(code, "\n\n", file=r.path, append=TRUE)


    anlys <- "Plot()"
    if (input$do_cmt)
      cat("# If accessing data with a name other than d, must add  data=NAME\n",
          paste("#   to the", anlys, "call, where NAME is the name of your",
          "data frame"), "\n", sep="", file=r.path, append=TRUE)

  })
  output$saved_plot <- renderPlot({ plotInput() })


  # access web page help file
  output$help <- eventReactive(input$do_help, {
    shiny::req(input$do_help)
    fp <- system.file("shiny_apps/help/Trellis.html", package="lessR")
    browseURL(fp)
  })

}  # end server

shinyApp(ui, server)

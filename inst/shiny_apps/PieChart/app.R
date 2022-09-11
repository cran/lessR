# ---------
# PieChart1
# ---------

library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=1, main_cex=1.25, suggest=FALSE)

clr.one <- list(
  "#96AAC3", "dodgerblue3", "cornflowerblue", "darkblue", "pink2", "red3",
  "darkred", "darkorange2", "lightcoral", "salmon", "ivory", "wheat3",
  "burlywood4", "sienna", "goldenrod2", "yellow2", "darkseagreen2",
  "springgreen3", "seagreen4", "violetred", "lavender", "thistle3",
  "lightcoral", "magenta3", "mediumorchid",
  "black", "gray45", "gray75", "gray95", "white")

clr.edge <- list("off", "black", "gray50", "gray75", "white", 
  "darkblue", "darkred", "darkgreen", "rosybrown2", "slategray2", "thistle1",
  "coral", "gold", "ivory")

clr.hole <- list("white", "gray75", "gray95", "gray98", "aliceblue", 
  "antiquewhite", "ghostwhite", "whitesmoke", "ivory", "mintcream", 
  "mistyrose", "azure1", "seashell", 
  "beige", "cornsilk", "darkblue", "darkred", "darkgreen")

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


    tabPanel("PieChart",
      pageWithSidebar(
        headerPanel(div("Pie Chart", class="hp")),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),

          tags$hr(),
          h4(div("Colors", class="head")),
          checkboxInput("do_geom", "view options", FALSE),
          conditionalPanel(condition="input.do_geom == true",
            selectInput("myFill", "fill",
              choices=list("Qualitative"=list("hues"), "Sequential"=clr.qual)),
            selectInput("myColor", label="color", choices=clr.edge),
            sliderInput("myTrans", label="trans", min=0, max=1, value=0),
          ),

          tags$hr(),
          h4(div("Hole", class="head")),
          checkboxInput("do_hole", "view options", FALSE),
          conditionalPanel(condition="input.do_hole == true",
            sliderInput("myHole", "hole", min=0, max=1, value=0.65),
            selectInput("myHoleFill", label="hole_fill", choices=clr.hole),
          ),

          tags$hr(),
          h4(div("Values Display", class="head")),
          checkboxInput("do_values", "view options", FALSE),
          conditionalPanel(condition="input.do_values == true",
            selectInput("myValues", "values", choices=list("%", "input", "off")),
            selectInput("myValuesColor", "values_color",
               choices=list("white", "gray", "darkgray", "black", "red", "green3")),
            selectInput("myValuesPos", "values_position",
               choices=list("in", "out")),
            sliderInput("myValuesSize", "values_size",
                        min=0, max=2, value=0.9, step=0.1)
          ),

          tags$hr(),
          h4(div("Save", class="head")),
          checkboxInput("do_pdf", "set up", FALSE),
          conditionalPanel(condition="input.do_pdf == true",
            sliderInput("w", "width (inches):", min=3, max=20, value=8),
            sliderInput("h", "height (inches):", min=3, max=20, value=6),
            checkboxInput("do_cmt", "include comments in R file", TRUE),
            actionButton(inputId="btn_pdf", "Save"),
            tags$p(div("Save pdf file and R code file",
                  style="margin-top:.25em;"))
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

    is.cat <- sapply(d, function(x) {
      lu.x <- length(unique(x))
      x.char <- (is.character(x) || is.factor(x) || lu.x<11) && (lu.x<nrow(d))
    })

    if (all(!is.cat)) {
      message("A pie chart displays the values of a categorical variable.\n",
              "Categorical variables have non-numeric values or, if numeric,\n",
              "  defined here as 10 or fewer unique values.\n\n",
              "There are no categorical variables in this data set.")
      stopApp()
    }
    the.cats <- names(d)[is.cat]

    updateSelectInput(session, inputId="x.col", label="x variable",
                      choices=c("Select a categorical variable" = "",
                              names(d)[is.cat]))
    
    output$nrows <- renderText({paste("Number of rows of data:", nrow(d))})
    output$ncols <- renderText({paste("Number of variables:", ncol(d))})

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


# ------------- The PieChart --------------
# -----------------------------------------

  output$myPlot <- renderPlot({

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
     
   vals$p <- PieChart(x, data=NULL,
        fill=input$myFill, color=input$myColor, trans=input$myTrans,
        hole=input$myHole, hole_fill=input$myHoleFill,
        values=input$myValues, values_color=input$myValuesColor,
        values_position=input$myValuesPos,
        values_size=input$myValuesSize,
        main=x.name, quiet=TRUE)

      p_fill <- input$myFill == "hues"
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_hole <- input$myHole == .65
      p_holefill <- input$myHoleFill == "white"
      p_sort <- input$mySort == "0"
      p_values <- input$myValues == "%"
      p_values_color <- input$myValuesColor == "white"
      p_values_position <- input$myValuesPos == "in"
      p_values_size <- input$myValuesSize == 0.9
      
      out <- paste("PieChart(", x.name, sep="")

      if (!p_fill) out <- paste(out, ", fill=\"", input$myFill, "\"", sep="")
      if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
      if (!p_trans) out <- paste(out, ", trans=", input$myTrans, sep="")
      if (!p_hole) out <- paste(out, ", hole=", input$myHole, sep="")
      if (!p_holefill) out <- paste(out, ", hole_fill=\"", 
                                        input$myHoleFill, "\"", sep="")
      if (!p_values) out <- paste(out, ", values=\"", input$myValues, "\"",
                                  sep="")
      if (!p_values_color) out <- paste(out, ", values_color=\"", 
                                        input$myValuesColor, "\"", sep="")
      if (!p_values_position) out <- paste(out, ", values_position=\"", 
                                        input$myValuesPos, "\"", sep="")
      if (!p_values_size) out <- paste(out, ", values_size=", 
                                       input$myValuesSize, sep="")

      out <- paste(out, ")", sep="")
      cat(out, "\n")
      vals$code <- out  # save the code for a pdf file
  })  # end renderPlot

  # print stats
  output$summary <- renderPrint({
    shiny::req(vals$p)
    p <- vals$p
    out2 <- c(p$out_txttl, " ", p$out_counts, " ", p$out_chi)
    for (i in 1:length(out2)) cat(out2[i], "\n")
  })

  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- vals$code

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
    y.name <- paste("Count of", x.name)

    pdf.fname <- paste("pc_", x.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)

   PieChart(x, data=NULL,
        fill=input$myFill, color=input$myColor, trans=input$myTrans,
        hole=input$myHole, hole_fill=input$myHoleFill,
        values=input$myValues, values_color=input$myValuesColor,
        values_position=input$myValuesPos,
        values_size=input$myValuesSize,
        main=x.name, quiet=TRUE,
        pdf_file=pdf.path,
        width=as.numeric(input$w), height=as.numeric(input$h))

    # R code
    r.fname <- paste("pc_", x.name, ".r", sep="")
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
    txt <- ifelse (input$myRows, ", row_names=1", "")
    read.code <- paste("d <- Read(\"", read.path, "\"", txt, ")", sep="")
    is.local <- !grepl("http://", read.path, fixed=TRUE)
    if (is.local && input$do_cmt) {
      read.path <- file.path("PATHtoFILE", read.path) 
      read.code <- paste("d <- Read(\"", read.path, "\"", txt, ")", sep="")
      cat("# For security, the path to your data file is",
          "not available here\n", file=r.path, append=TRUE)
      cat("# Either browse for the data file, with nothing",
          "between the quotes,\n", file=r.path, append=TRUE)
      cat("# or replace PATHtoFILE with the actual path\n\n",
          file=r.path, append=TRUE)
    }
    cat("d <- Read(\"\")\n", file=r.path, append=(is.local && input$do_cmt))
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

# ---------
# Histogram
# ---------

library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=1, suggest=FALSE)

clr.one <- list(
  "#96AAC3", "dodgerblue3", "cornflowerblue", "darkblue", "pink2", "red3",
  "darkred", "darkorange2", "lightcoral", "salmon", "ivory", "wheat3",
  "burlywood4", "sienna", "goldenrod2", "yellow2", "darkseagreen2",
  "springgreen3", "seagreen4", "violetred", "lavender", "thistle3",
  "lightcoral", "magenta3", "mediumorchid",
  "black", "gray45", "gray75", "gray95", "white")

clr.den_g <- c("steelblue3", clr.one[2:length(clr.one)])
clr.den_n <- c("pink1", clr.one[2:length(clr.one)])

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


    tabPanel("Histogram",
      pageWithSidebar(
        headerPanel(div("Histogram", class="hp")),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),

        uiOutput("slider_bw"),
        uiOutput("slider_bs"),

        tags$hr(),
        h4(div("Colors", class="head")),
        checkboxInput(inputId="myGeom", label="view options", value=FALSE),
        conditionalPanel(condition="input.myGeom == true",
          selectInput("myFill", "fill",
             choices=list("Constant"=clr.one, "Qualitative"=list("hues"),
                          "Sequential"=clr.qual)),
          selectInput("myColor", "color", choices=clr.edge),
          sliderInput("myTrans", "trans", min=0, max=1, value=0)
        ),

        tags$hr(),
        h4(div("Values, Cumulate", class="head")),
        checkboxInput(inputId="myValCm", label="view options", value=FALSE),
        conditionalPanel(condition="input.myValCm == true",
          checkboxInput("myValues", "display values", value=FALSE),
          selectInput("myCumlt", "cumulate", choices=list("off", "on", "both"))
        ),

        tags$hr(),
        h4(div("Smooth", class="head")),
        checkboxInput(inputId="myDens", label="view options", value=FALSE),
        conditionalPanel(condition="input.myDens == true",
          checkboxInput(inputId="myHist", label="show_histogram", value=TRUE),
          checkboxInput(inputId="myRug", label="rug", value=FALSE),
          radioButtons("myType", "type",
                     c("general"="general", "normal"="normal", "both"="both")),
          uiOutput("slider_bndwd"),
          selectInput("myFill_gen", "fill_general", choices=clr.den_g),
          selectInput("myFill_nrm", "fill_normal", choices=clr.den_n)
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

    updateSelectInput(session, inputId="x.col", label="x variable",
                      choices=c("Select a numerical variable" = "",
                              names(d)[sapply(d, is.numeric)]))
    
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



# ------- Bin Width and Bin Start ---------
# -----------------------------------------

  # get default bin_width and bin_start values for input sliders
  observeEvent(input$x.col, {  # processed while still on Get Data tab

    x.name <- input$x.col
    x <- data()[, x.name]

    vals$min.x <- min(x, na.rm=TRUE)
    max.x <- max(x, na.rm=TRUE)

    h <- suppressWarnings(hist(x, plot=FALSE, breaks="Sturges"))
    vals$bw_def <- h$breaks[2]-h$breaks[1]
    if (vals$bw_def == 0.5) vals$bw_def <- 1
    vals$rng <- max.x - vals$min.x
    vals$bw1 <- vals$rng/60
    if (vals$min.x > 1) vals$bw1 <- floor(vals$bw1)
    if (vals$bw1 == 0) vals$bw1 <- 0.5 
    vals$bw2 <- vals$rng/2.1
    if (vals$bw2 > 5) vals$bw2 <- ceiling(vals$bw2)
    vals$bw1 <- round(vals$bw1, 3)
    vals$bw2 <- round(vals$bw2, 3)

    pret <- pretty(c((vals$min.x-(.01*vals$min.x)), max.x))[1]
    vals$bs1 <- pret - (vals$bw_def)
    vals$bs_def <- pret
    vals$bs2 <- vals$min.x
    if(vals$bs1 > vals$bs2) vals$bs2 <- vals$bs1
    if (vals$min.x > 1) {
      vals$bs1 <- floor(vals$bs1)
      vals$bs2 <- floor(vals$bs2)
    }
  })
  
  # bin_width slider, only activates for a variable change
  output$slider_bw <- renderUI({
    if (!is.null(vals$bw1)) {
      sld <- sliderInput(inputId="slider_bw", label="bin_width",
                         min=vals$bw1, max=vals$bw2, value=vals$bw_def)
    }
  })

  # bin_start slider, only activates for a variable change
  output$slider_bs <- renderUI({
    if (!is.null(vals$bs2))
      sliderInput(inputId="slider_bs", label="bin_start",
                  min=vals$bs1, max=vals$bs2, value=vals$bs_def)
  })
  

# ---------- Density Bandwidth ------------
# -----------------------------------------
  get_bw <- function(x) {
      bw <- bw.nrd0(x)
      irep <- 0
      repeat {  # iterated value of bw
        irep <- irep + 1
        d.gen <- suppressWarnings(density(x, bw))  # no missing data
        xd <- diff(d.gen$y)
        flip <- 0
        for (j in 2:length(xd))
          if (sign(xd[j-1]) != sign(xd[j])) flip <- flip + 1
        if (flip > 1  &&  irep <= 25)
          bw <- 1.1 * bw
        else
          break;
      }  # end repeat

      vals$bndwd_def <- bw  # cutoff of 7 to keep bw*.15 > 1
      vals$bndwd1 <- ifelse (bw>7, floor(bw*0.15), round(bw*0.15, 2))
      if (vals$bndwd1 == 0) vals$bndwd1 <- 0.00001
      vals$bndwd2 <- ifelse (bw>7, ceiling(bw*1.5), round(bw*1.5, 2))
  }

  observeEvent(input$x.col, {  # if switch variable
    if (input$myDens) { 
      x.name <- input$x.col
      shiny::req(x.name)
      x <- na.omit(data()[, x.name])
      get_bw(x)

      sld <- sliderInput(inputId="slider_bndwd", label="bandwidth",
                         min=vals$bndwd1, max=vals$bndwd2, value=vals$bndwd_def)
    }
  })

  # get default band width and min, max for input slider when
  #  "view options" button is checked
  observeEvent(input$myDens, {
    if (input$myDens) {
      x.name <- input$x.col
      shiny::req(x.name)
      x <- na.omit(data()[, x.name])
      get_bw(x)
    }
  })

  # band width slider, only activates for a variable change
  output$slider_bndwd <- renderUI({
    if (!is.null(vals$bndwd_def)) {
      sld <- sliderInput(inputId="slider_bndwd", label="bandwidth",
                         min=vals$bndwd1, max=vals$bndwd2, value=vals$bndwd_def)
    }
  })


# ------------ The Histogram --------------
# -----------------------------------------

  output$myPlot <- renderPlot({

    nbins <- ifelse (!is.null(input$slider_bw), vals$rng/input$slider_bw, 9999)
    # two renderPlot calls before a slider value is obtained
    go <- TRUE
    if (is.null(input$slider_bw)) go <- FALSE
    if (is.null(vals$bw_def)) go <- FALSE
    if (!is.null(input$slider_bs))
      if (input$slider_bs > vals$min.x) go <- FALSE
    if (nbins > 150) go <- FALSE
    if (nbins < 2) go <- FALSE
#   if (input$do_pdf) go <- FALSE  # leaves a blank plot

    if (go) {
      x.name <- input$x.col
      shiny::req(x.name)
      x <- data()[, x.name]
      y.name <- paste("Count of", x.name)

      out <- paste("Histogram(", x.name, sep="")

      if (!input$myDens) {
        vals$h <- Histogram(x, data=NULL,
               bin_width=input$slider_bw, bin_start=input$slider_bs,
               fill=input$myFill, color=input$myColor, trans=input$myTrans,
               values=input$myValues, cumulate=input$myCumlt,
               xlab=x.name, ylab=y.name,
               quiet=TRUE)
      
        p_bin_width <- input$slider_bw == vals$bw_def
        p_bin_start <- input$slider_bs == vals$bs_def 
        p_fill <- input$myFill == "#96AAC3"
        p_color <- input$myColor == "off"
        p_trans <- input$myTrans == 0
        p_values <- input$myValues == FALSE
        p_cumul <- input$myCumlt == "off"

  if (!p_bin_width) out <- paste(out, ", bin_width=",input$slider_bw, sep="")
  if (!p_bin_start) out <- paste(out, ", bin_start=",input$slider_bs, sep="")
  if (!p_fill) out <- paste(out, ", fill=\"", input$myFill, "\"", sep="")
  if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
  if (!p_trans) out <- paste(out, ", trans=", input$myTrans, sep="")
  if (!p_values) out <- paste(out, ", values=", input$myValues, sep="")
  if (!p_cumul) out <- paste(out, ", cumulate=\"", input$myCumlt, "\"", sep="")
      }

    else {  # density plot
        shiny::req(input$slider_bndwd)
  
        fg.rgb <- col2rgb(input$myFill_gen) 
        vals$fg.trns <- rgb(fg.rgb[1], fg.rgb[2], fg.rgb[3],
                       alpha=80, maxColorValue=255)
        fn.rgb <- col2rgb(input$myFill_nrm) 
        vals$fn.trns <- rgb(fn.rgb[1], fn.rgb[2], fn.rgb[3],
                       alpha=80, maxColorValue=255)

        vals$h <- Histogram(x, data=NULL,
               bin_width=input$slider_bw, bin_start=input$slider_bs,
               density=input$myDens, rug=input$myRug, type=input$myType,
               bandwidth=input$slider_bndwd, show_histogram=input$myHist,
               fill_general=vals$fg.trns, fill_normal=vals$fn.trns,
               xlab=x.name, ylab=y.name, quiet=TRUE)

        p_dens <- input$myDens == FALSE
        p_rug <- input$myRug == FALSE
        p_type <- input$myType == "general"
        p_bw <- (abs(input$slider_bndwd-vals$bndwd_def) < 1)
        p_hist <- input$myHist == TRUE
        p_fill_general <- input$myFill_gen == "steelblue3"
        p_fill_normal <- input$myFill_nrm == "pink1"
        if (!p_dens) out <- paste(out, ", density=", input$myDens, sep="")

        if (!p_rug) out <- paste(out, ", rug=", input$myRug, sep="")
        if (!p_type) out <- paste(out, ", type=\"", input$myType, "\"", sep="")
        if (!p_bw) out <- paste(out, ", bandwidth=", input$slider_bndwd, sep="")
        if (!p_hist) out <- paste(out, ", show_histogram=",input$myHist, sep="")
        if (!p_fill_general) out <- paste(out, ", fill_general=\"",
                                          input$myFill_gen, "\"", sep="")
        if (!p_fill_normal) out <- paste(out, ", fill_normal=\"",
                                        input$myFill_nrm, "\"", sep="")
    }  # end dens

      out <- paste(out, ")", sep="")
      cat(out, "\n")
      vals$code <- out  # save the code for a pdf file

    }  # end go
  })  # end renderPlot


  # print stats
  output$summary <- renderPrint({

    shiny::req(vals$h)
    h <- vals$h

    if (!is.null(input$slider_bs)) if (input$slider_bs < vals$min.x) {
      if (!input$myDens)
        out2 <- c(h$out_summary, " ", h$out_outliers, " ", h$out_freq)
      else
        out2 <- c(h$out_stats, " ", h$out_ss, " ", h$out_outliers)
      for (i in 1:length(out2)) cat(out2[i], "\n")
    }
  })

  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- vals$code

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
    y.name <- paste("Count of", x.name)

    pdf.fname <- paste("hs_", x.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)


    if (!input$myDens)
      Histogram(x, data=NULL,
             bin_width=input$slider_bw, bin_start=input$slider_bs,
             fill=input$myFill, color=input$myColor, trans=input$myTrans,
             values=input$myValues, cumulate=input$myCumlt,
             xlab=x.name, ylab=y.name, quiet=TRUE,
             pdf_file=pdf.path,
             width=as.numeric(input$w), height=as.numeric(input$h))
    else  # density
      Histogram(x, data=NULL,
             bin_width=input$slider_bw, bin_start=input$slider_bs,
             density=input$myDens, rug=input$myRug, type=input$myType,
             bandwidth=input$slider_bndwd, show_histogram=input$myHist,
             fill_general=vals$fg.trns, fill_normal=vals$fn.trns,
             xlab=x.name, ylab=y.name, quiet=TRUE,
             pdf_file=pdf.path,
             width=as.numeric(input$w), height=as.numeric(input$h))

    # R code
    r.fname <- paste("hs_", x.name, ".r", sep="")
    r.path <- file.path(path.expand("~"), r.fname)
    cat("\n")
    message("---------------------------------------------")
    cat("Files written to folder:", path.expand("~"), "\n")
    message("---------------------------------------------")
    cat("pdf file: ", pdf.fname, "\n")
    cat("R code file: ", r.fname, "\n")
    message("---------------------------------------------")
    cat("\n")


    if (input$do_cmt)
      cat("# The first line of any R/lessR session is always\n",
          "library(\"lessR\")\n# Next read your data into R, then analysis\n\n",
          file=r.path)
    read.path <- vals$theRead
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

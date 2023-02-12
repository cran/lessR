# ---------
# Histogram
# ---------

library(shiny)
library(lessR)

clr.one <- list(
  "#96AAC3", "dodgerblue3", "cornflowerblue", "steelblue", "darkblue",
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

clr.den_g <- c("steelblue3", clr.one[2:length(clr.one)])
clr.den_n <- c("pink1", clr.one[2:length(clr.one)])

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
          uiOutput("d.show"),

        ),  # end sidbarPanel

        mainPanel(
          tableOutput("d.table"),
          tags$style(type="text/css", "#d.table {font-size: .95em;}")
        )

      )  # end sidbarLayout
    ),  # end tabPanel 1


    tabPanel("Histogram",
      pageWithSidebar(
        titlePanel(""),

        sidebarPanel(
          selectInput('x.col', 'x Variable', ""),

        tags$hr(),
        checkboxInput("myBins", div("Bins", class="view"), FALSE),
        conditionalPanel(condition="input.myBins == true",
          uiOutput("slider_bw"),
          uiOutput("slider_bs")
        ),

        tags$hr(),
        checkboxInput("myGeom", div("Colors", class="view"), FALSE),
        conditionalPanel(condition="input.myGeom == true",
          selectInput("myFill", "fill",
             choices=list("Constant"=clr.one, 
                          "Qualitative"=clr.qual,
                          "Sequential"=clr.seq)),
          selectInput("myColor", "color", choices=clr.edge),
          sliderInput("myTrans", "transparency", min=0, max=1, value=0)
        ),

        tags$hr(),
        checkboxInput("myValCm", div("Values, Cumulate", class="view"), FALSE),
        conditionalPanel(condition="input.myValCm == true",
          checkboxInput("myValues", "values", value=FALSE),
          selectInput("myCumlt", "cumulate", choices=list("off", "on", "both"))
        ),

        tags$hr(),
        checkboxInput("mySmooth", div("Smooth", class="view"), FALSE),
        conditionalPanel(condition="input.mySmooth == true",
          checkboxInput("myDens", "density", TRUE),
          checkboxInput("myHist", "show_histogram", TRUE),
          checkboxInput("myRug", "rug", FALSE),
          radioButtons("myType", "type",
                     c("general"="general", "normal"="normal", "both"="both")),
          uiOutput("slider_bndwd"),
          selectInput("myFill_gen", "fill_general", choices=clr.den_g),
          selectInput("myFill_nrm", "fill_normal", choices=clr.den_n)
        ),

          tags$hr(),
          checkboxInput("do_pdf", div("Save", class="view"), FALSE),
          conditionalPanel(condition="input.do_pdf == true",
            sliderInput("w", "width (inches):", min=3, max=20, value=8),
            sliderInput("h", "height (inches):", min=3, max=20, value=6),
            checkboxInput("do_cmt", "include comments in R file", TRUE),
            actionButton(inputId="btn_pdf", "Save"),
            tags$p(div("Save pdf file and R code file",
                  style="margin-top:.25em;"))
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

  options(shiny.maxRequestSize=50*1024^2)  # max upload file size is 50MB

  v <- reactiveValues()
  v$x.new <- FALSE


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

# -----------------------------------------


# ------- Bin Width and Bin Start ---------
# -----------------------------------------

  # get default bin_width and bin_start values for initial histogram
  #  and bin width and start sliders
  # default bin width is v$bw
  observeEvent(input$x.col, {  # processed while still on Get Data tab
    v$x.new <- TRUE

    x.name <- input$x.col
    x <- data()[, x.name]

    v$min.x <- min(x, na.rm=TRUE)
    max.x <- max(x, na.rm=TRUE)

    h <- suppressWarnings(hist(x, plot=FALSE, breaks="Sturges"))
    v$bw <- h$breaks[2]-h$breaks[1]
    if (v$bw == 0.5) v$bw <- 1
    v$rng <- max.x - v$min.x
    v$bw1 <- v$rng/45
    if (v$min.x > 1) v$bw1 <- floor(v$bw1)
    if (v$bw1 == 0) v$bw1 <- 0.5 
    v$bw2 <- v$rng/2.5
    if (v$bw2 > 5) v$bw2 <- ceiling(v$bw2)
    v$bw1 <- round(v$bw1, 3)
    v$bw2 <- round(v$bw2, 3)

    pret <- pretty(c((v$min.x-(.01*v$min.x)), max.x))[1]
    v$bs1 <- pret - (v$bw)
    v$bs <- pret
    v$bs2 <- v$min.x
    if(v$bs1 > v$bs2) v$bs2 <- v$bs1
    if (abs(v$min.x) > 1) {
      v$bs1 <- floor(v$bs1)
      v$bs2 <- floor(v$bs2)
    }

    updateSliderInput(inputId="slider_bw", label="bin_width",
                min=NA, max=NA, value=NA)

    updateSliderInput(inputId="slider_bs", label="bin_start",
                min=NA, max=NA, value=NA)
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

      v$bndwd <- bw  # cutoff of 7 to keep bw*.15 > 1
      v$bndwd1 <- ifelse (bw>7, floor(bw*0.15), round(bw*0.15, 2))
      if (v$bndwd1 == 0) v$bndwd1 <- 0.00001
      v$bndwd2 <- ifelse (bw>7, ceiling(bw*1.5), round(bw*1.5, 2))
  }

  observeEvent(input$x.col, {  # if switch variable
    if (input$myDens) { 
      x.name <- input$x.col
      shiny::req(x.name)
      x <- na.omit(data()[, x.name])
      get_bw(x)

      sliderInput(inputId="slider_bndwd", label="bandwidth",
                  min=v$bndwd1, max=v$bndwd2, value=v$bndwd)
    }
  })

  # get default band width and min, max for input slider when
  #  "parameters" button is checked
  observeEvent(input$myDens, {
    if (input$myDens) {
      x.name <- input$x.col
      shiny::req(x.name)
      x <- na.omit(data()[, x.name])
      get_bw(x)
    }
  })

  # band width slider, only activates for a variable change
  # runs whenever an input$ in the function changes
  output$slider_bndwd <- renderUI({
    if (!is.null(v$bndwd)) {
      sliderInput(inputId="slider_bndwd", label="bandwidth",
                  min=v$bndwd1, max=v$bndwd2, value=v$bndwd)
    }
  })


# ------------ The Histogram --------------
# -----------------------------------------

  output$myPlot <- renderPlot({
    if (input$myBins) {
      if (is.null(input$slider_bw)) {
    # bin_width slider, only activates for a variable change
    output$slider_bw <- renderUI({
      req(!is.null(v$bw1))
      sliderInput(inputId="slider_bw", label="bin_width",
                  min=v$bw1, max=v$bw2, value=v$bw)
    })
      }

    # bin_start slider, only activates for a variable change
      if (is.null(input$slider_bs)) {
    output$slider_bs <- renderUI({
      req(!is.null(v$bs1))
      sliderInput(inputId="slider_bs", label="bin_start",
                  min=v$bs1, max=v$bs2, value=v$bs)
    })
      }
    }

    # the sliders update later, so the old value can be current for a new var
    # update to avoid the extra computation and distracting intermediate plot
    if (!is.null(input$slider_bw)) {
      if (input$slider_bw < v$bw1  || input$slider_bw > v$bw2) {
        if (!input$myBins)
          updateSliderInput(session,"slider_bw", min=v$bw1, max=v$bw2,
                            value=v$bw)
      req(input$slider_bw >= v$bw1)  # takes a while for the update
      req(input$slider_bw <= v$bw2)
      }
    }
    if (!is.null(input$slider_bs)) {
      if (input$slider_bs < v$bs1  || input$slider_bs > v$bs2) {
        if (!input$myBins) 
          updateSliderInput(session,"slider_bs", min=v$bs1, max=v$bs2,
                            value=v$bs)
      req(input$slider_bs >= v$bs1)  # takes a while for the update
      req(input$slider_bs <= v$bs2)
      }
    }

    # switching to a new var, v$x.new is TRUE, initiates the histogram
    #   computations twice, so skip the first histogram and take the second
    go.new <- v$x.new
    if (go.new) {
      v$x.new <- FALSE
      req(!go.new) 
    }

    # when Bins button clicked do not want any new re-renderings, but get two
    # this hack stops the first re-rendering for the first instance
    # slider values are only null before any click on the Bins button
    if (input$myBins) req(!is.null(input$slider_bw))

    shiny::req(input$x.col)
    x.name <- input$x.col
    x <- data()[, x.name]
    y.name <- paste("Count of", x.name)

    # when beginning, sliders will be NULL
    in.bw <- ifelse (is.null(input$slider_bw), v$bw, input$slider_bw)
    in.bs <- ifelse (is.null(input$slider_bs), v$bs, input$slider_bs)
    v$in.den <- ifelse (input$mySmooth, TRUE, FALSE)

    out <- paste("Histogram(", x.name, sep="")

    if (!v$in.den) {

      v$h <- Histogram(x, data=NULL,
             bin_width=in.bw, bin_start=in.bs, bin_end=NULL,
             fill=input$myFill, color=input$myColor, transparency=input$myTrans,
             values=input$myValues, cumulate=input$myCumlt,
             xlab=x.name, ylab=y.name,
             quiet=TRUE)
    
      p_bin_width <- in.bw == v$bw
      p_bin_start <- in.bs == v$bs 
      p_fill <- input$myFill == "#96AAC3"
      p_color <- input$myColor == "off"
      p_trans <- input$myTrans == 0
      p_values <- input$myValues == FALSE
      p_cumul <- input$myCumlt == "off"

  if (!p_bin_width) out <- paste(out, ", bin_width=", in.bw, sep="")
  if (!p_bin_start) out <- paste(out, ", bin_start=", in.bs, sep="")
  if (!p_fill) out <- paste(out, ", fill=\"", input$myFill, "\"", sep="")
  if (!p_color) out <- paste(out, ", color=\"", input$myColor, "\"", sep="")
  if (!p_trans) out <- paste(out, ", transparency=", input$myTrans, sep="")
  if (!p_values) out <- paste(out, ", values=", input$myValues, sep="")
  if (!p_cumul) out <- paste(out, ", cumulate=\"", input$myCumlt, "\"", sep="")
    }

    else {  # density plot
      shiny::req(input$slider_bndwd)

      fg.rgb <- col2rgb(input$myFill_gen) 
      v$fg.trns <- rgb(fg.rgb[1], fg.rgb[2], fg.rgb[3],
                     alpha=80, maxColorValue=255)
      fn.rgb <- col2rgb(input$myFill_nrm) 
      v$fn.trns <- rgb(fn.rgb[1], fn.rgb[2], fn.rgb[3],
                     alpha=80, maxColorValue=255)

      v$h <- Histogram(x, data=NULL,
             bin_width=in.bw, bin_start=in.bs,
             density=input$myDens, rug=input$myRug, type=input$myType,
             bandwidth=input$slider_bndwd, show_histogram=input$myHist,
             fill_general=v$fg.trns, fill_normal=v$fn.trns,
             xlab=x.name, ylab=y.name, quiet=TRUE)

      p_dens <- input$myDens == FALSE
      p_rug <- input$myRug == FALSE
      p_type <- input$myType == "general"
      p_bw <- (abs(input$slider_bndwd-v$bndwd) < 1)
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
    v$code <- out  # save the code for a pdf file


    # print stats
    output$summary <- renderPrint({

    shiny::req(v$h)
    h <- v$h

#   v$go <- TRUE
#   if (v$go) { 
      if (!v$in.den)
        out2 <- c(h$out_summary, " ", h$out_outliers, " ", h$out_freq)
      else
        out2 <- c(h$out_stats, " ", h$out_ss, " ", h$out_outliers)
      for (i in 1:length(out2)) cat(out2[i], "\n")
#   }
  })

  v$x.new <- FALSE
  })  # end renderPlot

  # clicking on the Save button generates a pdf file 
  plotInput <- eventReactive(input$btn_pdf, {

    code <- v$code

    x.name <- input$x.col
    shiny::req(x.name)
    x <- data()[, x.name]
    y.name <- paste("Count of", x.name)

    pdf.fname <- paste("hs_", x.name, ".pdf", sep="")
    pdf.path <- file.path(path.expand("~"), pdf.fname)

    # styles before re-set in interact() were saved
    style(lab_cex=getOption("l.cex"))
    style(axis_cex=getOption("l.axc"))

    if (!v$in.den)
      Histogram(x, data=NULL,
             bin_width=input$slider_bw, bin_start=input$slider_bs,
             fill=input$myFill, color=input$myColor, transparency=input$myTrans,
             values=input$myValues, cumulate=input$myCumlt,
             xlab=x.name, ylab=y.name, quiet=TRUE,
             pdf_file=pdf.path,
             width=as.numeric(input$w), height=as.numeric(input$h))

    else  # density
      Histogram(x, data=NULL,
             bin_width=input$slider_bw, bin_start=input$slider_bs,
             density=input$myDens, rug=input$myRug, type=input$myType,
             bandwidth=input$slider_bndwd, show_histogram=input$myHist,
             fill_general=v$fg.trns, fill_normal=v$fn.trns,
             xlab=x.name, ylab=y.name, quiet=TRUE,
             pdf_file=pdf.path,
             width=as.numeric(input$w), height=as.numeric(input$h))

    # reset back to shiny setting
    style(lab_cex=1.201, axis_cex=1.011, suggest=FALSE)

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
      cat("# When you have your data table, do the histogram analysis of a\n",
          "#   continuous variable in the data table\n",
          "# d is the default data frame name, so no need to specify\n",
          sep="", file=r.path, append=TRUE)
    cat(code, "\n\n", file=r.path, append=TRUE)


    anlys <- "Histogram()"
    if (input$do_cmt)
      cat("# If accessing data with a name other than d, must add  data=NAME\n",
          paste("#   to the", anlys, "call, where NAME is the name of your",
          "data frame"), "\n", sep="", file=r.path, append=TRUE)

  })
  output$saved_plot <- renderPlot({ plotInput() })


  # access web page help file
  output$help <- eventReactive(input$do_help, {
    shiny::req(input$do_help)
    fp <- system.file("shiny_apps/help/Histogram.html", package="lessR")
    browseURL(fp)
  })

}  # end server

shinyApp(ui, server)

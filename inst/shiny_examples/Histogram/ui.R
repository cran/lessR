library(shiny)

ui <- fluidPage(  # Define UI for app that draws a histogram

  titlePanel("Histogram"),  # App title

  sidebarLayout(  # Sidebar layout with input and output definitions

    sidebarPanel(  # Sidebar panel for inputs

      sliderInput(inputId = "myWidth",  # Input: Slider for the number of bins
                  label = "bin_width:",
                  min = 1000, max = 20000, value = 10000
      ),

      sliderInput(inputId = "myStart",
                  label = "bin_start",
                  min = 25000, max = 31000, value = 28000
      ),

      selectInput(inputId = "myFill",
            label = "fill",
            selected = "lightsteelblue3",
            choices = list(
              `Individual Colors` = list(
                "lightsteelblue3", "darkred", "gray45", "gray75",
                "wheat3", "burlywood3", "burlywood4", "slategray3",
                "darkseagreen2", "seagreen4", "#90A5C3",
                "lightblue3", "sienna", "lavender", "lightsalmon",
                "lightcoral", "magenta3", "purple"),
              `Range of Colors` = list(
                "reds", "rusts", "yellows",
                "olives", "greens", "emeralds", "turquoises",
                "aquas", "blues", "purples", "violets",
                "magentas", "grays", "colors")
            )
      ),

      selectInput(inputId = "myColor",
                  label = "color",
                  choices = list("off", "black", "white", "darkgray",
                                 "darkblue", "gold")
      ),

      sliderInput(inputId = "myTrans",  # Input: Slider for bar transparency
                  label = "trans:",
                  min = 0, max = 1, value = 0
      ),

      checkboxInput(inputId = "myDens",
                    label = "density",
                    value = FALSE
      ),

      checkboxInput(inputId = "myRug",
                    label = "rug",
                    value = FALSE
      ),

      checkboxInput(inputId = "myValues",
                    label = "values",
                    value = FALSE
      ),

      selectInput(inputId = "myCumulate",
                  label = "cumulate",
                  choices = list("off", "on", "both")
      ),

    ),

    mainPanel(  # Main panel for displaying outputs
      plotOutput(outputId = "myPlot")  # Output: Histogram
    )
  )
)

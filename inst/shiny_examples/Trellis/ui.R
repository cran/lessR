library(shiny)

ui <- fluidPage(  # Define UI for app that draws a histogram

  titlePanel("Violin, Box, Scatter Plot"),  # App title

  sidebarLayout(  # Sidebar layout with input and output definitions

    sidebarPanel(  # Sidebar panel for inputs

      selectInput(inputId = "myFill",
                  label = "fill",
                  choices = list(
                    "black", "red", "darkred", "slategray3", "magenta3",
                    "darkseagreen2", "sienna", "gold")
      ),

      selectInput(inputId = "myColor",
                  label = "color",
                  choices = list("off",
                                 "black", "red", "darkred", "slategray3", "magenta3",
                                 "darkseagreen2", "sienna", "gold")
      ),

      sliderInput(inputId = "myTrans",
                  label = "trans",
                  min = 0, max = 1, value = 0
      ),

      selectInput(inputId = "myShape",
                  label = "shape",
                  choices = list("circle", "square", "diamond",
                                 "triup", "tridown")
      ),

      sliderInput(inputId = "mySize",
                  label = "size",
                  min = 0.5, max = 10, value = 1.25
      ),

      sliderInput(inputId = "myOutSize",
                  label = "out_size",
                  min = 0.5, max = 10, value = 1.25
      ),

      selectInput(inputId = "myVBS",
                  label = "vbs_plot",
                  choices = list("vbs", "vb", "vs", "v", "bs", "b", "s")
      ),

      checkboxInput(inputId = "myMean",
                    label = "vbs_mean",
                    value = FALSE
      ),

      checkboxInput(inputId = "myFences",
                    label = "fences",
                    value = FALSE
      )
    ),

    mainPanel(  # Main panel for displaying outputs
      plotOutput(outputId = "myPlot")  # Output: Scatterplot
    )

  )
)

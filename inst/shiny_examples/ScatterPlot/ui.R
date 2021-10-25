library(shiny)

ui <- fluidPage(  # Define UI for app that draws a histogram

  titlePanel("Scatterplot"),  # App title

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

      sliderInput(inputId = "mySize",
                  label = "size",
                  min = 0.5, max = 10, value = 1.25
      ),

      selectInput(inputId = "myShape",
                  label = "shape",
                  choices = list("circle", "square", "diamond",
                                 "triup", "tridown")
      ),

      sliderInput(inputId = "myEllipse",  # Input
                  label = "ellipse",
                  min=0, max=0.99, value=0
      ),

      selectInput(inputId = "myFit",
                  label = "fit",
                  choices = list("off", "loess", "lm", "exp", "sqrt",
                                 "reciprocal", "null")
      ),

      checkboxInput(inputId = "myErrors",
                    label = "plot_errors",
                    value = FALSE
      ),

      sliderInput(inputId = "myMDcut",
                  label = "MD_cut",
                  min=0, max=12, value=0
      )
    ),

    mainPanel(  # Main panel for displaying outputs
      plotOutput(outputId = "myPlot")  # Output: Scatterplot
    )

  )
)

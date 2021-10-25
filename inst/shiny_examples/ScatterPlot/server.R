library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=.9)

d <- Read("Employee", quiet=TRUE)

server <- function(input, output) {

  output$myPlot <- renderPlot({

     Plot(Years, Salary,
          fill=input$myFill, color=input$myColor, trans=input$myTrans,
          size=input$mySize, shape=input$myShape, plot_errors=input$myErrors,
          fit=input$myFit, ellipse=input$myEllipse, MD_cut=input$myMDcut,
          quiet=TRUE)
  })

}

library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=.9)

d <- Read("Employee", quiet=TRUE)

server <- function(input, output) {

  output$myPlot <- renderPlot({

     Plot(Salary, by1=Dept,
          fill=input$myFill, color=input$myColor, trans=input$myTrans,
          size=input$mySize, shape=input$myShape, vbs_plot=input$myVBS,
          vbs_mean=input$myMean, fences=input$myFences, out_size=input$myOutSize,
          quiet=TRUE)
  })

}

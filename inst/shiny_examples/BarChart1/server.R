library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=.9)

d <- Read("Employee", quiet=TRUE)

server <- function(input, output) {

  output$myPlot <- renderPlot({

     BarChart(Dept, fill=input$myFill, color=input$myColor, trans=input$myTrans,
              horiz=input$myHoriz, sort=input$mySort,
              values=input$myValues, values_color=input$myValuesColor,
              values_position=input$myValuesPosition,
              values_size=input$myValuesSize,
              quiet=TRUE)

  })

}

library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=.9)

d <- Read("Employee", quiet=TRUE)

server <- function(input, output) {

  output$myPlot <- renderPlot({

     BarChart(Dept, by=Gender,
              fill=input$myFill, color=input$myColor, trans=input$myTrans,
              horiz=input$myHoriz, stack100=input$my100, beside=input$myBeside,
              values=input$myValues, values_color=input$myValuesColor,
              values_size=input$myValuesSize,
              quiet=TRUE)

  })

}


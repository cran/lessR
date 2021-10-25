library(shiny)
library(lessR)
style(lab_cex=1.2, axis_cex=1)

d <- Read("Employee", quiet=TRUE)

server <- function(input, output) {

  output$myPlot <- renderPlot({
    
     dens <- FALSE
     if (input$myRug) dens <- TRUE
     if (input$myDens) dens <- TRUE

     Histogram(Salary, bin_width=input$myWidth, bin_start=input$myStart,
               density=dens, rug=input$myRug, cumulate=input$myCumulate,
               values=input$myValues,
               fill=input$myFill, color=input$myColor, trans=input$myTrans,
               quiet=TRUE)

  })

}

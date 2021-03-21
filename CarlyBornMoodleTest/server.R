#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    output$userInputAll <- renderUI({
            str1 <- paste("you selected dates", input$dates[1], input$dates[2])
            str2 <- paste("you selected ", input$selectedDepartment, ", and", input$selectedTeachMode)
            str3 <- paste("<br> currently waiting for the real R code")
            HTML(paste(str1, str2, str3, sep = '<br/>'))
        }
    )
})

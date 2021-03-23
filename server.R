#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# reference Hiro's R script functions
source('module_bar.R', local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$userInputAll <- renderUI({
            str1 <- paste("you selected time", input$selectedYear, input$selectedTerm)
            str2 <- paste("and you selected ", input$selectedDepartment)
            HTML(paste(str1, str2, sep = '<br/>'))
        }
    )
    output$barChart <- renderPlot({
        termYear <- paste(toString(input$selectedYear), input$selectedTerm, sep = "")
        plot_counts(termYear, div = 'All', subj_full = input$selectedDepartment)
    })
})

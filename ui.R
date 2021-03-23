#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# setwd(~/Desktop/RShinyDataSquad/CarlyBornMoodleTest)

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    ),
    fluidRow(column(3, dateRangeInput("dates", h3("Date range"))),
             
             column(3, radioButtons("selectedDepartment", h3("Department"),
                                    choices = list("all", "Biology", "Spanish"),
                                    selected = "all")),
             
             column(3, 
                    selectInput("selectedYear", h3("year"), 
                                   choices = c(2014:2020), 
                                   selected = "2020"),
                    selectInput("term", h3("term"), 
                                choices = c("Fall", "Winter", "Spring"), 
                                selected = "Spring")),
             
             column(3, htmlOutput("userInputAll"))
    )
))

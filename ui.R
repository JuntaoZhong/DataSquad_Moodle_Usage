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


courseVector <- unlist(read.csv("./sb21_division_lookup.csv")[,"fullname"])

shinyUI(fluidPage(
    # Application title
    titlePanel("Moodle Usage Visualization"),
    mainPanel(plotOutput("barChart",height = 500)),
    fluidRow(column(3, selectInput("selectedDepartment", h3("Department"),
                                choices = courseVector,
                                selected = "Mathematics"),
                    
                    # split sub-column in half
                    column(6, selectInput("selectedYear", h3("year"), 
                                          choices = c(2014:2020), 
                                          selected = "2020")),
                    column(6, radioButtons("selectedTerm", h3("term"), 
                                           choices = c("Fall", "Winter", "Spring"), 
                                           selected = "Spring"))),
             
             column(3, htmlOutput("userInputAll"))
    )
))

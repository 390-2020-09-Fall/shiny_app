#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#cd
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(fpp3)
library(dplyr)
library(readr)


stem1 <- read_csv("data/scbi.stem1.csv")
#stem1<- read.csv("https://github.com/390-2020-09-Fall/SCBI-ForestGEO-Data/blob/master/tree_main_census/data/census-csv-files/scbi.stem1.csv")


# Define UI for application that draws a histogram

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("sp", label = "Species", choices =as.character(unique(stem1$sp))) # I got this "choice" suggestion from Emma:)
        ),
        mainPanel(
            plotOutput("distplot")
        )
    )
)

server <- function(input, output) {

    #filter the specie the user chosed

     chosen_species <- reactive({
       species <- filter(stem1, sp == input$sp)
       return(species)
     })

    #plot the non-interactive plot
    output$distplot <- renderPlot({
        ggplot(chosen_species()) +
            geom_boxplot(aes(x =sp,y=dbh,fill=status))+
            labs(x = "Species", y = "Diameter at breast height", title = "The relationship between species and DBH")
    })

}

# Run the application
shinyApp(ui = ui, server = server)


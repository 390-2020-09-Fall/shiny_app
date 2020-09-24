#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(fpp3)
library(dplyr)
library(readr)

stem1 <- read_csv("data/scbi.stem1.csv")
stem2 <- read_csv("data/scbi.stem2.csv")
stem3 <- read_csv("data/scbi.stem3.csv")

full <- rbind(stem1, stem2, stem3) %>%
  mutate(ExactDate = lubridate::mdy(ExactDate),
         DFstatus = as.factor(DFstatus),
         dbh = as.numeric(dbh))
alive <- full %>%
  filter(DFstatus == "alive")
other <- full %>%
  filter(DFstatus != "alive")

facet_labels <- c(`1` = "Census 1 in 2008-2010", `2` = "Census 2 in 2013", `3` = "Census 3 in 2018")

alive %>% ggplot(aes(dbh)) +
  geom_histogram() +
  facet_wrap(~CensusID, labeller = labeller(CensusID = facet_labels)) +
  labs(x = "Diameter at breast height (dbh), unit: centimeter", y = "number of trees",
       title = "Histogram of dbh of trees in census",
       subtitle = "(for alive trees only)")
other %>% ggplot(aes(dbh)) +
  geom_histogram() +
  facet_wrap(~CensusID, labeller = labeller(CensusID = facet_labels)) +
  labs(x = "Diameter at breast height (dbh), unit: centimeter", y = "number of trees",
       title = "Histogram of dbh of trees in census",
       subtitle = "(for non-alive trees)")

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

  # chosen_species <- reactive({
  #   species <- filter(stem1, sp == input$sp)
  #   return(species)
  # })

  #plot the non-interactive plot
    output$distplot <- renderPlot({
      ggplot(stem1) +
        geom_boxplot(aes(x =sp,y=dbh,fill=status))+
        coord_flip()+
        labs(x = "diameter at breast height", y = "Species", title = "The relationship between species and DBH")
    })

}

# Run the application
shinyApp(ui = ui, server = server)

shinyapp


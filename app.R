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
library(readr)

stem1 <- read_csv("data/scbi.stem1.csv")
stem2 <- read_csv("data/scbi.stem2.csv")
stem3 <- read_csv("data/scbi.stem3.csv")

alive <- rbind(stem1, stem2, stem3) %>%
  mutate(
    ExactDate = lubridate::mdy(ExactDate),
    DFstatus = as.factor(DFstatus),
    dbh = as.numeric(dbh),
    CensusID = as.numeric(CensusID),
    census_time = ifelse(CensusID == 1, "2008-2010",
                         ifelse(
                           CensusID == 2, "2013",
                           ifelse(CensusID == 3, "2018", "0")
                         ))
  ) %>%
  filter(DFstatus == "alive")

ui <- fluidPage(
  titlePanel("Histogram of dbh of trees in census (for alive trees only)"),
  sidebarLayout(sidebarPanel(
    selectInput("census_time",
                "Time of Census:",
                choices = as.character(unique(alive$census_time)))
  ),
  mainPanel(plotOutput("distPlot")))


)

# Define server logic required to draw a histogram
server <- function(input, output) {

  census_filter <- reactive({
    each_census <-
      filter(alive, census_time == toString(input$census_time))
    return(each_census)
  })

  output$distPlot <- renderPlot({
    ggplot(census_filter(), aes(x = dbh)) +
      geom_histogram() +
      labs(x = "Diameter at breast height (dbh), unit: centimeter", y = "number of trees")
  })

}

# Run the application
shinyApp(ui = ui, server = server)

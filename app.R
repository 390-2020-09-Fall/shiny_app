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
library(tsibble)
library(lubridate)
library(fpp3)
library(fpp2)
library(ggfortify)
library(lubridate)
total_alive <- rbind(new_scbi_stem1_alive, new_scbi_stem2_alive)  %>%
  select("sp", "quadrat", "dbh", "ExactDate","date",
         "quadrat", "treeID", "StemTag", "tag",   "MeasureID") %>%
  filter(!dbh %in% c(0, "NULL")) %>%
  filter(StemTag != 10) %>%
  group_by(treeID, StemTag) %>%
  filter(n() >= 2 )

total_alive %>%
  group_by(treeID, StemTag) %>%
  summarize(freq = n()) %>%
  filter(freq >= 2)

total_alive <- total_alive %>%
  mutate(date = mdy(ExactDate))

total_alive <- rbind(new_scbi_stem1_alive, new_scbi_stem2_alive)  %>%
  select("sp", "quadrat", "dbh", "ExactDate","date",
         "quadrat", "treeID", "StemTag", "tag",   "MeasureID") %>%
  filter(!dbh %in% c(0, "NULL")) %>%
  filter(StemTag != 10) %>%
  group_by(treeID, StemTag) %>%
  filter(n() >= 2 )

total_alive %>%
  group_by(treeID, StemTag) %>%
  summarize(freq = n()) %>%
  filter(freq >= 2)

total_alive <- total_alive %>%
  mutate(date = mdy(ExactDate))

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "sp", label = "Specie: ",
                        choices = c("amar", "beth", "caca", "frni", "havi", "ilve", "libe", "saca", "vipr"),
                        selected = "libe"),

        ),

        mainPanel(
            plotOutput(outputId = "plot")
        )
    )
)
#help from  and Sophie's suggestion
server <- function(input, output) {

    df_subset <- reactive({
        a <- filter(total_alive, sp == input$sp)
        return(a)
    })

    output$plot <- renderPlot({
        ggplot(data = df_subset(), aes_string(x=input$sp, y=input$sp)) +
            geom_line(aes(x=date, y= dbh,
                          group = interaction(treeID, StemTag),
                          color = quadrat)) +
            ggtitle("Specie's DBH based on Quadrat (2009-2013)") +
            ylab("Diameter At Breast Height") +
            xlab("Year")


    })
}

shinyApp(ui = ui, server = server)

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

stem3 <- read.csv("tree_main_census/data/census-csv-files/scbi.stem3.csv")
stem2 <-read.csv("tree_main_census/data/census-csv-files/scbi.stem2.csv")
stem1<-read.csv("tree_main_census/data/census-csv-files/scbi.stem1.csv")

scbi_stem2_notalive <- scbi_stem2 %>%
  filter(DFstatus != "alive" &
           DFstatus != "prior" &
           DFstatus != "missing" &
           DFstatus != "NULL")

scbi_stem2_alive <- scbi_stem2  %>%
  filter(DFstatus != "stem dead" &
           DFstatus != "broken below" &
           DFstatus != "prior" &
           DFstatus != "missing")

scbi_stem1_notalive <- scbi_stem1 %>%
  filter(DFstatus != "alive" &
           DFstatus != "prior" &
           DFstatus != "missing" &
           DFstatus != "NULL")

scbi_stem1_alive <- scbi_stem1  %>%
  filter(DFstatus != "stem dead" &
           DFstatus != "broken below" &
           DFstatus != "prior" &
           DFstatus != "missing")
new_scbi_stem1_alive <-scbi_stem1_alive %>%
  #filter(sp %in% c("libe")) %>%
  filter(quadrat <=300 |
           quadrat >= 1000) %>%
  filter(treeID <= 300)

new_scbi_stem1_alive <-new_scbi_stem1_alive[duplicated(new_scbi_stem1_alive$tag), ]
new_scbi_stem1_alive <-new_scbi_stem1_alive[duplicated(new_scbi_stem1_alive$treeID), ]

new_scbi_stem2_alive <-scbi_stem2_alive %>%
  #filter(sp %in% c( "libe")) %>%
  filter(quadrat <=300 |
           quadrat >= 1000) %>%
  filter(treeID <= 300)


new_scbi_stem2_alive <-new_scbi_stem2_alive[duplicated(new_scbi_stem2_alive$tag), ]
new_scbi_stem2_alive <-new_scbi_stem2_alive[duplicated(new_scbi_stem2_alive$treeID), ]



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

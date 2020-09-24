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

stem_2 <- read_csv("data/scbi.stem2.csv")
stem_3 <- read_csv("data/scbi.stem3.csv")
spptable <- read_csv("data/scbi.spptable.csv")

dbh_diff <- stem_2 %>%
    rename(dbh_13 = dbh) %>%
    left_join(stem_3, by = c("treeID", "stemID", "sp")) %>%
    mutate(dbh_13 = as.numeric(dbh_13),
           dbh_18 = as.numeric(dbh)) %>%
    filter(!is.na(dbh_13), !is.na(dbh_18), status.y == "A") %>% # filter for trees that are alive
    group_by(treeID, stemID, sp) %>%
    summarize(dbh_diff = dbh_18 - dbh_13)

dbh_diff <- dbh_diff %>%
    left_join(spptable, by = c("sp")) %>%
    select(treeID, stemID, Family, dbh_diff)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Growth in diameter at breast height per stem between 2013-2018"),

    # Sidebar to select families to compare
    sidebarLayout(
        sidebarPanel(
            selectInput("family",
                        "Tree Family",
                        selected = unique(dbh_diff$Family)[1:2],
                        choices = unique(dbh_diff$Family),
                        multiple = TRUE)
        ),

        # Plot output
        mainPanel(
           plotOutput("distPlot", height = "70vh")
        )
    )
)

# Define server logic required to draw boxplots
server <- function(input, output) {

    # Reactive function to subset data based on input selection
    df_subset <- reactive({
        data <- dbh_diff %>%
            filter(Family %in% input$family)
        return(data)
    })

    output$distPlot <- renderPlot({
        ggplot(df_subset(), aes(dbh_diff)) +
            geom_boxplot(aes(color = Family))  +
            xlab("Growth (cm)") +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())

    })
}

# Run the application
shinyApp(ui = ui, server = server)

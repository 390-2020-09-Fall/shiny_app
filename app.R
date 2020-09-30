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
library(hrbrthemes)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "stems_plot",
                               label = "Census year:",
                               choices = c("2008-2010" = "1",
                                 "2013" = "2",
                                 "2018" = "3")
            ),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 20)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("histPlot")
        ))
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    stem1 <- here::here("data/scbi.stem1.csv") %>%
        read_csv()
    stem2 <- here::here("data/scbi.stem2.csv") %>%
        read_csv()
    stem3 <- here::here("data/scbi.stem3.csv") %>%
        read_csv()

    stems <- rbind(stem1, stem2, stem3)




    output$histPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- stems_plot[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins)

        stems_plot <- stems %>%
            filter(dbh != "NULL") %>%
            filter(CensusID %in% input$stems_plot) %>%
            #       filter(CensusID %in% c("1", "2")) %>%
            na.omit(stems) %>%
            mutate(CensusID = as.factor(CensusID),
                   dbh = as.numeric(dbh)) %>%
            filter(dbh < 500)

        # draw the histogram with the specified number of bins
        ggplot(data = stems_plot, aes(x = dbh, fill = CensusID)) +
            geom_histogram(bins = input$bins, alpha = 0.5, position = "identity") +
            labs(title = "Distrubution of Tree Diameters at the Smithsonian Conservation Biology \n Institute (SCBI) in Front Royal, VA ",
                 x = "Diameter at Breast Height (DBH)",
                 y= "Number of Trees")

    })
}

# Run the application
shinyApp(ui = ui, server = server)

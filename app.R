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
            checkboxGroupInput(inputId = "stemdata",
                               label = "Census year:",
                               choices = c("stem1" = "stem1",
                                 "stem2" = "stem2",
                                 "stem3" = "stem3")
            ),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 15)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("histPlot")
        ))
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    stem1 <- here::here("data/scbi.stem3.csv") %>%
        read_csv()
    stem2 <- here::here("data/scbi.stem3.csv") %>%
        read_csv()
    stem3 <- here::here("data/scbi.stem3.csv") %>%
        read_csv()

    stems <- rbind(stem1, stem2, stem3)

    stem1 <- stem1 %>%
        filter(dbh != "NULL") %>%
        na.omit(stem1) %>%
        mutate(dbh = as.numeric(dbh))

    stem2 <- stem2 %>%
        filter(dbh != "NULL") %>%
        na.omit(stem2) %>%
        mutate(dbh = as.numeric(dbh))

    stem3 <- stem3 %>%
        filter(dbh != "NULL") %>%
        na.omit(stem3) %>%
        mutate(dbh = as.numeric(dbh))

     stems <- stems %>%
        filter(dbh != "NULL") %>%
        na.omit(stems) %>%
        mutate(dbh = as.numeric(dbh))

     x_reactive <- reactive({


         switch(input$stemdata,
                "stem1" = stem1$stem1,          # If the value is 'geyser', return data from faithful
                "stem2" = stem2$stem2, # if the value is 'economics' return unemployment data
                "stem3" = stem3$stem3 # If the value is 'diamonds' return price data
                )

     })

    output$histPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
#        x    <- faithful[, 2]
#        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        ggplot(data = x_reactive, aes(dbh)) +
            geom_histogram(data = stems, fill = "red", alpha = 0.2) +
#            geom_histogram(data = stem2, fill = "blue", alpha = 0.2) +
#            geom_histogram(data = stem3, fill = "green", alpha = 0.2) +
            stat_bin(bins = 15) +
#            scale_fill_manual(values = c("#788475", "#5E5D5C")) +
            labs(title = "Distrubution of Tree Diameters at the Smithsonian Conservation Biology \n Institute (SCBI) in Front Royal, VA ",
                 x = "Diameter at Breast Height (DBH)",
                 y= "Number of Trees",
                 fill="") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme_ipsum()

    })
}

# Run the application
shinyApp(ui = ui, server = server)

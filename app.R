#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("distribution of dbh of trees measured in first 2 censuses"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            #Input for bin1 variable
            sliderInput("bin1",
                        "Number of bins for Census 1:",
                        min = 0,
                        max = 200,
                        value = 20)
        ,
        #Input for bin2 variable
            sliderInput("bin2",
                    "Number of bins for Census 2:",
                    min = 0,
                    max = 200,
                    value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    stem1 <- read_csv("data/scbi.stem1.csv") %>% na.omit()
    stem2 <- read.csv("data/scbi.stem2.csv") %>% na.omit()

    output$distPlot <- renderPlot({
        # generate bins based on input bin1 and bin2 from ui.R
        bin1 <- seq(min(stem1$dbh), max(stem1$dbh), length.out = input$bin1 + 1)
        bin2 <- seq(min(stem2$dbh), max(stem2$dbh), length.out = input$bin2 + 1)

        # draw the histogram with the specified number of bins
        #max count at break = 1 is 55293 for stem 2 and 40167 for stem 1 --> choose the higher range.
        hist(stem1$dbh, breaks=bin1, ylim = c(0,55300),col=rgb(1,0,0,0.5), xlab="dbh",
             ylab="frequency", main = " ")

        # Second with add=T to plot on top
        hist(stem2$dbh, breaks=bin2, ylim = c(0,55300), col=rgb(0,0,1,0.5), add=T)

        # Add legend
        legend("topright", legend=c("Census 1: 2008-2010","Census 2: 2013"),
               col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
               pt.cex=2, pch=15)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

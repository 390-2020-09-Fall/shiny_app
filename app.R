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

# Load data
spptable <- here::here("data/scbi.spptable.csv") %>%
  read_csv()
stem1 <- here::here("data/scbi.stem1.csv") %>%
  read_csv()
stem2 <- here::here("data/scbi.stem2.csv") %>%
  read_csv()
stem3 <- here::here("data/scbi.stem3.csv") %>%
  read_csv() %>%
  filter(dbh != "NULL") %>%
  mutate(dbh = as.numeric(dbh))

# Combine all three census tables
stem <- rbind(stem1, stem2, stem3) %>%
  drop_na(dbh) %>%
  mutate(
    dbh = as.numeric(dbh),
    CensusID = as.factor(CensusID),
    Census = case_when(
      CensusID == 1 ~ "2008-2010",
      CensusID == 2 ~ "2013",
      CensusID == 3 ~ "2018"
    )
  ) %>%
  dplyr::filter(dbh > 0)

# Join stem and spp tables
stem_spp <- left_join(stem, spptable, by = "sp")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        #
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      ggplot(stem_spp, aes(x = dbh, color = Census)) +
        geom_freqpoly(position = "identity", binwidth = 125) +
        xlab("Diameter at Breast Height (DBH)") +
        ylab("Count") +
        ggtitle("Distribution of DBH at SCBI-ForestGeo site across census years")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

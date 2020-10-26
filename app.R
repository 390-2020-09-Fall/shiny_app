#
# Raw data from SCBI_ForestGEO dataset
# specifically, tree census data (2008, 2013, 2018) and tree ecology data
#
# censusJoin is a dataframe defined in MP3_eking03.Rmd
#


library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Growth Rate Histograms using SCBI Census"),

  sidebarLayout(
    sidebarPanel(
      selectInput("year_range", "Change in DBH (cm) from: ",
                  c("2008 to 2013" = "diffdbh_0813", "2013 to 2018" = "diffdbh_1318")),
      radioButtons("hist_type", "Histogram Type: ",
                  c("Count" = "count", "Density" = "density"))
    ),

    mainPanel(
      plotOutput("histPlot")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {


    output$histPlot <- renderPlot({

      # adding DBH growth coloumns
      censusDBH <- censusJoin %>%
        mutate(diffdbh_0813 = dbh2013 - dbh2008,
               diffdbh_1318 = dbh2018 - dbh2013) %>%
        filter((diffdbh_0813 > 0) & (diffdbh_1318 > 0)) %>%
        filter((diffdbh_0813 < 50) & (diffdbh_1318 < 50))


      # series of if statements using user input
      # using if() because I could not make direct changes to ggplot chunk
      if ((input$year_range == "diffdbh_0813") &
          (input$hist_type == "count")) {

        censusPlot <- censusDBH %>%
          ggplot(aes(diffdbh_0813, after_stat(count))) +
          xlab("Change in DBH (cm) from 2008 to 2013") +
          ylab("Count")

      }

      if ((input$year_range == "diffdbh_0813") &
          (input$hist_type == "density")) {

        censusPlot <- censusDBH %>%
          ggplot(aes(diffdbh_0813, after_stat(density))) +
          xlab("Change in DBH (cm) from 2008 to 2013") +
          ylab("Density")

      }

      if ((input$year_range == "diffdbh_1318") &
          (input$hist_type == "count")) {

        censusPlot <- censusDBH %>%
          ggplot(aes(diffdbh_1318, after_stat(count))) +
          xlab("Change in DBH (cm) from 2013 to 2018") +
          ylab("Count")

      }

      if ((input$year_range == "diffdbh_1318") &
          (input$hist_type == "density")) {

        censusPlot <- censusDBH %>%
          ggplot(aes(diffdbh_1318, after_stat(density))) +
          xlab("Change in DBH (cm) from 2013 to 2018") +
          ylab("Density")

      }


      # finishing the plot: adding geom, theme, and facets
      censusPlot +
        geom_histogram(color = "white", fill = "#8789B4") +
        facet_grid(canopy_position~drought_tolerance) +
        theme_bw()

    })
}

# Run the application
shinyApp(ui = ui, server = server)

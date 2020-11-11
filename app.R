#
# Raw data from SCBI_ForestGEO dataset
# specifically, tree census data (2008, 2013, 2018) and tree ecology data
#
# censusJoin is a dataframe defined in MP3_eking03.Rmd
#


library(shiny)
library(ggplot2)
library(tidyverse)
library(readr)


# Importing data sets from data folder
tree_ecology <- read_csv("data/SCBI_ForestGEO_sp_ecology.csv")
treecensus_2018 <- read_csv("data/scbi.stem3.csv",
                            col_types = cols(ExactDate = col_date(format = "%m/%d/%Y"),
                                             dbh = col_double(), hom = col_double()))
treecensus_2013 <- read_csv("data/scbi.stem2.csv",
                            col_types = cols(ExactDate = col_date(format = "%m/%d/%Y")))
treecensus_2008 <- read_csv("data/scbi.stem1.csv",
                            col_types = cols(ExactDate = col_date(format = "%m/%d/%Y")))


# Creating 'censusJoin': a table that only includes individuals that were alive in all three censuses
## the most unique identifier per row is stemID, not treeID
censusJoin <- tree_ecology %>%
  filter(live_form == "tree") %>%
  select(sp = spcode, canopy_position, drought_tolerance) %>%
  full_join(treecensus_2008, by = "sp") %>%
  filter(!is.na(dbh)) %>%
  filter(!is.na(canopy_position)) %>%
  filter(status == "A") %>%
  left_join(treecensus_2013, by = c("sp", "treeID", "tag", "quadrat", "gx", "gy")) %>%
  mutate(same = ifelse(stemID.x == stemID.y, TRUE, FALSE)) %>%
  filter(!(same == FALSE)) %>%
  select(-stemID.y, -StemTag.y, -same) %>%
  rename(stemID = stemID.x, StemTag = StemTag.x) %>%
  left_join(treecensus_2018, by = c("sp", "treeID", "tag", "quadrat", "gx", "gy")) %>%
  mutate(same = ifelse(stemID.x == stemID.y, TRUE, FALSE)) %>%
  filter(!(same == FALSE)) %>%
  select(-stemID.y, -StemTag.y, -same) %>%
  rename(stemID = stemID.x, StemTag = StemTag.x) %>%
  select(treeID, tag, stemID, StemTag, sp, canopy_position, drought_tolerance, quadrat, gx, gy,
         dbh2008 = dbh.x, hom2008 = hom.x, date2008 = ExactDate.x, status2008 = status.x,
         dbh2013 = dbh.y, hom2013 = hom.y, date2013 = ExactDate.y, status2013 = status.y,
         dbh2018 = dbh, hom2018 = hom, date2018 = ExactDate, status2018 = status) %>%
  filter(status2018 == "A" & status2013 == "A") %>%
  select(-status2008, -status2013, -status2018)


# Code borrowed from SCBI_ForestGEO_Data/R_scripts/ForestGEO_plot_map.R
# Finding the topographic wetness index and lat/long (NAD83), then appending it to 'censusJoin'
library(rgdal)
library(raster)
library(elevatr)
library(dynatopmodel)

ext <- extent(747370.6, 747785.8, 4308505.5, 4309154.8)
xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- 5
r <- raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)
proj4string(r) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

q <- get_elev_raster(r, z=14)

r <- raster(ext, res = 5)
q <- resample(q, r)

proj4string(q) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

layers <- build_layers(q)

r <- raster(ext, res = 20)
layers@layers[[4]] <- resample(layers@layers[[3]], r)
#sp::plot(layers, main=c("Elevation masl", "Upslope area (log(m^2/m))", "TWI ((log(m^2/m))", "TWI per quadrat"))

titles <- c("Elevation masl", "Upslope area (log(m^2/m))", "TWI ((log(m^2/m))", "TWI per quadrat")
files <- c("plot_elevation", "plot_upslope", "plot_TWI", "plot_TWI_quadrat")

NAD83.SW <- c(747385.521, 4308506.438)
NAD83.NW <- c(747370.676, 4309146.156)

Offset <- atan2(NAD83.NW[1] - NAD83.SW[1], NAD83.NW[2] - NAD83.SW[2])

grid2nad83 <- function(x, y) {
  NAD83.X <- NAD83.SW[1] + (x*cos(Offset) + y*sin(Offset))
  NAD83.Y <- NAD83.SW[2] + (-x*sin(Offset) + y*cos(Offset))
  nad83 <- list(NAD83.X, NAD83.Y)
  names(nad83) <- c("NAD83_X", "NAD83_Y")
  nad83
}

censusJoin <- data.frame(censusJoin, grid2nad83(censusJoin$gx, censusJoin$gy))
censusJoin_XY <- censusJoin %>% dplyr::select(NAD83_X, NAD83_Y)

twi_values <- raster::extract(layers[[3]], censusJoin_XY, method="simple")

censusJoin$TWI <- twi_values

# ----------------------------




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

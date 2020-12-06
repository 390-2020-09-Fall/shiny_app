library(shiny)
library(tidyverse)
library(readr)

scbi_stem1 <- read_csv("data/scbi.stem1 copy.csv")
scbi_stem2 <- read_csv("data/scbi.stem2 copy.csv")
scbi_stem3 <- read_csv("data/scbi.stem3 copy.csv")
scbi_spptable <- read_csv("data/scbi.spptable copy.csv")

stem1 <- scbi_stem1 %>%
  filter(dbh != "NULL") %>%
  mutate(dbh = as.numeric(dbh)) %>%
  select(sp, dbh, CensusID) %>%
  group_by(sp) %>%
  summarize(
    avg_dbh_sp = mean(dbh),
    census_year = mean(CensusID)
  )

stem2 <- scbi_stem2 %>%
  filter(dbh != "NULL") %>%
  mutate(dbh = as.numeric(dbh)) %>%
  select(sp, CensusID, dbh) %>%
  group_by(sp) %>%
  summarize(
    avg_dbh_sp = mean(dbh),
    census_year = mean(CensusID)
  )

stem3 <- scbi_stem3 %>%
  filter(dbh != "NULL") %>%
  mutate(dbh = as.numeric(dbh)) %>%
  select(sp, CensusID, dbh) %>%
  group_by(sp) %>%
  summarize(
    avg_dbh_sp = mean(dbh),
    census_year = mean(CensusID)
  )

stem_joined <- rbind(stem1, stem2, stem3)

genus_dbh <- stem_joined %>%
  inner_join(scbi_spptable, by = "sp") %>%
  select(Genus, avg_dbh_sp, census_year) %>%
  group_by(Genus, census_year) %>%
  summarize(avg_dbh = mean(avg_dbh_sp)) %>%
  mutate(
    CensusID = as_factor(census_year),
    census = case_when(
      CensusID == 1 ~ "2008",
      CensusID == 2 ~ "2013", # Replaced it with this case_when() statement
      CensusID == 3 ~ "2018"
    )
  ) %>%
  select(Genus, avg_dbh, census) # Selected columns to simplify

ui <- fluidPage(
  titlePanel("Average DBH for Each Genus in the SCBI Census"),

  sidebarLayout(
    sidebarPanel(
      selectInput("census",
                  "Census",
                  choices = as.character(unique(genus_dbh$census)) # Substituted a vector made from the data rather than  homemade vector which would is harder to match on
      )


    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    genus_dbh_filtered <- filter(
      genus_dbh,
      census == toString(input$census)
    )
    return(genus_dbh_filtered)
  })

  output$distPlot <- renderPlot({
    ggplot(data = filtered(), mapping = aes(reorder(Genus, -avg_dbh), y = avg_dbh)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(x = "Genus", y = "Average Diameter at Breast Height (DBH)") +
      theme(axis.text.x = element_text(angle = 90, size = 11))
  })
}

shinyApp(ui = ui, server = server)


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
  summarize(avg_dbh_sp = mean(dbh),
            census_year = mean(CensusID))

stem2 <- scbi_stem2 %>%
  filter(dbh != "NULL") %>%
  mutate(dbh = as.numeric(dbh)) %>%
  select(sp, CensusID, dbh) %>%
  group_by(sp) %>%
  summarize(avg_dbh_sp = mean(dbh),
            census_year = mean(CensusID))

stem3 <- scbi_stem3 %>%
  filter(dbh != "NULL") %>%
  mutate(dbh = as.numeric(dbh)) %>%
  select(sp, CensusID, dbh) %>%
  group_by(sp) %>%
  summarize(avg_dbh_sp = mean(dbh),
            census_year = mean(CensusID))

stem_joined <- rbind(stem1, stem2, stem3)

genus_dbh <- stem_joined %>%
  inner_join(scbi_spptable, by = "sp") %>%
  select(Genus, avg_dbh_sp, census_year) %>%
  group_by(Genus, census_year) %>%
  summarize(avg_dbh = mean(avg_dbh_sp))

year.labs <- c("2008", "2013", "2018")
names(year.labs) <- c("1", "2", "3")



ui <- fluidPage(


    titlePanel("Average DBH for Each Genus in the SCBI Census"),


    sidebarLayout(
      sidebarPanel(
        selectInput("census_year", "CensusYear",
                    choices = c("2008" = "1","2013" = "2", "2018" = "3"),
                    selected = "3"),
        checkboxGroupInput("Genus", "Genus",
                           choices = c("Acer","Ailanthus","Amelanchier","Asimina",
                                       "Berberis","Carpinus","Carya","Castanea","Celtis",
                                       "Cercis","Chionanthus","Cornus","Corylus",
                                       "Crataegus","Diospyros","Elaeagnus","Euonymus",
                                       "Fagus","Fraxinus","Hamamelis","Ilex","Juglans",
                                       "Juniperus","Lindera","Liriodendron","Lonicera","Nyssa",
                                       "Paulownia","Pinus","Platanus","Prunus","Quercus",
                                       "Rhododendron","Robinia","Rosa","Rubus","Sambucus",
                                       "Sassafras","Tilia","Ulmus","Viburnum"),
                           selected = c("Acer","Ailanthus","Amelanchier","Asimina",
                                        "Berberis","Carpinus","Carya","Castanea","Celtis",
                                        "Cercis","Chionanthus","Cornus","Corylus",
                                        "Crataegus","Diospyros","Elaeagnus","Euonymus",
                                        "Fagus","Fraxinus","Hamamelis","Ilex","Juglans",
                                        "Juniperus","Lindera","Liriodendron","Lonicera","Nyssa",
                                        "Paulownia","Pinus","Platanus","Prunus","Quercus",
                                        "Rhododendron","Robinia","Rosa","Rubus","Sambucus",
                                        "Sassafras","Tilia","Ulmus","Viburnum"),
                           inline = FALSE),
      ),
      mainPanel(
        plotOutput("distPlot")
      )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        Genus <- input$Genus
        CensusYear <- input$census_year
        params <- c(Genus, CensusYear)

        ggplot(data = genus_dbh, mapping = aes(reorder(Genus, -avg_dbh), y = avg_dbh, fill = census_year))+
          geom_bar(color = "white", stat='identity')+
          labs(title = "Average DBH for Each Genus in the SCBI Census", subtitle = "dark blue: 2008        medium blue: 2013        light blue: 2018", x = "Genus", y = "Average DBH")+
          theme(axis.text.x=element_text(angle = 90, size = 9), legend.position = "none")
    })

}


shinyApp(ui = ui, server = server)

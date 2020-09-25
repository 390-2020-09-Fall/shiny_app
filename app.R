
library(shiny)
library(tidyverse)
library(readr)

setwd("~/Desktop/390/SCBI-ForestGEO-Data/tree_main_census/data/census-csv-files/")
scbi_stem1 <- read_csv("scbi.stem1.csv")
scbi_stem2 <- read_csv("scbi.stem2.csv")
scbi_stem3 <- read_csv("scbi.stem3.csv")
scbi_spptable <- read_csv("scbi.spptable.csv")

# Set "base plot" of points that won't change
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
        selectInput("census_year", "Census Year",
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
      plotOutput("avg_dbh")
    )
)




server <- function(input, output) {

    output$distplot <- renderPlot({

        ggplot(data = genus_dbh, mapping = aes(x = Genus, y = avg_dbh, fill = census_year))+
          geom_bar(color = "white", stat='identity')+
          labs(title = "Average DBH for Each Genus in the SCBI Census", subtitle = "dark blue: 2008        medium blue: 2013        light blue: 2018", x = "Genus", y = "Average DBH")+
          theme(axis.text.x=element_text(angle = 90, size = 9), legend.position = "none")
    })

}




shinyApp(ui = ui, server = server)

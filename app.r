# load packages ------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(shinydashboard)
library(janitor)
library(sf) 
library(gganimate)
library(transformr) #need?
library(magick) #need?
library(here)
library(raster) #need?
library(tmap)
library(lubridate)
library(dplyr)
library(lwgeom)

# add data ----------------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016")
fire <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# sub data ---------------------------------------------------------------

fire <- fire %>% 
  clean_names() %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() %>% 
  mutate (fire_name = str_to_title(fire_name)) %>% 
  dplyr::filter (acres > 200)

fire_causes <- fire %>%
  mutate(fire_cause = case_when(
    cause == 0 ~ "Unknown",
    cause == 1 ~ "Lightning",
    cause == 2 ~ "Equipment Use",
    cause == 3 ~ "Smoking",
    cause == 4 ~ "Campfire",
    cause == 5 ~ "Debris",
    cause == 6 ~ "Railroad",
    cause == 7 ~ "Arson",
    cause == 8 ~ "Playing with Fire",
    cause == 9 ~ "Miscellaneous",
    cause == 10 ~ "Vehicle",
    cause == 11 ~ "Powerline",
    cause == 12 ~ "Firefighting Training",
    cause == 13 ~ "Non-firefighting Training",
    cause == 14 ~ "Unknown",
    cause == 15 ~ "Structure",
    cause == 16 ~ "Aircraft",
    cause == 17 ~ "Volcanic",
    cause == 18 ~ "Escaped Prescribed Burn",
    cause == 19 ~ "Illegal Campfire"
  )) %>% 
  mutate (fire_cause_simplified = case_when(
    fire_cause %in% c("Lightning", "Volcanic") ~ "Natural Cause",
    fire_cause %in% c('Equipment Use','Smoking','Campfire','Debris','Railroad','Arson','Playing with Fire',
                      'Miscellaneous','Vehicle','Powerline','Firefighting Training','Non-firefighting Training',
                      'Structure', 'Aircraft', 'Escaped Prescribed Burn', 'Illegal Campfire') ~ "Human Cause",
    fire_cause == "Unknown" ~ "Unknown"
  )) %>% 
  st_make_valid() %>% 
  mutate(year = as.numeric(year)) %>% 
  drop_na(year)

# user interface ---------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Fire Causes", tabName = "fire_causes"),
      menuItem("Acres Burned", tabName = "acres_burned")
    )
  ),
  dashboardBody(tabItems(
      tabItem(
        tabName = "fire_causes",
        fluidRow(
          box(title = "title",
              checkboxGroupInput(inputId = "check_fire_causes",
                                 label = "Select Fire Cause(s) to explore:",
                                 choices = c(unique(fire_causes$fire_cause)))),
          box(plotOutput(outputId = "fire_causes_graph")),
          box(title = "title",
              sliderInput(inputId = "date_fire_causes",
                         label = "Input Year(s)",
                         min = 1890, max = 2019, value = c(10, 20)))
        )
      )
  )
))


#server --------------------------------------------------------------------

server <- function(input, output) {
  
  fire_causes_count <- reactive({
    fire_causes %>% 
      filter(fire_cause == fire_causes$check_fire_causes) %>% 
      group_by(fire_causes$check_fire_causes, fire_causes$year) %>% 
      count()
  })
  output$fire_causes_graph <- renderPlot({
    ggplot(data = fire_causes_count, aes(x = fire_cause, y = ))
  })
  
}

#run shiny app --------------------------------------------------------------

shinyApp(ui = ui, server = server)




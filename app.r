# load packages ------------------------------------------------------------

# General packages
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(dplyr)
library(kableExtra)

# Shiny packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)

# Map packages
library(sf) 
library(gganimate)
library(transformr) #need?
library(magick) #need?
library(tmap)
library(ggthemes)

#library(lwgeom)

# add data ----------------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016") %>% 
  st_transform(crs = 4326)

fire <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# sub data ------------------------------------------------------------------------------------------

#test data (limit data size to stop r from freezing)-------------
fire <- fire %>% 
  clean_names() %>% 
  dplyr::filter (acres > 200) %>% 
  mutate (fire_name = str_to_title(fire_name)) %>%
  st_transform(crs = 4326) %>% 
  mutate (decade = case_when(
    year < 1900 ~ "1890s",
    year < 1909 ~ "1900s",
    year < 1919 ~ "1910s",
    year < 1929 ~ "1920s",
    year < 1939 ~ "1930s",
    year < 1949 ~ "1940s",
    year < 1959 ~ "1950s",
    year < 1969 ~ "1960s",
    year < 1979 ~ "1970s",
    year < 1989 ~ "1980s",
    year < 1999 ~ "1990s",
    year < 2009 ~ "2000s",
    year < 2019 ~ "2010s"
  )) %>% 
  st_as_sf() %>% 
  st_simplify(dTolerance = 100) %>% 
  mutate(year = as.numeric(year))
  

# data for fire causes --------------------
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
  arrange(fire_cause) %>% 
  mutate (fire_cause_simplified = case_when(
    fire_cause %in% c("Lightning", "Volcanic") ~ "Natural Cause",
    fire_cause %in% c('Equipment Use','Smoking','Campfire','Debris','Railroad','Arson','Playing with Fire',
                      'Miscellaneous','Vehicle','Powerline','Firefighting Training','Non-firefighting Training',
                      'Structure', 'Aircraft', 'Escaped Prescribed Burn', 'Illegal Campfire') ~ "Human Cause",
    fire_cause == "Unknown" ~ "Unknown"
  )) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))

# data for fire sizes

fire_size <- fire %>% 
  mutate(area_categorical = case_when(
    acres < 1000 ~ "0-1,000",
    acres < 5000 ~ "1,000-5,000",
    acres < 10000 ~ "5,000-10,000",
    acres < 20000 ~ "10,000-20,000",
    acres < 40000 ~ "20,000-40,000",
    acres < 100000 ~ "40,000-100,000",
    acres < 500000 ~ "100,000-450,000"
  )) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(area_categorical = as.factor(area_categorical))  %>% 
  filter(!is.na(decade)) %>% 
  st_as_sf() 


  mutate(area_categorical = fct_relevel(area_categorical, 
                                        levels = c("0-1,000", "1,000-5,000", "5,000-10,000",
                                                  "10,000-20,000", "20,000-40,000", "40,000-100,000",
                                                   "100,000-450,000")))

# user interface ---------------------------------------------------------

ui <- navbarPage(
  "Navigation Bar!",
   theme = shinytheme("united"),
   tabPanel("Fire History",
            h1("Title"),
            p("text"),
            mainPanel(plotOutput(outputId = "gganimate_map"))),
   tabPanel("Fire Size",
            h1("Title"),
            p("text"),
            sidebarLayout(
              sidebarPanel(radioButtons(inputId = "check_area",
                                        label = "Select Fire Size",
                                        choices = c(unique(fire_size$area_categorical)))),
              mainPanel("Graph and Table Here",
                        plotOutput(outputId = "area_graph"),
                        tableOutput(outputId = "area_sum_table")))),
  navbarMenu("Fire Causes",
             tabPanel("All",
                      sidebarLayout(
                        sidebarPanel("text here",
                                     checkboxGroupInput(inputId = "check_fire_causes",
                                                        label = "Select Fire Cause(s) to explore:",
                                                        choices = c(unique(fire_causes$fire_cause))),
                                     sliderInput(inputId = "date_fire_causes1",
                                                 label = "Input Year(s)",
                                                 min = 1880, max = 2019, value = c(1900,1920),
                                                 sep = "")),
                        mainPanel("Graph and Table Here",
                                  plotOutput(outputId = "fire_causes_graph"),
                                  leafletOutput('fire_causes_map')))),
             tabPanel("Natural vs Human Caused"))
)

#server --------------------------------------------------------------------

server <- function(input, output) {
  
  #gganimate map for intro page
  output$gganimate_map <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border, color = "grey80") +
      geom_sf(data = fire, fill = "red", alpha = 0.8, color = "red") +
      theme_classic() +
      theme_map() 
      #labs(title = "Year: {frame_time}") +
      #transition_time(year)
  })
  # data frame for all fire causes
  fire_causes_count <- reactive({
      fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause, year) %>% 
      count()
  })
  
  y_axis_lim <- reactive({ fire_causes_count %>% max(n)})
  
  # graph for fire causes
  output$fire_causes_graph <- renderPlot({
    ggplot(data = fire_causes_count(), aes(x = year, y = n)) +
      geom_point(aes(color = fire_cause)) +
      geom_line(aes(color = fire_cause)) +
      labs(x = "\nYear", y = "Number of Occurances\n") +
      theme_minimal() +
      scale_color_discrete(name = "Fire Cause") +
      scale_y_continuous(expand = c(0,0)) + #limits = c(0,max(variable)+10); input$date_fire_causes1[2], max(fire_causes_count$year)
      scale_x_continuous(expand = c(0,0))
      
  })
  #map for fire causes
  output$fire_causes_map <- renderLeaflet({
    tm_shape(data = ca_border) +
      tm_fill(data = fire_causes_count())
  })

  area_decades_count <- reactive({
    fire_size %>% 
      filter(area_categorical %in% input$check_area) %>% 
      group_by(decade, area_categorical) %>% 
      count()
  })
  
  output$area_graph <- renderPlot({
    ggplot(data = area_decades_count(), aes(x = decade, y = n)) + 
      geom_col(fill = "red") +
      scale_x_discrete(expand = c(0,0),
                       drop = F) +
      scale_fill_discrete(drop = F) +
      scale_y_continuous(expand = c(0,0)) +
      labs (x = "\nTime", y = "Number of Fires\n") +
      theme_classic()
  })
  
  area_decades_sum <- reactive({
    fire_size %>% 
      dplyr::select(decade, area_categorical) %>% 
      filter(area_categorical == input$check_area) %>% 
      group_by(area_categorical) %>% 
      count()
  })
  
  output$area_sum_table <- renderTable({
    area_decades_sum() %>% 
      kable(col.names = c("Area", "Sum"), align = 'c') %>% 
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "respsonsive"),
        full_width = T,
        position = "center")
  })
}


#run shiny app --------------------------------------------------------------

shinyApp(ui = ui, server = server)





##issues
# errror when order levels in area_categorical: Outer names are only allowed for unnamed scalar atomic inputs
# error for map on first page: 'x' and 'units' must have length > 0 ; Error in points[[1]] : subscript out of bounds
# set y axis to start at 0
# kable error: cannot coerce class ‘c("kableExtra", "knitr_kable")’ to a data.frame; length of 'dimnames' [2] not equal to array extent

  tm_shape(fire_causes) +
    tm_fill("area_categorical")


area_decades_sum <- fire_causes %>% 
      dplyr::select(decade, area_categorical) %>% 
      filter(area_categorical =="0-1,000") %>% 
      group_by(area_categorical) %>% 
      count()

area_decades_sum %>% 
  kable(col.names = c("Cause", "Count"), align = 'c') %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "respsonsive"),
    full_width = T,
    position = "center"
  )
crs(fire_causes)

#fire cuases issues: the graph doesnt recognize the years 
#daysofweekdisabled not working. R doesnt recognize the function
#is there a way to get the drop down calendar to not show up???
# r says it doesnt recognize the geometry, but it does still produce a graph. 
# st_simplify not working 
  
  area_decades_sum <- fire_causes %>% 
      dplyr::select(decade, area_categorical) %>% 
      filter(area_categorical %in% c("0-1,000" ,  "1,000-5,000"  )) %>% 
      group_by(area_categorical) %>% 
      count() 
  
  area_decades_sum %>% 
      kable(col.names = c("Decade", "Area", "Sum"), align = 'c') %>% 
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "respsonsive"),
        full_width = T,
        position = "center")


  tabPanel("Fire Causes",
           sidebarLayout(
             sidebarPanel("text here",
                          checkboxGroupInput(inputId = "check_fire_causes",
                                             label = "Select Fire Cause(s) to explore:",
                                             choices = c(unique(fire_causes$fire_cause))),
                          sliderInput(inputId = "date_fire_causes1",
                                      label = "Input Year(s)",
                                      min = 1880, max = 2019, value = c(1900,1920),
                                      sep = "")),
             mainPanel("Graph and Table Here",
                       plotOutput(outputId = "fire_causes_graph")))),


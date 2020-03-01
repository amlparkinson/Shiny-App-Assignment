# load packages ------------------------------------------------------------

# General packages
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(dplyr)

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

# add data ----------------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016") %>% 
  st_transform(crs = 4269) #4326

fire_raw <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# sub data ------------------------------------------------------------------------------------------

#test data (limit data size to stop r from freezing)-------------
fire <- fire_raw %>% 
  clean_names() %>% 
  dplyr::filter (acres > 200) %>% 
  mutate (fire_name = str_to_title(fire_name)) %>%
  st_transform(crs = 4269) %>% 
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
  #st_simplify(dTolerance = 10) %>% error. converts geometry to empty values :(
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
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))

#data for fire causes to compare natural vs human casued fires ------------------------------
fire_cause_simplified <- fire_causes %>% 
  mutate (fire_cause_simplified = case_when(
    fire_cause %in% c("Lightning", "Volcanic") ~ "Natural Cause",
    fire_cause %in% c('Equipment Use','Smoking','Campfire','Debris','Railroad','Arson','Playing with Fire',
                      'Miscellaneous','Vehicle','Powerline','Firefighting Training','Non-firefighting Training',
                      'Structure', 'Aircraft', 'Escaped Prescribed Burn', 'Illegal Campfire') ~ "Human Cause",
    fire_cause == "Unknown" ~ "Unknown"
  )) %>% 
  filter (fire_cause_simplified != "Unknown") 

# sum number of yearly fires
fire_causes_simplified_count <- fire_cause_simplified %>% 
  group_by(year, fire_cause_simplified) %>% 
  count() 

#sum yearly acres burned 
fire_causes_simplified_acres <- fire_cause_simplified %>% 
  group_by(year, fire_cause_simplified) %>% 
  summarise(yearly_acres_burned = sum(acres)) %>% 
  st_drop_geometry() 

#join data frames
fire_causes_simplified_count_acres <- inner_join(fire_causes_simplified_count, 
                                                 fire_causes_simplified_acres, by = c("year", "fire_cause_simplified"))

# data for fire sizes ---------------------------------------------------------------------------
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
  st_as_sf() %>% 
  mutate(area_categorical = fct_relevel(area_categorical, levels = c("0-1,000", "1,000-5,000", "5,000-10,000",
                                                                     "10,000-20,000", "20,000-40,000", "40,000-100,000",
                                                                     "100,000-450,000"))) 

# user interface ---------------------------------------------------------

ui <- navbarPage(
  "Navigation Bar!",
   theme = shinytheme("united"),
   tabPanel("Fire History",
            h1("Title"),
            #mainPanel(plotOutput(outputId = "gganimate_map")),
            p("text")),
   tabPanel("Fire Season",
            h1("title")),
   tabPanel("Fire Length",
            h1("title")),
   tabPanel("Fire Size",
            h1("Title"),
            p("text"),
            sidebarLayout(
              sidebarPanel(radioButtons(inputId = "select_area",
                                        label = "Select Fire Size (Acres)",
                                        choices = c("0-1,000", "1,000-5,000", "5,000-10,000", "10,000-20,000", "20,000-40,000", "40,000-100,000", "100,000-450,000"))), #unique(fire_size$area_categorical)
              mainPanel("Graph and Table Here",
                        plotOutput(outputId = "area_graph"),
                        tableOutput(outputId = "area_sum_table"),
                        leafletOutput(outputId = 'size_decades_map')))),
  navbarMenu("Fire Causes",
             tabPanel("All",
                      sidebarLayout(
                        sidebarPanel("text here",
                                     multiInput(inputId = "check_fire_causes",
                                                 label = "Select Fire Cause(s) to explore:",
                                                 choices = c(unique(fire_causes$fire_cause))),
                                     #checkboxGroupInput(inputId = "check_fire_causes",
                                      #                  label = "Select Fire Cause(s) to explore:",
                                       #                 choices = c(unique(fire_causes$fire_cause))),
                                     sliderInput(inputId = "date_fire_causes1",
                                                 label = "Select Range of Year(s)",
                                                 min = 1880, max = 2019, value = c(1880,2019),
                                                 sep = "")),
                        mainPanel("Graph and Table Here",
                                  plotOutput(outputId = "fire_causes_graph"),
                                  leafletOutput('fire_causes_map'),
                                  tableOutput(outputId = 'fire_causes_table')))),
             tabPanel("Natural vs Human Caused",
                      sidebarLayout(
                        sidebarPanel("text",
                                     radioButtons(inputId = "select_count_area",
                                                  label = "Pick:",
                                                  choices = c("Total Annual Fires" = "n",
                                                              "Annual Acres Burned" = "yearly_acres_burned"))),
                        mainPanel("Graph Here",
                                  plotOutput(outputId = "fire_causes_simplified_graph"))
                      )))
)

#server --------------------------------------------------------------------

server <- function(input, output) {
  
  #gganimate map for intro page
  #output$gganimate_map <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border, color = "grey80") +
      geom_sf(data = fire, fill = "firebrick4", alpha = 0.8, color = "firebrick4") +
      theme_classic() +
      theme_map() 
     # labs(title = "Year: {frame_time}") +
     # transition_time(year)
  #})
  
#data frame for the number of fires that occurred per decade grouped by fire size
  area_decades_count <- reactive({
    fire_size %>% 
      filter(area_categorical == input$select_area) %>% 
      group_by(decade, area_categorical) %>% 
      summarize(n = sum())
  })
  
#graph output for fire size per decade 
  output$area_graph <- renderPlot({
    ggplot(data = area_decades_count(), aes(x = decade, y = n)) + 
      geom_col(fill = "firebrick4", alpha= 0.7) +
     # geom_errorbar(aes(x= decade, ymin = )) +
      scale_x_discrete(expand = c(0,0),
                       drop = F) +
      scale_fill_discrete(drop = F) +
      scale_y_continuous(expand = c(0,0)) +
      labs (x = "\nTime", y = "Number of Fires\n") +
      theme_classic() +
      theme(plot.margin = unit(c(5,5,5,5), "lines"))
  })
  
#data frame to total ALL the fire sizes that occurred throughout the fire history
  area_decades_sum <- fire_size %>% 
    group_by(area_categorical) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    rename("Total Number of Fires" = n) %>% 
    rename ("Fire Size" = area_categorical)
  
#table for the total fire sizes
  output$area_sum_table <- renderTable(
    area_decades_sum, 
    striped =T, 
    bordered = T,
    align = 'c'
  )
  
  size_decades_map_data <- reactive({
    fire_size %>% 
      filter(area_categorical == input$select_area)
  })
  
#  output$size_decades_map <- renderPlot({
#    ggplot() +
#      geom_sf(data = ca_border, color = "grey80") +
#      geom_sf(data = size_decades_map_data(), aes(fill = decade), alpha = 0.8, color = "red")
#  })
  
  #output$size_decades_map <- renderLeaflet({
  #   leaflet(size_decades_map_data)
  #})
  
  
  
  
  
# data frame for all fire causes
  fire_causes_count <- reactive({
      fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause, year) %>% 
      count()
  })
  
  fire_causes_count_sum <- reactive ({
    fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause) %>% 
      summarise(sum_fires = n())
  })
# graph for fire causes
  output$fire_causes_graph <- renderPlot({
    ggplot(data = fire_causes_count(), aes(x = year, y = n)) +
      geom_point(aes(color = fire_cause)) +
      geom_line(aes(color = fire_cause)) +
      labs(x = "\nYear", y = "Number of Occurances\n") +
      theme_minimal() +
      scale_color_discrete(name = "Fire Cause") +
      expand_limits(y = 0) +
      scale_y_continuous(expand = c(0,0)) + #limits = c(0,max(variable)+10); input$date_fire_causes1[2], max(fire_causes_count$year)
      scale_x_continuous(expand = c(0,0)) 
  })
  
# table for total fire causes
  output$fire_causes_table <- renderTable({
    fire_causes_count_sum
  })
  #map for fire causes
 # output$fire_causes_map <- renderLeaflet({
 #   tm_shape(data = ca_border) +
 #     tm_fill(data = fire_causes_count())
 # })

  fire_causes_simplified_count_acres_select <- reactive({
    fire_causes_simplified_count_acres %>% 
      dplyr::select(input$select_count_area)
      
  })
  
  count_plot <- ggplot(data = fire_causes_simplified_count_acres, aes(x = year, y = n)) +
    geom_point(aes(color = fire_cause_simplified)) +
    geom_line(aes(color = fire_cause_simplified)) +
    scale_color_discrete(name = "Cause") +
    theme_minimal() +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) +
    labs(x = "\nYear", y = "Number of Occurances\n")
  
  acres_plot <- ggplot(data = fire_causes_simplified_count_acres, aes(x = year, y = yearly_acres_burned)) +
    geom_point(aes(color = fire_cause_simplified)) +
    geom_line(aes(color = fire_cause_simplified)) +
    scale_color_discrete(name = "Cause") +
    theme_minimal() +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) +
    labs(x = "\nYear", y = "Acres Burned\n") # add commas to y axis by installing scales package, then in scale y continous label = comma
  
  
  output$fire_causes_simplified_graph <- renderPlot({
    
   if(input$select_count_area == "Total Annual Fires") {
     print(ggplot(data = fire_cause_simplified_count_acres_select, 
            aes(x = year, y = yearly_acres_burned)) +
       geom_point(aes(color = fire_cause_simplified)) +
       geom_line(aes(color = fire_cause_simplified)))}
    else {print("No")}
  # if(input$select_count_area == "Annual Acres Burned") {print(acres_plot)}
  
    }) # can try to make this reactive by looking at fire size (include all as an option, maybe create new column, then unite it and use str_detect in filter to inlcude all option)
  
}
  
  
#run shiny app --------------------------------------------------------------

shinyApp(ui = ui, server = server)



  area_decades_count <- fire_size %>% 
      filter(area_categorical == "1,000-5,000") %>% 
      group_by(decade, area_categorical) %>% 
      summarize(n = sum())
##issues
# # dropped decades for largest size class
# either mutate fct_relevel or ordered/factor() works to assign levels (even though get the unnamed scalar inpus error using mutate fct_relevel, it still works). but levels show up as unordered numbers in the shiny app bc theyre recognized as factor class, but they appear as their actual names when the class is a character but the levels are unordered
  # fix was to manually enter the size classes
  
## to do
# add percentages at top of bars for fire size graph?


  

  ggplot() +
    geom_sf(data = ca_border, color = "grey80") +
    geom_sf(data = fire_sub, aes(fill = YEAR_), color = NA, alpha = 0.5)
  






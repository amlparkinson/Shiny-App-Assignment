# load packages ------------------------------------------------------------

# General packages
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(dplyr)
library(scales)

# Shiny packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)

# Map and graph packages
library(sf) 
library(gganimate)
library(transformr) #need?
library(magick) #need?
library(tmap)
library(ggthemes)
library(ggridges)

# add data ----------------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016") %>% 
  st_transform(crs = 4326) 

fire_raw <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# sub data ------------------------------------------------------------------------------------------

#test data (limit data size to stop r from freezing)-------------
fire <- fire_raw %>% 
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
  #st_simplify(dTolerance = 10) %>% error. converts geometry to empty values :(
  mutate(year = as.numeric(year))
  

# sub data for fire length and season -------------------------------------

fire_date <- fire %>% 
  drop_na(alarm_date) %>% 
  drop_na(cont_date) %>% 
  mutate (alarm_year = lubridate::year(alarm_date), 
          alarm_month = lubridate::month(alarm_date),
          alarm_day = lubridate::day(alarm_date),
          alarm_day_of_year = lubridate::yday(alarm_date), #** convert mm/dd/yyyy to day of the year(0-365)
          cont_year = lubridate::year(cont_date), 
          cont_month = lubridate::month(cont_date),
          cont_day = lubridate::day(cont_date), 
          cont_day_of_year = lubridate::yday(cont_date)) %>% 
  mutate (length_of_fires = (cont_day_of_year - alarm_day_of_year)) %>%  # doesnt account for fires that stated in one year, but ended in another
  mutate (decade = case_when(
    year < 1939 ~ "1920s-1930s", # combine 1920s adn 1930s data
    year < 1949 ~ "1940s",
    year < 1959 ~ "1950s",
    year < 1969 ~ "1960s",
    year < 1979 ~ "1970s",
    year < 1989 ~ "1980s",
    year < 1999 ~ "1990s",
    year < 2009 ~ "2000s",
    year < 2019 ~ "2010s"
  )) # no alarm date for fires before 1920s

fire_season_post1970 <- fire_date %>% 
  filter(alarm_year %in% c(1970:2019)) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(alarm_year) %>% 
  summarize(min_alarm = min(alarm_month),
            mean_alarm = mean(alarm_month),
            max_alarm = max(alarm_month),
            sample_size = n()) 

mean_season <- ggplot(fire_season_post1970, aes(x = alarm_year, y = mean_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(5, 9),
                     breaks = seq(5, 9, by = 1),
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  labs(x = "", y = "Average Fire Start Month\n")

min_season <-  ggplot(fire_season_post1970, aes(x = alarm_year, y = min_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(1, 8),
                     breaks = seq(1, 8, by = 1),
                     labels = c("Jan", "Feb","Mar","Apr", "May", "Jun", "Jul", "Aug")) +
  labs(x = "", y = "Earliest Fire Start Month\n")

max_season <- ggplot(fire_season_post1970, aes(x = alarm_year, y = max_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(6, 12),
                     breaks = seq(6, 12, by = 1),
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "", y = "Latest Fire Start Month\n")

season_ggridges <- ggplot(fire_date, aes(x = alarm_month, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = "Fire Start Month", y = "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0),
                     lim = c(1, 12),
                     breaks = seq(1,12, by = 1), 
                     labels = c("Jan", "Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_discrete(expand = c(0,0)) 

`# fire length -------------------------------------------------------------

fire_length_sub_pos<- fire_date %>% 
  filter(length_of_fires >= 0) 
fire_length_sub_neg <- fire_date %>% 
  filter(length_of_fires < -100) %>% 
  mutate(length_of_fires = ((365 - alarm_day_of_year) + cont_day_of_year))
fire_length <- rbind(fire_length_sub_pos, fire_length_sub_neg) %>% 
  mutate(alarm_year = as.numeric(alarm_year))

fire_length_no_outliers <- fire_length %>% 
  filter(length_of_fires < 75)

length_no_outliers <- ggplot(fire_length_no_outliers, aes(x = length_of_fires, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = 'Length of Fires', y= "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

length_all <- ggplot(fire_length, aes(x = length_of_fires, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = 'Length of Fires', y= "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

length_season <- ggplot(fire_length, aes(x = alarm_month, y = length_of_fires)) +
  geom_point() +
  theme_minimal() +
  labs(x = "\nFire Start Month", y = "Length of Fires\n") +
  scale_x_continuous(expand = c(0,0),
                     lim = c(1, 12),
                     breaks = seq(1,12, by = 1),
                     labels = c("Jan", "Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))  +
  scale_y_continuous(expand = c(0,0)) 

# data for fire sizes ---------------------------------------------------------------------------
fire_size <- fire %>% 
  mutate(area_categorical = case_when(
    acres < 1000 ~ "200-1,000",
    acres < 5000 ~ "1,000-5,000",
    acres < 10000 ~ "5,000-10,000",
    acres < 20000 ~ "10,000-20,000",
    acres < 40000 ~ "20,000-40,000",
    acres < 100000 ~ "40,000-100,000",
    acres < 500000 ~ "100,000-450,000"
  )) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(decade)) %>% 
  st_as_sf() %>% 
  mutate(area_categorical = as.factor(area_categorical))  %>% 
  mutate(area_categorical = fct_relevel(area_categorical, levels = c("200-1,000", "1,000-5,000", "5,000-10,000",
                                                                     "10,000-20,000", "20,000-40,000", "40,000-100,000",
                                                                     "100,000-450,000"))) 
# data for fire causes --------------------------------------------------------------------------
fire_causes <- fire %>%
  mutate(fire_cause = case_when(
    cause == 0 ~ "Unknown",
    cause == 1 ~ "Lightning",
    cause == 2 ~ "Equipment Use",
    cause == 3 ~ "Smoking",
    cause %in% c(4,19) ~ "Campfire",
    cause == 5 ~ "Debris",
    cause == 6 ~ "Railroad",
    #cause == 7 ~ "Arson",
    cause == 8 ~ "Playing with Fire",
    cause %in% c(9, 16, 7, 12, 13) ~ "Miscellaneous",
    cause == 10 ~ "Vehicle",
    cause == 11 ~ "Powerline",
    #cause == 12 ~ "Firefighting Training",
    #cause == 13 ~ "Non-firefighting Training",
    cause == 14 ~ "Unknown",
    cause == 15 ~ "Structure",
    #cause == 16 ~ "Aircraft",
    cause == 17 ~ "Volcanic",
    cause == 18 ~ "Escaped Prescribed Burn"
    #cause == 19 ~ "Illegal Campfire"
  )) %>% 
  arrange(fire_cause) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year)) 

fire_un <- fire_causes %>% filter(fire_cause == "Unknown")
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
#fire_simplified_count_acres_pivot <- fire_causes_simplified_count_acres %>% 
 # pivot_longer(n, yearly_acres_burned,
  #             names_to = )

# graphs for server
count_plot <- ggplot(data = fire_causes_simplified_count_acres, aes(x = year, y = n)) +
  #geom_point(aes(color = fire_cause_simplified)) +
  geom_line(aes(color = fire_cause_simplified), show.legend = F) +
  geom_area(aes(fill= fire_cause_simplified), position ="identity") + # position="idendity" = critical componenet. otherwise the colored area is VERY off
  scale_fill_discrete(name = "Cause") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0),
                     lim = c(0, 125)) + 
  scale_x_continuous(expand = c(0,0),
                     lim = c(1910, 2020)) +
  labs(x = "\nYear", y = "Number of Occurances\n")

acres_plot <- ggplot(data = fire_causes_simplified_count_acres, aes(x = year, y = yearly_acres_burned)) +
  # geom_point(aes(color = fire_cause_simplified)) +
  geom_line(aes(color = fire_cause_simplified), show.legend = F) +
  geom_area(aes(fill= fire_cause_simplified), position ="identity") +
  scale_fill_discrete(name = "Cause") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0),
                     label = comma,
                     lim = c(0, 1250000)) + 
  scale_x_continuous(expand = c(0,0),
                     lim = c(1910, 2020)) +
  labs(x = "\nYear", y = "Acres Burned\n") 

# gganimate sub-data ---------------------------------------------------------------------------
fire_animate <- fire %>% 
  clean_names() %>%  
  filter(!is.na(year)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1900) %>% 
  filter(year <= 1935) %>% 
  st_as_sf() %>% 
  mutate (fire_name = str_to_title(fire_name))  %>% 
  dplyr::filter (acres > 6000)  %>% 
  mutate(year = as.numeric(year)) 

# user interface ---------------------------------------------------------

ui <- navbarPage(
  "Navigation Bar!",
   theme = shinytheme("united"),
   tabPanel("Fire History",
            h1("Background"),
           # mainPanel(plotOutput(outputId = "gganimate_map")),
            p("This App explores trends in fire history for California. There have been over 21,000 recorded fires in that have burned xx acres. Due to technical 
              difficulties, only fires over 200 acres, which represents 9,584 fires, were included for analysis.
              
              Over 38 million acres have burned in California, representing almost 34% of land in the state. "),
            h1("Data"),
            p("Data was obtained from the Fire and Resource Assessment Program (FRAP) from CalFire (https://frap.fire.ca.gov/frap-projects/fire-perimeters/). 
              Data has been collected since 1880s, but there are relatively very few recorded observations from 1880s to the early 1900s. Likely this data set represents 
              an incomplete record, thus results comparing changes to fire trends from early 1900s should be interpreted with caution.
              To help with this issue, 1880s-early 1900s data was omitted and in some cases combined to form a larger data set. Additionally, due to incomplete data points, 
              some analyses include fires than in the dataset. The total number of fires used in an analysis is noted on each page."),
           h3("Note"),
           p("In order to display maps, graphs, and conduct calculations in real time, fires less than 200 acres were removed from analysis. Technical difficulties made it difficult to conduct analyses 
             and greate maps with such a large data set." )),
   tabPanel("Fire Season and Fire Length",
            h3("Background"),
            p("This page explores trends in fire season and duration that a fire burned, aka fire length. Research has shown that fire season has changed compared to historic norms; fires are starting \
            earlier and ending later. It has been estimated that the fire season has increased by 75 days. Several factors are driving this change, of which climate change is considered the key driver. 
              Hotter temperatures, reduced snowpack, and earlier snowmelt create longer dry seasons with more drought stressed vegetation. These factors in addition to increased vegetation density due to fire supresssion
              also lengthen the duration of fires as more densely packed, drought stressed vegetation is more flamable and thus easier to spread."),
            h3("Data and Methods"),
            p("Alarm date refers to the day a fire started. This data was used to determine trends in fire season. Data on alarm date was available for
              2,480 fires (from the 9,584). There was no data on alarm date available pre-1920 and there were Very few fires from the 1920s-mid 1900s that contained this information. To avoid misinterpreting any results, fires from before 1970 were exlcuded from this 
              analysis."),
            p("Fire length was calculated by converting date data (mm/dd/yyyy) to day of the year (0-365) format, then subtracting containment 
               date (day the fire was declared contained) from alarm date. A few fires started in one year and ended in another. In this case, the following equation was used: (365 - alarm_date) + containment_date).
            A few fires had the containment day recorded as before the alarm day, so these were removed as it was assumed these data points were the result of human error. In total 2,477 fires had accurate data for both
              fire alarm date and containment date."),
            sidebarLayout(
              sidebarPanel(pickerInput(inputId = "select_summary_stat",
                                        label = "Select Summary Statistic to Explore",
                                        choices = c("Earliest Alarm Date " = "min_alarm", 
                                                    "Average Alarm Date" = "mean_alarm", 
                                                    "Last Alarm Date" = "max_alarm", 
                                                    "Distribution of All Alarm Dates" = "all_decade",
                                                    "Relationship Between Fire Length and Fire Season" = "length_and_season",
                                                    "Distribution of All Fire Lengths" = "length_graph_all",
                                                    "Distribution of All Fire Lengths With no Outliers" = "length_graph_no_outliers"),
                                        options(list(style = "btn-danger")))),
              mainPanel("Graph here",
                        plotOutput(outputId = "season_summary_graph"))
              )),
   tabPanel("Fire Size",
            h3("Background"),
            p("This page explores how fire size has changed over the decades. Since fire suppression policies were enacted, vegetation density was allowed to accumulate for decades. Because of this
              fire size and severity has increased. Of California's largest wildfires (> 100,000 acres) 40% occurred after 2000 and 89% occurred after 1970. "),
            h3("Data and Methods"),
            p("Size was calculted for each polygon in ArcGIS. Units are in acres. There are few data points available from 1880s-early 1900s, so those should be 
              interpreted with caution."),
            sidebarLayout(
              sidebarPanel(radioButtons(inputId = "select_area",
                                        label = "Select Fire Size (Acres)",
                                        choices = c("200-1,000", 
                                                    "1,000-5,000", 
                                                    "5,000-10,000", 
                                                    "10,000-20,000", 
                                                    "20,000-40,000", 
                                                    "40,000-100,000", 
                                                    "100,000-450,000"))), #unique(fire_size$area_categorical)
              mainPanel(" ",
                        plotOutput(outputId = "area_graph"),
                        tableOutput(outputId = "area_sum_table"),
                        plotOutput(outputId = 'size_decades_map')))),
  navbarMenu("Fire Causes",
             tabPanel("All",
                      h3("Background"),
                      p("Recent large and deadly wildfires caused by powerlines have taken up a lot of time on the news, but the occurances of these large wildfires are realtively 
                        rare compared to the hundreds of small wildfires (< 1,000 acres) that occur every year but do not make the news. Therefore, less is known about the causes of smaller fires
                        and overall causes of historic fires and how those have changed over the years."),
                      h3("Data and Methods"),
                      p("There are 14 categories for causes of wildfires; exlcuding unknown category. There are 4,420 fires with fire causes recorded; 5,116 fires had an unknown cause."),
                      sidebarLayout(
                        sidebarPanel(" ",
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
                                  tableOutput(outputId = 'fire_causes_table'),
                                  plotOutput('fire_causes_map')))),
             tabPanel("Natural vs Human Caused",
                      h3("Background"),
                      p("This page compares fire occurances and acres burned between human caused and natural fires. Natural fires and fires started by lightening or volcanic 
                        activity. Fires with unknown causes were excluded from this analysis."),
                      sidebarLayout(
                        sidebarPanel(" ",
                                     radioButtons(inputId = "select_count_area",
                                                  label = "Pick:",
                                                  choices = c("Total Annual Fires" = "n",
                                                              "Annual Acres Burned" = "yearly_acres_burned"))),
                        mainPanel("Graph Here",
                                  plotOutput(outputId = "fire_causes_simplified_graph"),
                                  tableOutput(outputId = "fire_simplified_decades_summary"))
                      )))
)

#server --------------------------------------------------------------------

server <- function(input, output) {
  
#gganimate map for intro page
  # output$gganimate_map <- renderPlot({
  #   anim_plot <- ggplot(data = fire_animate) +
  #     `(data = ca_border, color = `"grey80") +``
  #     geom_sf(data = fire_animat```e, aes(fill = decade, color = decade), alpha = 0.8) +
  #     theme_classic() +
  #     theme_map() +
  #     labs(t`qq1`      ease_aes("linear") +
  #     shadow_mark(alpha = 0.3)
  #   
  #   animate(anim_plot, fps = 10, end_pause = 30)
  #   #ssave as gif and call that
  # })
  
# output for summary stats of fire season
  output$season_summary_graph <- renderPlot({
    if (input$select_summary_stat == "min_alarm") {print(min_season)}
    if (input$select_summary_stat == "mean_alarm") {print(mean_season)}
    if (input$select_summary_stat == "max_alarm") {print(max_season)}
    if (input$select_summary_stat == "all_decade") {print(season_ggridges)}
    if(input$select_summary_stat == "length_graph_all") {print(length_all)}
    if(input$select_summary_stat == "length_graph_no_outliers") {print(length_no_outliers)}
    if(input$select_summary_stat == "length_and_season") {print(length_season)}
  })

#data frame for the number of fires that occurred per decade grouped by fire size
  area_decades_count <- reactive ({
    fire_size %>% 
      filter(area_categorical == input$select_area) %>% 
      group_by(decade, area_categorical) %>% 
      count() %>% 
      ungroup(decade, area_categorical)  
     # complete(decade, area_categorical, fill = list(n = 0))
  })
  
#graph output for fire size per decade 
  output$area_graph <- renderPlot({
    ggplot(data = area_decades_count(), aes(x = decade, y = n)) + 
      geom_col(fill = "red4", alpha= 0.7) +
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

# map for size data   
  output$size_decades_map <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border, color = "grey80") +
      geom_sf(data = area_decades_count(), fill = "red4", alpha = 0.8, color = NA) +
      theme_map()
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

# map for fire causes
 output$fire_causes_map <- renderPlot({
   ggplot() +
     geom_sf(data = ca_border) +
     geom_sf(data = fire_causes_count(), aes(fill = fire_cause), color = NA) +
     theme_map() +
     scale_fill_discrete(name="Cause") 
 }) 

# sum fire occurances and acres burned
  fire_causes_count_sum <- reactive ({
    fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause) %>% 
      summarise(sum_fires = n(),
                sum_acres = sum(acres)) %>% 
      mutate(sum_acres = format(round(sum_acres), big.mark=","),
             sum_fires = format(sum_fires, big.mark=",")) %>% 
      st_drop_geometry() %>% 
      rename("Total Fire Occurances" = sum_fires,
             "Total Acres Burned" = sum_acres,
             "Fire Cause" = fire_cause)
  })
  
# table for total fire causes
  output$fire_causes_table <- renderTable({
    fire_causes_count_sum()
  })

  # output$fire_causes_simplified_graph <- renderPlot({
  #   if(input$select_count_area == "n") {print(count_plot)}
  #   else {print(acres_plot)}
  # })
  
  # fire_simplified_decades <- fire_cause_simplified %>% 
  #   group_by(fire_cause_simplified, decade) %>% 
  #   summarise(n = n(),
  #             decade_acres = sum(acres))
  # output$fire_simplified_decades_summary <- renderTable({
  #   fire_simplified_decades()
  # })
}
  

#run shiny app --------------------------------------------------------------

shinyApp(ui = ui, server = server)


##issues
# # dropped decades for largest size class
# either mutate fct_relevel or ordered/factor() works to assign levels (even though get the unnamed scalar inpus error using mutate fct_relevel, it still works). but levels show up as unordered numbers in the shiny app bc theyre recognized as factor class, but they appear as their actual names when the class is a character but the levels are unordered
# fix was to manually enter the size classes
# if else produces one of the graphs. If if statement produces none. Same if put the if statement in the reactive and the data() in the render
# ggplot doesnt appear in app
# error with fire sizes table for lightening: Evaluation error: TopologyException: Input geom 1 is invalid: Ring Self-intersection at or near point -123.72272736189028 41.591593843466249 at -123.72272736189028 41.591593843466249.
  
## to do
# add percentages at top of bars for fire size graph?


#sub data
fire_causes_map_data <- fire_causes %>%
  filter(fire_cause == "Lightning") %>% 
  filter(year >2000)

ggplot() +
  geom_sf(data = ca_border) +
  geom_sf(data = fire_causes_map_data, aes(fill = fire_cause), color = NA) +
  theme_map() +
  scale_fill_discrete(name="Cause")


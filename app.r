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
  
# decades fire map for intro data-----------------------------------------------------------

fire_facet <- fire %>% filter(!is.na(decade)) %>% filter(decade != "1890s")
fire_decade_count <- fire_facet %>% group_by(decade) %>% count()

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

# fire length -------------------------------------------------------------

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
  labs(x = 'Length of Fires (days)', y= "") +
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


# user interface ---------------------------------------------------------

ui <- navbarPage(
  "California Fire",
   theme = shinytheme("united"),
   tabPanel("Fire History",
            h1("Background"),
            p("In some places fire is a natural and essential component of the ecosystem, however fear of wildfire due to its uncontrollability and the threat it posed to human lives and essential industries, such as timber 
             industries, resulted in the passage of fire suppression policies in the early 1900s. While these policies were successful in reducing the number of wildfires, they had unintended consequences that have 
              changed the structure and composition of fire prone ecosystems, such as forests and chaparral. These consequences have created unnaturally dense vegetation which has resulted in larger, more frequent, 
              and more severe wildfires than historic norms."),
            p("This App explores trends in fire history for California. There have been over 21,000 recorded fires from mid 1880s to 2018 that have burned over 38 million acres; this represents 37.6% of land in the state."),
            h1("Data"),
            p("Fire perimeter polygons were obtained from the Fire and Resource Assessment Program (FRAP) from CalFire (https://frap.fire.ca.gov/frap-projects/fire-perimeters/).  Data is available from 1880s to 2018, 
              but there are relatively very few recorded observations from 1880s to the early 1900s. Thus, data from these years do not represent a complete record and comparisons from these years to later years should 
              be interpreted with caution.   To help with this issue, in some cases data from 1880s-early 1900s was omitted. While there were over 21,000 recorded fires, for simplicity only fires over 200 acres, 
              which represents 9,584 fires, were included for analysis."),
            p("Recorded data included fire cause, fire start date, fire containment date, fire name, and fire size in acres. However, not every fire had records of all of these data points. Thus, some analyses include less fires 
              than are in the final dataset. The total number of fires used in an analysis is noted on each page."),
            mainPanel(plotOutput(outputId = "fire_facet_graph", height=1000, width=1000))),
   tabPanel("Fire Season and Fire Length",
            h3("Background"),
            p("This page explores trends in fire season and duration that a fire burned, aka fire length. Research has shown that fire season has changed compared to historic norms; fires are starting \
            earlier and ending later. It has been estimated that the fire season has increased by 75 days. Several factors are driving this change, of which climate change is considered the key driver. 
              Hotter temperatures, reduced snowpack, and earlier snowmelt create longer dry seasons with more drought stressed vegetation. These factors in addition to increased vegetation density due to fire supresssion
              also lengthen the duration of fires as more densely packed, drought stressed vegetation is more flamable and thus easier to spread."),
            h3("Data and Methods"),
            p("Alarm date refers to the day a fire started. This data was used to determine trends in fire season. Data on alarm date was available for
              2,480 fires (from the 9,584). There was no data on alarm date available pre-1920 and there were very few fires from the 1920s-mid 1900s that contained this information. To avoid misinterpreting any results, fires from before 1920 were exlcuded from this 
              analysis. Additionally, fires from 1920 and 1930s were combined into a single category due to small sample sizes."),
            p("Fire length was calculated by converting date data (mm/dd/yyyy) to day of the year (0-365) format, then subtracting containment 
               date (day the fire was declared contained) from alarm date. A few fires started in one year and ended in another. In this case, the following equation was used: (365 - alarm_date) + containment_date).
            A few fires had the containment day recorded as before the alarm day, so these were removed as it was assumed these date entries were the result of human error. In total 2,477 fires had accurate data for both
              fire alarm date and containment date."),
            h3("Results"),
            p("Fire start dates are highly variable over time. Fires are occuring earlier more frequently, but it was not uncommon for the last fires of the year to occur all the way up to December. However, the end of the fire season is occuring slightly later than 
              pre-2000s. The fire season has not ended in September since the mid 1990s. On average though, the average fire start date has not changed much with most fires still occuring in mid summer. However, looking at the fourth graph, there does appear to be a 
              slight shift in the fire season: fire season was variable from the 1920s-1950s, however this could be due to the smaller smaple size, then from 1960s-1990s August was the peak fire season until the 2000s-2010s when the peak fire season shifted to the earlier 
              summer months of June or July. "),
            p("In regards to fire length, unexpectedly the dry summer months have the longest fires. According to the data some fires burned for a very long time (over 100 days). This could be due to human error inputing data, but some fires can burn underground for a very long time even after
              the main aboveground fire was put out. Either way, fires that burned for over 80 days were removed for easier comparison. This showed that from the 1980s, there have been more fires burning longer than historic norms. "),
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
              mainPanel("",
                        plotOutput(outputId = "season_summary_graph")))),
   tabPanel("Fire Size",
            h3("Background"),
            p("This page explores how fire size has changed over the decades. Since fire suppression policies were enacted, vegetation density was allowed to accumulate for decades. Because of this
              fire size and severity has increased."),
            h3("Data and Methods"),
            p("Fire size was calculted in ArcGIS for each fire scar greater than 200 acres There are few recorded fires available from 1880s-early 1900s, so those should be 
              interpreted with caution."),
            h3("Results"),
            p("Fires greater than 10,000 acres have been occuring more frequently in the 2000s and 2010s. Of California's largest wildfires (> 100,000 acres) 40% occurred after 2000 and 89% occurred after 1970."),
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
                      p("Recent large and deadly wildfires caused by powerlines typically dominate the news cycle, but the occurances of these large wildfires are realtively 
                        rare compared to the hundreds of small wildfires (< 1,000 acres) that occur every year but do not make the news. Therefore, less is known about the causes of smaller fires
                        and overall causes of historic fires and how those have changed over the years."),
                      h3("Data and Methods"),
                      p("There are 14 categories for causes of wildfires; exlcuding unknown category. There are 4,420 fires with fire causes recorded; 5,116 fires had an unknown cause."),
                      h3("Results"),
                      p("It is difficult to draw definitive conclusions from this data as over half of the fires had to be removed because their fire cause was listed as unknown and inclusion of these fires, if their 
                        cause was known, could change any results. However, fires in the 2000s and 2010s, especially those with less obscure causes like lightning  powerlines, are likely well documented."),
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
                        mainPanel("",
                                  plotOutput(outputId = "fire_causes_graph"),
                                  tableOutput(outputId = 'fire_causes_table'),
                                  plotOutput('fire_causes_map')))),
             tabPanel("Natural vs Human Caused",
                      h3("Background and Methods"),
                      p("This page compares fire occurances and acres burned between human caused and natural fires. Natural fires are fires started by lightening or volcanic 
                        activity. Fires with unknown causes were excluded from this analysis."),
                      h3("Results"),
                      p("Annual acres burned is highly variable for both natural and human caused fires. From the early 1900s to the mid 1970s, the number of fires and acres burned remained realitvely low with most caused by humans. 
                        Starting in the mid 1970s, acres burned and number of fires increased from both natural and human causes but still remained highly variable. In some years, naturally caused fires burned more than human caused fires."),
                      sidebarLayout(
                        sidebarPanel(" ",
                                     radioButtons(inputId = "select_count_area",
                                                  label = "Pick:",
                                                  choices = c("Total Annual Fires" = "n",
                                                              "Annual Acres Burned" = "yearly_acres_burned"))),
                        mainPanel("",
                                  plotOutput(outputId = "fire_causes_simplified_graph"),
                                  tableOutput(outputId = "fire_simplified_decades_summary"))))),
  tabPanel("Author Information",
           h2("Author"),
           p("Anne-Marie Parkinson"),
           h2("Contact Information"),
           p("aparkinson@bren.ucsb.edu"),
           h2("Publication Date"),
           p("December 27, 2020"))
)

#server --------------------------------------------------------------------

server <- function(input, output) {
  
#intro facet wrap graph
  output$fire_facet_graph <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border, color = "grey80") +
      geom_sf(data = fire_facet, fill = "red4", color = "red4") +
      theme_classic() +
      theme_map () +
      facet_wrap(~decade)
  })
  
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
    rename ("Fire Size (acres)" = area_categorical)
  
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
      geom_sf(data = area_decades_count(), aes(fill = decade), alpha = 0.8, color = NA) +
      theme_map()
  })
  
# data frame for all fire causes
  fire_causes_count <- reactive({
      fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause, year) %>% 
      summarize(acres_count = sum(acres))
  })
  
# graph for fire causes
  output$fire_causes_graph <- renderPlot({
    ggplot(data = fire_causes_count(), aes(x = year, y = acres_count)) +
      geom_point(aes(color = fire_cause)) +
      geom_line(aes(color = fire_cause)) +
      labs(x = "\nYear", y = "Acres Burned\n") +
      theme_minimal() +
      scale_color_discrete(name = "Fire Cause") +
      expand_limits(y = 0) +
      scale_y_continuous(expand = c(0,0),
                         label = comma) + #limits = c(0,max(variable)+10); input$date_fire_causes1[2], max(fire_causes_count$year)
      scale_x_continuous(expand = c(0,0),
                         lim = c(1880, 2020)) 
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

  output$fire_causes_simplified_graph <- renderPlot({
    if(input$select_count_area == "n") {print(count_plot)}
    else {print(acres_plot)}
  })
  
}
  

#run shiny app --------------------------------------------------------------

shinyApp(ui = ui, server = server)



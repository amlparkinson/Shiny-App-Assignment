# load packages ------------------------------------------------------
library(tidyverse)
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

# load data ---------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016")
fire <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# clean data --------------------------------------------------------------

fire <- fire %>% 
  clean_names() %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() %>% 
  mutate (fire_name = str_to_title(fire_name)) #str_to_title converts observations to first letter capitalized other letters lowercase

# small sample of main data set to use to creat the code so r wont freeze as often---------------------

fire <- fire %>% 
  clean_names() %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() %>% 
  mutate (fire_name = str_to_title(fire_name)) %>% 
  dplyr::filter (acres > 200) %>% 
  st_simplify(dTolerance = 100) %>% 
  mutate(year = as.numeric(year)) 

plot(fire)

# parse dates and calculate how many days the fire burned -----------------------------------   

fire <- fire %>% 
  mutate (alarm_year = lubridate::year(alarm_date), 
          alarm_month = lubridate::month(alarm_date),
          alarm_day = lubridate::day(alarm_date),
          alarm_day_of_year = lubridate::yday(alarm_date), #** convert mm/dd/yyyy to day of the year(0-365)
          cont_year = lubridate::year(cont_date), 
          cont_month = lubridate::month(cont_date),
          cont_day = lubridate::day(cont_date), 
          cont_day_of_year = lubridate::yday(cont_date)) %>% 
  mutate (length_of_fires = (cont_day_of_year - alarm_day_of_year)) # doesnt account for fires that stated in one year, but ended in another

#there's many missing dates. deal with this later
# if (alarm_month = cont_month, then mutate(length_of_fire = (cont_day - alarm_day)(+1??))), else

  fire_yearly_data <- fire %>% 
    drop_na(year) %>% 
    mutate (decade = case_when(
      year < 1900 ~ "pre-1900",
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
    ))
  # 77 fires had no year recorded
  
  fire_area <- fire %>% 
    mutate(area_categorical = case_when(
      #acres < 200 ~ "<200",
      acres < 1000 ~ "0-1,000",
      acres < 5000 ~ "1,000-5,000",
      acres < 10000 ~ "5,000-10,000",
      acres < 20000 ~ "10,000-20,000",
      acres < 40000 ~ "20,000-40,000",
      acres < 100000 ~ "40,000-100,000",
      acres < 500000 ~ "100,000-450,000"
    ))
  
# gganimate -------------------------------------------------------------------------------------

ggplot() +
    #geom_sf(data = ca_border, color = "grey80") +
    geom_sf(data = fire, fill = "red", alpha = 0.8, color = "red") +
    theme_classic() +
    theme_map() +
    labs(title = "Year: {frame_time}") +
    transition_time(year)# not working. Error: stat_sf requires the following missing aesthetics: geometry

ca_fires_tmap <- tm_basemap("Esri.WorldImagery") +
  tm_shape(fire_causes) +
  tm_fill(palette = "red", alpha = 0.7)
tmap_mode("view")
ca_fires_tmap


# count things ------------------------------------------------

decade_fires <- fire_yearly_data %>% 
  group_by(decade) %>% 
  count()

yearly_fires <- fire_yearly_data %>% 
  group_by(year) %>% 
  count()

fire_causes_top <- fire %>% 
  group_by(cause) %>% 
  count()

fire_area_count <- fire_area %>% 
  group_by(area_categorical) %>% 
  count()

# fire causes ---------------------------------------------------------------

#sub-data
fire_causes <- fire2 %>%
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
  st_make_valid()


# count 
fire_causes_simplified_count <- fire_causes %>% 
  group_by(fire_cause_simplified, year) %>% 
  count()

# kable extra
fire_causes_simplified_count %>% 
  kable(col.names = c("Cause", "Count"), align = 'c') %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "respsonsive"),
    full_width = T,
    position = "center"
  )
  
  

# graph
ggplot(data = fire_causes, aes(x = fire_cause)) +
  geom_bar(fill = "dark red") +
  coord_flip() +
  labs (x = NULL,
        y = NULL,
        title = 'TBD') +
  theme_bw() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) 
  #geom_text(aes(label = ))) # is there a way to add label over the bars to display their count? maybe can do this when have reactive data? maybe can set up the data frame to be based off of a table and can use the count from there
  

# dist of acreage ----------------------------------------------------

ggplot(fire, aes(x = acres)) +
  geom_histogram()

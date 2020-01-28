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
fire <- fire %>% lwgeom::st_make_valid() %>% sf::st_collection_extract()
plot(fire)
# clean data --------------------------------------------------------------

fire <- fire %>% 
  clean_names() %>% 
  mutate (fire_name = str_to_title(fire_name)) %>%   #str_to_title converts observations to first letter capitalized other letters lowercase
  dplyr::select(-comments, -agency, -unit_id, -state, -inc_num, -shape_length)
  
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

# check to see if plot works with smaller subset

sub_fires <- fire %>% filter (Acres > 1000)
plot(sub_fires, max.plot = 19)

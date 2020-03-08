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
library(ggridges)
# load data ---------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016")
fire <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# clean data --------------------------------------------------------------

fire_1 <- fire_raw %>% 
  clean_names() %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() %>% 
  mutate (fire_name = str_to_title(fire_name)) #str_to_title converts observations to first letter capitalized other letters lowercase

fire_ac <- fire_1 %>% 
  summarise(sumss = sum(acres))

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

# how many data point have alarm date data?
fire_count_alarm <- fire %>% 
  drop_na(alarm_date)
fire_count_cont <- fire %>% 
  drop_na(cont_date)
fire_count_alarm_cont <- fire %>% 
  drop_na(alarm_date) %>% 
  drop_na(cont_date)

#range of data points
min(fire_count_alarm_cont$year)
max(fire_count_alarm_cont$year)
ggplot(fire_count_alarm_cont, aes(x = year)) +
  geom_histogram() # very few data points before 1950. can probably average those

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
  )) %>%  # no alarm date for fires before 1920s
  mutate(alarm_month_abb = month.abb[alarm_month])
  
fire_count <- fire_date %>% group_by(decade) %>% count()

# very few data points have containment day entered BEFORE the alarm date. remove those.

#fire season. very few fires from before 1970 had complete record of alarm adn containment dates, so combined those into two grpups (1920s-1939, 1940-1969)
fire_season_pre1939 <- fire_date %>% 
  filter(alarm_year < 1939) %>% 
  summarize(min_alarm = min(alarm_month),
            mean_alarm = mean(alarm_month),
            max_alarm = max(alarm_month),
            sample_size = n()) %>% 
  mutate(alarm_year = "1939")

fire_season_pre1969 <- fire_date %>%
  filter(alarm_year %in% c(1940:1969)) %>% 
  summarize(min_alarm = min(alarm_month),
            mean_alarm = mean(alarm_month),
            max_alarm = max(alarm_month),
            sample_size = n()) %>% 
  mutate(alarm_year = "1969")

fire_season_post1970 <- fire_date %>% 
  filter(alarm_year %in% c(1970:2019)) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(alarm_year) %>% 
  summarize(min_alarm = min(alarm_month),
            mean_alarm = mean(alarm_month),
            max_alarm = max(alarm_month),
            sample_size = n()) 

fire_season_summary <- rbind(fire_season_pre1939, fire_season_pre1969, fire_season_post1970) 
  

ggplot(fire_season_post1970, aes(x = alarm_year, y = mean_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                   lim = c(5, 9),
                   breaks = seq(5, 9, by = 1),
                   labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  labs(x = "", y = "Average Fire Start Month\n")

ggplot(fire_season_post1970, aes(x = alarm_year, y = min_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(1, 8),
                     breaks = seq(1, 8, by = 1),
                     labels = c("Jan", "Feb","Mar","Apr", "May", "Jun", "Jul", "Aug")) +
  labs(x = "", y = "Earliest Fire Start Month\n")

ggplot(fire_season_post1970, aes(x = alarm_year, y = max_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(6, 12),
                     breaks = seq(6, 12, by = 1),
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "", y = "Latest Fire Start Month\n")

ggplot(fire_date, aes(y = alarm_year, x = alarm_month)) +
  geom_point() # weird graph

# doesnt look like anything has changed. probs too much annual variation. Look at average for a decade
ggplot(fire_date, aes(x = alarm_month, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = "Fire Start Month", y = "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0),
                     lim = c(1, 12),
                     breaks = seq(1,12, by = 1), 
                     labels = c("Jan", "Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_discrete(expand = c(0,0)) 
 # scale_y_discrete(limits = rev(fire_season_post1970$decade)) # not working


#ggplot(fire_season_post1970, aes(x = acres, y = decade)) +
#  geom_density_ridges(aes(fill = decade)) # jnot helpful


# 
fire_season_post1970 <- fire_date %>% 
  filter(alarm_year %in% c(1970:2019)) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(decade)
  
# fire length  ------------------------------------------------------------
fire_length_sub_pos<- fire_date %>% 
  filter(length_of_fires >= 0) 

fire_length_sub_neg <- fire_date %>% 
  filter(length_of_fires < -100) %>% 
  mutate(length_of_fires = ((365 - alarm_day_of_year) + cont_day_of_year))

fire_length <- rbind(fire_length_sub_pos, fire_length_sub_neg) %>% 
  mutate(alarm_year = as.numeric(alarm_year))

fire_length_no_outliers <- fire_length %>% 
  filter(length_of_fires < 100)

fire_length_mod <- fire_length %>% filter (length_of_fires >=1)

ggplot(fire_length, aes(x = length_of_fires, y = acres)) +
  geom_point() # huh. weird graph

(cont_day_of_year - alarm_day_of_year)

length_no_outliers <- ggplot(fire_length_no_outliers, aes(x = length_of_fires, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = 'Length of Fires', y= "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

ggplot(fire_length_mod, aes(x = length_of_fires, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = 'Length of Fires', y= "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

ggplot(fire_length_mod, aes(x = alarm_month, y = length_of_fires)) +
  geom_point() +
  theme_minimal() +
  labs(x = "\nFire Start Month", y = "Length of Fires\n") +
  scale_x_continuous(expand = c(0,0),
                     lim = c(1, 12),
                     breaks = seq(1,12, by = 1),
                     labels = c("Jan", "Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))  +
  scale_y_continuous(expand = c(0,0)) 
# toher sub data---------------------------------------
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
  
# map -------------------------------------------------------------------------------------

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



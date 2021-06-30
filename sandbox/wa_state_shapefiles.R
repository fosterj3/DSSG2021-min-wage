library(tidyverse)
library(sf)
library(RColorBrewer)


# create a couple temp files
temp <- tempfile()
temp2 <- tempfile()

# download the zip folder from the internet save to 'temp'
download.file(
  "https://ofm.wa.gov/sites/default/files/public/legacy/pop/geographic/tiger10/tract10.zip",
  temp)

# unzip the contents in 'temp' and save unzipped content in 'temp2'
unzip(zipfile = temp, exdir = temp2)

# finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
census_tract10_shp <- list.files(temp2, pattern = ".shp$", full.names = TRUE)

# read the shapefile as simple features
# see: https://r-spatial.github.io/sf/
census_tract_10_sf <- sf::read_sf(census_tract10_shp)

names(census_tract_10_sf)
glimpse(census_tract_10_sf)

# subset to what we want
# King = 033
# Snohomish = 061
# Pierce = 053
project_county_fp <- c("033", "061", "053")

# only get tracts we need for the project
sub_tracts <- census_tract_10_sf %>%
  filter(COUNTYFP10 %in% project_county_fp)

# visualize tracts
ggplot() +
  geom_sf(data = sub_tracts,aes(fill = POPWHITE)) + 
  scale_fill_distiller(palette = "Purples", trans = "reverse") + 
  labs(fill = "population", title = "Overall population in Snohomish, King and Pierce County", subtitle = "Washington State") +
  theme_void()



#Wrangle Data to make it Long - doing this for Race columns  
sub_tract_long <- pivot_longer(sub_tracts, cols = POPWHITE:POPTWO2, names_to = "race", values_to = "race_value")
sub_tract_long <- sub_tract_long %>% mutate(race = str_remove(race,".*POP" ))
sub_tract_long <- sub_tract_long %>% mutate(race = str_remove(race,"\\d")) #I'm not sure what the digits mean
sub_tract_long <- sub_tract_long %>% mutate(race = str_to_title(race))


# visualize tracts by race 
ggplot() +
  geom_sf(data = sub_tract_long,aes(fill = race_value, geometry = geometry)) + 
  scale_fill_distiller(palette = "Spectral", trans = "reverse") + 
  labs(fill = "population", title = "Overall population in Snohomish, King and Pierce County", subtitle = "Washington State") +
  theme_void() +
  facet_wrap(~race)


ggplot() +
  geom_sf(data = census_tract_10_sf,aes(fill = POPWHITE)) + 
  scale_fill_distiller(palette = "Purples", trans = "reverse") + 
  labs(fill = "population", title = "") +
  theme_void() + theme(legend.position = "none")




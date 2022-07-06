#--------------------------------------------------------------------------------
# Flavia Trigo, created on 05/May/2021
# last edited: 27/Aug/2021
#--------------------------------------------------------------------------------

# set wd
if (Sys.info()[7] == "fcb5018") {
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7] == "Flavia") {
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7] == "tom") {
  setwd("/Users/tom/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
}

# clear global env
rm(list = ls())

#install.packages(c("tmap", "tmaptools"))
library(tidyverse)
library(tmap) # for function geocode_OSM
library(tmaptools) # for function geocode_OSM
library(egg) # for theme_article

#Load data
data <-
  read_csv(
    "../2.chapter/2.data_cleaned/NHM-MZUSP_cleaned.csv",
    # handles brazilian writing (i.e. ç, ã)
    locale = locale(encoding = "utf-8")
  ) %>%
  filter(!is.na(species)) %>%
  mutate(bird_id = as.factor(bird_id))


# -----------------------------------------------------------------
## package ggmap - function goecode - Geocodes (finds latitude and longitude of) a location using the
## Google Geocoding API. Note: To use Google's Geocoding API, you must first enable the API in the
## Google Cloud Platform Console. See ?register_google.
#?register_google is paid, so instead I will try using open street map instead of google (function geocode_OSM)
# ------------------------------------------------------------------

## 1) Calculate gps with geocode function ####----------------------------------------------------------------
# for all observ in data, apply geocode function to each and return coords.
for (i in 1:nrow(data)) {
  print(i)
  if (is.na(data$lon_dd[i])) {
    # try ignores errors and warning messages, which I get because some locations can't be found
    # function geocode will run for each row in data$location, and will return Lat/Long in WGS84
    x <-
      try(geocode_OSM(
        data$location[i],
        projection = 4326,
        return.first.only = TRUE,
        details = F
      ))
    # when the result is a list it means it found coords for the location, so get coords and store it in columns lat_dd/lon_dd
    if (is.list(x)) {
      data$lon_dd[i] <- x$coords['x']
      data$lat_dd[i] <- x$coords['y']
      # if result is not a list then it means is NULL or a character (error or warning message), so store it
      # as NA
    } else {
      data$lon_dd[i] <- NA
      data$lat_dd[i] <- NA
    }
  }
  
}

# check if there are NA coords
table(is.na(data$lat_dd))
table(is.na(data$lon_dd))
# cool! all false.

NAS <- data %>%
  filter(is.na(lat_dd))
 
fn <- './2.data_cleaned/NHM-MZUSP_with_coordinates.csv'
write.csv(data, fn, row.names = FALSE)

## 2) Check if the points fall within Brazil (plot) #####------------------------------------------------------------------------------

library(sf)
map_brazil <-
  st_read('./2.data_raw/other_map_files/Estados_Brasil.shp')

loc_sf <- data %>%
  dplyr::select(bird_id, location, lon_dd, lat_dd) %>%
  drop_na() %>%
  st_as_sf(
    coords = c('lon_dd', 'lat_dd'),
    crs = 4326,
    remove = FALSE
  )

# plot map and points
p <-  ggplot() +
  # add Brazil map
  geom_sf(data = map_brazil, fill = "grey", color = "grey") +
  # add study locations onto map
  geom_sf(data = loc_sf, fill = NA) +
  # geom_text(
  #   data = loc_sf,
  #   aes(x = lon_dd, y = lat_dd, label = bird_id),
  #   size = 5,
  #   check_overlap = TRUE
  # ) +
  coord_sf() +
  theme_article() +
  theme(legend.position = "none")

ggsave(
  "./2.results/plots/map_locations.png",
  p,
  width = 7,
  height = 7,
  units = "in"
)

ggsave(
  "./2.results/plots/map_locations.pdf",
  p,
  width = 7,
  height = 7,
  units = "in"
)


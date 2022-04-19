#--------------------------------------------------------------------------------
# Flavia Trigo, created on 05/Aug/2021
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

library(sf)
library(units)
library(tidyverse)

## ---------------------------------------------------------
## projection
## ---------------------------------------------------------

saaeac <- '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'

## ---------------------------------------------------------
## B) LOAD THE BIRD SPECIES MAPS, MAKING SURE THEY ARE A 
##    SUPERSET OF THE SPECIES FROM THE OCCUPANCY DATA
## ---------------------------------------------------------

# load the cleaned species ranges in saaeac

# check for study species: load the occupancy data and make it spatial
occupancy <-
  read.csv('./2.data_cleaned/NHM-MZUSP_with_coordinates.csv') %>%
  # change from " " to "_" for consistency with my data
  mutate(species = str_replace(species, " ", "_")) %>%
  # coerce bird_id into factor
  mutate(bird_id = as.factor(bird_id)) %>%
  # transform coordinate columns into simple features (WGS84 projection)
  st_as_sf(
    coords = c('lon_dd', 'lat_dd'),
    crs = 4326,
    remove = FALSE
  ) %>%
  # transform projection from WGS84 into SAAEAC
  st_transform(crs = saaeac)

# create vector to filter out the ranges and continental margins based on my study species
study_species <- occupancy$species
study_species <- unique(study_species)

# load the cleaned species ranges in saaeac
birds <- st_read('./2.data_cleaned/bird_maps/cleaned_ranges.shp') %>%
  # change from " " to "_" for consistency with my data
  mutate(SCINAME = str_replace(SCINAME, " ", "_")) %>%
  # filter shapefile by the species I need
  filter(SCINAME %in% unique(study_species)) %>%
  # transform into saaeac
  st_transform(crs = saaeac) %>%
  # rename column to match the occupancy data
  rename(species = SCINAME)

## load the buffered continental margins
land_buff <- st_read("./2.data_cleaned/bird_maps/coastline/new_world_continental_coastline_buffered.shp") %>%
  st_transform(crs = saaeac)

# check the data and coastlines match up
plot(st_geometry(occupancy))
plot(land_buff, border='red', col=NA, add=TRUE)


## ---------------------------------------------------------
## D) EXTRACT THE CONTINENTAL MARGIN OF SPECIES RANGES
##    SUPERSET OF THE SPECIES FROM THE OCCUPANCY DATA
## ---------------------------------------------------------

# There is an awful lot of irrelevant range data in the cleaned
# ranges: global distributions of species that occur in the ACF
# and ranges of maritime species that occur along the coast. 
# This operation removes that - we only need terrestrial margins
# for the calculations

# convert polygon ranges to lines - we don't want to create closed
# polygons of range, just get the margins as lines
birds_mln <- st_cast(birds, 'MULTILINESTRING', do_split=FALSE) %>%
  st_transform(crs=saaeac)

all(st_is_valid(birds_mln))

# get the coastal margin 
# this will throw out a warning, but this is okay to be ignored at this scale
coastal_margin <- st_difference(birds_mln, st_geometry(land_buff))

# write that data out to file
st_write(coastal_margin, './2.data_cleaned/bird_maps/coastline/Bird_coastal_range_margins.shp', delete_layer=TRUE)

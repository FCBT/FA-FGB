#--------------------------------------------------------------------------------
# Flavia Trigo, created on 04/Jun/2021
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

#install.packages(c("raster", "sf", "units", "tidyverse"))
library(sf)
library(units)
library(tidyverse)
library(egg)
library(gridExtra)

## 1) set projection saaeac ##---------------------------------------------------------------------------
saaeac <-
  '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
#--------------------------------------------------------------------------------------

## 2) load data #########

# check for study species: load the occupancy data (in WGS84)
occupancy <-
  read_csv('./2.data_cleaned/NHM-MZUSP_with_coordinates.csv') %>%
  # change from " " to "_" for consistency with my data
  mutate(species = str_replace(species, " ", "_")) %>%
  # get rid of any duplicates
  filter(!duplicated(bird_id)) %>%
  # coerce bird_id into factor
  mutate(bird_id = as.factor(bird_id)) %>%
  # transform coordinate columns into simple features (WGS84 projection)
  st_as_sf(
    coords = c('lon_dd', 'lat_dd'),
    crs = 4326,
    remove = FALSE
  ) %>%
  # transform projection from WGS84 into SAAEAC
  st_transform(crs = saaeac)%>%
  dplyr::select(-c(9:20))

# create vector to filter out the ranges and continental margins based on my study species
study_species <- occupancy$species
study_species <- unique(study_species) 
# load the cleaned species ranges from David (NEE paper), but only for the 24 sp I need
ranges <-
  st_read('./2.data_cleaned/bird_maps/cleaned_ranges.shp') %>%
  # change from " " to "_" for consistency with my data
  mutate(SCINAME = str_replace(SCINAME, " ", "_")) %>%
  # filter shapefile by the species I need
  filter(SCINAME %in% study_species) %>%
  # transform into saaeac
  st_transform(crs = saaeac) %>%
  # rename column to match the occupancy data
  rename(species = SCINAME)

# load continental margins from NEE paper
continental_margin <-
  st_read('./2.data_cleaned/bird_maps/Bird_continental_range_margins.shp') %>%
  # change from " " to "_" for consistency with my data
  mutate(SCINAME = str_replace(SCINAME, " ", "_")) %>%
  # filter shapefile by the species I need
  filter(SCINAME %in% study_species) %>%
  # transform into saaeac
  st_transform(crs = saaeac) %>%
  # rename column to match the occupancy data
  rename(species = SCINAME)

# load continental margins from NEE paper
coastal_margin <-
  st_read('./2.data_cleaned/bird_maps/coastline/Bird_coastal_range_margins.shp') %>%
  # filter shapefile by the species I need
  filter(species %in% study_species) %>%
  # transform into saaeac
  st_transform(crs = saaeac)
  
coastal_margin <- st_transform(coastal_margin, crs = saaeac)

# 3) calculate distances ##########

# create new columns in dataset
occupancy$distance <- NA
occupancy$inrange <- NA
occupancy$in_costal_margin <- NA

for (i in 1:nrow(occupancy)) {
  # create index with species name to match distance being calculated in the next step
  j <- which(occupancy$species[i] == continental_margin$species)
  
  # calculate distance between bird coordinate and continental margin for that species
  occupancy$distance[i] <-
    st_distance(occupancy$geometry[[i]], continental_margin$geometry[[j]])
  # calculate distance between bird coordinate and the buffered range (= 0 means inside the range; !=0 means outside the range)
  occupancy$inrange[i] <-
    st_distance(occupancy$geometry[[i]], ranges$geometry[[j]]) == 0
  
  # if the specimen is not inside the range:
  if (occupancy$inrange[i] == FALSE) {
    # create a limit distance of 25km that will still be considered within 
    # the range (like a buffer, this is to account for points that fall within 
    # inlets near the coastline which results in this points being considered outside 
    # the range)
    costal_outlier <-
      st_is_within_distance(occupancy$geometry[[i]], coastal_margin$geometry[[j]], dist = 25000)
    # transform the outcome from st_is_within_distance into logical (if greater then zero = TRUE)
    occupancy$in_costal_margin[i] <- lengths(costal_outlier) > 0
    
  } else {
    #if the specimen is already inside the range, just paste TRUE
    occupancy$in_costal_margin[i] <- TRUE
  }
  
}

# add sign (positive numbers = inside the range, negative numbers = outside the range)
occupancy <- occupancy %>%
  mutate(distance_sgn = ifelse(in_costal_margin, distance,-distance)) %>%
  mutate(dist_km = distance_sgn / 1000)

data2save <- occupancy
# remove geometry column
data2save$geometry <- NULL

fn <- "./2.data_cleaned/NHM-MZUSP_dist2edge_data.csv"
write_csv(data2save, fn)

# 4) plot hist of distances #################
hist_dist <- ggplot(data = occupancy, aes(x = dist_km)) +
  geom_histogram() +
  xlab("Distances from the range edge (km)") +
  theme_article()

range(occupancy$dist_km)
# -705.7041 2312.7123

#save png
ggsave(
  "./2.results/plots/hist_distances.png",
  hist_dist,
  height = 7,
  width = 10,
  units = "in"
)
#save pdf
ggsave(
  "./2.results/plots/hist_distances.pdf",
  hist_dist,
  height = 7,
  width = 10,
  units = "in"
)

# check species that are more than 100km outside the range
too_far_out <- occupancy %>%
  filter(dist_km <= -100)

table(too_far_out$source)
# 22 observations, 7 from MZUSP, 15 from NHM

far_away <- "./2.data_cleaned/NHM-MZUSP_more_than_100km_edge.csv"
write_csv(too_far_out, far_away)

## 5) plot map with locations ####

map_brazil <-
  st_read("./2.data_raw/other_map_files/Estados_Brasil.shp")

## save multiple plots to a single pdf
pdf("./2.results/plots/species_inrange_withdate.pdf",
    onefile = TRUE)
sp <- unique(occupancy$species)
for (i in 1:length(sp)) {
  ranges_subset <- ranges %>%
    filter(species == sp[i])
  occupancy_subset <- occupancy %>%
    mutate(from1985 = ifelse(date_collected >= 1985, TRUE, FALSE)) %>%
    filter(species == sp[i])
  inrange_subset <- occupancy %>%
    filter(species == sp[i],
           in_costal_margin == FALSE)
  
  p <- ggplot() +
    geom_sf(data = map_brazil,
            fill = "lightsteelblue2",
            color = "lightsteelblue4") +
    geom_sf(data = ranges_subset,
            fill = "palegreen3",
            alpha = 0.5) +
    geom_sf(data = occupancy_subset,
            aes(colour = in_costal_margin, shape = from1985),
            size = 2) +
    scale_color_manual(values = c("TRUE" = "springgreen4", "FALSE" = "red3")) +
    scale_shape_manual(values = c("TRUE" = 22, "FALSE" = 24)) +
    labs(color = "inside the range", shape = " >= 1985") +
    geom_text(
      data = inrange_subset,
      aes(
        x = lon_dd,
        y = lat_dd,
        label = round(dist_km, 2)
      ),
      nudge_y = -0.8,
      size = 4,
      check_overlap = TRUE
    ) +
    ggtitle(sp[i]) +
    coord_sf() +
    theme_article()
  grid.arrange(p)
}
dev.off()

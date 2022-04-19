### script modified from David - 'Geoprocessing_range_distances.R' ### 


library(raster)
install.packages('reshape')
library(reshape)
library(sf)
library(units)
library(tidyverse)

## ---------------------------------------------------------
## A) PROJECTION CHOICE
## ---------------------------------------------------------

# This is the usual projection choice nightmare. We are primarily interested
# in distances, which are not typically well preserved on large scale maps. 
# Ideally we want to be working with geodesic distances, but these are likely
# to give a performance hit in calculations and also st_buffer does not work
# correctly on geographic coordinates for getting the coastal regions. 
# Having said that, it doesn't really work sensibly to buffer any projected 
# coordinates on the new world continental scale, since the shapes and 
# distances get pretty distorted. 

# We care most about precision in the local distances around the SA coastal 
# forest so do distance calculations on South America Albers Equal Area Conic 
# projection. Distances get transformed anyway for analysis, which will 
# mitigate errors at trans-continental scales? 

saaeac <- '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'

# http://projectionwizard.org/ suggests Azimuthal equidistant as a 
# equidistant projection at hemisphere scale
# az_eqd <- '+proj=aeqd +lat_0=5.6 +lon_0=-94.2'


## ---------------------------------------------------------
## B) LOAD THE BIRD SPECIES MAPS, MAKING SURE THEY ARE A 
##    SUPERSET OF THE SPECIES FROM THE OCCUPANCY DATA
## ---------------------------------------------------------

# load the cleaned species ranges in WGS84 and transform
ranges <- st_read('./2.data_cleaned/bird_maps/cleaned_ranges.shp')

# check for study species: load the occupancy data and make it spatial
my_occupancy <- read.csv('./2.data_cleaned/NHM-MZUSP_data_gps_complete.csv')
my_occupancy$Bird_ID <- base::as.factor(my_occupancy$Bird_ID)

#occupancy <- st_as_sf(occupancy, coords=c('Lon','Lat'), crs=4326)
my_occupancy <- my_occupancy %>%
  # get rid of NA in Lat, Lon, and FA
  drop_na(Lat, Lon, TotalFA) %>%
  # coerce into character to be able to transform into simple feature in the next step
  mutate(Lat = as.character(Lat),
         Lon = as.character(Lon))

my_occupancy <- st_as_sf(my_occupancy, coords = c('Lon', 'Lat'), crs=4326)

str(my_occupancy)

#occupancy <-st_transform(occupancy, saaeac)
my_occupancy <- st_transform(my_occupancy, saaeac)

# get the species names and format to match BL SCINAME
my_study_species <- my_occupancy$Species

# davids study_species is character, so I am coercing mine into character
my_study_species <- as.character(my_study_species)

my_found <- match(my_study_species, unique(birds$SCINAME))
#study_species[is.na(found)]

#Xenops rutilus in the range map; Xenops rutilans in my dataframe
#Hylatomus lineatus in the range map; Dryocopus lineatus in my dataframe
# I changed the names and now they are the same as on the maps, so NA should be zero. 
my_study_species[is.na(my_found)]


## ----------------------------------------------------------------------------------
## E) GET DISTANCES FROM ALL SITES TO CONTINENTAL BOUNDARIES 
##    FOR THE FOCAL SPECIES FOUND IN THE OCCUPANCY MATRIX
## ---------------------------------------------------------

continental_margin <- st_read('./1.2.2.3.Data_cleaned/Bird_maps/Bird_continental_range_margins.shp')

# We need the margins and the ranges: distance from a line is simple
# and distance from a polygon is zero when the point is in the polygon.
# Together this allows us to add a sign to distance to edge, showing 
# inside or outside the range - that is, if the distance from the polygon is greater than zero,
# the point is outside the polygon, therefore outside the range. 

# reduce the occupancy 'sf' object to the raw occupancy matrix
# reduce my_occupancy to species, bird_ID and geometry
my_occupancy_matrix <- my_occupancy[,c(-3:-7)]
# got rid of 'geometry' column
st_geometry(my_occupancy_matrix) <- NULL

# turn it into matrix
my_occupancy_matrix <- as.matrix(my_occupancy_matrix)

# name rows as site_study e.g FA_anjos
rownames(my_occupancy_matrix) <- with(my_occupancy, paste(Bird_ID))

# Reduce the continental margins and ranges to the focal species
# and check the columns of the occupancy matrix and rows of features
# are in the same order.

my_focal_species_indices <- match(my_study_species, continental_margin$SCINAME)
any(is.na(my_focal_species_indices))

my_focal_species_margin <- continental_margin[my_focal_species_indices,]

my_focal_species_range <- birds[my_focal_species_indices,]

# Now get distance matrices from the sites to the margins and ranges
# Helpfully, points _inside_ a range have a zero distance to it and
# the algorithm runs very quickly (particularly compared to st_within,
# which runs glacially slowly in this case).

# This outputs a matrix of site by species, surprisingly quickly.
# - unclass it to remove the 'units' class 
my_site_to_margin_dist <- unclass(st_distance(my_occupancy, my_focal_species_margin))
min(my_site_to_margin_dist) #1053.052 
max(my_site_to_margin_dist) #2525305

# do the same thing with the polygon ranges to identify points in polygons
# Note that the distances here are to the complete range boundaries, not
# just the continental margins
#site_to_range_dist <- unclass(st_distance(occupancy, focal_species_range))
my_site_to_range_dist <- unclass(st_distance(my_occupancy, my_focal_species_range))
max(my_site_to_range_dist) #3065736
min(my_site_to_range_dist) #0

# use these two to add a sign to the distance from edge (negative is outside 
# the official range).
my_site_to_margin_dist <- my_site_to_margin_dist * ((my_site_to_range_dist == 0) * 2 - 1)
max(my_site_to_margin_dist)
str(my_site_to_margin_dist)
is.numeric(my_site_to_margin_dist)

hist(my_site_to_margin_dist)
hist(sqrt(abs(my_site_to_margin_dist)) * sign(my_site_to_margin_dist))

# label by site and study
my_site_to_margin_dist <- data.frame(my_site_to_margin_dist)

# david labelled by site and study; I will label by birdID
my_site_to_margin_dist$Bird_ID <- my_occupancy$Bird_ID

# Melt the data into long form
my_dists <- reshape2::melt(my_site_to_margin_dist, id.vars='Bird_ID', variable_name = 'species')
names(my_dists)[3] <- 'dist_edge'
my_dists <- my_dists[! is.na(my_dists$dist_edge),]

# adding species name column to my_dist data frame
str(my_dists)

my_occupancy1 <- my_occupancy %>%
  dplyr::select(Bird_ID, Species)

my_occupancy1$geometry <- NULL

my_dists1 <- base::merge(my_dists, my_occupancy1, by="Bird_ID")
#drop species column, the one with only x1, x2...
my_dists1 <- my_dists1[,-2]

#_____________________________________________________________________________________________________
# grouping all species and Bird_ID back together by the nearest distance to an edge. because I had repeated observations for 
# species and Bird_IDs, I dont know why - perhaps they were distances to different margins of the range.
# I will discuss that with David later and see how to deal with that, for now, the mean will have to do.

my_dists1 <- my_dists1 %>% 
  group_by(Bird_ID, Species) %>% 
  summarise_all(list(dist_edge = mean))

hist(my_dists_near$near_dist_edge)
hist(my_dists_mean$mean_dist_edge)
#_____________________________________________________________________________________________________

# transform distance to km
my_dists1$dist_edge <- my_dists1$dist_edge / 1000
my_dists1$sqrt_dist_edge <- with(my_dists1, sqrt(abs(dist_edge)) * sign(dist_edge))
hist(my_dists1$sqrt_dist_edge)

# merge the rest of data
my_dist2 <- base::merge(my_dists1, my_occupancy, by="Bird_ID", all=FALSE)
# get rid of column species.y
my_dist2 <- my_dist2[,c(-5)]
my_dist2 <- my_dist2[,c(-10)] 

#reorder columns
my_dist2 <- my_dist2[, c(2, 1, 3:9)]
#fix column name
names(my_dist2)[1] <- 'Species'

my_fc <- read.csv('./data_cleaned/LSR_FA_forest_cover_IF_20200624.csv', header = TRUE, stringsAsFactors = FALSE)

my_fc <- subset(my_fc, select = c(Bird_ID, Species, fc_2011_prop))

my_dataset <- merge(my_dist2, my_fc, by=c('Species', 'Bird_ID'), all.x=TRUE)

#reorder columns
my_dataset <- my_dataset[, c(1:6, 10, 7:9)]

# save those values
fn <- sprintf('./data_cleaned/LSR_FA_model_data_DE_FC_%s.csv', format(Sys.time(), '%Y%m%d'))
write.csv(my_dataset, fn)



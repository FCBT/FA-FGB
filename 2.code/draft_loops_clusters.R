# set wd
if(Sys.info()[7] == "fcb5018"){
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7] == "Flavia"){
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7]== "tom"){
  setwd("/Users/tom/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
}

# clear global env
rm(list = ls())

library(rgdal)
library(raster)
library(sf)
library(units)
library(tidyverse)
#library(landscapemetrics)
#library(egg) # for more professional looking plots
library(fpc) # for finding best number of clusters

## 1) set projection saaeac ##---------------------------------------------------------------------------
saaeac <-
  '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
#--------------------------------------------------------------------------------------

## 2) load data ##----------
data_FC <- read_csv("./2.data_cleaned/NHM-MZUSP_dist2edge_data.csv") %>%
  st_as_sf(
    coords = c('lon_dd', 'lat_dd'),
    crs = 4326,
    remove = FALSE
  ) %>%
  # transform projection from WGS84 into SAAEAC
  #st_transform(crs = saaeac) %>%
  # remove columns unnecessary for analysis
  dplyr::select(-c(subspecies, distance, distance_sgn, inrange, in_costal_margin, source)) %>%
  # filter from 1985 because that's the first year I have landcover maps for
  dplyr::filter(date_collected >= 1985) # n= 314

# 3) create new column called 'decade' to subset years in 4 decades: 1985-1994;----
# 1995-2004; 2005-2010; 2015-2017. 

data_FC <- data_FC %>%
  mutate(decade = case_when(
    date_collected <= 1994 ~ 1985,
    date_collected >= 1995 & date_collected <= 2004 ~ 1995,
    date_collected >= 2005 & date_collected <= 2014 ~ 2005,
    date_collected >= 2015 ~ 2015)) %>%
  relocate(decade, .after = date_collected)

map_brazil <-
  st_read("./2.data_raw/other_map_files/Biomas_Brasil.shp") %>%
  mutate(NOME = as.factor(NOME)) %>%
  st_transform(crs = 4326)

# 4) create column with biome of each specimen ---------------------------------
data_FC$biome <- NA

for (i in 1:length(map_brazil$NOME)){
  print(i)
  # get the biome
  name_biome <- as.character(map_brazil$NOME[i])
  
  # calculate if specimens points intersect with i biome
  intersect_loc <- st_intersects(map_brazil$geometry[i], data_FC$geometry, sparse = FALSE)
  
  data_FC$biome[intersect_loc] <- name_biome
  
}

## 5) handling outliers --------------------------------------------------------
# table(data_FC$biome)
# 
# # Amazônia       Caatinga        Cerrado Mata Atlântica       Pantanal
# # 21              5             52            209              3
# 
# table(is.na(data_FC$biome))
# # some NAs = 24
# # visual check
# NAs <- data_FC %>%
#   filter(is.na(biome) == TRUE)
# 
# ggplot() +
#   geom_sf(data = map_brazil,
#           (aes(fill = NOME))) +
#   scale_fill_brewer(palette = "Pastel1")+
#   geom_sf(data = NAs) +
#   coord_sf() +
#   theme_article()

# they are slightly outside the shapefile of the Atlantic Forest, but location 
# state "Parque Nacional da Foz do Iguaçu, so effectively they are in the 
#Atlantic Forest biome, so turn NAs into 'Atlantic Forest'
data_FC <- data_FC %>%
  mutate(biome = ifelse((is.na(biome) == TRUE), "Mata Atlântica", biome))

# 6) load map/year per specimens -----------------------------------------------

# rearrange columns, remove columns not needed for this part of analyses and create 
# new column with raster file names
data_FC <- data_FC %>%
  relocate(biome, .after = decade) %>%
  dplyr::select(-c(date_collected, location)) %>%
  mutate(biome = toupper(biome)) %>%
  mutate(biome = str_replace(biome, " ", "")) %>%
  mutate(biome = str_replace(biome, "Â", "A")) %>%
  mutate(biome = str_replace(biome, "Ô", "O")) %>%
  mutate(file_name = paste(biome, decade, sep="-")) %>%
  mutate(file_name = paste0("COLECAO_5_", file_name, ".tif")) %>%
  arrange(file_name)

rm(map_brazil)
rm(name_biome)

# swich off spherical geometry due to errors when using st_bbox  
sf::sf_use_s2(FALSE)    

## function to find best cluster for specimens location on a given map. Reprojecting 
## large pieces of the map takes too long, so I want to find clusters with each year/biome 
## map that makes it more efficient. 
# find_cluster <- function(specimens_loc){
#   print("started to find cluster")
#   
#   # get unique coordinates
#   sites <- unique(specimens_loc[,c("lon_dd", "lat_dd")])
#   
#   # calculate distances
#   x <- st_distance(sites)  
#   
#   # to remove the units (st_distance output is a matrix with units, by setting to null we can cluster using kmeans)
#   units(x) <- NULL
#   
#   # find ideal number of clusters
#   best_nc <- pamk(x, krange = 1:10)
#   print(paste(best_nc$nc, "clusters for", data_FC$file_name[i]))
#   
#   sites$cluster <- best_nc[["pamobject"]]$clustering
#   
#   #add data to the main dataframe
#   data_clust <- st_join(specimens_loc, sites)
#   
#   #check the join worked and the longitudes are the same
#   print(paste0("Has st_join worked for ",data_FC$file_name[i], "? ",
#     all(data_clust$lon_dd.x == data_clust$lon_dd.y)
#     ))
#  
#   points_extent <- data_clust %>%
#     group_by(cluster) %>%
#     summarise(extent = st_as_sfc(st_bbox(geometry)))
#   
#   return(points_extent)
#   
#   
# }

# projection: south america albers equal area conic
saaeac <- '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'

## function to crop maps to sites extents
crop_reproject_map <- function(points_extent, raster_mapbioma){
  
  # Convert each extent in specimens_bbox to a `sp` extent object for use in `crop`
  for(j in 1:nrow(points_extent)){
    print("started to crop/reproject")
    
    # Buffer the region by 0.1 degrees to get coverage around the landscape.
    # This is roughly 10 kilometres which is a wide enough border to allow
    # us to test the scale of effects.
    # - this throws out a warning, but this is only a rough border.
    specimens_bbox <- st_buffer(points_extent$extent[j], 0.1)
    
    specimens_extent <- extent(matrix(st_bbox(specimens_bbox), ncol=2))
    
    # Crop the landcover data
    sites_landcover <- raster::crop(raster_mapbioma, specimens_extent)
    print(paste(data_FC$file_name[i], "cluster", points_extent$cluster[j], 
                "cropped at", Sys.time()))
    
    # changing map ID to only forest - transforms it into a binary map (1 = forest; 0 = matrix)
    # map biomas value 3 = forest
    sites_forest <- sites_landcover == 3
    print(paste(data_FC$file_name[i], "cluster", points_extent$cluster[j], 
                "transformed into binary at", Sys.time()))
    
    # This takes a little while to run!
    # method ngb because map is categorical
    forest_saaeac <- projectRaster(sites_forest, crs=saaeac, method='ngb', res=30)
    print(paste(data_FC$file_name[i], "cluster", points_extent$cluster[j],
                "reprojected at", Sys.time()))
    
    #choose file name to save map
    fn <- sprintf('./2.data_cleaned/binary_saaeac_maps/binary_saaeac_%s_%s', points_extent$cluster[j], data_FC$file_name[i])
    #save maps
    writeRaster(forest_saaeac, fn, row.names = FALSE, overwrite=TRUE)
    print(paste(data_FC$file_name[i], "cluster", points_extent$cluster[j],
                "saved at", Sys.time()))
    
  }
  
}


## for loop using functions to crop maps, transform them into binary (forest=1, matrix=0), reproject.
file_path <- "./2.data_raw/map_biomas/"
map_list <- unique(data_FC$file_name)

for(i in 1:length(map_list)){
  
  raster_fn <- paste0(file_path,map_list[i])
  raster_map <- raster(raster_fn)
  
  specimens_loc <- data_FC %>% 
    filter(file_name == map_list[i])
  
  if(nrow(specimens_loc) != 1){
    
    # find number of clusters
    points_extent <- find_cluster(specimens_loc)
    
    
    
  } else {
    
    # transform into a coordinate (crs=WGS84)
    specimens_loc <- st_as_sf(specimens_loc, coords=c('lon_dd','lat_dd'), crs=4326)
    
    # Get the bounding box and convert to a polygon
    # points_extent <- st_as_sfc(st_bbox(specimens_loc))
    points_extent <- tibble(extent = st_as_sfc(st_bbox(specimens_loc)), cluster = 1)
  }
  
  # crop maps based on sites extent
  sites_landcover <- crop_reproject_map(points_extent, raster_map)
  
}


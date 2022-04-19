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
library(landscapemetrics)
#library(egg) # for more professional looking plots
#library(fpc) # for finding best number of clusters

## 1) set projection saaeac ##---------------------------------------------------------------------------
saaeac <-
  '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
#--------------------------------------------------------------------------------------

## 2) load data ##----------
data <- read_csv("./2.data_cleaned/NHM-MZUSP_dist2edge_data.csv") %>%
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

data <- data %>%
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
data$biome <- NA

for (i in 1:length(map_brazil$NOME)){
  print(i)
  # get the biome
  name_biome <- as.character(map_brazil$NOME[i])
  
  # calculate if specimens points intersect with i biome
  intersect_loc <- st_intersects(map_brazil$geometry[i], data$geometry, sparse = FALSE)
  
  data$biome[intersect_loc] <- name_biome
  
}

## 5) handling outliers --------------------------------------------------------
    # table(data$biome)
    # 
    # # Amazônia       Caatinga        Cerrado Mata Atlântica       Pantanal
    # # 21              5             52            209              3
    # 
    # table(is.na(data$biome))
    # # some NAs = 24
    # # visual check
    # NAs <- data %>%
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
data <- data %>%
  mutate(biome = ifelse((is.na(biome) == TRUE), "Mata Atlântica", biome))

# 6) load map/year per specimens -----------------------------------------------

# rearrange columns, remove columns not needed for this part of analyses and create 
# new column with raster file names
data <- data %>%
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

# check nearest neighbour
# among all points = 152
# among biomes = 
# amazonia 2005 = 15 meters

specimens <- data %>% 
  dplyr::filter(biome == "AMAZONIA" & decade == 2005)

# get unique coordinates
sites <- unique(specimens[,c("lon_dd", "lat_dd")])

# calculate distances
x <- st_distance(sites)

# to remove the units (st_distance output is a matrix with units, by setting to null we can cluster using kmeans)
units(x) <- NULL
x <- x %>% order()
min(x)


# swich off spherical geometry due to errors when using st_bbox  
sf::sf_use_s2(FALSE)    
    

## function to crop maps to sites extents
crop_reproject_map <- function(points_extent, raster_map) {
  # Buffer the region by 0.1 degrees to get coverage around the landscape.
  # This is roughly 10 kilometres which is a wide enough border to allow
  # us to test the scale of effects.
  # - this throws out a warning, but this is only a rough border.
  specimens_bbox <- st_buffer(points_extent$extent, 0.1)
  
  specimens_extent <-
    extent(matrix(st_bbox(specimens_bbox), ncol = 2))
  
  # Crop the landcover data
  sites_landcover <- raster::crop(raster_map, specimens_extent)
  print(
    paste(
      data$file_name[i],
      "bird_id",
      points_extent$bird_id[j],
      "cropped at",
      Sys.time()
    )
  )
  
  # changing map ID to only forest - transforms it into a binary map (1 = forest; 0 = matrix)
  # map biomas value 3 = forest
  sites_forest <- sites_landcover == 3
  print(
    paste(
      data$file_name[i],
      "bird_id",
      points_extent$bird_id[j],
      "transformed into binary at",
      Sys.time()
    )
  )
  
  # This takes a little while to run!
  # method ngb because map is categorical
  forest_saaeac <-
    projectRaster(sites_forest,
                  crs = saaeac,
                  method = 'ngb',
                  res = 30)
  print(
    paste(
      data$file_name[i],
      "bird_id",
      points_extent$bird_id[j],
      "reprojected at",
      Sys.time()
    )
  )
  
  #choose file name to save map
  fn <-
    sprintf(
      './2.data_cleaned/binary_saaeac_maps/binary_saaeac_%s_%s',
      points_extent$bird_id[j],
      data$file_name[i]
    )
  #save maps
  writeRaster(forest_saaeac,
              fn,
              row.names = FALSE,
              overwrite = TRUE)
  print(paste(
    data$file_name[i],
    "bird_id",
    points_extent$bird_id[j],
    "saved at",
    Sys.time()
  ))
  return(forest_saaeac)
  
}

calculate_forest_cover <- function(map_processed, points_extent) {
  # create several buffer radius for checking scale of effect
  buffer_radius <-
    as.numeric(seq(from = 100, to = 1500, by = 100))
  point <-
    st_as_sf(points_extent,
             coords = c('lon_dd', 'lat_dd'),
             crs = 4326)
  point <- st_transform(point, saaeac)
  
  FC_list = list()
  for (k in 1:length(buffer_radius)) {
    FC_percentage <- sample_lsm(
      map_processed,
      point,
      plot_id = points_extent$bird_id[j],
      shape = "circle",
      size = buffer_radius[k],
      all_classes = TRUE,
      what = "lsm_c_pland"
    )  %>%
      filter(class == 1) %>%
      pull(value)
    
    if (nrow(FC_percentage) > 0) {
      FC_list[[k]] <-
        data.frame(FC_buffer = buffer_radius[k], FC_percentage$value)
    } else{
      FC_list[[k]] <- data.frame(FC_buffer = buffer_radius[k], 0)
    }
    
  }
  
  return(bind_rows(FC_list))
}



## for loop using functions to crop maps, transform them into binary (forest=1, matrix=0), reproject.
file_path <- "./2.data_raw/map_biomas/"
map_list <- unique(data$file_name)
data  <- data %>% mutate(FC = map(species, function(x) {
}))

for (i in 1:length(map_list)) {
  raster_fn <- paste0(file_path, map_list[i])
  raster_map <- raster(raster_fn)
  
  specimens_loc <- data %>%
    filter(file_name == map_list[i])
  
  # transform into a coordinate (crs=WGS84)
  specimens_loc <-
    st_as_sf(specimens_loc,
             coords = c('lon_dd', 'lat_dd'),
             crs = 4326)
  
  # Get the bounding box and convert to a polygon
  f <- function(x) {
    st_as_sfc(st_bbox(x))
  }
  
  points_extent <- specimens_loc %>%
    mutate(extent = map(geometry, f)) %>%
    unnest(extent)
  
  # crop maps based on sites extent
  for (j in 1:nrow(points_extent)) {
    # Convert each extent in specimens_bbox to a `sp` extent object for use in `crop`
    print(paste(
      "started to crop/reproject: Sp",
      j,
      " of ",
      nrow(points_extent)
    ))
    
    # crop and reproject
    map_processed <-
      crop_reproject_map(points_extent[j, ], raster_map)
    
    # calculate forest cover
    indx <- which(points_extent$bird_id[j] == data$bird_id)
    
    print(paste(
      "forest cover calculated for bird_id",
      points_extent$bird_id[j]
    ))
    data$FC[[indx]] <-
      calculate_forest_cover(map_processed, points_extent[j, ])
  }
  
}

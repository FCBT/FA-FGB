#--------------------------------------------------------------------------------
# Flavia Trigo, created on 05/Aug/2021
# last edited: 27/Aug/2021
#--------------------------------------------------------------------------------


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
library(egg) # for more professional looking plots


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
  dplyr::select(-c(subspecies, 9:26)) %>%
  # filter from 1985 because that's the first year I have landcover maps for
  dplyr::filter(date_collected >= 1985) # n= 316

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
  st_read("/Users/flavia/Library/CloudStorage/OneDrive-ImperialCollegeLondon/1.PHD/2.R-dir/shared_maps_berween_chapters/other_map_files/Biomas_Brasil.shp") %>%
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

# 5) handling outliers --------------------------------------------------------
# table(data$biome)
# 
# # Amazônia       Caatinga       Cerrado       Mata Atlântica      Pantanal
# # 22              5             53            209                 3
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

# switch off spherical geometry due to errors when using st_bbox 
# from 1.0.0 version of sf package, some calculations with S2 geometries
# will give an error instead of a warning, so switch off in this case
# because I am just putting a big enough buffer around the area I need to work with 
sf::sf_use_s2(FALSE)    


## for loop using functions to crop maps, transform them into binary (forest=1, matrix=0), reproject.
file_path <- "./2.data_raw/map_biomas/"
data  <- data %>% mutate(FC = map(species, function(x){}))

for(i in 1:nrow(data)){
  
  raster_fn <- paste0(file_path,data$file_name[i])
  raster_map <- raster(raster_fn)
  
  specimens_loc <- data[i,] 
  
  # transform into a coordinate (crs=WGS84)
  specimens_loc <- st_as_sf(specimens_loc, coords=c('lon_dd','lat_dd'), crs=4326)
  
  # Get the bounding box and convert to a polygon
  specimens_bbox <-st_as_sfc(st_bbox(specimens_loc))
  
  specimens_buffer <- st_buffer(specimens_loc, 0.1)
  
  # transform into extent for the crop function  
  specimens_extent <- extent(matrix(st_bbox(specimens_buffer), ncol=2))
    
  # Crop the landcover data
  sites_landcover <- raster::crop(raster_map, specimens_extent)
  print(paste(data$file_name[i], "bird_id", specimens_loc$bird_id, 
                 "cropped at", Sys.time()))
    
    # changing map ID to only forest - transforms it into a binary map (1 = forest; 0 = matrix)
    # map biomas value 3 = forest
    sites_forest <- sites_landcover == 3
    print(paste(data$file_name[i], "bird_id", specimens_loc$bird_id, 
                "transformed into binary at", Sys.time()))
    
    rm(sites_landcover)
    # This takes a little while to run!
    # method ngb because map is categorical
    forest_saaeac <- projectRaster(sites_forest, crs=saaeac, method='ngb', res=30)
    print(paste(data$file_name[i], "bird_id", specimens_loc$bird_id,
                "reprojected at", Sys.time()))
    
    rm(sites_forest)
    #choose file name to save map
    fn <- sprintf('./2.data_cleaned/binary_saaeac_maps/binary_saaeac_%s_%s', specimens_loc$bird_id, data$file_name[i])
    #save maps
    writeRaster(forest_saaeac, fn, row.names = FALSE, overwrite=TRUE)
    print(paste(data$file_name[i], "bird_id", specimens_loc$bird_id,
                "saved at", Sys.time()))
    
   
     # calculate forest cover
    
    # create several buffer radius for checking scale of effect 
    buffer_radius <- as.numeric(seq(from = 100, to = 1500, by = 100))
    # point <- st_as_sf(specimens_loc, coords=c('lon_dd','lat_dd'), crs=4326)
    point <- st_transform(specimens_loc, saaeac)
    
    FC_list <- list()
    for(k in 1:length(buffer_radius)){ 
      FC_percentage <- sample_lsm(forest_saaeac, point,
                                  plot_id = data$bird_id[i], 
                                  shape = "circle", 
                                  size = buffer_radius[k],
                                  all_classes = TRUE,
                                  what = "lsm_c_pland")  %>%
        filter(class == 1) 
      
      if(nrow(FC_percentage) > 0){
      FC_list[[k]] <- data.frame(FC_buffer = buffer_radius[k], FC_percentage$value)
      }else{
        FC_list[[k]] <- data.frame(FC_buffer = buffer_radius[k], 0)
      }
    }
    
    indx <- which(specimens_loc$bird_id == data$bird_id)
    data$FC[[indx]] <- bind_rows(FC_list)
    print(paste("forest cover calculated for bird_id", specimens_loc$bird_id, 
                "using map", data$file_name[i]))
    
  }

data_unnested <- data %>%
  unnest(FC)

# transform all NA into 0 - if class forest is not present, then transform into 0 instead of NA
data_unnested$FC_percentage.value[is.na(data_unnested$FC_percentage.value)] <- 0

csv_name <- "./2.data_cleaned/MZUSP_FC_calculated.csv"
write_csv(data_unnested, csv_name)

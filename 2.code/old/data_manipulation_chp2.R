# Flavia Bellotto-Trigo
# created: 11 Feb 2020
# last modify: 04 June 2020

# setting wd in pc or laptop
if(Sys.info()[7] == "fcb5018"){
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
} else if (Sys.info()[7] == "Flavia"){
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
}

library(tidyverse)
install.packages('signs')
library(signs)
#load data
data <- read.csv("./1.2.2.2.Data_raw/MZUSP_data_collection_4.csv", stringsAsFactors = FALSE)

# turn empty values into zeros
data[data == ""] = 0

# turn na into zeros
data[is.na(data$Location_GPS)] <- 0

# get rid of first row (empty); columns: 7 and from 28 to 47.
data <- data[,c(-7, -28:-47)]


data <- data %>% 
  # coerce character to numeric
  mutate( X1st_RT_mm = as.numeric(X1st_RT_mm), 
          X2nd_RT_mm = as.numeric(X2nd_RT_mm),
          X3rd_RT_mm = as.numeric(X3rd_RT_mm),
          X1st_LT_mm = as.numeric(X1st_LT_mm),
          X2nd_LT_mm = as.numeric(X2nd_LT_mm),
          X3rd_LT_mm = as.numeric(X3rd_LT_mm)) %>%
  # select left tarsus rows, take and round their mean and store it in a new column
  mutate(LT_mean = round(rowMeans(dplyr::select(., X1st_LT_mm, X2nd_LT_mm, X3rd_LT_mm)), digits = 2),
  # select right tarsus rows, take and round their mean and store it in a new column
         RT_mean = round(rowMeans(dplyr::select(., X1st_RT_mm, X2nd_RT_mm, X3rd_RT_mm)), digits = 2)) %>%
  # select left wing rows, take and round their mean and store it in a new column
  mutate(LW_mean = round(rowMeans(dplyr::select(., X1st_LW_mm, X2nd_LW_mm, X3rd_LW_mm)), digits = 2),
  # select right wing rows, take and round their mean and store it in a new column
         RW_mean = round(rowMeans(dplyr::select(., X1st_RW_mm, X2nd_RW_mm, X3rd_RW_mm)), digits = 2)) %>%
  # subtract left tarsus from right tarsus and store absolute value in a new column
  mutate(TarsusFA = abs(LT_mean - RT_mean)) %>% 
  # subtract left wing from right wing and store the absolute value in a new column
  mutate(WingFA = abs(LW_mean - RW_mean)) %>%
  # add tarsus and wing, and store the absolute value in a new column
  mutate(TotalFA = round((TarsusFA + WingFA), digits = 2))

min(data$TotalFA)
max(as.numeric(data$TotalFA))
table(is.na(data$TarsusFA))

fn <- sprintf('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2/1.2.2.3.Data_cleaned/MZUSP_dataset_totalFA_%s.csv', format(Sys.time(), '%Y%m%d'))
write.csv(data, fn, row.names = FALSE)

##################### most updated file ########################################################
data <-read.csv("./1.2.2.3.Data_cleaned/MZUSP_dataset_totalFA_20200623.csv", stringsAsFactors = FALSE)
str(data)

library(tidyverse)

data$Bird_ID <-as.factor(data$Bird_ID)

# remove repeated measuments for FA columns
data <- data[,c(-8:-25)]

#### sorting locations in  Degrees, Minutes, Seconds ####
#### Convert into decimal degrees####
location_dd <- data %>%
  separate(Location_GPS, c("lon_degrees", "lat_degrees"), sep="S") %>%
  #remove 'w' from lon_degrees
  mutate(lon_degrees = str_remove_all(lon_degrees, "W"))%>%
  #remove degree marks 
  mutate(lon_degrees = str_replace_all(lon_degrees,"[°'\""']",""),
         lat_degrees = str_replace_all(lat_degrees,"[°'\""']","")) %>%
  #convert commas to decimal
  mutate(lon_degrees = str_replace_all(lon_degrees,"[,]","."),
         lat_degrees = str_replace_all(lat_degrees,"[,]",".")) %>%
  #split into deg, min and sec
  separate(lat_degrees,into = c("deg_lat","min_lat","sec_lat"),sep = " ", fill = "right") %>%
  separate(lon_degrees,into = c("deg_lon","min_lon","sec_lon"),sep = " ", fill = "right") 
 
######################### Removing NA from Lat/Lon #######################################################
location_dd <- location_dd %>% drop_na(deg_lat,deg_lon, min_lat, min_lon, sec_lon, sec_lat)
#remove rows with NAs in location columns (deg_*, min_*, sec_*)
#location_dd <- location_dd[!is.na(location_dd$deg_lat), ]
#location_dd <- location_dd[!is.na(location_dd$deg_lon), ]
#location_dd <- location_dd[!is.na(location_dd$min_lat), ]
#location_dd <- location_dd[!is.na(location_dd$min_lon), ]
#location_dd <- location_dd[!is.na(location_dd$sec_lat), ]
#location_dd <- location_dd[!is.na(location_dd$sec_lon), ]


#----- I removed all NA in the last chunck of code, so this is not necessary ------------------------------------
# transform empty values and na into zeros - I wasn't able to do it together with the code above
# the next steps to convert to decimal degrees do not work if sec_lat and sec_lon are na values
#   location_dd[location_dd== ""] <- 0
#   location_dd[is.na(location_dd)] <- 0
#   table(is.na(location_dd)) 
#------------------------------------------------------------------------------------------------------------------

location_dd1 <- location_dd %>%
  # convert into decimal degrees
  mutate(lat_dd = round(as.numeric(deg_lat), digits = 4) + round((as.numeric(min_lat)/60), digits = 4) + round((as.numeric(sec_lat)/3600), digits = 4),
         lon_dd = round(as.numeric(deg_lon), digits = 4) + round((as.numeric(min_lon)/60), digits = 4) + round((as.numeric(sec_lon)/3600), digits = 4)) %>%
  dplyr::select(-c(deg_lon:sec_lat))

#write.csv(location_dd1,"./1.2.2.3.Data_cleaned/Bird-ID_dd.csv")

#----- all GPS points are in the same unit, so no need for this chunk of code anymore ----------------------
# I used it when some points were in UTM, but I rewrote everyting in the csv file
#### merging columns ####
#transform zeros into NA before uniting, otherwise it will unite 'zeros' as well
# location_dd1[location_dd1 == 0] <- NA
# location_dd3 <- location_dd1 %>%
  # unite columns
#  unite("Lat", c("lat", "lat_dd")) %>%
#  unite("Lon", c("lon", "lon_dd")) %>%
  # get rid of NA_ from uniting columns, can't use NA as a replacement because it is not a character vector, I will replace "" with NA in a next line of code
#  mutate(Lon = str_replace_all(Lon,  "[_NA NA_NA NA_]", ""),
#         Lat = str_replace_all(Lat, "[_NA NA_NA NA_]", ""))

# transform empty spaces in NA again before adding minus signs
# location_dd3[location_dd3== ""] <- NA
#--------------------------------------------------------------------------------------------------------------

location_dd2 <- location_dd1 %>%
  # add '-' to the coordinates as Brazil is in the south west hemisphere 
  mutate(Lat = lat_dd*(-1),
         Lon = lon_dd*(-1))%>%
  # remove columns
  dplyr::select(-c(lat_dd,lon_dd))
  
# reorder columns
location_dd2 <- location_dd2[, c(1,4,3,7:9,5,6,2)]
  
fn2 <- sprintf('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2/1.2.2.3.Data_cleaned/LSR_1991-2017_lat-lon-FA_%s.csv', format(Sys.time(), '%Y%m%d'))
write.csv(location_dd2, fn2, row.names = FALSE)


######################### transforming Lat/Lon into coordinates #########################
library(sf)
library(raster)
library(rgdal)

location_dd2$Bird_ID <-as.factor(location_dd2$Bird_ID)
coord1 <- location_dd2 %>% dplyr::select(Bird_ID, Lon, Lat)
coord1 <- st_as_sf(coord1, coords = c('Lon','Lat'), crs=4326)

####################### plot points ###########################
map_bioma <- st_read('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/7_Extra-curricular/ForCristina/map-files/Biomas_Brasil.shp')
map_estados <- st_read('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/7_Extra-curricular/ForCristina/map-files/Estados_Brasil.shp')
birds <- st_read('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2/1.2.2.3.Data_cleaned/Bird_maps/cleaned_ranges.shp')
map_AF <- map_bioma %>% filter(NOME == "Mata Atlântica")

library(ggplot2)
x11()
ggplot() + 
  geom_sf(data = map_estados, fill = "grey", color = "grey") +
  geom_sf(data = coord1 ,size = 1) +
  ggtitle("Specimens location in Brazil") +
  coord_sf() +
  theme_classic()

ggplot() + 
  geom_sf(data = map_estados, fill = "grey", color = "grey") +
  geom_sf(data = map_AF, fill= "light green", color = "light green", size = 1)+
  geom_sf(data = coord1 ,size = 1) +
  ggtitle("Species location \nin the Atlantic Forest") +
  coord_sf() +
  theme_classic()


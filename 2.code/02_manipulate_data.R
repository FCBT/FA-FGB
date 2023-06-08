#--------------------------------------------------------------------------------
# Flavia Trigo, created on 05/May/2021
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

library(tidyverse)

#Load data
data <- read_csv("./2.data_cleaned/NHM-MZUSP_data-combined.csv",
                 # handles brazilian writing (i.e. ç, ã)
                 locale = locale(encoding = "utf-8")) %>%
  filter(!is.na(species))

# transform all zeros into NA
data$location_gps[data$location_gps == "0"] <-NA


## 1) Manipulate coordinates ####

data <- data %>%
  # get rid of the W in the column location_gps
  mutate(location_gps = str_replace_all(location_gps, "W", ""))%>%
  # separate coordinates into two columns using "S", which will remove 'S'.
  separate(location_gps, into = c("lon", "lat"), sep = "S") %>%
  # replace signs with one space
  mutate_at(c("lon","lat"), str_replace_all, pattern="[°′″']",replacement=" ") %>%
  # replace double-spaces with one space
  mutate_at(c("lon","lat"), str_replace_all, pattern="  ",replacement=" ") %>%
  # separate degrees, minutes and seconds in longitude with a space
  separate(lon, into = c("lon_d", "lon_m", "lon_s"), sep = " ") %>%
  #separate degrees, minutes and seconds in latitude with a space
  separate(lat, into = c("lat_d", "lat_m", "lat_s"), sep = " ") %>%
  # replace empty values with a zero. 1 second is equal to about 31 meters at the equator, 
  # therefore 60 seconds would only be a maximum of 1.8km (that will be the maximum variation in the data due to unknowing values for 'seconds', only 62 values are unknown)
  mutate_at(c("lon_s","lat_s"), str_replace_all, pattern="^$",replacement="0") %>%
  # remove unicode enconding (don't know where it came from) 
  mutate_at(c("lon_s", "lon_m", "lat_s", "lat_m"), str_replace_all, pattern="\u202f", replacement="") %>%
  # convert into decimal degrees
  mutate(lon_dd = (as.numeric(lon_d) + (as.numeric(lon_m)/60) + (as.numeric(lon_s)/3600))*-1,
         lat_dd = (as.numeric(lat_d) + (as.numeric(lat_m)/60) + (as.numeric(lat_s)/3600))*-1) %>% 
  #de-select these columns, because we don't need each individual one in the data frame
  dplyr::select(-c(lon_d,lon_m,lon_s,lat_d,lat_m,lat_s)) %>%
  # reorder columns lon and lat after bird_id
  relocate(c(lon_dd,lat_dd), .after= bird_id) %>%
  # add Brasil to the rows, because some locations were found in other countries with geocode_OSM
  mutate(location = paste(location, " Brasil", sep=",")) %>% 
  # remove duplicates from bird_id (will remove all the "unk" as well)
  filter(!bird_id == "unk") 


# ## 2) Handling outliers ####
# found some outliers, points are falling off the coast of Brazil and it is not an island; lat should be -23 not -28.  
outlier <- filter(data, location == "Estacao Biologica de Boraceia, Salesópolis, SP, Brasil")%>%  
  dplyr::select(bird_id, location, species, lon_dd, lat_dd)

# replace lat= -28 by -23
data <- data %>%  
  mutate(lat_dd = ifelse((location == "Estacao Biologica de Boraceia, Salesópolis, SP, Brasil") 
                         & (lat_dd < -27), -23.63333, lat_dd))

# replace "sem data, mas recente, perguntar" with 2012
data <- data %>% 
  mutate(date_collected = ifelse(date_collected == "sem data, mas recente, perguntar", 2012, date_collected))

# replace 2004/9 with 2004
data <- data %>% 
  mutate(date_collected = ifelse(date_collected == "2004/9", 2004, date_collected))


# filter out "unknowns" (from NHM), as it is essential to have the bird_id for the analysis
data <- data %>%
  filter(!date_collected == "Unk", !bird_id == "Unk")

table(data$source)  
# MZUSP   NHM 
# 479   183 


## 3) Manipulate species name #####------------------------------------------------------------------------
## looking at the species names in the cleaned Birdlife ranges by Orme et al 2019, I will have to change 
## two of the species names to match the ranges. So I will do this here.
## Xenops rutilus in the range map; Xenops rutilans in mzusp dataframe
## Hylatomus lineatus in the range map; Dryocopus lineatus in mzusp dataframe

# replace names
data <- data %>%  
  mutate(species = ifelse((species == "Xenops rutilans"), "Xenops rutilus", species)) %>%
  mutate(species = ifelse((species == "Dryocopus lineatus"), "Hylatomus lineatus", species))


# save changes
fn <- './2.data_cleaned/NHM-MZUSP_cleaned.csv'
write_csv(data, fn)

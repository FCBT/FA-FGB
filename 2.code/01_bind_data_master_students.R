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

#Load data with gps updated manually
fla_data <- read_csv("./2.data_cleaned/MZUSP_gps_updated.csv",
                     # handles Brazilian encoding (e.g. 'ç', 'ã') 
                     locale = locale(encoding = "utf-8")) %>%
  #removes rows with NA
  filter(!is.na(Species)) %>%
  #create a column called 'source' and fills rows with 'Fla'
  mutate(source = "MZUSP")

#add underscores and remove brackets from name to standardize column names
names(fla_data) <- str_replace_all(names(fla_data), " ", "_")
names(fla_data) <- str_replace_all(names(fla_data), fixed("("), "")
names(fla_data) <- str_replace_all(names(fla_data), fixed(")"), "")


#load data from master students
# called 12 species because they only measured 12 species in common to 'the ones I did in 'fla_data'
student_data <- read_csv("2.data_raw/12species.csv",
                         locale = locale(encoding = "iso-8859-1")) %>%
  #create column to match columns in 'fla_data', but fill 'source' rows with 'students' for this dataset 
  mutate(Feather_tail = NA, source = "NHM") %>%
  #rename columns with fla_data columns names
  dplyr::select(colnames(fla_data))

# merge datasets
# both datasets = 684 observations
all_data <- rbind(fla_data, student_data)

#change column names to lower case  
names(all_data) <- tolower(names(all_data))

all_data  <- all_data %>% 
  # get rid of any duplicates (NHM data has some)
  filter(!duplicated(bird_id))

# naming and saving file
write.csv(all_data, './2.data_cleaned/NHM-MZUSP_data-combined.csv', row.names = FALSE)



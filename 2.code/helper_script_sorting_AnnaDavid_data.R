# flavia trigo, created on 09/June/2021

# set wd
if(Sys.info()[7] == "fcb5018"){
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7] == "Flavia"){
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7]== "tom"){
  setwd("/Users/tom/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
}

# clear global env
rm(list=ls())

#read in required packages
library(tidyverse)

# read files from all folders
folder_list <- list.files(path="./2.data_raw/anna_davies_data_original/")

# create empty lists to store data for each species
all_coordinates <- list()
dist_all <- list()
dist_scaled <- list()
fgb_all <- list()

# for the items in 'folder_list':
for(i in 1:length(folder_list)){
  
  #get path to each folder
  path_to_folder <- paste0("./2.data_raw/anna_davies_data_original/", folder_list[i])
  # list the files in each folder
  file_list <- list.files(path = path_to_folder)
  
  # for each file in each of the folders above:
  for(j in 1:length(file_list)) {
    
    #get path to each file
    path_to_file <- paste0(path_to_folder,"/", file_list[j])
    # load file ignoring all the messages from readr
    temp_file <- read_csv(path_to_file, col_types = cols()) %>%
      # add a column with the name of file data is coming from
      mutate(source = file_list[j]) %>%
      # change position of new column
      relocate(source, .before= everything())
    
    # if the file is the first one in the folder:
    if(j == 1) {
      
      # add the file to this list and rename species ID column to bird_id for consistency
      all_coordinates[[i]] <- temp_file %>% rename(bird_id = Species_ID)
    
      # if the file is the second in the folder:  
    } else if(j == 2){
      
      # add the file to this list and rename all columns removing "Y" from column name for consistency
      dist_all[[i]] <- temp_file %>% rename_with(~str_replace(.x,"Y",""), starts_with("Y")) %>% 
                                     # and rename species id to specimen id
                                     rename(bird_id = species_id)
      
      # if the file is the third in the folder:
    } else if(j == 3){
      
      # add the file to this list and rename all columns that end with "_ids" to "bird_id" for consistency
      dist_scaled[[i]] <- temp_file %>% rename_with(~"bird_id", ends_with("_ids"))
      
      # if the file is the fourth in the folder:
    } else if(j == 4){
      
      # add the file to this list and rename all columns that end with "_ids" to "bird_id" for consistency
      fgb_all[[i]]<- temp_file %>% rename_with(~"bird_id",ends_with("_ids"))
      
    }
    
    }
  
}

# now bind all lists for each dataset into one dataframe
all_coordinates <- all_coordinates %>% bind_rows()
dist_all <- dist_all %>% bind_rows()
dist_scaled <- dist_scaled %>% bind_rows()
fgb_all <- fgb_all %>% bind_rows()


# save all dataframes
path_name <- "./2.data_cleaned/anna_davies_data_cleaned/"
write_csv(all_coordinates,file = paste0(path_name, "all_species_coordinates.csv"))
write_csv(dist_all, file = paste0(path_name, "all_species_dist.csv"))
write_csv(dist_scaled, file=paste0(path_name, "all_species_dist_scaled.csv"))
write_csv(fgb_all, file=paste0(path_name, "all_species_fgb.csv"))

## for chapter 2 ###

# setting wd in pc or laptop
if(Sys.info()[7] == "fcb5018"){
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
} else if (Sys.info()[7] == "Flavia"){
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
}

library(tidyverse)

# create 2 columns in all band_measurements files: species_name and ID_specimen_on_tag
band_measures$species_name <- 'celeus-flavescens'
band_measures$ID_specimen_on_tag <- 'ID82945'

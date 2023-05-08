#--------------------------------------------------------------------------------
# Flavia Trigo, created on 06/June/2021
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

#read in required packages
library(tidyverse)
library(egg)

# clear global env
rm(list = ls())

#Load data
data <- read_csv("./2.data_cleaned/NHM-MZUSP_with_coordinates.csv",
                 # handles brazilian writing (i.e. ç, ã)
                 locale = locale(encoding = "utf-8"),
                 col_types = cols(
                   species = col_character(),
                   subspecies = col_character(),
                   date_collected = col_character(),
                   bird_id = col_character(),
                   location = col_character(),
                   sex = col_character(),
                   `1st_lw_mm` = col_double(),
                   `2nd_lw_mm` = col_double(),
                   `3rd_lw_mm` = col_double(),
                   `1st_rw_mm` = col_double(),
                   `2nd_rw_mm` = col_double(),
                   `3rd_rw_mm` = col_double(),
                   `1st_lt_mm` = col_double(),
                   `2nd_lt_mm` = col_double(),
                   `3rd_lt_mm` = col_double(),
                   `1st_rt_mm` = col_double(),
                   `2nd_rt_mm` = col_double(),
                   `3rd_rt_mm` = col_double(),
                   feather_tail = col_character(),
                   source = col_character())) %>%
  filter(!is.na(species))

# I get warnings after loading the data, but these are okay because they are indeed NA values
data <- data %>%
  # adds '_' between species names
  mutate(species = str_replace_all(species, pattern = " ", replacement = "_")) %>%
  # transforms variables with mm in the name to numeric
  mutate_at(vars(matches("mm") ), ~ifelse(. == 9999, NA, .)) %>% 
  # transform dataset to long format
  pivot_longer(cols = 9:20) %>%
  # separate column 'name' into 'replicates' (3 measurements), 'type' (left or right, wing or tarsus), 'unit' (millimeters) based on the default sep='_'
  separate(name, into = c("replicates","type","unit")) %>%
  # change replicates column to numeric by getting rid of "st, nd, rd" using substring() - which keeps the first character of that string.
  mutate(replicates = as.numeric(substring(replicates,1,1))) %>%
  group_by(type, bird_id) %>%
  # calculate the mean and sd from replicates
  mutate(mean = mean(value,na.rm = T), sd = sd(value,na.rm = T)) %>%
  # remove columns that are not needed anymore
  dplyr::select(-replicates, -value) %>%
  # keep rows with distinct values, as the mean and sd would appear in three rows due to the replicates
  distinct() %>%
  # create new column with left and right, and update values in 'type' with wing or tarsus
  mutate(side = substring(type,1,1), type = substring(type,2,2)) %>%
  # transforms this rows from 'side' into columns
  pivot_wider(names_from = side, values_from = c(mean,sd)) %>%
  # calculate FA by subtracting one side from the other. By keeping the sign, I will know which side is bigger.
  mutate(fa_left_right = mean_l-mean_r) %>%
  # calculate FA ratio between sites
  mutate(fa_ratio = mean_l/mean_r) %>%
  # here I calculate the absolute FA, as it is the non-directional asymmetry between bilateral traits. 
  mutate(fa_abs_mm = abs(fa_left_right)) %>%
  # remove columns that are not needed now
  dplyr::select(-c(location, feather_tail))

# one outlier, looks reasonable - nothing too weird
d <- data %>% filter(fa_abs_mm > 9.5)

# plot by species
plot <- data %>%
  ggplot(aes(x = fa_ratio))+
  geom_histogram(na.rm = TRUE)+
  facet_wrap(~species)+
  xlab("Fluctuating Asymmetry ratio")+
  theme_article()

ggsave("./2.results/plots/fa_ratio_per_species.pdf", plot = plot, width = 7, height= 7, units= "in")
ggsave("./2.results/plots/fa_ratio_per_species.png", plot = plot, width = 7, height= 7, units= "in")

table(data$source)
# MZUSP   NHM 
# 958     366

fn1 <- "./2.data_cleaned/fa/NHM-MZUSP_fa_wing_tarsus.csv"
write_csv(data, file = fn1)



# # there are some warnings about NAs - they are fine
# which(is.na(data$`1st_lt_mm`))
# which(is.na(test$`1st_lt_mm`))
# identical(data$`1st_lt_mm`, test$`1st_lt_mm`)
# data_na <- data %>% filter(is.na(`1st_lw_mm`))
# test_na <- test %>% filter(is.na(`1st_lt_mm`))
# test_126 <- test %>% filter(bird_id == 91687)
# data_126 <- data %>% filter(bird_id == 91687)

#--------------------------------------------------------------------------------
# Flavia Trigo, created on 06/May/2021
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

# read in required packages
library(tidyverse)

# load digitized coordinates by Anna Davies from pictures taken by Flavia Trigo
# all_species_coordinates.csv was created with script "helper_script_sorting_AnnaDavid_data.R"
fgb_data <- read_csv("./2.data_cleaned/anna_davies_data_cleaned/all_species_coordinates.csv")
metadata <- read_csv("./2.data_cleaned/anna_davies_data_cleaned/AAmetadata_fgb.csv")

# make sure all column names are lower case -> change column names to lower case 
names(fgb_data) <- tolower(names(fgb_data))
names(metadata) <- tolower(names(metadata))

# merge fgb_data and metadata to get species name based on source
fgb_data <- left_join(metadata, fgb_data, by="source")

#change format of the data from wide to long
fgb_data <- fgb_data %>%
  # use part of the column name as value for the rows (X1 becomes column X, value 1), and point is the measurement.
  pivot_longer(!c(bird_id,species, source), names_to = c(".value", "half_fgb"),
               names_pattern = "(.)(.*)") %>%
  drop_na()

# nest creates a column with data (columns not used in group_by) 
fgb_nested <- fgb_data %>%
  group_by(species, bird_id) %>%
  nest()

# define function that takes a dataframe of coordinates and returns dataframe of distances
dist_df <- function(df){
  
  df <- df %>%
    # coerce column character to numeric
    mutate(half_fgb = as.numeric(half_fgb)) %>%
    # filter only "full bars" (i.e black + white band), therefore every two half_fgb = one fgb
    filter((half_fgb %% 2) != 0)
  
    # assign new dataframe with one column with fgb width values and another with the number_id of fgb
  df_new <- data.frame(fgb_width = rep(NA,nrow(df)-1), fgb = 1:(nrow(df)-1))
  
  #loop over old dataframe (dont need the last point, because that's the end od data)
  for(i in 1:nrow(df)-1){
    # calculate distance between i and i+1
    df_new$fgb_width[i] <- sqrt((df$x[i] - df$x[i+1])^2 + (df$y[i] - df$y[i+1])^2) 
  }
  
  #return new df
  return(df_new)
}

# once nested we can use map to apply a function to each nested dataframes
# apply function to nested dataframes and unnest the new column
fgb_unnested <- fgb_nested %>%
  mutate(data_transformed = map(data, dist_df)) %>%
  unnest(data_transformed) %>%
  dplyr::select(-data)

# plot by species
hist <- fgb_unnested %>%
  ggplot(aes(x = fgb_width))+
    geom_histogram()+
    facet_wrap(~species)+
  xlab("FGB width")+
  theme_article()

#save png
ggsave(
  "./2.results/plots/hist_fgb.png",
  hist,
  height = 7,
  width = 10,
  units = "in"
)
#save pdf
ggsave(
  "./2.results/plots/hist_fgb.pdf",
  hist,
  height = 7,
  width = 10,
  units = "in"
)

# take the average FGB per individual
fgb_mean <- fgb_unnested %>%
        group_by(species, bird_id)%>%
        summarise(mean_fgb = mean(fgb_width), n_bars = max(fgb))



fn1 <- "./2.data_cleaned/fgb_width/MZUSP_fgb_mean_per_individual.csv"
write_csv(fgb_mean, fn1)
fn2 <- "./2.data_cleaned/fgb_width/MZUSP_fgb_width_per_bar.csv"
write_csv(fgb_unnested, fn2)  
  
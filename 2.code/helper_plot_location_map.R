# set wd
if (Sys.info()[7] == "fcb5018") {
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7] == "Flavia") {
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
} else if (Sys.info()[7] == "tom") {
  setwd("/Users/tom/OneDrive - Imperial College London/1.PHD/2.R-dir/2.chapter")
}

# clear global env
rm(list = ls())

#install.packages(c("raster", "sf", "units", "tidyverse"))
library(sf)
library(units)
library(tidyverse)
library(egg)
library(gridExtra)

## 1) set projection saaeac ##---------------------------------------------------------------------------
saaeac <-
  '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
#--------------------------------------------------------------------------------------

## 2) load occupancy data ##------------------------------------------------------
occupancy <- read_csv("./FA_FC_final.csv")%>%
  # transform coordinate columns into simple features (WGS84 projection)
  st_as_sf(
    coords = c('lon_dd', 'lat_dd'),
    crs = 4326,
    remove = FALSE
  ) %>%
  # transform projection from WGS84 into SAAEAC
  st_transform(crs = saaeac)

## 3) mutate occupancy data ##----------------------------------------------------
# to add info to figure legend
occupancy_subset <- occupancy %>%
  #if column feather_tail is NA, then add "TRUE" to new column and vv
  mutate(is_fgb = ifelse(is.na(feather_tail), TRUE, FALSE))%>%
  #if column is_fgb is False, then individual was measured both for FA and FGB
  mutate(is_fgb = case_when(is_fgb == TRUE ~ "FA",
                   is_fgb == FALSE ~ "FA and FGB"))

map_brazil <-
  st_read("./2.data_raw/other_map_files/Estados_Brasil.shp")

map_biomes <-
  st_read("./2.data_raw/other_map_files/Biomas_Brasil.shp") %>%
  mutate(NOME = as.factor(NOME)) %>%
  st_transform(crs = 4326)


 p <-ggplot() +
  geom_sf(data = map_brazil,
          fill = "lightsteelblue2",
          color = "lightsteelblue4") +
  geom_sf(data = map_biomes,
          aes(fill = NOMES),
          alpha = 0.5)+
  geom_sf(data = occupancy_subset,
          aes(shape = is_fgb),
          size = 2) +
  scale_shape_manual(values = c("FA and FGB" = 19, "FA" = 2))+
  coord_sf() +
  theme_article() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.1),
        legend.text = element_text(size=10, face="bold"))

 ggsave(
   "./2.results/plots/map_locations.png",
   p,
   width = 7,
   height = 7,
   units = "in"
 )
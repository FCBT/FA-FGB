traits <- read_csv("./ATLANTIC_BIRD_TRAITS_completed_2018_11_d05.csv")
glimpse(traits)


left <- traits$Tarsus_length_left.mm.
right <- traits$Tarsus_length_right.mm.

str(left) #n=5640
str(right) #n=17231

left <- str_replace(left, "NA", NA_character_) 
right <- str_replace(right, "NA", NA_character_)

left <- left %>%
  as.data.frame()%>%
  drop_na()

right <- right %>%
  as.data.frame()%>%
  drop_na()

traits %>% 
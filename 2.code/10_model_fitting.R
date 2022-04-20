library(tidyverse)
library(brms)

FA_FC_data <- read_csv("FA_FC_final.csv")

FA_FC_data %>% head()

set.seed(1)

bay_mod_t <- FA_FC_data %>%
mutate(ind_size_fa = (mean_l + mean_r)/2)%>%
filter(FC_buffer == 600, type == "t", dist_km >= 0) %>%
brm(bf(fa_left_right ~ 0, sigma ~ log(dist_km) + (log(dist_km) | species)),
   data = .,
   family = gaussian,
   cores = 4, chains = 4, iter = 10000, 
   control=list(adapt_delta = 0.99))

bay_mod_w <- FA_FC_data %>%
mutate(ind_size_fa = (mean_l + mean_r)/2)%>%
filter(FC_buffer == 600, type == "w", dist_km >= 0) %>%
brm(bf(fa_left_right ~ 0, sigma ~ log(dist_km) + (log(dist_km) | species)),
   data = .,
   family = gaussian,
   cores = 4, chains = 4, iter = 10000, 
   control=list(adapt_delta = 0.99))

save(bay_mod_t,bay_mod_w,file = "./2.results/bayesmodels.RData")
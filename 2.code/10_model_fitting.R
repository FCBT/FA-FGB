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

#FC
bay_mod_t_FC <- FA_FC_data %>%
    filter(FC_buffer == 600, type == "t") %>%
    mutate(FC_proportion = FC_percentage.value/100) %>%
    brm(bf(fa_left_right ~ 0, sigma ~ FC_proportion * log(dist_km) + (log(dist_km) | species)),
        data = .,
        family = gaussian,
        cores = 4, chains = 4, iter = 10000, 
        control=list(adapt_delta = 0.99))

bay_mod_w_FC <- FA_FC_data %>%
    filter(FC_buffer == 600, type == "w") %>%
    mutate(FC_proportion = FC_percentage.value/100) %>%
    brm(bf(fa_left_right ~ 0, sigma ~ FC_proportion * log(dist_km) + (log(dist_km) | species)),
        data = .,
        family = gaussian,
        cores = 4, chains = 4, iter = 10000, 
        control=list(adapt_delta = 0.99))

save(bay_mod_t,bay_mod_w,bay_mod_t_FC,bay_mod_w_FC,file = "./2.results/bayesmodels.RData")

# load("./2.results/bayesmodels.RData")

# dist <- log(seq(min(FA_FC_data$dist_km), max(FA_FC_data$dist_km),length.out = 100))
# sp <- FA_FC_data %>%
#     filter(species != "Thalurania_glaucopis") %>%
#     pull(species) %>%
#     unique()

# pred_df <- list()
# for(i in 1:length(sp)){
#     df_sub <- FA_FC_data %>% filter(species == sp[i])
#     pred_df[[i]] <- tibble(species = sp[i],
#     dist_km = log(seq(min(df_sub$dist_km),max(df_sub$dist_km),length.out = 10)))

# }

# pred_df <- bind_rows(pred_df)
# pred_df <- cbind(pred_df,predict(bay_mod_w, newdata = pred_df)) %>%
#     as_tibble()

# p <- ggplot() +
#   geom_point(data = FA_FC_data %>% filter(type == "w", species != "Pyriglena_leucoptera"), aes(x = log(dist_km), y = fa_left_right), shape=3) + 
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   labs(y=NULL, x = NULL)
#     # facet_wrap(~species, scales = "free")

# p +
#     geom_ribbon(data = pred_df %>% filter(species != "Pyriglena_leucoptera"), 
#     aes(x = (dist_km), ymin = Q2.5, ymax = Q97.5, group = species),
#     alpha = 0.2)


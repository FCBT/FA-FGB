# use this script to fit model in terminal, which is quicker - see notebook for more details.
library(tidyverse)
library(brms)

# load data
FGB_FC_data = read_csv("../2.data_model/FGB_FC_final.csv")

# fit models

set.seed(1)

models <- list()

#get buffers vectors
buffers <- unique(FGB_FC_data$FC_buffer)

#loop over buffers and fit all models
k = 1

start_time <- Sys.time()
start_time

for(i in buffers){
    print(paste(i, k))
    models[[paste(i)]] <- FGB_FC_data %>% 
    filter(FC_buffer == i) %>% 
    brm(bf(mean_fgb ~ FC_proportion * log(dist_km) + (FC_proportion * log(dist_km)| species)),
        data = .,
        family = gaussian,
        cores = 4, chains = 4, 
        iter = 10000, 
        control=list(adapt_delta = 0.99),
        silent = 2)

    k <- k+1
    
}
end_time <- Sys.time()
end_time

print(end_time - start_time)

#save output
save(models, file =  "../2.results/bayesmodels_buffers_FGB_randef.RData")
print("models saved")
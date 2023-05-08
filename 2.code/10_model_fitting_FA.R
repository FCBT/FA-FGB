library(tidyverse)
library(brms)

#load data
FA_FC_data <- read_csv("../2.data_model/FA_FC_final.csv") 

set.seed(1)

#fit models
models <- list()

#get buffers and trait vectors
buffers <- unique(FA_FC_data$FC_buffer)
types <- unique(FA_FC_data$type)

start_time <- Sys.time()
start_time

#loop over both and fit all models
k = 1
for(i in buffers){
    for(j in types){
        print(paste(i, j, k / (length(buffers) * length(types))))
       models[[paste(i,j, sep = " ", " ")]] <- FA_FC_data %>%
            filter(FC_buffer == i, type == j) %>%
            brm(bf(fa_left_right ~ 0, sigma ~ FC_proportion * log(dist_km) + (FC_proportion * log(dist_km) | species)),
                data = .,
                family = gaussian,
                cores = 4, chains = 4, iter = 10000, 
                control=list(adapt_delta = 0.99),
                silent = 2)
        k <- k+1
    }
}
end_time <- Sys.time()
end_time

print(end_time - start_time)
#save output
save(models, file =  "../2.results/bayesmodels_buffers_FA_randef.RData")
print("models saved")
library(tidyverse)
library(brms)

#load data and transform
FA_FC_data <- read_csv("FA_FC_final.csv") %>%
    mutate(FC_proportion = FC_percentage.value/100, log_dist = log(dist_km)) %>%  #add columns
    filter(!is.na(FC_buffer))

FA_FC_data %>% head()

set.seed(1)

#fit models
models <- list()

#get buffers and trait vectors
buffers <- unique(FA_FC_data$FC_buffer)
types <- unique(FA_FC_data$type)

#loop over both and fit all models
k = 1
for(i in buffers){
    for(j in types){
        print(paste(i, j, k / (length(buffers) * length(types))))
       models[[paste(i,j, sep = " ", " ")]] <- FA_FC_data %>%
            filter(FC_buffer == i, type == j) %>%
            brm(bf(fa_left_right ~ 0, sigma ~ FC_proportion * log(dist_km) + (log(dist_km) | species)),
                data = .,
                family = gaussian,
                cores = 4, chains = 4, iter = 10000, 
                control=list(adapt_delta = 0.99),
                silent = 2)
        k <- k+1
                
    }
}

#save output
save(models, file =  "./2.results/bayesmodels_buffers.RData")

#initial analysis
load("./2.results/bayesmodels_buffers.RData")

#get estimates
est_list <- list()

for(i in 1:length(models)){
    summary_x <- summary(models[[i]]) #summary object
    df <- summary_x$fixed #get fixed effects
    df$name <- names(models)[i] #get the buffer/trait name from the list
    df$param <- row.names(df) #add the parameters
    row.names(df) <- NULL
    df <- df %>%
        tidyr::separate(name,c("buffer","trait")) #sepateate name into buffer size and trait

    est_list[[i]] <- df
}

#plot with errorbars
bind_rows(est_list) %>%
    filter(abs(Rhat - 1) < 0.1) %>%
    mutate(buffer = as.numeric(buffer)) %>%
    ggplot(aes(x=buffer,y=Estimate))+
        geom_point()+
        geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`))+
        facet_grid(param ~ trait, scales = "free")

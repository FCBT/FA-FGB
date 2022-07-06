#--------------------------------------------------------------------------------
# Flavia Trigo, created on 27/Aug/2021
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


library(brms) # for Bayesian analysis
library(tidyverse) # for tidy code
library(egg) # for nice professional figures
library(lme4) # for frequentist analysis
library(lmerTest) # for p-values


# clear global env
rm(list = ls())

# load forest cover data
FC <- read_csv("./2.data_cleaned/FC_calculated.csv", col_types = cols(
  species = col_character(),
  bird_id = col_character()
))

# turn all NAs in column percentage of FC into zero
FC$FC_percentage.value[is.na(FC$FC_percentage.value)] <- 0

#load distance data
dist <- read_csv("./2.data_cleaned/NHM-MZUSP_dist2edge_data.csv")%>%
  dplyr::select(-c(11:14)) %>%
  dplyr::select(-"feather_tail")

# function to keep the sign of distance (negative for outside the range and positive for inside the range)
sqrt_sign <- function(x){sign(x) * sqrt(abs(x))}

# calculate sqrt dist in km
dist <- dist %>%
  mutate(sqrt_dist = sqrt_sign(dist_km)) %>%
  relocate(sqrt_dist, .after = dist_km)

# load feather growth bar data
FGB <-read_csv("./2.data_cleaned/fgb_width/MZUSP_fgb_mean_per_individual.csv", col_types = cols(
  species = col_character(),
  bird_id = col_character(),
  mean_fgb = col_double(),
  n_bars = col_double()
))

# load fluctuating asymmetry data
FA <-read_csv("./2.data_cleaned/fa/NHM-MZUSP_fa_wing_tarsus.csv")

# join FA data to distances
FA_data <- dist %>%
  dplyr::left_join(FA)

# join FA_dist data to forest cover
FA_FC_data <- FA_data %>%
  dplyr::left_join(FC) %>%
  filter(!is.na(fa_left_right))
View(FA_FC_data)


# join FGB data to distances
FGB_data <- dist %>%
  dplyr::left_join(FGB, by = "bird_id") %>%
  select(-species.y)%>%
  rename(species = species.x) %>%
  filter(!is.na(mean_fgb))

# joins FGB_dist to forest cover
FGB_FC_data <- FGB_data %>%
  dplyr::left_join(FC)


# 1) FA #####
table(is.na(FA_data$sqrt_dist))

ggplot(FA_FC_data, aes(x=fa_ratio)) + 
  geom_histogram(bins = 30)

FA_FC_data %>%
  ggplot(aes(x=sqrt_dist, y = fa_ratio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~type)+
  theme_article()
  
ggplot(FA_FC_data, aes(x=fa_abs_mm)) + 
  geom_histogram(bins = 30)

FA_FC_data %>%
  ggplot(aes(x=sqrt_dist, y = fa_ratio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~type)+
  theme_article()
cor.test(x=FA_data$sqrt_dist, y = FA_data$fa_ratio, method = "pearson")
# not correlated 0.036 

# # plot correlation 
# library(ggpubr)
#  ggscatter(FA_data, x="sqrt_dist", y="fa_ratio", 
#                                add= "reg.line", conf.int = TRUE,   # add regression line and CI
#                                color = "type", palette = c("grey57", "grey17"), # color by groups
#                                shape = "type" )+ # change shape of points by group
#   stat_cor(aes(color = type), method = "pearson", label.x =0.2, p.accuracy = 0.001) + # add correlation coef
#   ylab("Fluctuating Asymmetry ratio") + #change y label
#   xlab("Distance to edge (sqrt km)") + #change x label
#   theme_article() #+
#   # theme(legend.title = element_blank(), #remove legend title
#   #       legend.position = c(0.85, 0.9), # adjust legend position
#   #       legend.key.size = unit(1,"cm"), # change legend size
#   #       legend.key.height = unit(0.6, "cm"), # change legend height
#   # ) 

# calculate Bayesian model for FA as a function of distance
FA_fit <- brms::brm(formula = fa_ratio ~ sqrt_dist + (1 + sqrt_dist|species), 
                   data = FA_data, 
                   family = gaussian(link = "identity"),
                   prior = set_prior("normal(0,10)", class = "b"),
                   cores = 4)

summary(FA_fit, loo =TRUE)
stancode(FA_fit)
standata(FA_fit)


#calculate Bayesian model for FA as a function of distance and forest cover for different radius

data_600r <- FA_FC_data %>% dplyr::filter(FC_buffer == 600) 

FA_fit_600 <- brms::brm(formula = fa_ratio ~ sqrt_dist * FC_percentage.value + (sqrt_dist*FC_percentage.value|species), 
                    data = data_600r, 
                    family = gaussian(link = "identity"),
                    prior = set_prior("normal(0,10)", class = "b"),
                    cores = 4,
                    iter = 5000
)

FA_FC_data_new <- FA_FC_data %>%
  filter(!is.na(FC_buffer))

FA_bayes_results <- list()

buffer <- unique(FA_FC_data_new$FC_buffer)

for( i in 1:length(buffer)){
    
    this_buffer <- buffer[i]
    model_data <- FA_FC_data %>% 
      dplyr::filter(FC_buffer == this_buffer, dist_km > -500) %>%
      group_by(species) %>% filter(diff(range(dist_km)) > 200) %>%
      mutate(FC_percentage.value  = FC_percentage.value/100)
    
    
    lmer_fit <- lmer(fa_abs_mm ~ sqrt_dist * FC_percentage.value + (1|species),
                     data = model_data)
    
    
    fit <- brm(bf(fa_abs_mm ~ sqrt_dist * FC_percentage.value, sigma ~ sqrt_dist), data = model_data, family = gaussian())
    
    plot(fit)
    
    FA_fit <- brms::brm(formula = fa_abs_mm ~ sqrt_dist * FC_percentage.value + (sqrt_dist + FC_percentage.value|species), 
                        data = model_data, 
                        family = gaussian(link = "identity"),
                        # prior = set_prior("normal(0,10)", class = "b"),
                        cores = 4,
                        iter = 4000
                         )
    
    FA_bayes_results[[i]] <- FA_fit
  }
  

name_buffer <- paste("fc_percentage_", this_buffer, "_radius")
summary(FA_fit, waic = TRUE)
saveRDS()
FA_results


model_data %>%
  ggplot(aes(dist_km, (fa_abs_mm), color = species))+
      geom_point()

# 2) FGB #####

FGB_data %>%
  # filter(dist_km > 0.0)  %>%
  ggplot(aes(x = dist_km, y = log(mean_fgb), color = species))+
  geom_point()+
  geom_smooth(method = "lm",se = F)+
  theme(legend.position = "none")


ggplot(FGB_data, aes(x=log(mean_fgb))) + 
  geom_histogram(bins = 30)

FGB_fit <- brms::brm(formula = log(mean_fgb) ~ sqrt_dist + (1+ sqrt_dist|species), 
                    data = FGB_data, 
                    family = gaussian(link = "identity"),
                    #prior = set_prior("normal(0,10)", class = "b"),
                    cores = 4, iter = 6000)

summary(FGB_fit, waic = TRUE)
stancode(FGB_fit)
standata(FGB_fit)

data_600r_FGB <- FGB_FC_data %>% dplyr::filter(FC_buffer == 600) 

FGB_fit_600 <- brms::brm(formula = fa_ratio ~ sqrt_dist * FC_percentage.value + (sqrt_dist*FC_percentage.value|species), 
                        data = data_600r_FGB, 
                        family = gaussian(link = "identity"),
                        prior = set_prior("normal(0,10)", class = "b"),
                        cores = 4,
                        iter = 5000
)



### Data analyses | FA ###


# setting wd in pc or laptop
if(Sys.info()[7] == "fcb5018"){
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
} else if (Sys.info()[7] == "Flavia"){
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
}

library(lme4)
library(lmerTest) # to get p values for gaussian lmer
#install.packages('sjPlot')
library(sjPlot)
library(ggplot2)
# load data for model
data <- read.csv('./1.2.2.3.Data_cleaned/LSR_FA_model_data_DE_FC_20200624.csv', stringsAsFactors = FALSE)

str(data)
data$Bird_ID <-as.factor(data$Bird_ID)
head(data)

# drop columns not necessary for analysis
data <- data[,c(-1)]

#rename column
names(data)[7] <- 'forest_cover_prop'

data$log_totalFA <- log(data$TotalFA + 1)

pairs(~ sqrt_dist_edge + forest_cover_prop + log_totalFA, data=data)

max(data$TotalFA)

#-------------------------------------------------------------------------------------
#outlier <- data %>% select(TotalFA) %>% summarise(fun = max)
fix(data) # real totalFA value is 2.27; I forgot to add a '.' to the 
# dataframe for one of the wing measurements - i fixed that in the masterfile,
# but havent fixed in the model file -DO THAT!
#-------------------------------------------------------------------------------------
library(dplyr)
data1<- data %>%
  mutate(Categories = cut(forest_cover_prop,breaks = c(0.0,0.3,0.6,1.0),labels = F))
  # c("0-30%", "30-60%", "60-100%")
glmer_model_FA_2 <- lmer(log_totalFA ~ sqrt_mean_dist_edge + forest_cover_prop + (1|Date_Collected) + (1|Species) +(1|Location),
                       data=data1, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

data$forest_cover_prop <- (data$forest_cover_prop) + 0.01

### this one resulted in a singular fit ###--------------------------------------------------------------------
# I am gonna try to simplify the model...
glmer_model_FA <- lmer(TotalFA ~ sqrt_dist_edge + forest_cover_prop + (1|Date_Collected) + (1|Species) +(1|Location),
                      data=data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
#--------------------------------------------------------------------------------------------------------------
### this one also resulted on a singular fit #
glmer_model_FA <- lmer(TotalFA ~ sqrt_dist_edge + (1|Date_Collected) + (1|Species) +(1|Location),
                       data=data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
#--------------------------------------------------------------------------------------------------------------
glmer_model_FA <- lmer(TotalFA ~ forest_cover_prop + (1|Date_Collected) + (1|Species) +(1|Location),
                       data=data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
#--------------------------------------------------------------------------------------------------------------
# THIS ONE WORKED ## no singular fit - removed location from random effects
glmer_model_FA2 <- lmer(TotalFA ~ sqrt_dist_edge * forest_cover_prop + (1|Date_Collected) + (1|Species),
                       data=data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(glmer_model_FA2)
anova(glmer_model_FA)
?lmer

plot_model(glmer_model_FA)
plot_model(glmer_model_FA, type = "int")
theme_set(theme_classic())

plot_model(glmer_model_FA2, type = "int", 
           terms = c("sqrt_dist_edge", "forest_cover_prop"), 
           mdrt.values = "meansd",
           title="",
           axis.title = c("Sqrt distance to edge (km)","Total FA (mm)"),
           legend.title = "FC mean and sd")

plot_model(glmer_model_FA2, type = "pred", 
           title="",
           terms = "sqrt_dist_edge")
           #show.data = TRUE)

#, 
#           terms = c("sqrt_dist_edge", "forest_cover_prop"), 
#           mdrt.values = "meansd",
#           title="",
#           axis.title = c("Sqrt distance to edge (km)","Total FA (mm)"),
#           legend.title = "FC mean and sd")










plot_model(glmer_model_FA, type= "resid")
get_model_data()

plot_model(glmer_model_FA_2, type = "pred", terms = c("sqrt_mean_dist_edge", "forest_cover_prop"))
plot_model(glmer_model_FA_2, group.terms = c(1,2))
p <- plot_model(glmer_model_FA_2, type = "diag")
plot_grid(p)
summary(glmer_model_FA_2)

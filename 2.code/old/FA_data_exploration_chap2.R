# setting wd in pc or laptop
if(Sys.info()[7] == "fcb5018"){
  setwd("C:/Users/fcb5018/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
} else if (Sys.info()[7] == "Flavia"){
  setwd("C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2")
}

data <-read.csv("./1.2.2.3.Data_cleaned/MZUSP_dataset_totalFA.csv", stringsAsFactors = FALSE)
str(data)
head(data)

data$Sex <- as.factor(data$Sex)




##### based on Palmer et al, 2001 ############

# For bilateral traits, genotype and environment typically affect both sides similarly,
# so the right and left sides exhibit positive covariation. This is why the variance of 
# the difference between two sides os such a convenient number:

# var(R - L) = var(R) + var(L) - 2 covar(RL)

# where covar(RL) is the covariance bewtween R and L

# covar(RL) = Sum[(R-MeanR)(L-MeanL)]/(n-1)

# and where R and L are the population means of the right (R) and left (L) sides, 
# and N is the number of individuals.In theory, the term 2 covar(RL) removes all 
# of the positive covariation between R and L due to genotype and environment, 
# leaving only the uncorrelated random variation of R and L due to developmental instability.



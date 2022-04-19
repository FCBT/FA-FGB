install.packages('pwr')
library(pwr)
?pwr

# FGB ~ dist_edge + forest_cover + 1|date + 1|location + 1|species 
# p = 6 all effects + intercept

# power analysis for linear models
pwr.f2.test()
pwr.f2.test(u = NULL, v = NULL, f2 = NULL, sig.level = 0.05, power = NULL)
# u: degrees of freedom for numerator = p - 1
                                      # p = predictor 


# f2: effect size (cohen.ES)
# sig.level: significance level (type 1 error) - 5% = 0.05
# power: power of test (1-type 2 error) - 0.8 (literature)

?cohen.ES
cohen.ES(test = "f2", size = "large")
cohen.ES(test = "anov", size = "small")
# effect.size = 0.02 (small)
# effect.size = 0.15 (medium)
# effect.size = 0.35 (large)

#substitute all, but 'v'
lm_power <- pwr.f2.test(u = 6, f2 = 0.15, sig.level = 0.05, power = 0.8)
plot(lm_power)

anova_power <- pwr.anova.test(k=2, f=0.1, sig.level = 0.05, power = 0.8)
plot(anova_power)


# find 'n' by substituting 'v' value from above and 'p'
# v: degrees of freedom for denominator = n - p
# n = number of observations
# for small effect size
# v = n - p
681 = n - 6
n <- 681 + 6
n

# for medium effect size
91 = n - 6
n = 97
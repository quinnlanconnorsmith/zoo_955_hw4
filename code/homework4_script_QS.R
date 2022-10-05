library(tidyverse)

bees_df <- read_tsv('data/bees.txt') %>% 
  mutate(
    # Per Olaf's instruction at the end of class 10/4, turn 
    # Hive from a numeric into a factor.
    Hive = as.factor(Hive),
    # Also per Olaf's instruction at the end of class 10/4,
    # convert the "Infection" metric which quantifies the 
    # degreee of infection into a simple yes/no for whether
    # a bee is infected or not.
    isInfected = Infection > 0)

##### Q1 ##### 
# Does variance of spore density appear homogeneous among hives? Why or why not? #
model1 <- lm(Spobee~Hive, data=bees_df)

summary(model1)

#Not homogeneous, we have different coefficients depending on Hive number

##### Q2 ##### 
# Try some transformations of the response variable to homogenize the variances 
# (or at least improve it). Which transformation of spore density seems reasonable? Why?

model2 <- lm((Spobee)^2~Hive, data=bees_df)

summary(model2)
#Still different w/ square transform
model3 <- lm((Spobee)^0.5~Hive, data=bees_df)

summary(model3)
#I guess this is better? Still got some wildly different estimates 

model4 <- lm((Spobee)^0.25~Hive, data=bees_df)

summary(model4)
#4th root transformation is the 'best'?


##### Q3 ##### 

# Develop a simple linear model for transformed spore density. Include infection 
# (fInfection01), number of bees (sBeesN) and their interaction as explanatory 
# variables. Check for a hive effect by plotting standardized residuals (see the 
# residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show 
# your code and your plots. Do residuals look homogenous among hives?


##### Q4 ##### 

# What are the advantages of including hive as a random effect, rather than as a fixed effect?
#This allows for additional model degrees of freedom to be conserved
#Because we may not be interested in specific hives influence on spores (also this varies based on so many things), we
#can lump hives together as a random effect 

##### Q5 ##### 

# Step 3. Choose a variance structure or structures (the random effects). What 
# random effects do you want to try?


##### Q6 ##### 

# Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package 
# (transformed spore density is response, fInfection01, sBeesN, and interaction 
# are the explanatory variables). Show your code.


##### Q7 ##### 

# Step 5. Compare the linear regression and ME model(s) with a likelihood ratio 
# test, including correction for testing on the boundary if needed. Use the anova() 
# command. This will re-fit your lmer model with maximum likelihood, but this is OK 
# (note there are some debates about exactly how to best compare an lm and lmer 
# model). Show your work and the results. Which random effect structure do you 
# choose based on the results?


##### Q8 ##### 

# Step 6. Check the model: plot standardized residuals vs. fitted values and vs. 
# each predictor. (You can get standardized residuals with residuals(yourmodel, 
# type='pearson')). How do they look?


##### Q9 ##### 

# Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a 
# reduced model without the interaction term, also fit with ML. Use anova() to 
# compare the models. Which model do you choose? Why?


##### Q10 ##### 

# Step 8. Iterate #7 to arrive at the final model. Show your work. What is your 
# final set of fixed effects?


##### Q11 ##### 

# Step 9. Fit the final model with REML. Check assumptions by plotting a histogram 
# of residuals, plotting Pearson standardized residuals vs. fitted values, and 
# plotting Pearson standardized residuals vs. explanatory variables. Are there 
# issues with the model? If so, how might you address them?


##### Q12 ##### 

# Step 10. Interpret the model. The summary() command is useful here. What have 
# you learned about American Foulbrood?


##### Q13 ##### 

# Calculate the correlation between observations from the same hive as 
# variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). 
# Given the correlation among observations from the same hive, do you think it's 
# a good use of time to sample each hive multiple times? Why or why not?


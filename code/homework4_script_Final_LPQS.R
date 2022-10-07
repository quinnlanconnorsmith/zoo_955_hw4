library(tidyverse)
library(nlme)


bees_df <- read_tsv('data/bees.txt') %>% 
  mutate(
    # Per Olaf's instruction at the end of class 10/4, turn 
    # Hive from a numeric into a factor.
    Hive = as.factor(Hive),
    # Also per Olaf's instruction at the end of class 10/4,
    # convert the "isInfected" metric which quantifies the 
    # degreee of isInfected into a simple yes/no for whether
    # a bee is infected or not.
    isInfected = Infection > 0)

##### Q1 ##### 
# Does variance of spore density appear homogeneous among hives? Why or why not? #
model1 <- lm(Spobee~Hive, data=bees_df)

summary(model1)
plot(model1, ask=F)

#Not homogeneous, we have different variances depending on Hive number

##### Q2 ##### 
# Try some transformations of the response variable to homogenize the variances 
# (or at least improve it). Which transformation of spore density seems reasonable? Why?

model2 <- lm((Spobee)^2~Hive, data=bees_df)
plot(model2)
summary(model2)
#Still different w/ square transform
model3 <- lm((Spobee)^0.5~Hive, data=bees_df)
plot(model3)
summary(model3)
#I guess this is better? Still got some wildly different estimates 
model4 <- lm((Spobee)^0.25~Hive, data=bees_df)
plot(model4)
summary(model4)

#Log transform
bees_df$logspobee <-log10(bees_df$Spobee)
bees_df[bees_df == -Inf] <-0

model5 <- lm(logspobee~Hive, data=bees_df)
plot(model5)

summary(model5)
#So log is pretty good! 
#Variance seems to be fairly constant across hives. 

##### Q3 ##### 

# Develop a simple linear model for transformed spore density. Include isInfected 
# (fisInfected01), number of bees (sBeesN) and their interaction as explanatory 
# variables. Check for a hive effect by plotting standardized residuals (see the 
# residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show 
# your code and your plots. Do residuals look homogenous among hives?

model6 <- lm(logspobee ~ isInfected + BeesN + isInfected*BeesN, data=bees_df)
summary(model6)
plot(bees_df$Hive, residuals(model6, type='pearson'))
#Residuals do not look homogeneous among the hives 

##### Q4 ##### 

# What are the advantages of including hive as a random effect, rather than as a fixed effect?
#This allows for additional model degrees of freedom to be conserved
#Because we may not be interested in specific hives influence on spores (also this varies based on so many things), we
#can lump hives together as a random effect 

##### Q5 ##### 

# Step 3. Choose a variance structure or structures (the random effects). What 
# random effects do you want to try?
#I think that hive is the most logical thing to include as a random effect 

##### Q6 ##### 

# Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package 
# (transformed spore density is response, fisInfected01, sBeesN, and interaction 
# are the explanatory variables). Show your code.

mega_model<- lme(logspobee ~ isInfected + BeesN + isInfected*BeesN, random = ~1|Hive, method = 'REML', data=bees_df)
summary(mega_model)
plot(mega_model)

##### Q7 ##### 

# Step 5. Compare the linear regression and ME model(s) with a likelihood ratio 
# test, including correction for testing on the boundary if needed. Use the anova() 
# command. This will re-fit your lmer model with maximum likelihood, but this is OK 
# (note there are some debates about exactly how to best compare an lm and lmer 
# model). Show your work and the results. Which random effect structure do you 
# choose based on the results?

gls_model <- gls(logspobee ~ isInfected + BeesN + isInfected*BeesN, data=bees_df)
anova(gls_model, mega_model)

#The random intercept (Hive) is a better model because p<0.001 and AIC is lower 

##### Q8 ##### 

# Step 6. Check the model: plot standardized residuals vs. fitted values and vs. 
# each predictor. (You can get standardized residuals with residuals(yourmodel, 
# type='pearson')). How do they look?

mega_model_residuals <- residuals(mega_model, type='pearson')
mega_model_fitted <- fitted.values(mega_model)
#Residuals vs. fitted 
plot(mega_model_fitted,mega_model_residuals)
abline(h=0)

#The rest 
plot(mega_model_residuals~isInfected, data=bees_df)
abline(h=0)
plot(mega_model_residuals~BeesN, data=bees_df)
abline(h=0)
plot(mega_model_residuals~Hive, data=bees_df)
abline(h=0)
#These look alright distributed around 0 

##### Q9 ##### 

# Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a 
# reduced model without the interaction term, also fit with ML. Use anova() to 
# compare the models. Which model do you choose? Why?

mega_model2<- lme(logspobee ~ isInfected + BeesN + isInfected*BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
mega_model3<- lme(logspobee ~ isInfected + BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
anova(mega_model2, mega_model3)

#This interaction term can be dropped


##### Q10 ##### 

# Step 8. Iterate #7 to arrive at the final model. Show your work. What is your 
# final set of fixed effects?

mega_model4<- lme(logspobee ~ isInfected + BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
mega_model4_dropinf<- lme(logspobee ~ BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
mega_model4_dropbees<- lme(logspobee ~ isInfected, random = ~1|Hive, method = 'ML', data=bees_df)

summary(mega_model4)
summary(mega_model4_dropinf)
summary(mega_model4_dropbees)

anova(mega_model4, mega_model4_dropinf)
#Okay, so isInfected is significant 
anova(mega_model4, mega_model4_dropbees)
#Bees are also significant 

mega_model_final_ML <-lme(logspobee ~ isInfected+ BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
summary(mega_model_final_ML)
##### Q11 ##### 

# Step 9. Fit the final model with REML. Check assumptions by plotting a histogram 
# of residuals, plotting Pearson standardized residuals vs. fitted values, and 
# plotting Pearson standardized residuals vs. explanatory variables. Are there 
# issues with the model? If so, how might you address them?

mega_model_final <-lme(logspobee ~ isInfected+BeesN, random = ~1|Hive, method = 'REML', data=bees_df)
hist(residuals(mega_model_final))
#Slight left tail 
final_residuals <-residuals(mega_model_final, type='pearson')
final_fitted <-fitted.values(mega_model_final)

#Same as above but new fitted and residuals 
plot(final_fitted,final_residuals)
abline(h=0)

#The rest 
plot(final_residuals~isInfected, data=bees_df)
abline(h=0)
plot(final_residuals~BeesN, data=bees_df)
abline(h=0)
plot(final_residuals~Hive, data=bees_df)
abline(h=0)

#Still looks alright, balanced around 0 
#Bees as a model term was not significant but the model had a slightly better AIC. We could drop that to make a simpler model

##### Q12 ##### 

# Step 10. Interpret the model. The summary() command is useful here. What have 
# you learned about American Foulbrood?

summary(mega_model_final)
#Given the bees_df data, an interaction between Bees and binomial infection rate is not significant
#However, log spores is correlated to determining Infection and the number of bees
#BeesN had an statistically insignicant p-value but a probable biological p-value 

##### Q13 ##### 

# Calculate the correlation between observations from the same hive as 
# variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). 
# Given the correlation among observations from the same hive, do you think it's 
# a good use of time to sample each hive multiple times? Why or why not?

#None of the below works lmao 
#We should be able to pull the random effect from the summary, but I can't find it 

#Trying VarCorr

# Get the variance of the random effect
var_hive_resid <- VarCorr(lme_logspobee_final)
var_hive <- as.numeric(var_hive_resid[1])
var_resid <- as.numeric(var_hive_resid[2])

# Correlation
var_hive/(var_hive + var_resid)

#Correlation between observations is high (.8906137)
#In general, observations will be similar within the same hive 
#However, some individual hive variance is large, so in a study with few hives, 
#repeated measures of a hive may be beneficial 


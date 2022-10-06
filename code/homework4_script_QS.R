library(tidyverse)
library(nlme)

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

##### Q3 ##### 

# Develop a simple linear model for transformed spore density. Include infection 
# (fInfection01), number of bees (sBeesN) and their interaction as explanatory 
# variables. Check for a hive effect by plotting standardized residuals (see the 
# residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show 
# your code and your plots. Do residuals look homogenous among hives?

model6 <- lm(logspobee ~ Infection + BeesN + Infection*BeesN, data=bees_df)
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
# (transformed spore density is response, fInfection01, sBeesN, and interaction 
# are the explanatory variables). Show your code.

mega_model<- lme(logspobee ~ Infection + BeesN + Infection*BeesN, random = ~1|Hive, method = 'REML', data=bees_df)
summary(mega_model)
plot(mega_model)

##### Q7 ##### 

# Step 5. Compare the linear regression and ME model(s) with a likelihood ratio 
# test, including correction for testing on the boundary if needed. Use the anova() 
# command. This will re-fit your lmer model with maximum likelihood, but this is OK 
# (note there are some debates about exactly how to best compare an lm and lmer 
# model). Show your work and the results. Which random effect structure do you 
# choose based on the results?

anova(model6, mega_model)

#??????????????

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
plot(mega_model_residuals~Infection, data=bees_df)
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

mega_model2<- lme(logspobee ~ Infection + BeesN + Infection*BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
mega_model3<- lme(logspobee ~ Infection + BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
anova(mega_model2, mega_model3)

#This interaction is not significant 


##### Q10 ##### 

# Step 8. Iterate #7 to arrive at the final model. Show your work. What is your 
# final set of fixed effects?

mega_model2<- lme(logspobee ~ Infection + BeesN + Infection*BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
mega_model_dropinf<- lme(logspobee ~ BeesN, random = ~1|Hive, method = 'ML', data=bees_df)
mega_model2_dropbees<- lme(logspobee ~ Infection, random = ~1|Hive, method = 'ML', data=bees_df)

summary(mega_model2)
summary(mega_model_dropinf)
summary(mega_model2_dropbees)

anova(mega_model2, mega_model_dropinf)
#Okay, so infection is significant 
anova(mega_model2, mega_model2_dropbees)
#Bees are not significant! (Speaking in terms of the model, not in real life)

mega_model_final_ML <-lme(logspobee ~ Infection, random = ~1|Hive, method = 'ML', data=bees_df)
summary(mega_model_final_ML)
##### Q11 ##### 

# Step 9. Fit the final model with REML. Check assumptions by plotting a histogram 
# of residuals, plotting Pearson standardized residuals vs. fitted values, and 
# plotting Pearson standardized residuals vs. explanatory variables. Are there 
# issues with the model? If so, how might you address them?

mega_model_final <-lme(logspobee ~ Infection, random = ~1|Hive, method = 'REML', data=bees_df)
hist(residuals(mega_model_final))
#Slight left tail 
final_residuals <-residuals(mega_model_final, type='pearson')
final_fitted <-fitted.values(mega_model_final)

#Same as above but new fitted and residuals 
plot(final_fitted,final_residuals)
abline(h=0)

#The rest 
plot(final_residuals~Infection, data=bees_df)
abline(h=0)
plot(final_residuals~BeesN, data=bees_df)
abline(h=0)
plot(final_residuals~Hive, data=bees_df)
abline(h=0)

##### Q12 ##### 

# Step 10. Interpret the model. The summary() command is useful here. What have 
# you learned about American Foulbrood?

summary(mega_model_final)
#Given the bees_df data, number of bees is not contained in the most parsimonious model 
#However, log spores is correlated to determining infection 

##### Q13 ##### 

# Calculate the correlation between observations from the same hive as 
# variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). 
# Given the correlation among observations from the same hive, do you think it's 
# a good use of time to sample each hive multiple times? Why or why not?

#None of the below works lmao 
#We should be able to pull the random effect from the summary, but I can't find it 

f1 <- function(final_fitted) {
  c(re_var = c(VarCorr(final_fitted)[[1]]),  ## RE variance
    resid_var  = sigma(final_fitted)^2)      ## residual variance
}

das_boot <- confint(mega_model_final,
              method = "boot",
              nsim = 1000,
              FUN = function(fitted) {
                c(re_var = c(VarCorr(fitted)[[1]]),
                  resid_var  = sigma(fitted)^2)
              })

get_variance(mega_model_final, component =c("random"))
var.random(mega_model_final)
?get_variance
??get_variance

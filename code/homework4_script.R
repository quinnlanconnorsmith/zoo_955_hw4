library(nlme) # Needed for the `lme()` fxn
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
    isInfected = as.factor(Infection > 0))
# Do we need to scale the BeesN column values somehow?
# Got a warning when I used `lmer()` about some predictor variables
# being on very different scales + instructions keep referencing
# the `sBeesN` column. Could that `s` stand for "scaled"?

##### Q1 ##### 
# Does variance of spore density appear homogenous among hives? Why or why not?

lm_spobee_hive <- lm(Spobee ~ Hive, data = bees_df)
plot(lm_spobee_hive, ask=F) # No, variance increases with higher values

##### Q2 ##### 
# Try some transformations of the response variable to homogenize the variances 
# (or at least improve it). Which transformation of spore density seems reasonable? Why?

bees_df_transf <- bees_df %>% 
  mutate(LogSpobee = log10(Spobee + 1),
         SqrtSpobee = sqrt(Spobee),
         CtrSpobee = Spobee - mean(bees_df$Spobee))

# Try log transform
lm_logspobee_hive <- lm(LogSpobee ~ Hive, data = bees_df_transf)
plot(lm_logspobee_hive, ask=F) 
summary(lm_logspobee_hive)
# This one appears to be the best because variance is constant (see plot 1 from this
# output) and the data are normally distributed (see plot 2, the Q-Q plot)

# Try square root transform
lm_sqrtspobee_hive <- lm(SqrtSpobee ~ Hive, data = bees_df_transf)
plot(lm_sqrtspobee_hive, ask=F) 
# This looks bad - variance increases with increasing values and the data are not 
# normally distributed (see q-q plot)

# Try centering
lm_ctrspobee_hive <- lm(CtrSpobee ~ Hive, data = bees_df_transf)
plot(lm_ctrspobee_hive, ask=F) 
# Also not so great - variance increases at higher values and not normally 
# distributed based on q-q plot.

##### Q3 ##### 

# Develop a simple linear model for transformed spore density. Include infection 
# (fInfection01), number of bees (sBeesN) and their interaction as explanatory 
# variables. Check for a hive effect by plotting standardized residuals (see the 
# residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show 
# your code and your plots. Do residuals look homogenous among hives?

lm_logspobee <- lm(LogSpobee ~ isInfected + BeesN + isInfected*BeesN,
                   data = bees_df_transf)
summary(lm_logspobee)
plot(bees_df_transf$Hive, residuals(lm_logspobee, type='pearson')) # Example above
plot(rstandard(lm_logspobee) ~ Hive, data = bees_df_transf) # Example from reading

# No, there is great variety in the variances and no apparent trend

##### Q4 ##### 

# What are the advantages of including hive as a random effect, rather than as a fixed effect?

# So that we can account for variation in data for each hive, but don't include
# it as a predictor. This will mean more predictive power (fewer degrees of freedom
# used up) for the final model.

##### Q5 ##### 

# Step 3. Choose a variance structure or structures (the random effects). What 
# random effects do you want to try?

# I think we just want Hive as a random effect. Infection should certainly
# be one of our fixed effects. I think we would also want to be able to 
# include population size (number of bees) as a fixed effect.

##### Q6 ##### 

# Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package 
# (transformed spore density is response, fInfection01, sBeesN, and interaction 
# are the explanatory variables). Show your code.

lme_formula <-  formula(LogSpobee ~ isInfected + BeesN + isInfected*BeesN)
lme_logspobee <- lme(lme_formula, random = ~1|Hive, method = "REML", data = bees_df_transf)
summary(lme_logspobee)
plot(lme_logspobee, ask=F) 

##### Q7 ##### 

# Step 5. Compare the linear regression and ME model(s) with a likelihood ratio 
# test, including correction for testing on the boundary if needed. Use the anova() 
# command. This will re-fit your lmer model with maximum likelihood, but this is OK 
# (note there are some debates about exactly how to best compare an lm and lmer 
# model). Show your work and the results. Which random effect structure do you 
# choose based on the results?

gls_logspobee <- gls(lme_formula, data = bees_df_transf)
anova(gls_logspobee, lme_logspobee)

# The model with Hives as the random effect structure is better due to the p-value
# showing it is significant and having the lower AIC.

##### Q8 ##### 

# Step 6. Check the model: plot standardized residuals vs. fitted values and vs. 
# each predictor. (You can get standardized residuals with residuals(yourmodel, 
# type='pearson')). How do they look?
res <- residuals(lme_logspobee, type='pearson')
fv <- fitted.values(lme_logspobee)

par(mfrow = c(2,2))

# Residuals vs fitted
plot(fv, res, ylab = "residuals", xlab = "fitted values")
abline(h = 0, lty = 'dotted')

plot(res ~ isInfected, data = bees_df_transf, ylab = "residuals")
abline(h = 0, lty = 'dotted')
plot(res ~ BeesN, data = bees_df_transf, ylab = "residuals")
abline(h = 0, lty = 'dotted')
plot(res ~ Hive, data = bees_df_transf, ylab = "residuals")
abline(h = 0, lty = 'dotted')

# Variances look pretty well distributed with no specific trends.

# Reset mfrow
par(mfrow = c(1,1))

summary(lme_logspobee)

##### Q9 ##### 

# Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a 
# reduced model without the interaction term, also fit with ML. Use anova() to 
# compare the models. Which model do you choose? Why?

lme_logspobee_ml_full <- lme(lme_formula, random = ~1|Hive, method = "ML", data = bees_df_transf)
lme_logspobee_ml_nointeraction <- lme(LogSpobee ~ isInfected + BeesN,
                                       random = ~1|Hive, method = "ML", data = bees_df_transf)
anova(lme_logspobee_ml_full, lme_logspobee_ml_nointeraction)

# Keeping the interaction term was not significantly different from dropping it, however
# dropping it gives us fewer degrees of freedom (we can predict more) and has a lower AIC.
# So, we should choose the model without the interaction term.

##### Q10 ##### 

# Step 8. Iterate #7 to arrive at the final model. Show your work. What is your 
# final set of fixed effects?

lme_logspobee_ml_nointeraction <- lme(LogSpobee ~ isInfected + BeesN,
                                      random = ~1|Hive, method = "ML", data = bees_df_transf)

lme_logspobee_ml_noinfection <- update(lme_logspobee_ml_nointeraction, .~. -isInfected)
anova(lme_logspobee_ml_nointeraction, lme_logspobee_ml_noinfection)
# The models are significantly different and the model without infection is worse than
# the model with it based on a higher AIC.

lme_logspobee_ml_nobees <- lme(LogSpobee ~ isInfected, random = ~1|Hive, method = "ML", data = bees_df_transf)
anova(lme_logspobee_ml_nointeraction, lme_logspobee_ml_nobees)
# The models are not significantly different but the model with the bees is slightly better due to AIC

##### Q11 ##### 

# Step 9. Fit the final model with REML. Check assumptions by plotting a histogram 
# of residuals, plotting Pearson standardized residuals vs. fitted values, and 
# plotting Pearson standardized residuals vs. explanatory variables. Are there 
# issues with the model? If so, how might you address them?

lme_logspobee_final <- lme(LogSpobee ~ isInfected + BeesN,
                           random = ~1|Hive, method = "REML", data = bees_df_transf)

hist(residuals(lme_logspobee_final))

res_final <- residuals(lme_logspobee_final, type='pearson')
fv_final <- fitted.values(lme_logspobee_final)

par(mfrow = c(2,2))

# Residuals vs fitted
plot(fv_final, res_final, ylab = "residuals", xlab = "fitted values")
abline(h = 0, lty = 'dotted')

plot(res_final ~ isInfected, data = bees_df_transf, ylab = "residuals")
abline(h = 0, lty = 'dotted')
plot(res_final ~ BeesN, data = bees_df_transf, ylab = "residuals")
abline(h = 0, lty = 'dotted')
plot(res_final ~ Hive, data = bees_df_transf, ylab = "residuals")
abline(h = 0, lty = 'dotted')

# Reset mfrow
par(mfrow = c(1,1))

# There's a slight pattern in the variances of the residuals. 

##### Q12 ##### 

# Step 10. Interpret the model. The summary() command is useful here. What have 
# you learned about American Foulbrood?

summary(lme_logspobee_final)

##### Q13 ##### 

# Calculate the correlation between observations from the same hive as 
# variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). 
# Given the correlation among observations from the same hive, do you think it's 
# a good use of time to sample each hive multiple times? Why or why not?

# Get the variance of the random effect
var_hive_resid <- VarCorr(lme_logspobee_final)
var_hive <- as.numeric(var_hive_resid[1])
var_resid <- as.numeric(var_hive_resid[2])

# Correlation
var_hive/(var_hive + var_resid)

# The observations from the same hive are highly correlated, so samples from the
# same hive would not be as useful.

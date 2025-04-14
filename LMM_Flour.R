###2/3/25
###linear mixed model with Flourescenc data 22-25
###JNBM and JW
set.seed(22)
setwd("/Users/janaaward/Desktop/R work/Flourescence ")

library("ggplot2")
library("dplyr")
#install and then load packagesinstall.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
library( ) # 
### Error about matrix dependency 
utils::install.packages("lme4", type = "source")
oo <- options(repos = "https://cran.r-project.org/")
utils::install.packages("Matrix")
utils::install.packages("lme4")
options(oo)

remove.packages("Matrix")
install.packages("Matrix")

remove.packages("Matrix")
remove.packages("lme4")
install.packages("lme4", type = "source")
library(lme4)
#####fixed redownloaded R and R studio to load new version of matrix

###read in the lme4 package, the lmertest package, robustlmm, MUMIn, and the emmeans package
install.packages('lme4') # for running a linear mixed model
library(lme4)
install.packages('lmerTest')
library(lmerTest)
install.packages('emmeans') # for pulling information out of statistical interactions
library(emmeans)
install.packages("robustlmm")  # a more statistically robust version of lme4
library(robustlmm)
install.packages("forcats") 
library(forcats)

install.packages("MuMIn")
 library(MuMIn)

# import data, be sure to check only sheet needed in window that pops open

# rename the file so you have one raw and one you are working with

#########clean up data ##############
# pull out just ones to keep

Fluor<- Fluorescence_22_24_adjusted
# Keep a subset of rows using a logical expression
Flour <- Fluorescence_22_24_adjusted[Fluorescence_22_24_adjusted$keep >= 1, ]
Fluor <- filter(Fluorescence_22_24_adjusted, keep=="1")
Fluorescence_22_24_adjusted <- readxl::read_xlsx("Fluorescence 22-24_adjusted.xlsx", "Sheet2", na = ".")

#########clean up data ##############
# pull out just ones to keep

#changing into factors and numeric
# e.g. 
Fluor$year <- as.factor(Fluor$year)
Fluor$field_season <- as.factor(Fluor$field_season)
Fluor$field_cond_score <- as.factor(Fluor$field_cond_score)
Fluor$tree_code <- as.factor(Fluor$tree_code)
Fluor$Sex_MFNB <- as.factor(Fluor$Sex_MFNB)
Fluor$mean_fvfm <- as.numeric(Fluor$mean_fvfm)
Fluor$site <- as.factor(Fluor$site)
Fluor$transect <- as.factor(Fluor$transect)
Fluor$keep <- as.factor(Fluor$keep)
Fluor$time <- as.numeric(Fluor$time)
Fluor$Time_01 <- as.numeric(Fluor$Time_01)
# Checking variables 
summary(Fluor$mean_fvfm)
summary(Fluor$field_season)
summary(Fluor$Sex_MFNB)
summary(Fluor$field_cond_score)
summary(Fluor$tree_code)
summary(Fluor$year)
summary(Fluor$site)
summary(Fluor$transect)
summary(Fluor$keep)
levels(Fluor$keep)
summary
# Check the structure
str(Fluor)

#demo$dbh_cm_22 <- as.numeric(demo$dbh_cm_22)
### recode sex columns
Fluor$simplesex <- fct_recode(Fluor$Sex_MFNB, F= "B") # all bissexuals become female



############################################################################Putting together the models
#######################
# USE REML to determine for random slopes to determine random effects


# stage is random slope here:  (1+stage_con||boat_no) 
# (1|tree_id)
# having an intercept varying among schools and among pupils within schools ( (1|schools)+(1|school:pupil)

# want smallest AIC ( within an estimation procedure. SO do not compare REML and ML AICs
#                    
##########################################################
#########################################

#  Fit the random effects using REML
# you should have one model with all three variables as random effects, all together
# three models with each 2-way combination
# three models with just one of the random effects

# could also try SITE instead of transect for random effect

modelfull<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
                  #2 way
                  Sex_MFNB:field_season + Sex_MFNB:field_cond_score +
                  #random effects
                  (1|transect)+(1|tree_code)+ (1|year),
                data= Fluor    , na.action = na.omit)
AIC(modelfull) ##-2423.916
BIC(modelfull) ##### record BIC  -2294.852

modelfull<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
                  #2 way
                  Sex_MFNB:field_season + Sex_MFNB:field_cond_score +
                  #random effects
                  (1|transect)+(1|tree_code),
                data= Fluor    , na.action = na.omit)
AIC(modelfull) ## -2403.999
BIC(modelfull) ##### record BIC  -2279.385
modelfull<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
                  #2 way
                  Sex_MFNB:field_season + Sex_MFNB:field_cond_score +
                  #random effects
                  (1|tree_code),
                data= Fluor    , na.action = na.omit)
AIC(modelfull) ## -2390.713
BIC(modelfull) ##### record BIC  -2270.55
modelfull<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
                  #2 way
                  Sex_MFNB:field_season + Sex_MFNB:field_cond_score +
                  #random effects
                  (1|tree_code) + (1|site),
                data= Fluor    , na.action = na.omit)
AIC(modelfull) ## -2399.269
BIC(modelfull) ##### record BIC -2274.655
### singular fit 
###still singular when removed time, year


modelfull1<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
                  #2 way
                  Sex_MFNB:field_season + Sex_MFNB:field_cond_score +
                  #random effects
                  (1|tree_code),
                data= Fluor    , na.action = na.omit)
#Sum Sq Mean Sq NumDF DenDF  F value    Pr(>F)    
#Sex_MFNB                  0.00210 0.00070     3   603   0.7466  0.524617    
#field_season              0.32368 0.32368     1   603 345.0828 < 2.2e-16 ***
 # field_cond_score          0.01738 0.00290     6   603   3.0880  0.005504 ** 
#  Sex_MFNB:field_season     0.00426 0.00142     3   603   1.5139  0.209756    
#Sex_MFNB:field_cond_score 0.01773 0.00127    14   603   1.3505  0.172696    
AIC(modelfull1) # record AIC here

summary(modelfull1) 
anova(modelfull1)
###model could not support the other random effects variation in tree code is VERY small, but must be kept to avoid pseuodo replication



###################################
###3# now fitting fixed effects with simple sex
model<-lmer(mean_fvfm ~ simplesex+field_season+ field_cond_score + 
                   #2 way
                   simplesex:field_season + simplesex:field_cond_score +
                   #random effects
                   (1|tree_code),
                   REML=FALSE, data= Fluor    , na.action = na.omit)
###singular
AIC(model) # record AIC here
BIC(model) ##### record BIC  -2496.854
summary(model) 
anova(model)


### removing Sex_MFNB:field_season 
model2<-lmer(mean_fvfm ~ simplesex+field_season+ field_cond_score + 
              #2 way
               simplesex:field_cond_score +
              #random effects
              (1|tree_code),
            REML=FALSE, data= Fluor    , na.action = na.omit)
####singular
AIC(model2) # record AIC here 
BIC(model2) ##### record BIC -2505.201
summary(model) 
anova(model2)

###
### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score +
model3<-lmer(mean_fvfm ~ simplesex+field_season+ field_cond_score + 
             
           #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit, lmerControl(optimizer="bobyqa"))
##still singular 
AIC(model3) # record AIC here 
BIC(model3) ##### record BIC  -2548.95
summary(model3) 
anova(model3)

### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score + sex_mfnb
model4 <-lmer(mean_fvfm ~ field_season+ field_cond_score + 
               
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit)
# still singular
AIC(model4) # record AIC here -2599.284
BIC(model4) ##### record BIC  -2559.23
summary(model4) 
anova(model4)


#### so far this is the best model
str(Fluor)
Fluor$field_cond_score_numeric <- as.numeric(Fluor$field_cond_score)

model3a<-lmer(mean_fvfm ~ +field_season+ field_cond_score_numeric + 
               
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit)
## singular fit
AIC(model3a) # record AIC here 
BIC(model3a) ##### record BIC 
summary(model3a) 
anova(model3a)

#### trying a season by condition interaction 
model4b<-lmer(mean_fvfm ~ simplesex+field_season+ field_cond_score + 
               field_cond_score:field_season +
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit)
##singular fit
AIC(model4b) # record AIC here 
BIC(model4b) ##### record BIC   -2521.281
summary(model4b) 
anova(model4b)

### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score +
model5<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
               
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit, lmerControl(optimizer="bobyqa"))
AIC(model5) # record AIC here -2596.941
BIC(model5) ##### record BIC  -2543.535
summary(model5) 
anova(model5)

### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score + sex_mfnb
model4c <-lmer(mean_fvfm ~ field_season+ field_cond_score_numeric + 
                
                #random effects
                (1|tree_code),
              REML=FALSE, data= Fluor    , na.action = na.omit)
# NOT singular
AIC(model4c) # record AIC here 
BIC(model4c) ##### record BIC  -2573
summary(model4c) 
anova(model4c)
#### best model so far

##3
# extrating residual
## use to olot model predictioed values vs residuals
residuals <- residuals(model4c)
predvalues <- predict(model4c)

plot(x=predvalues, y=residuals,
     xlab='predicted',
     Ylab= 'actual',
     Main= 'residuals')
######################
# now fitting fixed effects
model<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
              #2 way
              Sex_MFNB:field_season + Sex_MFNB:field_cond_score +
              #random effects
              (1|tree_code),
            REML=FALSE, data= Fluor    , na.action = na.omit)
AIC(model) # record AIC here  -2590.642
BIC(model) ##### record BIC  -2470.479
summary(model) 
anova(model)


### removing Sex_MFNB:field_season 
model2<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
               #2 way
               Sex_MFNB:field_cond_score +
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit)
AIC(model2) # record AIC here -2591.636
BIC(model2) ##### record BIC -2484.825
summary(model) 
anova(model2)

###
### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score +
model3<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
               
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit, lmerControl(optimizer="bobyqa"))
##still singular 
AIC(model3) # record AIC here -2596.941
BIC(model3) ##### record BIC  -2543.535
summary(model3) 
anova(model3)
### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score + sex_mfnb
model4 <-lmer(mean_fvfm ~ field_season+ field_cond_score + 
                
                #random effects
                (1|tree_code),
              REML=FALSE, data= Fluor    , na.action = na.omit)
AIC(model4) # record AIC here -2599.284
BIC(model4) ##### record BIC  -2559.23
summary(model4) 
anova(model4)
#### so far this is the best model
str(Fluor)
Fluor$field_cond_score_numeric <- as.numeric(Fluor$field_cond_score)

model3a<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score_numeric + 
                
                #random effects
                (1|tree_code),
              REML=FALSE, data= Fluor    , na.action = na.omit)
## singular fit
AIC(model3a) # record AIC here 
BIC(model3a) ##### record BIC 
summary(model3a) 
anova(model3a)
#### trying a season by condition interaction 
model4<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
               field_cond_score:field_season +
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit)
##singular fit
AIC(model4) # record AIC here -2580.204
BIC(model4) ##### record BIC  -2495.705
summary(model4) 
anova(model4)



### removing Sex_MFNB:field_season and Sex_MFNB:field_cond_score +
model5<-lmer(mean_fvfm ~ Sex_MFNB+field_season+ field_cond_score + 
               
               #random effects
               (1|tree_code),
             REML=FALSE, data= Fluor    , na.action = na.omit, lmerControl(optimizer="bobyqa"))
AIC(model5) # record AIC here -2596.941
BIC(model5) ##### record BIC  -2543.535
summary(model5) 
anova(model5)
################################
#Check for normality heterascedastity 
# check residuals
#### for model4 
Fluor.aov <- aov(mean_fvfm ~ Sex_MFNB, data = Fluor)

# Extract the residuals
aov_residuals <- residuals(object = Fluor.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
#Shapiro-Wilk normality test
#data:  aov_residuals
#W = 0.91171, p-value < 2.2e-16
plot(Fluor.aov, 1)

> 
FR <- 
# how does one interpret the Shapiro-wilk test?
#wikipedia notes: “The null-hypothesis of this test is that the population is normally distributed. Thus, if the p value is less than the chosen alpha level, then the null hypothesis is rejected and there is evidence that the data tested are not normally distributed.”
#less than 0.05 =not normal
#
leveneTest(mean_fvfm ~ Sex_MFNB, data = FR)
#     Df F value Pr(>F)
#group   Df F value Pr(>F)
#group   3  1.5546 0.1993
#629      
# how does one interpret this? 
# wikipedia states: “Levene's test is an inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.[1] Some common statistical procedures assume that variances of the populations from which different samples are drawn are equal. Levene's test assesses this assumption. It tests the null hypothesis that the population variances are equal (called homogeneity of variance or homoscedasticity). If the resulting p-value of Levene's test is less than some significance level (typically 0.05), the obtained differences in sample variances are unlikely to have occurred based on random sampling from a population with equal variances. Thus, the null hypothesis of equal variances is rejected and it is concluded that there is a difference between the variances in the population.”
#homogenality in null


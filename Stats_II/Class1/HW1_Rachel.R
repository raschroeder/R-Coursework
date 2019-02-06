###############################################
######## Class 1 - Homework ###################
###############################################

# On the Discovery program 'Naked and Afraid' survivalists are dropped off in
# the jungle completely naked with only one tool of their choosing. They must
# find water, food, and shelter and make their way to their extraction point 21
# days later. They can choose to leave at any point. Prior to coming on the show
# the survivalists are given a Primitive Survival Rating before they compete
# (PSRStart) (ranging from 0-10) based on their prior survival experience and
# mental fortitude. We are interested in knowing whether PSR is an informative
# predictor of # of days "survived" (0-21; LeaveDay) and how much weight they lost IF they
# made it the end (WeightLoss).

#Load Libraries you will need
library(dplyr)
library(ggpubr) #graph data
library(car)
library(boot)
library(pwr)

#Set working directory (you need to change this)
setwd("/Users/rachel/Box\ Sync/R\ Coursework/Stats_II/Class1/")
NAF<-read.csv("NakedandAfraidData.csv") #https://gist.github.com/anonymous/11ffb3405f951e3fae700d79eb9748d7#file-survivalist_recap-csv
# So I think this is Real show data! 


################################
########### Task 1:  ###########
################################
# Figure out the relationship between PSRStart and LeaveDay
# Filter the data to only have those who LEAVE

NAF.NotFinished<-NAF %>% filter(Leave=="TRUE")


#########################################################
# Plot the relationship between PSRStart and LeaveDay.  #
# Note you will want to "predict" LeaveDay              #
#########################################################
ggscatter(NAF.NotFinished, x = "PSRStart", y = "LeaveDay",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)

####################################################### 
# Create a regresison model and summerize the results #
#######################################################
StartScore.Leave<-lm(LeaveDay~PSRStart,data = NAF.NotFinished)
summary(StartScore.Leave)

# Summerize your findings in plain english

## PSR Start score does not predict the number of days until they leave - there is no relationship between the two


# Based on this model predict on what day a person would leave if they had a PSR of 3 #
# Show your work! 

## 2.668(3) + (-7.674) = 0.33
## They would leave the first day


# Explain Why this prediction is dangerious to make!  

## There are a couple outliers on either end of the spectrum that are likely driving the relationship
## The correlation is weak and the data do not fit the line well



##################################
# Boot the model fit, 2K times
###################################
set.seed(666)
Boot.StartScore.Leave <-Boot(StartScore.Leave, R = 2000)

# View results
hist(Boot.StartScore.Leave, legend="separate")


# get 95% confidence interval and report BCA CI around prediction 
confint(Boot.StartScore.Leave, level=.95)
## BCA CI = (-0.686, 4.242)

# Explain what you BCA CI tells about the slope! 

## Because the CI includes zero we cannot reject the null hypothesis - we do not know if the slope is pos or neg

################################
########### Task 2:  ###########
################################
# Figure out the relationship between PSRStart and LeaveDay
# Filter the data to only Stay the end

NAF.Finished<-NAF %>% filter(Leave=="FALSE")


#########################################################
# Plot the relationship between PSRStart and WeightLoss #
# Note you will want to "predict" WeightLoss            #
#########################################################
ggscatter(NAF.Finished, x = "PSRStart", y = "WeightLoss",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)

####################################################### 
# Create a regresison model and summerize the results #
#######################################################
StartScore.WeightLoss<-lm(WeightLoss~PSRStart,data = NAF.Finished)
summary(StartScore.WeightLoss)



# Write the results in APA format.
# For example [Social support significantly predicted depression scores, b = -.34,
# t(225) = 6.53, p < .001. Social support also explained a significant
# proportion of variance in depression scores, R2 = .12, F(1, 225) = 42.64, p <
# .001.]

## PSR Start scores significantly predicted weightloss, b = -5.018,
## t(52) = 2.948, p < .005. PSR Start scores also explained a significant
## proportion of variance in weightloss, R2 = .14, F(1, 52) = 8.692, p <
## .005.


# Assuming there is a true relationship between PSR and Weight Loss and you want
# to replicate this study at a power .95, how many subjects would you need?
# [Use the Multiple R-squared from the model you created]. Hint: set v=NULL

f2.weightloss <- 0.1432 / (1-0.1432) #(taken from Multiple R-squared)
pwr.f2.test(u = 1, v = NULL, f2 = f2.weightloss, sig.level = 0.05, power = .95)

## 78 + 1df = final n of 79

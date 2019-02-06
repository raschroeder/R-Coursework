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
setwd("C:/Users/AlexUIC/Box Sync/UIC Teaching/545 Regression Spring 2019/Week 1 - Correlations and LM")
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



####################################################### 
# Create a regresison model and summerize the results #
#######################################################


# Summerize your findings in plain english




# Based on this model predict on what day a person would leave if they had a PSR of 3 #
# Show your work! 




# Explain Why this prediction is dangerious to make!  





##################################
# Boot the model fit, 2K times
###################################



# get 95% confidence interval and report BCA CI around prediction 



# Explain what you BCA CI tells about the slope! 


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


####################################################### 
# Create a regresison model and summerize the results #
#######################################################



# Write the results in APA format.
# For example [Social support significantly predicted depression scores, b = -.34,
# t(225) = 6.53, p < .001. Social support also explained a significant
# proportion of variance in depression scores, R2 = .12, F(1, 225) = 42.64, p <
# .001.]








# Assuming there is a true relationship between PSR and Weight Loss and you want
# to replicate this study at a power .95, how many subjects would you need?
# [Use the Multiple R-squared from the model you created]. Hint: set v=NULL

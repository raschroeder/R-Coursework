###############################################
######## Class 1 - Homework ###################
###############################################

#Load Libraries you will need
library(car) #graph data
library(ppcor) #Advanced Correlations


#Set working directory (you need to change this)
setwd('C:/AlexFiles/SugerSync/UIC/Teaching/Graduate/545-Spring2018/Week 2 - Part and Partial Correlations')

# Import this data file
#### Note: Dataset take from https://onlinecourses.science.psu.edu/stat501/datasets
iqsize <- read.table("iqsize.txt", header=T)
names(iqsize)


################
### Overview ###
################
# How does your IQ relate to your physical measurment (brain size, Height, Weight)?
# Your job it figure out what predicts IQ. Also make sure to know how the IVs are all related to one another. 

######## Variables 
#DV = PIQ
#IV = Brain,Height,Weight


################################
########### Task 1:  ###########
################################
# Calculate the zero-order & semipartial correlation. 
# Label what you are doing

# zero-order Correlations (everything to everything)
# Between DV and IVs


# Between IVs


################################
### Theory question  1 #########
################################
# Do you see any problems with the correlations between the IVs in terms of
# using them in the same model at predictors? 



############
# semipartial correlations (DV is always PIQ, one control at time)



################################
### Theory question  2 #########
################################
# The relationship between PIQ~Brain got stronger when controling for WT or HT,
# relative the zero-order correlation what does that mean and why might this happen?



################################
########### Task 2:  ###########
################################
# Center your predictors!


# Run the regressions (one IV at time)



# Based on the pattern of correlations you found add 2 IVs at a time 
# Hint: Think about those theory questions. 



################################
### Theory question 3 ##########
################################
# Compare the estimates (R2 & slopes for brain model only with brain + WT and brain + HT)
# Explain how the R2 & slopes changes from the brain only model and why that might have happened







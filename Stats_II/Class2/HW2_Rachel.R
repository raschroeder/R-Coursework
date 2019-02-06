###############################################
######## Class 1 - Homework ###################
###############################################

#Load Libraries you will need
library(car) #graph data
library(ppcor) #Advanced Correlations


#Set working directory (you need to change this)
setwd('/Users/rachel/Box\ Sync/R\ Coursework/Stats_II/Class2')

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
Corr.1<-cor.test(iqsize$PIQ,iqsize$Brain, 
                        method = c("pearson"))
Corr.2<-cor.test(iqsize$PIQ,iqsize$Height, 
            method = c("pearson"))
Corr.3<-cor.test(iqsize$PIQ,iqsize$Weight,
            method = c("pearson"))
Corr.1 ## t = 2.4484, df = 36, p-value = 0.01935
Corr.2 ## t = -0.56137, df = 36, p-value = 0.578
Corr.3 ## t = 0.015073, df = 36, p-value = 0.9881


# Between IVs
Corr.IVs.1<-cor.test(iqsize$Height,iqsize$Weight, 
                 method = c("pearson"))
Corr.IVs.1 ## t = 5.8748, df = 36, p-value = 1.021e-06

Corr.IVs.2<-cor.test(iqsize$Height,iqsize$Brain, 
                     method = c("pearson"))
Corr.IVs.2 ## t = 4.3659, df = 36, p-value = 0.0001023

Corr.IVs.3<-cor.test(iqsize$Brain,iqsize$Weight, 
                     method = c("pearson"))
Corr.IVs.3 ## t = 3.5904, df = 36, p-value = 0.000977

################################
### Theory question  1 #########
################################
# Do you see any problems with the correlations between the IVs in terms of
# using them in the same model at predictors? 

# There is a significant relationship between Height, Weight, and Brain so they all account 
# for a lot of overlapping variance in the model. If they were venn diagram circles,
# the Height and Weight variables would be on top of each other.


############
# semipartial correlations (DV is always PIQ, one control at time)
CorrSP.1<-spcor.test(iqsize$PIQ, iqsize$Brain, iqsize$Weight)
CorrSP.1 ## p = 0.007
CorrSP.2<-spcor.test(iqsize$PIQ, iqsize$Brain, iqsize$Height)
CorrSP.2 ## p = 0.006

CorrSP.3<-spcor.test(iqsize$PIQ, iqsize$Weight, iqsize$Height)
CorrSP.3 ## p = 0.577
CorrSP.4<-spcor.test(iqsize$PIQ, iqsize$Height, iqsize$Weight)
CorrSP.4 ## p = 0.433

CorrSP.5<-spcor.test(iqsize$PIQ, iqsize$Weight, iqsize$Brain)
CorrSP.5 ## p = 0.184
CorrSP.6<-spcor.test(iqsize$PIQ, iqsize$Height, iqsize$Brain)
CorrSP.6 ## p = 0.017

################################
### Theory question  2 #########
################################
# The relationship between PIQ~Brain got stronger when controling for WT or HT,
# relative the zero-order correlation what does that mean and why might this happen?

# Height and Weight predict head size so they absorb a lot of the variance in the relationship

################################
########### Task 2:  ###########
################################
# Center your predictors!
iqsize$Brain.C<-scale(iqsize$Brain, center=TRUE)[,]
iqsize$Weight.C<-scale(iqsize$Weight, center=TRUE)[,]
iqsize$Height.C<-scale(iqsize$Height, center=TRUE)[,]

# Run the regressions (one IV at time)
M.Model.1<-lm(PIQ~ Brain.C, data = iqsize)
M.Model.2<-lm(PIQ~ Weight.C, data = iqsize)
M.Model.3<-lm(PIQ~ Height.C, data = iqsize)
summary(M.Model.1) # 0.0194 *
summary(M.Model.2) # 0.988 
summary(M.Model.3) # 0.578

# Based on the pattern of correlations you found add 2 IVs at a time 
# Hint: Think about those theory questions. 
M.Model.4<-lm(PIQ~ Brain.C+Height.C, data = iqsize)
summary(M.Model.4) # p-value: 0.002

M.Model.5<-lm(PIQ~ Brain.C+Weight.C, data = iqsize)
summary(M.Model.5) # p-value: 0.023

################################
### Theory question 3 ##########
################################
# Compare the estimates (R2 & slopes for brain model only with brain + WT and brain + HT)
# Explain how the R2 & slopes changes from the brain only model and why that might have happened

# brain only: Multiple R-squared:  0.1427; slope = 8.538
# brain + WT: Multiple R-squared:  0.1925; slope = 11.555
# brain + HT: Multiple R-squared:  0.2949; slope = 14.953 

# Brain + Height had the strongest relationship with IQ
# As you account for more and more of the variance, your R2 will increase. We know from running correlations
# that weight and height correlate with eachother and with brain size. This is because having a larger
# body means your brain will be larger. Height is more closely related to brain size and therefor
# produces the best R2 when added to the model. The relationship between IQ and brain size has largely 
# been debunked and at this point in time is only amplified by MRA's and white supremacists so
# not sure what this data is all about.

library(ggpubr) #graph data
ggscatter(iqsize, x = "Brain", y = "PIQ",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)


##########################################################
######## Class 2 - In class Assignment ###################
##########################################################

#Load Libraries you will need
library(car) #graph data
library(ppcor) #part corr

#Set working directory (you need to change this)
setwd('/Users/rachel/Box\ Sync/R\ Coursework/Stats_II/Class2')

# Import this data file
#### Note: Dataset give to me by Amanda Roy
Class2.Data<-read.csv('Class2InClassData.csv')
head(Class2.Data)

################ Variables of the day
# idnum = ID of subject
# gender = 1 = male, 2 = female [nominal]
# age = age of person
# Health = from 0 to 10 (most healthy today) [we are going to pretend its interval]
# Yrsmar = Number of years Married [interval]

###############################################################
# Question: Is the longer you are married the healthier you are, but we need to control for age!!
################################################################

################################
########### Task 1:  ###########
################################

## calcuate the semipartial and partial correlation 
#zero-order correlation between Health and Yrsmar
Corr.Result.1<-cor.test(Class2.Data$Health, Class2.Data$Yrsmar, 
                        method = c("pearson"))
Corr.Result.1

# Control for Age (SP)
CorrSP.1<-spcor.test(Class2.Data$Health, Class2.Data$Yrsmar, Class2.Data$age)
CorrSP.1

# Control for Age (P)
CorrP.1<-pcor.test(Class2.Data$Health, Class2.Data$Yrsmar, Class2.Data$age)
CorrP.1

#overall relationship didn't change much - there is no relationship between years and health when you've controlled for age

################################
########### Task 2:  ###########
################################
# Run the regression, Health~Yrsmar and a second regression controling for age. 
M.Model.1<-lm(Health~Yrsmar, data = Class2.Data)
M.Model.2<-lm(Health~Yrsmar+age, data = Class2.Data)
M.Model.1
M.Model.2

## Now control for age
# remember to control, you can residualize age form Yrsmar. 
# Then rerun your regression using the residualized Yrsmar
# Also plot health by residualized Yrsmar
Class2.Data$Age.from.Yrsmar<-residuals(lm(Yrsmar~age, Class2.Data))
M.Model.3<-lm(Health~Age.from.Yrsmar, data = Class2.Data)
summary(M.Model.3)

scatterplot(Health~Age.from.Yrsmar,Class2.Data, smoother=FALSE)

#The relationship is gone now - the estimate is now only 0.008

################################
########### Task 3:  ###########
################################
# you can just enter both Yrsmar and age into one lm model!
M.Model.2<-lm(Health~Yrsmar+age, data = Class2.Data)
summary(M.Model.2)

CorrSP.1<-spcor.test(Class2.Data$Health, Class2.Data$Yrsmar, Class2.Data$age)
CorrSP.1$estimate^2

CorrSP.2<-spcor.test(Class2.Data$Health, Class2.Data$age, Class2.Data$Yrsmar)
CorrSP.2$estimate^2
################################
########### Task 4:  ###########
################################
#Does years of marriage have anything to do with health once we control for age? NO
# Why?

################################
########### Task 5:  ###########
################################

# Let do the whole process again but seperarly for males and females
Class2.Males<-subset(Class2.Data, gender==1)
Class2.Females<-subset(Class2.Data, gender==2)

#zero-order corelation between Health and yearsmar
# Males
Corr.Result.2<-cor.test(Class2.Males$Health, Class2.Males$Yrsmar, 
                        method = c("pearson"))
Corr.Result.2
# Females
Corr.Result.3<-cor.test(Class2.Females$Health, Class2.Females$Yrsmar, 
                        method = c("pearson"))
Corr.Result.3
# Control for Age (SP)
# Males

# Females



# Run 2 LM Models: Yrsmar+age
# Males


# Females


# Bootstrap the .95BCa CI for each gender

library(boot)
# Males

# Females


################################
########### Task 6:  ###########
################################
# 1. Does the bootstraping match the conclusions from the pvalues for each gender model?


# 2. Compare the coef from each gender and explain in plain english what the results suggest


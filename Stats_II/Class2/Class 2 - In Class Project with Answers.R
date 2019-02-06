##########################################################
######## Class 2 - In class Assignment ###################
##########################################################

#Load Libraries you will need
library(car) #graph data
library(ppcor) #part corr

#Set working directory (you need to change this)
setwd('C:/AlexFiles/SugerSync/UIC/Teaching/Graduate/545-Spring2017/Week 2 - Correlations and LM')

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

## calculate the semipartial and partial correlation 
#zero-order corelation between Health and Yrsmar
cor(Class2.Data$Health, Class2.Data$Yrsmar)
# Control for Age (SP)

Sr1<-spcor.test(Class2.Data$Health, Class2.Data$Yrsmar, Class2.Data$age)
Sr1$estimate

# Control for Age (P)
pr1<-pcor.test(Class2.Data$Health, Class2.Data$Yrsmar, Class2.Data$age)
pr1$estimate


################################
########### Task 2:  ###########
################################
# Run the regression, Health~Yrsmar and a second regression controling for age. 
Model.1<-lm(Health~Yrsmar, data = Class2.Data)
summary(Model.1)

## Now control for age
# remember to control, you can residualize age form Yrsmar. 
# Then rerun your regression using the residualized Yrsmar
# Also plot health by residualized Yrsmar

Class2.Data$Yrsmar.control.age<-residuals(lm(Yrsmar~age,data = Class2.Data))
scatterplot(Health~Yrsmar.control.age,Class2.Data, smoother=FALSE)

Model.2<-lm(Health~Yrsmar.control.age, data = Class2.Data)
summary(Model.2)

################################
########### Task 3:  ###########
################################
# you can just enter both Yrsmar and age into one lm model!
Model.3<-lm(Health~Yrsmar+age, data = Class2.Data)
summary(Model.3)

################################
########### Task 4:  ###########
################################
#Does years of mariage have anything to do with health once we control for age?
# Why?
# No, age explained the whole thing

################################
########### Task 5:  ###########
################################

# Let do the whole process again but seperarly for males and females
Class2.Males<-subset(Class2.Data, gender==1)
Class2.Females<-subset(Class2.Data, gender==2)

#zero-order corelation between Health and gender
# Males
cor(Class2.Males$Health, Class2.Males$Yrsmar)
# Females
cor(Class2.Females$Health, Class2.Females$Yrsmar)

# Control for Age (SP)
# Males
Sr1.M<-spcor.test(Class2.Males$Health, Class2.Males$Yrsmar, Class2.Males$age)
Sr1.M$estimate
# Females
Sr1.F<-spcor.test(Class2.Females$Health, Class2.Females$Yrsmar, Class2.Females$age)
Sr1.F$estimate


# Run 2 LM Models: Yrsmar+age
# Males
Model.M<-lm(Health~Yrsmar+age, data = Class2.Males)
summary(Model.M)

# Females
Model.F<-lm(Health~Yrsmar+age, data = Class2.Females)
summary(Model.F)

# Bootstrap the .95BCa CI for each gender

library(boot)
# Males
BootParms.M <-Boot(Model.M, R = 2000)
confint(BootParms.M, level=.95, type="bca")

# Females
BootParms.F <-Boot(Model.F, R = 2000)
confint(BootParms.F, level=.95, type="bca")


################################
########### Task 6:  ###########
################################
# 1. Does the bootstraping match the conclusions from the pvalues for each gender model?


# 2. Compare the coef from each gender and explain in plain english what the results suggest



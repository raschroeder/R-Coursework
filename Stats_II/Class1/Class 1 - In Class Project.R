##########################################################
######## Class 1 - In class Assignment ###################
##########################################################

#Load Libraries you will need
library(MASS) 
library(ggpubr)
library(car) 
library(polycor) 
library(ggplot2)
library(pwr) 


#Set working directory (you need to change this)
setwd('/Users/rachel/Box\ Sync/R\ Coursework/Stats_II/Class1/')

# Import this data file
#### Note: Dataset give to me by Amanda Roy
Class1.Data<-read.csv('Class1InClassData.csv')
head(Class1.Data)


################ Variables of the day
# idnum = ID of subject
# gender = 1 = male, 2 = female [nominal]
# Health = from 0 to 10 (most healthy today) [we are going to pretend its interval]
# YesMar = Number of years Married [interval]


###############################################################
# Question: Is the longer you are married the healthier you are? 
################################################################


########### Task 1: is marriage related to health? 
# plot the scatter plot

library(ggpubr) #graph data
ggscatter(Class1.Data, x = "Yrsmar", y = "Health",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)
# Run a regression (predict health from marriage)
Yrsmar.1<-lm(Health~Yrsmar,data = Class1.Data)
summary(Yrsmar.1)
      
#If you feel up to it, try the fancy plot with red and blue dots
Class1.Data$predicted <- predict(Yrsmar.1)   # Save the predicted values with our real data
Class1.Data$residuals <- residuals(Yrsmar.1) # Save the residual values

library(ggplot2) 
ggplot(data = Class1.Data, aes(x = Yrsmar, y = Health)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = FALSE) +
  geom_segment(aes(xend = Yrsmar, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look

# Explain how health changes as function of marriage

### After 10, 20, and- 30 years of marriage what is your predicted health?
#10=7.8
#20=7.5
#30=7.4

# Explain how much variance is explained 
summary(Yrsmar.1)
# 2.6% of the variance


# Does this results makes sense? Might age or gender be driving the affect?
## aging would make health worse and men age worse than women

############ Task 2: is age related to health? 
# plot the scatter plot
ggscatter(Class1.Data, x = "age", y = "Health",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)

# Run pearson correlation between age and health.
Corr.Result.1<-cor.test(Class1.Data$Health, Class1.Data$age, 
                        method = c("pearson"))
Corr.Result.1
library(apa)
cor_apa(Corr.Result.1,format ="text")

# Explain in one sentence the result. 
## negative relationship between health and age


########### Task 3: is gender related to health? 
# plot the scatter plot
ggscatter(Class1.Data, x = "gender", y = "Health",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)

#run the point-by-serial
library(polycor) #Advanced Correlations
with(Class1.Data, 
     polyserial(gender,Health, ML=TRUE))

# Explain in one sentence the result. 
# -0.07777495
# 

########### Task 4: is age related to years married? 

# plot the scatter plot
ggscatter(Class1.Data, x = "age", y = "Yrsmar",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = FALSE, # Add correlation coefficient. see ?stat_cor
)

# Run pearson correlation between age and years married.
Corr.Result.2<-cor.test(Class1.Data$age, Class1.Data$Yrsmar, 
                        method = c("pearson"))
Corr.Result.2

########### Task 4: draw by hand a venn digram which captures the relationship between age, Health, and Yrsmar [be careful]
#Not make sure to convert everyting to R2 before drawing it





# Load necessary packages
library(ggplot2)  
library(afex) 
library(tidyr)
library(dplyr)
library(emmeans)
library(heplots)
library(car)
library(effsize)
#Readin data
setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class21/")


#some custom functions that I have written or stolen over the years
source("HelperFunctions.R") 

###In class example 1
FinalData<-read.csv("FinalDataSet.csv")
names(FinalData)

FinalData<-subset(FinalData,Subject<21)
# Restructure Data to make it long-format (now its in wide format)
FinalData.Long<-FinalData %>% gather(BAI.Score, BAI, Baseline:Session.3,convert = TRUE)


###############################
############# Plot ############
###############################
# Generate means and SE
Anova.Plot<-summarySEwithin(FinalData.Long,measurevar="BAI", 
                            withinvars=c("BAI.Score"),
                            betweenvars = c("Therapy"),
                            idvar="Subject", na.rm=FALSE, conf.interval=.95)
Anova.Plot

Plot.1<-ggplot(Anova.Plot, aes(x = BAI.Score, y = BAI, group=Therapy))+
  geom_col(aes(group=Therapy, fill=Therapy), position = position_dodge(width=0.9), colour="black")+ #added black border
  scale_y_continuous(expand = c(0,0),breaks=seq(0,60,15),
                     limits = c(0,60)) +
  geom_errorbar(aes(ymax = BAI + se, ymin= BAI - se), 
                position=position_dodge(width=0.9), width=0.25)+
  scale_fill_manual(values=c("gray50", "white"))+
  xlab('IBI Measurement Group')+
  ylab('Time of Interburst Interval (seconds)')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text= element_text(colour = "black"),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = .5, colour = "black"),
        legend.position = "right",
        legend.title = element_blank())
Plot.1

#################################
##ANOVA
#################################
MixedANOVA<-aov_car(BAI~ BAI.Score*Therapy + Error(Subject/BAI.Score), 
                    data = FinalData.Long)
MixedANOVA


#################################
##Check Assumptions 
#################################
#Levene's test
leveneTest(BAI~ BAI.Score*Therapy, data = FinalData.Long) 

# Box's M
BoxResult<-boxM(FinalData[,3:6],FinalData$Therapy)
BoxResult$cov
BoxResult

#################################
##Follow ups 
#################################



# HW 5: Mixed ANOVA

library(ggplot2)  
library(afex) 
library(tidyr)
library(dplyr)
library(emmeans)
library(car)
library(heplots)
library(effsize)

setwd("/Users/rachel/Box\ Sync/R\ Coursework/HW5")
source("HelperFunctions.R")
MothersVoice.Study<-read.csv("MothersVoice.Data.csv")

# Restructure
MothersVoice.Long<-MothersVoice.Study %>% gather(IBICondition, IBI, Baseline.IBI:Feedback.IBI,convert = TRUE)


# re-set the order using the `factor` command and set the order  
MothersVoice.Study$Level<-factor(MothersVoice.Study$Group, 
                       levels = c(1,2),
                       labels = c("LargeIBI","SmallIBI"))


# Generate means and SE
Anova.Plot<-summarySEwithin(MothersVoice.Long,measurevar="IBI", 
                            withinvars=c("IBICondition"),
                            betweenvars = c("Group"),
                            idvar="ID", na.rm=FALSE, conf.interval=.95)
Anova.Plot


# Graph
Plot.1<-ggplot(Anova.Plot, aes(x = IBICondition, y = IBI, group=Group))+
  geom_col(aes(group=Group, fill=Group), position = position_dodge(width=0.9), colour="black")+ #added black border
  scale_y_continuous(expand = c(0,0),breaks=seq(0,5,1),
                     limits = c(0,5)) +
  geom_errorbar(aes(ymax = IBI + se, ymin= IBI - se), 
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


# Calculate the ANOVA
MixedANOVA<-aov_car(IBI~ IBICondition*Group + Error(ID/IBICondition), 
                 data = MothersVoice.Long)
MixedANOVA



#Levene's test
MothersVoice.Long$Group <- as.factor(MothersVoice.Long$Group) #make the Group factors categorical variables
leveneTest(IBI~ IBICondition*Group, data = MothersVoice.Long) 

# Box's M
BoxResult<-boxM(MothersVoice.Study[,3:4],MothersVoice.Study$Group)
BoxResult$cov
BoxResult

# Follow-up: want to confirm interaction based on hypothesis
# Cut the data
IBICondition.by.Group<-emmeans(MixedANOVA,~IBICondition|Group)
test(pairs(IBICondition.by.Group), joint=TRUE)

# Pairwise follow up 
pairs(IBICondition.by.Group, adjust ='none')






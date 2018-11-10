
# Load necessary packages
library(ggplot2)  
library(afex) 
library(dplyr)    
library(emmeans)

################################
######## Readin data ##########
################################

setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class14")
DataSet<-read.csv("PracticeData.csv", sep = ",")

DataSet$Training <- factor(DataSet$training, 
                      levels=c(1,2), 
                      labels=c("No Social Training","Social ineptness reduction"))

DataSet$Emotional <- factor(DataSet$Emotional, 
                      levels=c(1,2,3), 
                      labels=c("Control","Mild electric shocks","Never ending statistics class"))

################################
######## Descriptives ##########
################################
Sum.Table<-DataSet %>% 
  group_by(Training,Emotional)  %>% 
        summarize(n = n(),
                 Means = mean(DV),
                 SD = sd(DV),
                 SEM = SD/n^.5)

Sum.Table

################################
############# Plot ############# 
################################

Plot.1<-ggplot(Sum.Table, aes(x = Training, y = Means, group=Emotional, fill=Emotional))+
  geom_bar(stat='identity',position="dodge", color='black')+
  geom_errorbar(aes(ymax = Means + SEM, ymin= Means), 
                position=position_dodge(width=0.9), width=0.25)+
  xlab('')+
  ylab('Weirdness')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
Plot.1

################################
############# ANOVA ############ 
################################
Nice.Table<-aov_car(DV~Emotional*Training + Error(SubjectID), 
                       data=DataSet)
Nice.Table

################################
######### Follow-up ############ 
################################
Simple.Effects.By.Type<-emmeans(Nice.Table, ~Training|Emotional)
Simple.Effects.By.Type




################################
###### Theory question ######### 
################################
Nice.Table2<-aov_car(DV~Emotional + Error(SubjectID), 
                    data=DataSet)
Nice.Table2

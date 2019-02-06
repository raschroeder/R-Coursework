# Homework 4

setwd("/Users/rachel/Box\ Sync/R\ Coursework/HW4")

MoralStudy<-read.csv("RmStudy.csv")

MoralStudy$Outcome<-factor(MoralStudy$Outcome, 
                           levels = c(1,2),
                           labels = c("Positive","Negative"))

MoralStudy$Intent<-factor(MoralStudy$Intent, 
                          levels = c(1,2),
                          labels = c("Positive","Negative"))

### Means plot : this is just to see the direction of the effect
library(ggplot2)
library(dplyr)
library(emmeans)

Means.Table<-
  MoralStudy %>% 
  group_by(Outcome,Intent) %>%
  summarise(Fix.Mean=mean(Rating))

Means.Lines <-ggplot(data = Means.Table, aes(x = Outcome, y=Fix.Mean, group=Intent))+
  scale_y_continuous(limits=c(-5,5),breaks = seq(0,20,2))+
  geom_line(aes(colour = Intent), size=2)+
  ylab("morality rating")+xlab("Outcome")+
  theme(legend.position = "right")
Means.Lines

### Bar plot : it isn't appropriate to use a line graph so here's a bar chart
Means.Table2<-
  MoralStudy %>% 
  group_by(Outcome,Intent) %>%
  summarise(N=n(),
            Means=mean(Rating),
            SS=sum((Rating-Means)^2),
            SD=sd(Rating),
            SEM=SD/N^.5)

Plot.1<-ggplot(Means.Table2, aes(x = Outcome, y = Means, group = Intent))+
  geom_col(aes(group=Intent, fill=Intent), position=position_dodge(width=0.9))+
  scale_y_continuous(expand = c(0, 0))+ # Forces plot to start at zero
  geom_errorbar(aes(ymax = Means + SEM, ymin= Means - SEM), 
                position=position_dodge(width=0.9), width=0.25)+ #dodge separates the columns into groups
  scale_fill_manual(values=c("#D3D3D3","#696969"))+
  xlab('')+
  ylab('morality rating')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(), #this makes the box outline open on top
        legend.title=element_blank())
Plot.1


# Run a two-way RM ANOVA 
library(afex)
ANOVA.Moral<-aov_car(Rating~ Outcome*Intent + Error(SS/Outcome*Intent), 
                     data = MoralStudy)
ANOVA.Moral

### SPSS-Like report
aov_car(Rating~ Outcome*Intent + Error(SS/Outcome*Intent), 
                     data = MoralStudy, return="univariate")

### Using Levene's test of HOV since there are less than 3 levels
library(car)
leveneTest(Rating~ Outcome*Intent, data = MoralStudy) 


### Follow up the interaction: we want to see that its driven by the fact that theres a huge
### difference between the Positive intent conditions, whereas there is no difference in 
### the Negative intent conditions
Outcome.by.Intent<-emmeans(ANOVA.Moral,~Outcome|Intent)
test(pairs(Outcome.by.Intent), joint=TRUE)

### Pairwise follow up 
pairs(Outcome.by.Intent, adjust ='none')

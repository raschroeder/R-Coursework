library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)
library(car)

### Build data frame
n=7; r=2; c=3;
KR.Study<-data.frame(ID=seq(1:(n*r*c)),
                         Level=c(rep("Young",(n*c)),rep("Adult",(n*c))),
                         Type=rep(c(rep("Control",n),rep("Unfamiliar",n),rep("Familiar",n)),2),
                         Time.Exploration = c(46,40,27,34,39,37,26,
                                              41,44,31,29,47,28,43,
                                              34,32,48,37,45,48,30,
                                              33,31,33,38,47,42,37,
                                              32,19,36,20,20,22,24,
                                              6,0,0,10,8,12,5))

# re-set the order using the `factor` command and set the order  
KR.Study$Level<-factor(KR.Study$Level, 
                           levels = c("Young","Adult"),
                           labels = c("Young","Adult"))

KR.Study$Type<-factor(KR.Study$Type, 
                          levels = c("Control","Unfamiliar","Familiar"),
                          labels = c("Control","Unfamiliar","Familiar"))

## Check the means and SS calculations
Means.Table<-KR.Study %>%
  group_by(Level,Type) %>%
  summarise(N=n(),
            Means=mean(Time.Exploration),
            SS=sum((Time.Exploration-Means)^2),
            SD=sd(Time.Exploration),
            SEM=SD/N^.5)
knitr::kable(Means.Table, digits = 2) 

## Plot Study
Plot.1<-ggplot(Means.Table, aes(x = Type, y = Means, group=Level))+
  geom_col(aes(group=Level, fill=Level), position=position_dodge(width=0.9), colour="black")+ #added black border
  scale_y_continuous(expand = c(0, 0))+ # Forces plot to start at zero
  geom_errorbar(aes(ymax = Means + SEM, ymin= Means - SEM), 
                position=position_dodge(width=0.9), width=0.25)+ #dodge separates the columns into groups
  scale_fill_manual(values=c("#D3D3D3","#696969"))+
  xlab('')+
  ylab('Time of Exploration')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(), #this makes the box outline open on top
        legend.title=element_blank())
Plot.1

## Test Assumptions
leveneTest(Time.Exploration ~ Level*Type, data = KR.Study) 
### not sig, continue analyzing

qqnorm(KR.Study$Time.Exploration)
qqline(KR.Study$Time.Exploration)
### mostly normal except in the tails

## Run ANOVA
ANOVA.KR.Results<-aov_car(Time.Exploration~Level*Type + Error(ID), 
                          data=KR.Study)
ANOVA.KR.Results

### Test hypotheses
## Cut the data by Type
Simple.Effects.By.Type<-emmeans(ANOVA.KR.Results, ~Level|Type)
Simple.Effects.By.Type

## Test for significance between Types using LSD
pairs(Simple.Effects.By.Type,adjust='none')
# Both groups should be behaving similarly regardless of age; this was found only to be true for the Control condition





# Libraries you will need
library(ggplot2)
library(afex) 
library(gplyr)    
library(emmeans)
library(HH)
# Practice ANOVA and Contrasts

# In an experiment, participants were asked to rate on a 5-point scale each of 24
# words on a specified attribute, either (a) how pleasant the meaning was, (b)
# frequency of usage in the language, (c) how pleasant the word sounded, or (d)
# how frequently the syllables of the word were used in the language.  After the
# words were rated, each participant wrote down as many of the words as he or she
# could remember.  Of interest was whether the number of words recalled by each
# participant varied as a function of rating condition.   More specifically, the
# researcher had the following hypotheses:
  
# (H1)	People would have better recall for words rated for pleasantness than
# words rated for the frequency of either the word or its syllables in the
# English language. 

# (H2)	Words rated for their frequency in the English language would be recalled
# better than words rated for the frequency of their syllables in the English
# language,z 

# (H3)	Words rated for pleasantness of meaning would be recalled
# best of all.

# Here are the results

setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class6")
WordNerdData<-read.csv("PracticeData.csv")
str(WordNerdData)
WordNerdData$Condition<-factor(WordNerdData$Condition,
                               levels=c(1,2,3,4),
                               labels=c("Pleasantness of\nMeaning","Frequency of\nWord", 
                                        "Pleasantness of\nSound","Frequency of\nSyllables"))

###### 1. Plot the data

#Check the means and SS calculations with dpylr
library(dplyr)
Means.Table<-WordNerdData %>%
  group_by(Condition) %>%
  summarise(N=n(),
            Means=mean(Recall),
            SS=sum((Recall-Means)^2),
            SD=sd(Recall),
            SEM=SD/N^.5)
Means.Table

#plot

library(ggplot2)

Means.Table$Condition<-factor(Means.Table$Condition, 
                              levels = c("Pleasantness of\nMeaning","Frequency of\nWord","Pleasantness of\nSound","Frequency of\nSyllables"),
                              labels = c("Pleasantness of\nMeaning","Frequency of\nWord","Pleasantness of\nSound","Frequency of\nSyllables"))

Plot.1<-ggplot(Means.Table, aes(x = Condition, y = Means))+
  geom_col()+
  scale_y_continuous(expand = c(0, 0))+ # Forces plot to start at zero
  geom_errorbar(aes(ymax = Means + SEM, ymin= Means - SEM), 
                position=position_dodge(width=0.9), width=0.25)+
  xlab('Rating Condition')+ # name these yourself
  ylab('Number Recalled')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        legend.title=element_blank())
Plot.1

###### 2. Run the ANOVA (check the assumptions)

library(afex)

Word.ANOVA<-aov_car(Recall~Condition + Error(ID), 
                     data=WordNerdData)
Word.ANOVA

## Assumputions (HOV)

library(HH)
hov(Recall~Condition, data=WordNerdData)
hovPlot(Recall~Condition, data=WordNerdData)

qqnorm(WordNerdData$Recall)
qqline(WordNerdData$Recall)

######	3.Follow-ups: State which you are doing (planned, unplanned) and 
######	which hypotheses they link too. 
###### [in the future we will not spell out those hypotheses like that]

Fitted.Model<-emmeans(Word.ANOVA, ~Condition)
set1 <- list(
  H1 = c(-2,2,-2,2),
  H2 = c(0,2,0,2),
  H3 = c(3,-1,-1,-1))

## (H1)	People would have better recall for words rated for pleasantness than
## words rated for the frequency of either the word or its syllables in the
## English language. 

Fitted.Model<-emmeans(Word.ANOVA, ~Condition)
set1 <- list(
  H1 = c(-2,2,-2,2))

Fitted.Model 
Planned<-contrast(Fitted.Model, set1, adjust = "none")
Planned

## (H2)	Words rated for their frequency in the English language would be recalled
## better than words rated for the frequency of their syllables in the English
## language,z 

Fitted.Model<-emmeans(Word.ANOVA, ~Condition)
set2 <- list(
  H2 = c(0,2,0,-2))

Fitted.Model 
Planned<-contrast(Fitted.Model, set2, adjust = "none")
Planned

## (H3)	Words rated for pleasantness of meaning would be recalled
## best of all.

Fitted.Model<-emmeans(Word.ANOVA, ~Condition)
set3 <- list(
  H3 = c(-2,2,-2,2))

Fitted.Model 
Planned<-contrast(Fitted.Model, set3, adjust = "none")
Planned

######	4. Follow-ups: Justify if you are correcting alpha/p-value in any way (you are free to
######	correct or not but you must defend your choice).

# Three tests is a small amount and they were all planned a prior, so no corrections are needed. If we were adding
# follow up contrasts to investigate unplanned tests we would need to correct those results.


###### 5. Do those Follow-ups you said you would do!  
###### Make sure you ANNOTATE what you are doing and explain it clearly so you can




###### 6. Reviewer demands pairwise comparisons. Explain why that is bad request but also do it and do Bon correction! 

Fitted.Model<-emmeans(Word.ANOVA, ~Condition)
pairs(Fitted.Model, adjust='bon')


###### 7. Get the effect sizes and try to write up JUST the results section in APA style with a figure (labeled)

#Creating a function to calculate the d
t_to_d<-function(t,df){
  d<-(2*t)/(df)^.5
  return(d)
}
# use the t and the df from the anova output
t_to_d(4.064,60)  #H1
t_to_d(3.464,60)  #H2
t_to_d(6.395,60)  #H3

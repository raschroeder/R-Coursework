setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class15")
DataSet<-read.csv("RMANOVA_Data.csv")

#Regular One-way ANOVA
library(afex)
Model.1<-aov_car(BP~ Audience_Size + Error(Id/Audience_Size), 
                 data = DataSet)
Model.1

### SPSS-Like report
aov_car(BP~ Audience_Size + Error(Id/Audience_Size), 
        data = DataSet, return="univariate")

### Manually Select DF Correction & $\eta_p^2$
aov_car(BP~ Audience_Size + Error(Id/Audience_Size), 
        data = DataSet,
        anova_table=list(correction = "HF", es='pes'))

### Pairwise follow up Fisher LSD
library(emmeans)
Fitted.Model<-emmeans(Model.1, ~Audience_Size)
Fitted.Model

pairs(Fitted.Model, adjust='none')

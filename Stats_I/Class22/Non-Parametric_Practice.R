
#######################
# Two-Way Chi-Squared #
#######################

########  Example 1
#When Chicagoans travel to NYC they are surprised by the way the locals fold
#their giant thin slices of pizza when they eat them. While folding New
#York-style pizza is both a proper and efficient manor to eat pizza, visitors
#think it is a strange and sometimes ignore the local custom, resulting in
#irritating the easily enraged locals.  You decide to test where or not "social
#pressure" is enough to get Chicagoans to fold their New York-style pizza. You
#bring in 40 born and raised Chicagoans and randomly assign them to one of two
#possible social pressure conditions. 1) High social pressure condition--they
#are forced to eat NY-slice at a table with 12 angry looking New Yorkers all
#eating folded pizza. 2) Low social pressure condition--they sit with 8 of their
#friends (all Chicagoans) who eat the pizza unfolded and 4 angry looking New
#Yorkers all eating folded pizza.  You measure whether your subjects succumbed
#to social pressure and folded their pizza.

Pizza.Exp2 <- as.table(rbind(c(19, 3), c(1, 17)))
dimnames(Pizza.Exp2) <- list(Style = c("Unfolded", "Folded"),
                             Social = c("LSP","HSP"))

(Pizza.Exp2.chi <- chisq.test(Pizza.Exp2,correct = TRUE))  # Prints test summary
Pizza.Exp2.chi$observed   # observed counts (same as M)
Pizza.Exp2.chi$expected   # expected counts under the null


chisq.test(Pizza.Exp2)$p.value        
chisq.test(Pizza.Exp2, simulate.p.value = TRUE, B = 10000)



########  Example 2 
#A researcher wants to find out if the polygraph match is
#good method for determining truthfulness.  To test this, an experimenter tells
#the subjects to commit a mock crime and steal an iPad from the lab and lie on
#the polygraph.  The remaining subjects are told not to steal and tell the truth
#about not stealing. The polygrapher, blind to which experimental condition each
#subject is in, tests each subject to determine if they committed the crime or
#not.

### Use the code above to run your chi-squared
Poly.Exp <- as.table(rbind(c(40, 12), c(20,48)))
dimnames(Poly.Exp) <- list(Outcome = c("Judged Innocent", "Judged Guilty"),
                             Condition = c("Truly Innocent","Truly Guilty"))

(Poly.Exp.chi <- chisq.test(Poly.Exp,correct = TRUE))  # Prints test summary
Poly.Exp.chi$observed   # observed counts (same as M)
Poly.Exp.chi$expected   # expected counts under the null



############################################
# Convert ANOVA Mixed ANOVA to Chi-Squared #
############################################
library(dplyr)
library(tidyr)

#Take the mixed practice data from before thanksgiving and convert it to a chi-square

setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class22/")
FinalData<-read.csv("FinalDataSet.csv")

# Keep only "Subject","Therapy","Baseline","Session.3"
FinalData<-subset(FinalData,Subject<21, select=c("Subject","Therapy","Baseline","Session.3"))

# Make the data long
FinalData.Long <- FinalData %>% gather(Time, DV, Baseline:Session.3,convert = TRUE)

# Create new column where you convert the DV (BAI score) into a catagorical:
# those with severe (BAI >= 36) vs those will those "not severe" (BAI < 36)
FinalData.Long$DV.Cat<-ifelse(FinalData.Long$DV >=36,"Severe","Not Severe")

# tabulate the new categorical DV by therapy and Time
Table.3.way<-FinalData.Long %>% group_by(Therapy,Time ) %>% count(DV.Cat)
Table.3.way

# Its easier to hand create a table for chi-square of you can convert the data
# back to wide format
Table.Therapy <- as.table(rbind(c(10, 9), c(10, 5)))
dimnames(Table.Therapy) <- list(Therapy = c("CBT","DT"),
                                Time = c("Pre", "Post"))
Table.Therapy

# Conduct two-way chi-square on the new table
chisq.test(Table.Therapy)

#### Example on your own

#### You're interested in whether people have preferences for the side to which
###a politician parts their hair, and whether these preferences vary by the
###politician's party (Republican or Democrat). Participants were shown 4
###pictures of politicians at a time and asked to pick their favorite. These
###politicians varied only in the part of their hair (left or right), and the
###pin on their suit (a donkey or an elephant). You get the following data from
###Jeremy, your collaborator, who wants you to run it through a chi-squared test
###of independence.



### HINT: Use tally() within your dplyr to calculate the number of people per
### combination of levels


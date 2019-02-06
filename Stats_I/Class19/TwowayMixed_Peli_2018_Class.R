
# Load necessary packages
library(ggplot2)  
library(afex) 
library(tidyr)
library(dplyr)
library(emmeans)
library(car)
library(heplots)
library(effsize)
#Readin data
setwd("/Users/rachel/Box\ Sync/R\ Coursework/Class19")

#some custom functions that I have written or stolen over the years
source("HelperFunctions.R") 

###In class example 1
PSSeliData<-read.csv("PSSeliData.csv")
View(PSSeliData)
names(PSSeliData)

# Restructure Data to make it long-format (now its in wide format)
# Extract variables we need
PSSeliData.Subset<-subset(PSSeliData,select=c("Subject.ID","Condition..1...difficult..2...easy.","Proportion..Intentional.Mind.Wandering.","Proportion..Unintentional.Mind.Wandering."))
head(PSSeliData.Subset)

# I hate those variable names! 
PSSeliData.Subset<-rename(PSSeliData.Subset, Condition = Condition..1...difficult..2...easy.,
       Intentional = Proportion..Intentional.Mind.Wandering.,Unintentional= Proportion..Unintentional.Mind.Wandering.)

head(PSSeliData.Subset)

#################################################
# Now reshape from wide to long                 #
# http://ademos.people.uic.edu/Chapter9.html    #
#################################################
# Remember use "gather", name the output `PSSeliData.Long`
PSSeliData.Long <- PSSeliData.Subset %>% gather(MindType, Proportion, Intentional:Unintentional,convert = TRUE)


# Add labels to Condition
PSSeliData.Long$Condition<-factor(PSSeliData.Long$Condition, 
                           levels = c(1,2),
                           labels = c("Difficult-SART","Easy-SART"))


###############################
############# Plot ############
###############################

##Generate means and SE form any long format dataset. 
Anova.Plot<-summarySEwithin(PSSeliData.Long,measurevar="Proportion", 
                            withinvars=c("MindType"),
                            betweenvars = c("Condition"),
                            idvar="Subject.ID", na.rm=FALSE, conf.interval=.95)

#To get a graph
Plot.1<-ggplot(Anova.Plot, aes(x = MindType, y = Proportion, fill=Condition))+
  geom_bar(stat='identity',position="dodge", color='black')+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,.45,.05),
                     limits = c(0,.45)) +
  geom_errorbar(aes(ymax = Proportion + se, ymin= Proportion - se), position=position_dodge(width=0.9), width=0.25)+
  scale_fill_manual(values=c("gray50", "white"))+
  xlab('Mind Wandering')+
  ylab('Proportion of Mind Wandering')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text= element_text(colour = "black"),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = .5, colour = "black"),
        legend.position = "top",
        legend.title = element_blank())
Plot.1

ggsave("Plot1.png", width = 5, height = 2.5, units = "in")

#############################
#### ANOVA ##################
#############################
# Run anova here

Mixed.1<-aov_car(Proportion~ Condition*MindType + Error(Subject.ID/MindType), 
                 data = PSSeliData.Long)
Mixed.1


#################################
##Check Assumptions [not done in paper]
#################################
# Brown-forthysth on between term and interaction
# Brown-Forsythe test on Group Only
leveneTest(Proportion~ Condition, data=PSSeliData.Long,center=median)

# Brown-Forsythe test on interaction
leveneTest(Proportion~ Condition*MindType, data=PSSeliData.Long,center=median)
#Box's M
data_wide <- PSSeliData.Long %>% spread(MindType, Proportion)

BoxResult<-boxM(data_wide[,3:4],data_wide$Condition)
BoxResult$cov
BoxResult

############################################
##Follow ups just as they did in the paper #
############################################
## They did not use the error terms of the ANOVA; 
## They followed it up between (not within)
# You will need to subset the data. I have done it for you

PSSeliData.Long.Intentional<-PSSeliData.Long[PSSeliData.Long$MindType=="Intentional",]
PSSeliData.Long.Unintentional<-PSSeliData.Long[PSSeliData.Long$MindType=="Unintentional",]

# Intentional


# Unintentional


########################################
##Follow ups using Error term of ANOVA #
########################################
# Follow it up the other way (within!) using error term from ANOVA. This will
# now allow further correction



# Is it easier story to tell this way? 

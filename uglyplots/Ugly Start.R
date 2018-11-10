



##################################################
################# Challenge 1 ####################
##################################################

#read the data set.  Its in wide format
#Its a mixed design (2x3) 
# 2 = classical singers vs Pop singers (between)
# 3 = Distruptions: None, Random coughing, Random screaming (within)
# DV = Number of errors
# Co-Var = years of experience
setwd("C:/Users/Matt/Downloads/Directory/543")
Singer.Study<-read.csv("2waySingersWide.csv")

source('HelperFunctions.R')

#make it long and keep the coviarate as covariate (Experience): MELT 
#Note you can use variable.name="Distruptions" to name your factor
library(dplyr)
library(tidyr)
singerlong<- Singer.Study %>%
  gather(., Disruptions, Data, None:Screaming)
singerlong <- melt(data = Singer.Study, id.vars=c('SubjectID','Singers','Experience'),
                  value.name = "Data", variable.name = "Disruptions")
singerlong


## Make an APA plot (leave out co-var)

sumr <- summarySEwithin(singerlong,measurevar=c("Data"), 
                                  withinvars=c("Disruptions"),
                                  betweenvars=c("Singers"),
                                  idvar="SubjectID", na.rm=FALSE, conf.interval=.95)
sumr

library(ggplot2)
dodge=position_dodge(width=0.9)

singerAPA <-ggplot(data = sumr, aes(x = Singers, y =Data, fill=Disruptions))+
  #facet_grid(.~ Singers, labeller=label_both)+
  #coord_cartesian(ylim = c(.02,.6))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(aes(ymax = Data + se, ymin=Data - se), position=dodge, width=0.25)+
  xlab("Genre")+ylab("Errors")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.key = element_blank())+
  scale_fill_grey()

singerAPA



#Make scatter plot with  x=Experience, y=Error (Facet distruption and singers)


sing.s <-ggplot(data = singerlong, aes(x = Experience, y =Data)) +
  facet_grid(Disruptions ~ Singers, labeller=label_both)+
  #coord_cartesian(ylim = c(.0,.8))+ 
  geom_point()+
  #geom_line()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE, color='black')+
  xlab("Experience")+ylab("Errors")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  
sing.s


##################################################
################# Challenge 2 ####################
##################################################

## remake the singers data in anway you want to tell any story you want (or just use a apa or scatter version)
## However, MAKE the visuals as ***Over the top*** as you can!! 


# use strange colors or photos of cats as backgrounds to the facets
# use different fonts everywhere - (keep it english)
# put arrows to highlight specific bars or dots (Point to all the things)
# put labels everywhere (like your reader has zero working memory)
# clashing colors for everywhere (like a blind monk was painting his version of heaven)
# Use crazy x and y scales (like log 10 vs square root- with proper labeling)
# The graph should still be readable.  Just crazy looking!!!







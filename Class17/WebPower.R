library(ggplot2)
library(WebPower)
library(dplyr)

##########################
# Some basic conversions #
##########################

# Cohen's f =  d / 2  (rought appromimation) ; So 2*f = d
# cohen's f =  sqrt(partial eta-sqaured / ( 1 - partial eta-sqaured))


####################
# One-way RM ANOVA #
####################
# Get Sample size needed at Power of .80 for f = .25 (d = .5)
RM.N<-wp.rmanova(n = NULL, ng = 1, nm = 4, f = 0.25, nscor = 1,
                            alpha = 0.05, power = .80, type = 1)
RM.N

# type: The value "0" is for between-effect; "1" is for within-effect; and "2" is for interaction effect on mixed ANOVA.

# Find the miminal Effect size you can detect give a specific sample size
# N = 30
RM.f<-wp.rmanova(n = 30, ng = 1, nm = 4, f = NULL, nscor = 1,
                 alpha = 0.05, power = .80, type = 1)
RM.f


# Relationship between Power and Sample size @ f = .25 (d = .50)
RM.MainEffect.N<-wp.rmanova(n = seq(10,300,1), ng = 1, nm = 4, f = 0.25, nscor = 1,
           alpha = 0.05, power = NULL, type = 1)
RM.MainEffect.N

# Plot Power Curve
RM.MainEffect.N<-as_data_frame(RM.MainEffect.N[c(1,7)])
ggplot(data=RM.MainEffect.N,aes(x=n, y=power))+
  geom_point()+
  xlab("Sample Size")+ylab("Power")+
  theme_bw()


# Relationship between Power and Effect size @ N = 50
RM.MainEffect.f<-wp.rmanova(n = 50, ng = 1, nm = 4, f = seq(.1,.5,.01), nscor = 1,
                          alpha = 0.05, power = NULL, type = 1)
RM.MainEffect.f

RM.MainEffect.f<-as_data_frame(RM.MainEffect.f[c(2,7)])
ggplot(data=RM.MainEffect.f,aes(x=f, y=power))+
  geom_point() +
  xlab("Cohen's f")+ylab("Power")+
  theme_bw()


# Two-way ANOVA

TW.MainEffect.N<-wp.kanova(n = seq(10,100,1), ndf = 2, f = .5, ng = 6, alpha = 0.05, power = NULL)
TW.MainEffect.N

TW.MainEffect.N<-as_data_frame(TW.MainEffect.N[c(1,7)])
TW.MainEffect.N$Type<-"Two-Way Between"

ggplot(data=TW.MainEffect.N,aes(x=n, y=power))+
  geom_point()+theme_bw()


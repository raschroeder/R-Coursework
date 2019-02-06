library(MASS)
library(tidyr)
library(dplyr)
library(Matrix)
library(afex)
library(emmeans)
library(car)
library(heplots)
#####################################
## Set Paramaters
#####################################
set.seed(666)

n   <- 30
NumberTreats <- 4
TreatMeans1   <- c(0, 0, 0, 0)
TreatMeans2   <- c(0, 0, 0, 0)


#####################################
## Automated Building of ANOVA
#####################################

## Generate Cov matrix
# create k x k matrix populated with sigma
sigma1=4
sigma2=8
r1=.5
r2=0
sd.r=.4


######################################
## One-way RM ANOVA Via Mente Carlo ##
######################################
# Set number of simulations and sample size
nSim         <- 500
Set.Alpha    <- .05

# Custom Function (must run parameters from start)
SimMixedWay <- function(alpha){
  S1  <- forceSymmetric(Matrix(rnorm(16, m=sigma1,s=1), 4))
  Rho1<- forceSymmetric(Matrix(rnorm(16, m=r1,s=sd.r), 4))
  S2  <- forceSymmetric(Matrix(rnorm(16, m=sigma2,s=1), 4))
  Rho2<- forceSymmetric(Matrix(rnorm(16, m=r2,s=sd.r), 4))
  
  # compute covariance between measures
  Sigma1 <- (S1^2) *Rho1
  Sigma2 <- (S2^2) *Rho2
  # put the variances on the diagonal 
  diag(Sigma1) <- sigma1^2  
  diag(Sigma2) <- sigma2^2 
  
  Sigma1<-nearPD(Sigma1)
  Sigma2<-nearPD(Sigma2)
  
  Simulation1 = mvrnorm(n=n, mu=TreatMeans1, Sigma=Sigma1$mat, empirical=FALSE)
  Simulation2 = mvrnorm(n=n, mu=TreatMeans2, Sigma=Sigma2$mat, empirical=FALSE)
  
  DataSim1<-data.frame(rbind(Simulation1,Simulation2))
  DataSim1$ID<-as.factor(rep(seq(1:(n*2))))
  DataSim1$Group<-as.factor(c(rep("CBT",n),rep("Control",n)))
  DataSim2<-gather(DataSim1,key = "Time", value = "BDI",X1:X4)
  Mixed.1<-suppressMessages(aov_car(BDI~ Time*Group + Error(ID/Time), 
                   data = DataSim2))
  Mixed.U<-summary(Mixed.1,return="univariate")
  PUncorrect<-Mixed.U$univariate.tests[4,6]
  
  data_wide <- DataSim2 %>% spread(Time, BDI)
  BoxResult<-boxM(data_wide[,3:6],data_wide$Group)
  BoxP<-BoxResult$p.value

  Time.by.Group<-emmeans(Mixed.1,~Time|Group, model = "multivariate")
  FollowUps<-as.data.frame(pairs(Time.by.Group, adjust = "tukey"))
  CountFU<-sum(FollowUps$p.value<.05)/12
  
  FollowUpsB<-as.data.frame(pairs(Time.by.Group, adjust = "bon"))
  CountFUB<-sum(FollowUpsB$p.value<.05)/12
  
  Group.by.Time<-emmeans(Mixed.1,~Group|Time, model = "multivariate")
  FollowUpsG<-as.data.frame(pairs(Group.by.Time, adjust = "tukey",model = "multivariate"))
  CountFUG<-sum(FollowUpsG$p.value<.05)/4

  Results<-c(PUncorrect,BoxP,CountFU,CountFUB,CountFUG)
  
  return(Results)
}

SimMixedWay(.05)

# Run Power analysis (n simulation )
Sim1<-replicate(1000, SimMixedWay(Set.Alpha))

Sim.Result<-as.data.frame(t(Sim1))
Sim.Result$SigInter<-ifelse(Sim.Result$V1<.05,1,0)
Sim.Result$SigBoxM<-as.factor(ifelse(Sim.Result$V2<.05,1,0))

Sim.Result %>% group_by(SigBoxM) %>% summarise(typeI=mean(SigInter), TypeIFU=mean(V3))

Sim.Result %>% group_by(SigBoxM,SigInter) %>% summarise(TypeIFU=mean(V3),TypeIFUB=mean(V4),
                                                        TypeIFUG=mean(V5))


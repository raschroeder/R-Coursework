library(afex)
library(MASS)
library(WebPower)

#####################################
## Set Paramaters
#####################################

SampleSize   <- 10
NumberTreats <- 4
TreatMeans   <- c(.8, 0, 0, 0)
stdevs       <- c(1, 1, 1, 1)
rho          <- .5

#####################################
## Automated Building of ANOVA
#####################################

# Set Factors
Subject <- factor(1:(SampleSize))
RM <- as.factor(1:NumberTreats)

DataSim <- expand.grid(RM,Subject)
names(DataSim) <- c("RM","Subject")


## Generate Cov matrix
# create k x k matrix populated with sigma
S <- matrix(stdevs, ncol=length(stdevs), nrow=length(stdevs))
rho=.4
# compute covariance between measures
Sigma <- t(S) * S * rho  
# put the variances on the diagonal 
diag(Sigma) <- stdevs^2  

# Simulate DV from COV matrix (exact matrix to get EFFECT SIZE)
DV.Simulated = mvrnorm(n=SampleSize, mu=TreatMeans, Sigma=Sigma, empirical=TRUE)
DataSim$DV<-as.vector(t(DV.Simulated))

###########################################
## Look at Simulation to get Effect size ##
###########################################
Result <- aov_car(DV ~ RM + Error(Subject/RM), DataSim, anova_table=list(correction = "none", es='pes'))
Result

# Convert Eta to cohen's f
eta<-Result$anova_table$pes
cohens.f = sqrt(eta / ( 1 - eta))
cohens.f


###########################
## One-way RM ANOVA Via Formula
###########################
RM.OneWay.N<-wp.rmanova(n = NULL, ng = 1, nm = 4, f = cohens.f, nscor = 1,
                      alpha = 0.05, power = .8, type = 1)
RM.OneWay.N

RM.OneWay.AP<-wp.rmanova(n = SampleSize, ng = 1, nm = 4, f = cohens.f, nscor = 1,
                         alpha = 0.05, power = NULL, type = 1)
RM.OneWay.AP

######################################
## One-way RM ANOVA Via Mente Carlo ##
######################################
# Set number of simulations and sample size
nSim         <- 500
Set.Alpha    <- .05

# Custom Function (must run parameters from start)
SimRMOneWay <- function(alpha){
  DV.Simulated <- mvrnorm(n=SampleSize, mu=TreatMeans, Sigma=Sigma, empirical=FALSE)
  DataSim$DV<-as.vector(t(DV.Simulated))
  aov.sim <- aov_car(DV ~ RM + Error(Subject/RM), DataSim, anova_table=list(correction = "GG", es='none')) 
  pvalue <- aov.sim$anova_table$`Pr(>F)`
  sig = pvalue < alpha
  return(sig)
}


# Run Power analysis (n simulation )
mean(replicate(nSim, SimRMOneWay(Set.Alpha)))



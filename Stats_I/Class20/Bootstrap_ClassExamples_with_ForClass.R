##############################################################
#####################Boostrapping basics
##############################################################

#################################
#lets Build a normal distrobution 
################################

?rnorm #lets us build a normal distrobution with whatever mean and SD we want! 

n=1e6 #Population Size
MeanofPop = 0 
SDofPop = 1

## Build the population
population<-rnorm(n, MeanofPop, SDofPop)
hist(population)

#Build a Probability density function (just like all those in your books)
plot(density(population))


#################################
#lets sample from our population
################################

SizeofSample = 10 #we are going to take small samples from our population

?sample # This function is going to let us sample from the population. 
#We can do it with or without replacement


#sample 1 - takes the population, removes a sample, and replaces with other ones
sample.1<-sample(population, SizeofSample, replace = TRUE)
hist(sample.1)

mean(sample.1)  #mean shoud be around 0
SE.1<-sd(sample.1)/sqrt(SizeofSample) # should be around SDofPop
SE.1

Estimated.SEM<-SDofPop/sqrt(SizeofSample)



#Sample 2
sample.2<-sample(population, SizeofSample, replace = TRUE)
hist(sample.2)

mean(sample.2)
SE.2<-sd(sample.2)/sqrt(SizeofSample) # should be around SDofPop/sqrt(SizeofSample)
SE.2


#why are the two simulated SE so differnet from the estimted SE?
# Cause we only did 1 sample! Too bootstrap we need to sample with replacement 1000s of times!

# We need to build a distrobution of sample means!
# We could code it ourselves using a nested series of functions
# lets write on the board what we want to happen and then we will examine how to code it below. 

samples.means<-replicate(1000,mean(sample(population, SizeofSample, replace = TRUE))) #takes 1000 samples and calcs the means
plot(density(samples.means))
sd(samples.means) # this is actually smaller than the esimated SEM. This is actually more accurate than estimating it.   


##############################################################
#####################Boostrapping Practice 1
##############################################################

#Lets build a beta distrobution

BetaDistro<-rbeta(n, shape1=2, shape2=25, ncp = 1)
plot(density(BetaDistro))

True.Median<-median(BetaDistro)
True.Median

#Lets bootstrapp a median if this skewed distrobution.  
Sample.Size.Median = 30
samples.medians<-replicate(1000,median(sample(BetaDistro, Sample.Size.Median, replace = TRUE)))

plot(density(samples.medians))
Boot.Median<-mean(samples.medians)
Boot.Median

Percent.Off<-(abs(True.Median-Boot.Median)/True.Median)*100
Percent.Off

#How can I reduce the error? 
##Increase the sample size or the number of estimates

##############################################################
#####################Boostrapping a Correlation
##############################################################

# lets pretend you had 20 people on two scales and you wanted to correlate their data: 
set.seed(666)
n=20
Corr.data <- data.frame(y = runif(n), x = runif(n))
Corr.data

# Original correlation
with(Corr.data, cor(x, y))
# So basically a weak relationship, lets examine CIs around the correlation

# Our function that will be used to feed into 
boot_corr <- function(data, resample_vector) {
  cor(data$x[resample_vector], data$y[resample_vector])
}

library(boot)
cor_results <- boot(Corr.data, boot_corr, sim = "ordinary",R = 1000)

boot.ci(cor_results)

##############################
### Some magic stuff below ###
##############################
# Let run a difference test on the SD difference between groups using boot function
# We will look to see if the differences include zero

# lets start with Some data Group 1 and 2, had a mean of 50, but group 1 has an
# SD = 25, and group 2 SD = 12.5
N=30
set.seed(42)
DatSet<-data.frame(Group1=rnorm(N,50,25),Group2=rnorm(N,50,12.5))
summary(DatSet)
sd(DatSet$Group1)
sd(DatSet$Group2)
# We need to define our test.
boot.variance <- function(data, i) {
  SD1=sd(data$Group1[i])
  SD2=sd(data$Group2[i])
  d=(SD1-SD2)
}

Var_results <- boot(DatSet, boot.variance, sim = "ordinary",R = 1000)

boot.ci(Var_results)


# We can run a Monte-Carlo simulation as well

MonteCarlo.variance <- function(N) {
  SD1=sd(rnorm(N,50,25))
  SD2=sd(rnorm(N,50,12.5))
  d=(SD1-SD2)
  return(d)
}

sample.size=30
M.Var<-replicate(10000,MonteCarlo.variance(sample.size))

Percentle.CI.Var<-quantile(M.Var, c(.05,.5,.95))
Percentle.CI.Var

##############################################################
#####################Small Group Challange 
##############################################################

# We will form groups of 3.
# The goal is to create your own bootstrapped between subjects t-test! 
# You will 1) build populations, 2) sample from each distrobution, 3) create difference scores, 3) use the difference score to calculate the boot t.test (all by code!)
# Remember SD of the bootstrapped distrobution of scores =  SEM of that distrobution
# SO, boot.t test = mean(differences scores)/SD(differences scores) [Note this is quick and dirty version]

# I have build you one function which will help at the end
# to get the pvalues for your t-test use tis function. 
# You need to enter the t and the n as the sample size of your t-test
# Run the code to load into memory
Boot.t.pvalue<-function(boot.t, sample.size) {
  pvalue<-2*pt(-abs(boot.t),df=(sample.size*2)-2)
  return(pvalue)
}

# When you find you t-value, you just need to do this pvalue<- Boot.t.pvalue(tvalue, sample.size)




###################################
#######Task 
###################################


#Step 1: Make chart of the steps you need to follow to solve the task 
# (my headers below will help you organize your chart). 


# here are the paramters you need to run this simulation study
#Make two Normal populations!
#population 1: mean = 60 , SD = 10  
#population 2: mean = 90 , SD = 20
n=1e6 #Population Size


### Build the populations

#graph each (just to check your code)


#Sample with 10 people  from each population and do each 10000 timess 


# Get a vector of the differences between each sample mean


#plot the vector (this is the distrobution of mean differences of 1000 studies on this experiment)


# Run a boot.t test and get the pvalue



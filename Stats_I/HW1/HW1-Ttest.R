####################
# t-Test Homework #
###################

# Daryl Bem published a controversial paper in 2011 that helped spark the
# argument over replication and the "file-drawer problem" (i.e., not telling
# people how many times the study failed to yield a significant result).  His
# paper, on psi, included 10 studies with a mean effect size of d = .22. You
# want to see for yourself the "file" drawer problem, and you replicate his
# first study, pre-cognition of erotic stimuli, 2 times. Once you use the same
# sample size as him, N = 100. The other time you run a power analysis based
# on his mean effect size and re-run this studies. (see the Study 1 in the paper
# for the methods details on Experiment 1: Note you will only test statistically
# the erotic images).


# Part A - Power: You decided to run a more conservative power analysis:
# One-sample t-test, alpha =.05, power = .95, two-tailed, d = .22 (not d = .25
# as he did) for erotic images. Write code below. 
# NOTE: The right answer rounds up to 271 



###############################################################################################
#Part B: Running analysis: You have run the 4 replications (2 x N =100, 2 X N =
#271). Run your one-sample t-test in R on the DV (% correct) for erotic items. 

# to get effect sizes (I wrote a function for you to do one-sample effect sizes)
one.sample.d<-function(data,mu){
  d = (mean(data)-mu)/sd(data)
  return(d)
}

# Read in Stuff
#set working directory  (where you put those CSV files)
setwd('C:/AlexFiles/SugarSync/UIC/Teaching/Graduate/543-Fall2018/Homework/T-test')

# This reads in the CSV files 
# (its ugly, but I will show you more later to do it better)
HitErotic1<-as.numeric(read.csv('HitErotic1.csv')$x)
HitErotic2<-as.numeric(read.csv('HitErotic2.csv')$x)
## Notes: 
# Replication 1 are the exact Replication
# Replication 2 is a the high powered one


# 1.	For the exact replications (N = 100), do exactly what Bem did:
#one-sample t, one-tailed, a=.05 


############## Replication 1 (Exact)
###Erotic Pictures
#t-test


# Effect size



##################################################################################
# 2.	For the higher power replications (N = 271), do: one-sample t, two-tailed,
# a=.05

############## Replication 4 (High Power)
###Erotic Pictures
#t-test



# Effect size



#Part C - Write up:  You will write up your results as if you conducted these
#studies and will submit them to an APA journal. Remember, numbers do not speak
#for themselves. This means that you must preface your write up with a reminder
#to the reader of your a) hypothesis, b) a quick reminder of the methods (Bem
#does it one sentence, and how your replication differed), c) and summarize in
#plain English what you found. A hallmark of a good write up is that someone not
#versed in statistics can still understand what you did and what you found.
#Finally, be sure to report all results in APA style (Don't forget you have Bem
#paper as a model). This should be very short (about one short paragraph).

# Write this in word (not R)





####################################################################################
# Part D - Theory questions: 

# 1.	If you assumed a different theory, that porn
# shocks people and thus messes with their PSI ability (thus you would have
# predicted the results in the other direction), how would you p-values change
# for each of the experiments. Would your results support that theory?
  
  
  
  
#2.	The data in each replication was created by simulating a normal distribution
#with a mu = .5 and sigma = .1. In short, the null was simulated to be true. 

#a.	Why would the results have come out significant for the exact replication
#had you used to two-tail experiment vs one-tailed?  In your answer make sure to
#explain how the critical values shift (Hint: pictures might be helpful, which
#you can hand draw).




# b.	Cohen's d was larger in exact replication vs the higher-powered replication
# for the erotic images.  Why might you have gotten large d for the
# higher-powered vs exact replication? Be specific. (Hint: it's a short answer,
# but be clear on your statistical theory why this happened.)





#3.	Had Bem instead used an independent samples t-test between the hit rate of
#erotic vs non-erotic images, how might have the results changed (Hint: Think
#about what is different in the one-sample vs two-sample t-test formula.)




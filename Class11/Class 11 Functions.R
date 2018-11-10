
#################################################################
#################################################################
##################  Basic Functions #############################
#################################################################
#################################################################

##Review: 
  
#How to sqaure a vector

Vector1<-c(5,2,4,2)
Vector1 * Vector1 
Vector1^2

# We can also write a function to do this. You've been using functions this whole time, let's
# look at the grammar of these functions
# 
# 'name'<- function("parameter'){
# 'product of function'<-'what you want the function to do goes here'
# return('product')
# }


## Same thing above as a function!

square.it <- function(x) {
  square <- x * x 
  return(square) 
} 


Results.1<-square.it(Vector1)

Results.1

##############################
####### Quick Practice #######
##############################

# Write your own function that does x*5 + 5

add.five <- function(x) {
  addition <- x * 5 + 5 
  return(addition) 
} 

Results.2<-add.five(x=Vector1)

Results.2



####################################################
##### Built in functions with custom functions #####
####################################################

##### What does this do?

Test.1 <- function(x) {
  test<- x * sum(x, na.rm = F) 
} 

?sum
sum(Vector1)

(Test.1(Vector1))

Vector2<-c(Vector1,NA)
Vector2

sum(Vector2) # Why doesn't this work!? You have to remove missing data

sum(Vector2, na.rm=T) #now can sum everything without the NA

Res<-Test.1(Vector2)
Res # WHY DOESN'T THIS WORK!?

Test.2 <-function(x){
  test<- x * sum(x, na.rm = T)
}

Res2<-Test.2(Vector2)
Res2



###################################################
##### Pass multiple arguments to your function ####
###################################################

Vector2<-c(2,4,5,6)



Test.3 <- function(x,y) {
  test<- (mean(x) * mean(y)) /  sum(y)
} 

(Test.3(x=Vector1,y=Vector2))


###############################################################
###Useful build in functions that we might apply within our functions
#############################################################
# http://www.sr.bham.ac.uk/~ajrs/R/r-function_list.html

builtins() # List all built-in functions

?NA        # Help page on handling of missing data values
abs(x)     # The absolute value of "x"
append()   # Add elements to a vector
c(x)       # A generic function which combines its arguments 
cbind()    # Combine vectors by row/column (cf. "paste" in Unix)
diff(x)    # Returns suitably lagged and iterated differences
identical()  # Test if 2 objects are *exactly* equal
jitter()     # Add a small amount of noise to a numeric vector
length(x)    # Return no. of elements in vector x
paste(x)     # Concatenate vectors after converting to character
range(x)     # Returns the minimum and maximum of x
rep(1,5)     # Repeat the number 1 five times
rev(x)       # List the elements of "x" in reverse order
seq(1,10,0.4)  # Generate a sequence (1 -> 10, spaced by 0.4)
sign(x)        # Returns the signs of the elements of x
sort(x)        # Sort the vector x
order(x)       # list sorted element numbers of x
unique(x)      # Remove duplicate entries from vector


#quick and dirty plots
hist(x)    #histogram
plot(x,y)  #scatter


#######################################################################################
################### Todays Challanges #################################################
#######################################################################################

# I'm running and experiment to see if the type of music I play affects my stats
# students' exam performance. I randomly assign one group of 50 to listen to
# Kendrick Lamar, while the other group of 50 listens to Lil Pump. I've
# simulated the data below:

## 
set.seed(5) # run these lines all together so we get the same numbers
dat_KL<-rnorm(50, 92, 5) # 
dat_LP<-rnorm(50, 65, 4)

# 1. Write a function to take the pooled variance of these two conditions

pooled.variance <- function(x,y) {
 pooledSS <- sum((x-mean(x))^2)+sum(((y-mean(y))^2))
 pooledDF <- (length(x)-1)+(length(y)-1)
 pooledv <- pooledSS / pooledDF
 return(pooledv) 
}

Results.3<-pooled.variance(x=dat_KL, y=dat_LP)

Results.3

# 2. Write a function to give you a t-value using this pooled variance function

my.t<-function(x,y){
  t<-(mean(x)-mean(y))/sqrt((pooled.variance(x,y)/length(x))+(pooled.variance(x,y)/length(y)))
  return(t)
}

t.val<-my.t(dat_KL,dat_LP)
t.val

# 3. Is it significant?

# this will give you a p-value: 2*pt([your t here], [your df here], lower=FALSE)
2*pt(abs(t.val), 98, lower=FALSE)

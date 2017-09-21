## Started 13 Sep 2017 ##
## By Lizzie ##

## Some help from JD (14 Sep 2017) ##
## Cleaned up on 18 Sep 2017 ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/git/projects/misc/phenconstraints")

# Read in data:
dat<-read.csv("data/growingseason_doy.csv", header=TRUE)
dat2<-read.csv("data/growingseason_doy2.csv", header = TRUE)

## Hmmm, for now skip sifting through real data
# So, just use fake data

# Hypothesis 2 (interpheno)

# `real' data
prephase<- rnorm(25, 40, 10)
postphase<- prephase + rnorm(25, 40, 10)
interpheno<-postphase-prephase
plot(postphase ~ interpheno, ylab="later phenophase", xlab="interpheno phase")


# resampled (JD, resamples only one thing at a time) ...
for (i in 1:100){
     interpheno.fake<-sample(interpheno, 25, replace=TRUE)
     # create the postphase based on above
     postphase.fake <- prephase+interpheno.fake
     # plot(postphase.fake~interpheno.fake)
     abline(lm(postphase.fake~interpheno.fake), col="lightblue")
}
abline(lm(postphase ~ interpheno), lwd=2)
# alternative method (not shown) use normal distributions that match your data and draw from those.


# resampled (Lizzie did, perhaps not best, it resamples two things) ... 
for (i in c(1:100)){
     # randomly create a pre-phase
     prephase.fake <- sample(prephase, 25, replace=TRUE)
     # use real data for interpheno
     interpheno.fake <- sample(interpheno, 25, replace=TRUE)
     # create the postphase based on above
     postphase.fake <- prephase.fake+interpheno.fake
     # plot(postphase.fake~interpheno.fake)
     abline(lm(postphase.fake~interpheno.fake), col="gray")
}


##
## Hypothesis 1 (null is obtain the intercept from the model with fixed slope)

# some thought exercises on a slope of 1
x <- rnorm(25, 50, 10)
y <- x+20
plot(y~x)
lm(y~x)

# now the 'real' data
x <- rnorm(25, 50, 10)
y <- 20 + 2.5*x + rnorm(25, 5, 10)
plot(y~x, ylim=c(20,220))
abline(lm(y~x))

# add null and plot (this bit from JD attempt see https://www.zoology.ubc.ca/~schluter/R/fit-model/)
#set slope
b = 1
#fit model with specified slope (to estimate intercept)
z <- lm(y ~ 1 + offset(b*x))
#or
z<-lm(y - b*x ~ 1)

y.null<-x+z[[1]]
abline(lm(y.null~x), col="red")


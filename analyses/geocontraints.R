## Started 21 Sep 2017 ##
## By Lizzie ##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/git/projects/misc/phenconstraints")

# Read in data:
dat<-read.csv("phenomeans.csv", header=TRUE)



##
## Hypothesis 1 (slope of 1 is hypothesis 1)

# 7/10 sig or close ... 
# 1) LO vs BB
# 2) Flo vs BB
# 3) Flo vs LO
# 4) Fru vs BB
# 5) Fru vs LO
# 6) Fru vs Flo
# 7) Sen vs Fru

# test against slope of 1 ... 
b = 1

## try one
x <-dat$bb
y <-dat$lo

plot(y~x)
abline(lm(y~x), col="black")
abline(lm((x+(lm(y ~ 1 + offset(b*x)))[[1]])~x), col="deepskyblue")

# now build a little f(x)
plothyp1 <- function(x,y, name){
    plot(y~x, main=name)
    abline(lm(y~x), col="black")
    abline(lm((x+(lm(y ~ 1 + offset(b*x)))[[1]])~x), col="deepskyblue")
    }

# now apply it
par(mfrow=c(2,4))
plothyp1(dat$bb, dat$lo, "LO vs BB")
plothyp1(dat$fl, dat$bb, "Fl vs BB")
plothyp1(dat$fl, dat$lo, "Fl vs LO")
plothyp1(dat$fr, dat$bb, "Fr vs BB")
plothyp1(dat$fr, dat$lo, "Fr vs LO")
plothyp1(dat$fr, dat$fl, "Fr vs Fl")
plothyp1(dat$sen, dat$fr, "Sen vs Fr")


##
## Hypothesis 2 (interpheno predicts postphase)

# 4/10 sig or close ... 
# 1) Flo-LO vs Flo
# 2) Flo-LO vs Fr. 
# 3) Fru-Flo vs Fr. 
# 4) Fru-Flo vs Sen

# dive in and build a little f(x)
plothyp2 <- function(postphase,interpheno,prephase, extraphase,name){
    plot(postphase~interpheno, main=name)
    for (i in 1:100){
        interpheno.fake <- sample(interpheno, 25, replace=TRUE)
        postphase.fake <- prephase+interpheno.fake+extraphase
        abline(lm(postphase.fake~interpheno.fake), col="lightblue")
    }
    abline(lm(postphase~interpheno), col="black", lwd=2)
}



# now apply it
par(mfrow=c(2,2))
plothyp2(dat$fl, dat$lo_fl, dat$lo, rep(0, 25), "Fl vs LO_Fl")
plothyp2(dat$fr, dat$lo_fl, dat$lo, dat$fl, "Fr vs LO-Fl")
plothyp2(dat$fr, dat$fl_fr, dat$fl, rep(0, 25),"Fr vs Fr-Fl")
plothyp2(dat$sen, dat$fl_fr, dat$fr, rep(0, 25),"Sen vs Fr-Fl")

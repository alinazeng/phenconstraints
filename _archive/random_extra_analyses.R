###Test data to make hypotheses figures
#Hyp1: previous stage (flow) constraints later stage (fruit); duration is random
flowstdoy<-c(1,2,3,4,5)
#flowdur<-c(5,5,5,5,5)
flowdur<-rnorm(5,mean=5, sd=1)
flowendoy<-flowstdoy+flowdur
frstdoy<-flowstdoy+flowdur
quartz(height=3, width=5)
par(mfrow=c(1,2))
plot(flowstdoy,frstdoy)
abline(lm(frstdoy~flowstdoy))
plot(flowdur,frstdoy)
abline(lm(frstdoy~flowdur))
#Hyp2: Interphase time (flow) constraints later stage (fruit); flow stard time is constant
flowstdoy<-c(1,1,1,1,1)
flowdur<-c(5,6,7,8,9)
flowendoy<-flowstdoy+flowdur
frstdoy<-flowstdoy+flowdur
plot(flowdur,frstdoy)
abline(lm(frstdoy~flowdur))








#Look at Dan & Lizzie's data from growth chambers to see if it matches Sally's:
#FIRST, WITH CHILLING
d<-read.csv("data/leafoutdays_growthchamber.wlc1.csv", header=T)

cols2 <- colorRampPalette(brewer.pal(8,"Accent"))(28)

#First, later vs. earlier:
quartz(height=3, width=7)
par(mfrow=c(1,2))
plot(d$BB.WLC1.HF , d$LO.WLC1.HF, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="Harvard Forest")
abline(lm(d$LO.WLC1.HF~d$BB.WLC1.HF))
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.HF~d$BB.WLC1.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.HF~d$BB.WLC1.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

plot(d$BB.WLC1.SH , d$LO.WLC1.SH, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte")
abline(lm(d$LO.WLC1.SH~d$BB.WLC1.SH))
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.SH~d$BB.WLC1.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.SH~d$BB.WLC1.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

##Now , stages vs. interphenophase:
d$LO_BB.WLC1.HF<-d$LO.WLC1.HF-d$BB.WLC1.HF
d$LO_BB.WLC1.SH<-d$LO.WLC1.SH-d$BB.WLC1.SH

quartz(height=6, width=7)
par(mfrow=c(2,2))
plot(d$LO_BB.WLC1.HF,d$BB.WLC1.HF,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l",main="Harvard Forest- Chill only")
mtext(paste("r2=",round(summary((lm(d$BB.WLC1.HF~d$LO_BB.WLC1.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.WLC1.HF~d$LO_BB.WLC1.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.HF~d$LO_BB.HF))

plot(d$LO_BB.WLC1.SH, d$BB.WLC1.SH,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte- Chill only")
mtext(paste("r2=",round(summary((lm(d$BB.WLC1.SH~d$LO_BB.WLC1.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.WLC1.SH~d$LO_BB.WLC1.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.WLC1.SH~d$LO_BB.WLC1.SH))

plot( d$LO_BB.WLC1.HF,d$LO.WLC1.HF,ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.HF~d$LO_BB.WLC1.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.HF~d$LO_BB.WLC1.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$LO.WLC1.HF~d$LO_BB.WLC1.HF))

plot(d$LO_BB.WLC1.SH,d$LO.WLC1.SH, ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.SH~d$LO_BB.WLC1.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.SH~d$LO_BB.WLC1.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$LO.WLC1.SH~d$LO_BB.WLC1.SH), lty=1, col="black")

#NOW WITH ALL EXPERIMENTAL TREATMENTS:
d<-read.csv("data/leafoutdays_growthchamber.csv", header=T)

cols2 <- colorRampPalette(brewer.pal(8,"Accent"))(28)
#First, later vs. earlier:
quartz(height=3, width=7)
par(mfrow=c(1,2))
plot(d$BB.HF, d$LO.HF, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="Harvard Forest")
abline(lm(d$LO.HF~d$BB.HF))
mtext(paste("r2=",round(summary((lm(d$LO.HF~d$BB.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.HF~d$BB.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

plot(d$BB.SH , d$LO.SH, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte")
abline(lm(d$LO.SH~d$BB.SH))
mtext(paste("r2=",round(summary((lm(d$LO.SH~d$BB.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.SH~d$BB.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

##Now , stages vs. interphenophase:
d$LO_BB.HF<-d$LO.HF-d$BB.HF
d$LO_BB.SH<-d$LO.SH-d$BB.SH

quartz(height=6, width=7)
par(mfrow=c(2,2))
plot(d$LO_BB.HF,d$BB.HF,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l",main="Harvard Forest")
mtext(paste("r2=",round(summary((lm(d$BB.HF~d$LO_BB.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.HF~d$LO_BB.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.HF~d$LO_BB.HF))

plot(d$LO_BB.SH, d$BB.SH,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte")
mtext(paste("r2=",round(summary((lm(d$BB.SH~d$LO_BB.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.SH~d$LO_BB.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.SH~d$LO_BB.SH))

plot( d$LO_BB.HF,d$LO.HF,ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.HF~d$LO_BB.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.HF~d$LO_BB.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$LO.HF~d$LO_BB.HF))

plot(d$LO_BB.SH,d$LO.SH, ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.SH~d$LO_BB.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.SH~d$LO_BB.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
abline(lm(d$LO.SH~d$LO_BB.SH), lty=1, col="black")



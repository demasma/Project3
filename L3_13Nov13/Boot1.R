#***************************************************************
#
#  	  Transplant Center 
#	  Bootstrapping 1
#
#***************************************************************


#***************************************************************
#
#  Read in the data
#
#***************************************************************

#Read data
r11xplant <- read.table("R11xplant.csv", sep = ",", header = T)

r11donor<-read.table("R11donor.csv", sep = ",", header = T)

uva <- read.table("UVAxplant.csv", sep = ",", header = T)

duke <- read.table("Dukexplant.csv", sep = ",", header = T)

mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

unc <- read.table("UNCxplant.csv", sep = ",", header = T)

setwd("~/Box Documents/")

#Source the bootstrapping functions
library(boot) #If you don't have this library, install it by: install.packages('boot')

source("TSbootfunctions.R")

#***************************************************************
#
# Bootstrap the differences
#
#***************************************************************

# UVa-MCV Differences in Kidney Transplants 
uva.kidney<-uva.xplant$Kidney
mcv.kidney<-mcv.xplant$Kidney
duke.kidney<-duke.xplant$Kidney
unc.kidney<-unc.xplant$Kidney

uva.liver<-uva.xplant$Liver
mcv.liver<-mcv.xplant$Liver
duke.liver<-duke.xplant$Liver
unc.liver<-unc.xplant$Liver

sd(uva.kidney)
sd(mcv.kidney)

##compute the difference between uva kidney transplants and mcv kidney 
## transplants from 1988 to 2011
mcv.liv.diff <- ts(uva.liver-mcv.liver,1988,2012)
unc.liv.diff <- ts(uva.liver-unc.liver,1988,2012)
duke.liv.diff <- ts(uva.liver-duke.liver,1988,2012)
liv.diff <- data.frame(mcv=mcv.liv.diff, unc=unc.liv.diff, duke=duke.liv.diff)

mcv.kid.diff <- ts(uva.kidney-mcv.kidney,1988,2012)
unc.kid.diff <- ts(uva.kidney-unc.kidney,1988,2012)
duke.kid.diff <- ts(uva.kidney-duke.kidney,1988,2012)
kid.diff <- data.frame(mcv=mcv.kid.diff, unc=unc.kid.diff, duke=duke.kid.diff)

png("./project/figures/diff_liver.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(3,1), ps=20)
ts.plot(mcv.liv.diff,ylab='UVa-MCV',
        main = "Difference in Number of Liver Transplants, UVA-MCV")
abline(0,0,lty=3)
ts.plot(unc.liv.diff,ylab='UVa-UNC',
        main = "Difference in Number of Liver Transplants, UVA-UNC")
abline(0,0,lty=3)
ts.plot(duke.liv.diff,ylab='UVa-Duke',
        main = "Difference in Number of Liver Transplants, UVA-Duke")
abline(0,0,lty=3)
par(mfrow=c(1,1), ps=20)
dev.off()

png("./project/figures/diff_kidney.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(3,1), ps=20)

ts.plot(mcv.kid.diff,ylab='UVa-MCV',
        main = "Difference in Number of Kidney Transplants, UVA-MCV")
abline(0,0,lty=3)
ts.plot(unc.kid.diff,ylab='UVa-UNC',
        main = "Difference in Number of Kidney Transplants, UVA-UNC")
abline(0,0,lty=3)
ts.plot(duke.kid.diff,ylab='UVa-Duke',
        main = "Difference in Number of Kidney Transplants, UVA-Duke")
abline(0,0,lty=3)
par(mfrow=c(1,1), ps=20)
dev.off()

plot(kid.diff)

# perform a paired t-test

t.test(uva.liver, mcv.liver,paired=T)
t.test(uva.liver, unc.liver,paired=T)
t.test(uva.liver, duke.liver,paired=T)

t.test(uva.kidney, mcv.kidney,paired=T)
t.test(uva.kidney, duke.kidney,paired=T)
t.test(uva.kidney, unc.kidney,paired=T)

##what are the results?

# plot the differences:
ts.plot(kid.diff,ylab='UVa-MCV',
        main = "Difference in Number of Transplants, UVA-MCV")

#Bootstrap the differences of the means 
bs.mean<-function(x,i)
{
  return(mean(x[i]))
}


##Bootstrap mean differences - syntax: boot(data= , statistic= , R= ) where R = number of replications, statistic=bs.mean
bs.kid.diff<-boot(kid.diff,bs.mean,R=2000)

bs.mcv.liv.diff<-boot(mcv.liv.diff,bs.mean,R=50000)
bs.unc.liv.diff<-boot(unc.liv.diff,bs.mean,R=50000)
bs.duke.liv.diff<-boot(duke.liv.diff,bs.mean,R=50000)

bs.mcv.liv.diff.100k<-boot(mcv.liv.diff,bs.mean,R=100000)
bs.unc.liv.diff.100k<-boot(unc.liv.diff,bs.mean,R=100000)
bs.duke.liv.diff.100k<-boot(duke.liv.diff,bs.mean,R=100000)
bs.mcv.liv.diff
bs.unc.liv.diff
bs.duke.liv.diff

bs.mcv.liv.diff.100k
bs.unc.liv.diff.100k
bs.duke.liv.diff.100k

bs.mcv.kid.diff<-boot(mcv.kid.diff,bs.mean,R=50000)
bs.unc.kid.diff<-boot(unc.kid.diff,bs.mean,R=50000)
bs.duke.kid.diff<-boot(duke.kid.diff,bs.mean,R=50000)

bs.mcv.kid.diff.100k<-boot(mcv.kid.diff,bs.mean,R=100000)
bs.unc.kid.diff.100k<-boot(unc.kid.diff,bs.mean,R=100000)
bs.duke.kid.diff.100k<-boot(duke.kid.diff,bs.mean,R=100000)

bs.mcv.kid.diff
bs.unc.kid.diff
bs.duke.kid.diff

bs.mcv.kid.diff.100k
bs.unc.kid.diff.100k
bs.duke.kid.diff.100k

kid.diff
##view the results 
bs.kid.diff

##plot the bootstrap results.  what do you observe?
par(mfrow=c(1,1), ps=20)
png("./project/figures/boot_mcv_liver.png", width=700, height=900)#, pointsize=30)
plot(bs.mcv.liv.diff,index=1, main="Bootstrapped Mean Difference UVa and MCV")
dev.off()
png("./project/figures/boot_unc_liver.png", width=700, height=900)#, pointsize=30)
plot(bs.unc.liv.diff,index=1)
dev.off()
png("./project/figures/boot_duke_liver.png", width=700, height=900)#, pointsize=30)
plot(bs.duke.liv.diff,index=1)
dev.off()
par(mfrow=c(1,1), ps=20)

par(mfrow=c(1,1), ps=70)
png("./project/figures/boot_mcv_kidney.png", width=700, height=900)#, pointsize=30)
plot(bs.mcv.kid.diff,index=1)
dev.off()
png("./project/figures/boot_unc_kidney.png", width=700, height=900)#, pointsize=30)
plot(bs.unc.kid.diff,index=1)
dev.off()
png("./project/figures/boot_duke_kidney.png", width=700, height=900)#, pointsize=30)
plot(bs.duke.kid.diff,index=1)
plot(bs.duke.kid.diff.100k,index=1)
dev.off()
par(mfrow=c(1,1), ps=20)

##find the confidence intervals for bs.kid.diff using bca and percentile
boot.ci(bs.mcv.liv.diff,0.95,type=c('bca','perc'))
boot.ci(bs.unc.liv.diff,0.95,type=c('bca','perc'))
boot.ci(bs.duke.liv.diff,0.95,type=c('bca','perc'))

boot.ci(bs.mcv.kid.diff,0.95,type=c('bca','perc'))
boot.ci(bs.unc.kid.diff,0.95,type=c('bca','perc'))
boot.ci(bs.duke.kid.diff,0.95,type=c('bca','perc'))
#boot.ci(bs.kid.diff,0.95,type=c('bca','perc'))


boot.ci(bs.mcv.liv.diff.100k,0.95,type=c('bca','perc'))
boot.ci(bs.unc.liv.diff.100k,0.95,type=c('bca','perc'))
boot.ci(bs.duke.liv.diff.100k,0.95,type=c('bca','perc'))

boot.ci(bs.mcv.kid.diff.100k,0.95,type=c('bca','perc'))
boot.ci(bs.unc.kid.diff.100k,0.95,type=c('bca','perc'))
boot.ci(bs.duke.kid.diff.100k,0.95,type=c('bca','perc'))

##Repeat the same process for liver transplants between UVA & Duke
##interperet results
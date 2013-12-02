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

setwd(rcodepath)

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
png("./project/figures/diff_liver.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(3,1), ps=20)
mcv.liv.diff <- ts(uva.liver-mcv.liver,1988,2012)
unc.liv.diff <- ts(uva.liver-unc.liver,1988,2012)
duke.liv.diff <- ts(uva.liver-duke.liver,1988,2012)
liv.diff <- data.frame(mcv=mcv.liv.diff, unc=unc.liv.diff, duke=duke.liv.diff)
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
mcv.kid.diff <- ts(uva.kidney-mcv.kidney,1988,2012)
unc.kid.diff <- ts(uva.kidney-unc.kidney,1988,2012)
duke.kid.diff <- ts(uva.kidney-unc.kidney,1988,2012)
kid.diff <- data.frame(mcv=mcv.kid.diff, unc=unc.kid.diff, duke=duke.kid.diff)

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
t.test(uva.kidney, mcv.kidney,paired=T)
t.test(uva.kidney, mcv.kidney,paired=F)

##what are the results?

# plot the differences:
ts.plot(kid.diff,ylab='UVa-MCV',
        main = "Difference in Number of Transplants, UVA-MCV")

#Bootstrap the differences of the means 
bs.mean<-function(x,i)
{
  return(mean(x[i]))
}

bs.mean(kid.diff)

##Bootstrap mean differences - syntax: boot(data= , statistic= , R= ) where R = number of replications, statistic=bs.mean
bs.kid.diff<-boot(kid.diff,bs.mean,R=2000)
bs.kid.diff<-boot(kid.diff,bs.mean,R=50000)
kid.diff
##view the results 
bs.kid.diff

##plot the bootstrap results.  what do you observe?
plot(bs.kid.diff,index=1)

##find the confidence intervals for bs.kid.diff using bca and percentile
boot.ci(bs.kid.diff,0.95,type=c('bca','perc'))


##Repeat the same process for liver transplants between UVA & Duke
##interperet results
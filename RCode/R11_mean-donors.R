#Bootstrap the differences of the means 
bs.mean<-function(x,i)
{
  return(mean(x[i]))
}

mean(r11.donor$Liver)
mean(r11.donor$Kidney)
r11.donor.k.mean.boot <- boot(r11.donor$Kidney, bs.mean, R=2000)
r11.donor.l.mean.boot <- boot(r11.donor$Liver, bs.mean, R=2000)

mean(r11.xplant$Liver)
mean(r11.xplant$Kidney)
(r11.xplant.l.mean.boot <- boot(r11.xplant$Liver, bs.mean, R=2000))
(r11.xplant.k.mean.boot <- boot(r11.xplant$Kidney, bs.mean, R=2000))

r11.xplant.l.mean.boot
boot.ci(r11.xplant.k.mean.boot,0.95,type=c('bca','perc'))


png("~/boot_r11.donor.png", width=700, height=900)#, pointsize=30)
par(mfrow=c(2,1), ps=20)
plot(r11.donor.k.mean.boot,index=1, main="Bootstrapped Mean Difference UVa and MCV")
plot(r11.donor.l.mean.boot,index=1, main="Bootstrapped Mean Difference UVa and MCV")
dev.off()
par(mfrow=c(1,1), ps=20)

boot.ci(bs.mcv.liv.diff,0.95,type=c('bca','perc'))
#***************************************************************
#
#     Transplant Center 
#			Observational Analysis
#
#***************************************************************

#*************************************************************
#
# Load the transplant data
# R11xplant.csv, R11donor.csv, UVAxplant.csv, Dukexplant.csv,
# MCVxplant.csv, & UNCxplant.csv (headers, "," delimiter)
#
#**************************************************************




##  Summarize the data?  What are we look at?
summary(r11.xplant)
summary(r11.donor)
xplant.past <- xplant[which(xplant$Year != "2013"), ]
nor11.xplant <- xplant.past[which(xplant.past$Location != "r11"), ]
tail(nor11.xplant)
##  How many years of transplant data are there?
range(r11.xplant$Year)
library(lattice)


png("./project/figures/bw_liver.png", width=900, height=900)
par(ps=40)
bwplot(Location ~ Liver, data=nor11.xplant, 
       xlab="Number of People",
       par.strip.text = list(cex = 1.75),
       main="Box Plots for Number of Liver Transplants for Select Region 11 Centers \n (1988-2012)")
dev.off()
png("./project/figures/bw_kidney.png", width=900, height=900, pointsize=40)
bwplot(Location ~ Kidney, data=nor11.xplant, 
       xlab="Number of People", 
       main="Box Plots for Number of  Transplants for Select Region 11 Centers \n (1988-2012)")
dev.off()
png("./project/figures/density_liver.png", width=900, height=900, pointsize=40)
densityplot(~Liver, groups=Location, data=nor11.xplant,
            main="Density Plots for Number of Liver Transplants for Select Region 11 Centers \n (1988-2012)",
            auto.key=list(space="right"))
dev.off()
png("./project/figures/density_kidney.png", width=900, height=900, pointsize=40)
densityplot(~Kidney, groups=Location, data=nor11.xplant,
            main="Density Plots for Number of Kidney Transplants for Select Region 11 Centers \n (1988-2012)",
            auto.key=list(space="right"))
dev.off()
##  How many organs?


#***************************************************************
#
# Soure the scatter plot matrix code and transplant plotting code
# SPM_Panel.R and Transplant.plots.R
#
#***************************************************************




#***************************************************************
#
# Scatter plot matrix 
#
#***************************************************************

##  Create a scatter plot matrix for liver transplants using UVA, Duke, MCV, UNC, & Region 11 donors
colnames(donor)
colnames(xplant)

donor$Liver_DD + donor$Liver_LD - donor$Liver
xplant$Liver_DD + xplant$Liver_LD - xplant$Liver

uva.xplant.liver <- uva.xplant[-26, ]$Liver
mcv.xplant.liver <- mcv.xplant[-26, ]$Liver
duke.xplant.liver <- duke.xplant[-26, ]$Liver
unc.xplant.liver <- unc.xplant[-26, ]$Liver
r11.xplant.liver <- r11.xplant[-26, ]$Liver

xplant.liver <- data.frame(uva=uva.xplant.liver, mcv=mcv.xplant.liver, 
                           duke=duke.xplant.liver, unc=unc.xplant.liver,
                           r11=r11.xplant.liver)
png("./project/figures/spm_liver.png", width=900, height=900, pointsize=30)
par(mfrow=c(1,1), ps=20)
uva.pairs(xplant.liver)
dev.off()

uva.xplant.kidney <- uva.xplant[-26, ]$Kidney
mcv.xplant.kidney <- mcv.xplant[-26, ]$Kidney
duke.xplant.kidney <- duke.xplant[-26, ]$Kidney
unc.xplant.kidney <- unc.xplant[-26, ]$Kidney
r11.xplant.kidney <- r11.xplant[-26, ]$Kidney

xplant.kidney <- data.frame(uva=uva.xplant.kidney, mcv=mcv.xplant.kidney, 
                           duke=duke.xplant.kidney, unc=unc.xplant.kidney,
                           r11=r11.xplant.kidney)
png("./project/figures/spm_kidney.png", width=900, height=900, pointsize=30)
par(mfrow=c(1,1), ps=20)
uva.pairs(xplant.kidney)
dev.off()


##  What do you notice about the distributions of data for liver transplants? Are any symmetric?


##  Which center has the highest correlation with region 11 liver donors?


##  Create a scatter plot matrix for pancreas transplants


##  What do you notice about the distributions of data for pancreas transplants? Are any symmetric?


##  Which center has the highest correlation with region 11 pancreas donors?


#***************************************************************
#
#  donortype.plot
#
#***************************************************************

uva.xplant <- uva.xplant[-26, c("Year", "Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")]
mcv.xplant <- mcv.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")  ]
duke.xplant <- duke.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")]
unc.xplant <- unc.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]
r11.xplant <- r11.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]

r11.donor <- r11.donor[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]

png("./project/figures/spm_liver.png", width=900, height=900, pointsize=30)
par(mfrow=c(1,1), ps=20)
dev.off()

# uva.xplant.kidney <- uva.xplant[-26, c("Kidney", "Kidney_DD", "Kidney_LD")]
# mcv.xplant.kidney <- mcv.xplant[-26, c("Kidney", "Kidney_DD", "Kidney_LD")]
# duke.xplant.kidney <- duke.xplant[-26, c("Kidney", "Kidney_DD", "Kidney_LD")]
# unc.xplant.kidney <- unc.xplant[-26, c("Kidney", "Kidney_DD", "Kidney_LD")]
# r11.xplant.kidney <- r11.xplant[-26, c("Kidney", "Kidney_DD", "Kidney_LD")]

# Note in all the plots that follow we should remove the 26th observation
# (2013) since the data were not complete for that year.

# Lung transplants-DD means deceased donor; LD means living donor
r11.donor$Kidney_LD - r11.xplant$Kidney_LD
png("./project/figures/donor_kidney.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(1,1), ps=20)
donortype.plot(cbind(r11.xplant$Kidney_DD, 
                     r11.xplant$Kidney_LD, 
                     r11.donor$Kidney_DD,
                     r11.donor$Kidney_LD), 
               title = "Kidney")
dev.off()

png("./project/figures/donor_liver.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(1,1), ps=20)
donortype.plot(cbind(r11.xplant$Liver_DD, 
                     r11.xplant$Liver_LD, 
                     r11.donor$Liver_DD,
                     r11.donor$Liver_LD), 
               title = "Liver")
dev.off()
donortype.plot(cbind(r11.xplant$Lung_DD[-26], r11.xplant$Lung_LD[-26], r11.donor$Lung_DD[-26],r11.donor$Lung_LD[-26]), title = "Lung")


##  Create a donortype.plot for heart transplants in region 11



##What do you observe in both the Lung and the Heart plots? 


#***************************************************************
#
#  region.plot
#
#***************************************************************

#  region.plot for Heart transplants
png("./project/figures/region_liver.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(1,1), ps=20)
region.plot(cbind(r11.xplant$Liver, r11.donor$Liver, 
                  uva.xplant$Liver, unc.xplant$Liver, 
                  mcv.xplant$Liver, duke.xplant$Liver), 
            title = "Liver")
dev.off()
##  Create a region.plot for Liver transplants
png("./project/figures/region_kidney.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(1,1), ps=20)
region.plot(cbind(r11.xplant$Kidney, r11.donor$Kidney, 
                  uva.xplant$Kidney, unc.xplant$Kidney, 
                  mcv.xplant$Kidney, duke.xplant$Kidney), 
            title = "Kidney")
dev.off()

##  Which center has more heart transplants?  liver transplants?


##  What else do you oberve?


#***************************************************************
#
#  center.plot
#
#***************************************************************

#center.plot for Pancreas transplants
png("./project/figures/center_liver.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(1,1), ps=20)
center.plot(cbind( uva.xplant$Liver, 
                   unc.xplant$Liver, 
                   mcv.xplant$Liver, 
                   duke.xplant$Liver), 
            title = "Liver")
dev.off()


png("./project/figures/center_kidney.png", width=900, height=900)#, pointsize=30)
par(mfrow=c(1,1), ps=20)
center.plot(cbind( uva.xplant$Kidney, 
                   unc.xplant$Kidney, 
                   mcv.xplant$Kidney, 
                   duke.xplant$Kidney), 
            title = "Kidney")
dev.off()

##  Create center.plot for Heart transplants


##  Which center does the most pancreas transplants over the last ten years?  Heart transplants?


##  What else do you oberve?




#=======================================================================
# This is the number of years in our data frame
#=======================================================================
#
len.xplant <- 26

#=======================================================================
# Add location to Transplant Data frames
#=======================================================================
#
r11.xplant$Location <- rep("r11", len.xplant)
uva.xplant$Location <- rep("uva", len.xplant)
duke.xplant$Location <- rep("duke", len.xplant)
mcv.xplant$Location <- rep("mcv", len.xplant)
unc.xplant$Location <- rep("unc", len.xplant)
us.xplant$Location <- rep("us", len.xplant)

#=======================================================================
# Add # of R11 donors to Transplant Data frames for Liver and Kidney
#=======================================================================
#
#uva.xplant$r11.donor.Liver <- r11.donor$Liver 
uva.xplant$LiverDonorR11 <- r11.donor$Liver 
#mcv.xplant$r11.donor.Liver <- r11.donor$Liver 
mcv.xplant$LiverDonorR11 <- r11.donor$Liver 
#duke.xplant$r11.donor.Liver <- r11.donor$Liver 
duke.xplant$LiverDonorR11 <- r11.donor$Liver 

uva.xplant$r11.donor.Kidney <- r11.donor$Kidney
mcv.xplant$r11.donor.Kidney <- r11.donor$Kidney
duke.xplant$r11.donor.Kidney <- r11.donor$Kidney 


#=======================================================================
# Merge Transplant Data frames for Liver and Kidney (for UVa, MCV and
#   Duke
#=======================================================================
#
# xplant <- merge(r11.xplant, uva.xplant, all.x=TRUE, all.y=TRUE)
# xplant <- merge(xplant, duke.xplant, all.x=TRUE, all.y=TRUE)
# xplant <- merge(xplant, mcv.xplant, all.x=TRUE, all.y=TRUE)
# xplant <- merge(xplant, unc.xplant, all.x=TRUE, all.y=TRUE)
# xplant <- merge(xplant, us.xplant, all.x=TRUE, all.y=TRUE)

xplant <- merge(mcv.xplant, uva.xplant, all.x=TRUE, all.y=TRUE)
xplant <- merge(xplant, duke.xplant, all.x=TRUE, all.y=TRUE)

#=======================================================================
# Make Location a factor and order it with UVa first
#=======================================================================
#
xplant$Location <- factor(xplant$Location, levels=c("uva", "mcv", "duke"), 
                          labels=c("UVa", "MCV", "Duke"),
                          ordered=TRUE)


#=======================================================================
# Create a new data frame with just the Liver, Kidney, Location and 
#   Year information
#=======================================================================
#
Xplant <- xplant[,c("Year", "Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD", "LiverDonorR11", "r11.donor.Kidney", "Location")]


#=======================================================================
# Add Locations to Ethnic data frames
#=======================================================================
#
uva.eth$Location <- rep("uva", len.xplant)
duke.eth$Location <- rep("duke", len.xplant)
mcv.eth$Location <- rep("mcv", len.xplant)
r11.eth$Location <- rep("r11", len.xplant)

#=======================================================================
# Merge the ethnic data frames into one data frame
#=======================================================================
#
eth <- merge(uva.eth, duke.eth, all.x=TRUE, all.y=TRUE)
eth <- merge(eth, mcv.eth, all.x=TRUE, all.y=TRUE)
eth <- merge(eth, r11.eth, all.x=TRUE, all.y=TRUE)

eth$Location <- factor(eth$Location)

#=======================================================================
# Extract only the important fields and records for each center 
#   individually:
#    - no 2013 (i.e. [-26, ])
#    - only liver and kidney donor information
#=======================================================================
#
uva.xplant <- uva.xplant[-26, c("Year", "Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")]
mcv.xplant <- mcv.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")  ]
duke.xplant <- duke.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")]
unc.xplant <- unc.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]
r11.xplant <- r11.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]

r11.donor <- r11.donor[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]

# Form data sets for the kidney transplants
colnames(uva.eth)
uva.l.eth <- uva.eth[-26, which(substr(colnames(uva.eth),1,5) == "Liver")]
uva.k.eth <- subdata("Kidney", uva.eth)
mcv.l.eth <- mcv.eth[-26, which(substr(colnames(mcv.eth),1,5) == "Liver")]
mcv.k.eth <- subdata("Kidney", mcv.eth)
duke.l.eth <- duke.eth[-26, which(substr(colnames(duke.eth),1,5) == "Liver")]
duke.k.eth <- subdata("Kidney", duke.eth)
r11.l.eth <- r11.eth[-26, which(substr(colnames(r11.eth),1,5) == "Liver")]
r11.k.eth <- subdata("Kidney", r11.eth)


#=======================================================================
# Combine counts for non-white people into a single field
#=======================================================================
#
# Remove year 2013 and combine all ethnic groups other than white into one category
# UVA
uva.l.e <- data.frame(uva.l.eth[-26, "Liver.W"], Liver.O = apply(uva.l.eth[-26,which(colnames(uva.l.eth) != "Liver.W")], 1, sum))
uva.l.e <- data.frame(uva.l.eth$Liver.W, Liver.O = apply(uva.l.eth[,which(colnames(uva.l.eth) != "Liver.W")], 1, sum))
uva.k.e <- data.frame(uva.k.eth[-26, "Kidney.W"], Kidney.O = apply(uva.k.eth[-26,which(colnames(uva.k.eth) != "Kidney.W")], 1, sum))

# MCV
mcv.l.e <- data.frame(mcv.l.eth[-26, "Liver.W"], Liver.O = apply(mcv.l.eth[-26,which(colnames(mcv.l.eth) != "Liver.W")], 1, sum))
mcv.k.e <- data.frame(mcv.k.eth[-26, "Kidney.W"], Kidney.O = apply(mcv.k.eth[-26,which(colnames(mcv.k.eth) != "Kidney.W")], 1, sum))

# Duke
duke.l.e <- data.frame(duke.l.eth[-26, "Liver.W"], Liver.O = apply(duke.l.eth[-26,which(colnames(duke.l.eth) != "Liver.W")], 1, sum))
duke.k.e <- data.frame(duke.k.eth[-26, "Kidney.W"], Kidney.O = apply(duke.k.eth[-26,which(colnames(duke.k.eth) != "Kidney.W")], 1, sum))

# R11
r11.l.e <- data.frame(r11.l.eth[-26, "Liver.W"], Liver.O = apply(r11.l.eth[-26,which(colnames(r11.l.eth) != "Liver.W")], 1, sum))
r11.k.e <- data.frame(r11.k.eth[-26, "Kidney.W"], Kidney.O = apply(r11.k.eth[-26,which(colnames(r11.k.eth) != "Kidney.W")], 1, sum))

#=======================================================================
# Set the working directory (mostly so figures don't end up in stupid
#  places
#=======================================================================
#
setwd("~/Box Documents/project/figures/")





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


#=======================================================================
# Set the working directory (mostly so figures don't end up in stupid
#  places
#=======================================================================
#
setwd("~/Box Documents/project/figures/")



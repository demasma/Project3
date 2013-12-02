# setwd("~/Google Drive/UVaGrad/6021/RCode")
# Directory <- dir()
# r.code.files <- Directory[grepl("R$",Directory)]
# files.of.interest <- c("")
# r.code.files <- r.code.files[!grepl("Boot1.R",r.code.files)]
# r.code.files <- r.code.files[!grepl("Boot2.R",r.code.files)]
# r.code.files
# r.code.files[1]
# for (i in 1:length(r.code.files)) {
#   print(r.code.files[i])
#   source(r.code.files[i])
# }

setwd("~/Projects")
#setwd("~/Documents/UVaGrad/Fall2013/SYS6021/")
setwd("./Project3/TransplantData")
r11.donor<-read.table("R11donor.csv", sep = ",", header = T)
us.donor<-read.table("USdonor.csv", sep = ",", header = T)

#r11.donor$Location <- rep("r11", len.xplant)
#us.donor$Location <- rep("us", len.xplant)

#donor <- merge(r11.donor, us.donor, all.x=TRUE, all.y=TRUE)
#donor$Location <- factor(donor$Location)

len.xplant <- 26
r11.xplant <- read.table("R11xplant.csv", sep = ",", header = T)
uva.xplant <- read.table("UVAxplant.csv", sep = ",", header = T)
duke.xplant <- read.table("Dukexplant.csv", sep = ",", header = T)
mcv.xplant <- read.table("MCVxplant.csv", sep = ",", header = T)
unc.xplant <- read.table("UNCxplant.csv", sep = ",", header = T)
us.xplant <- read.table("UStransplant.csv", sep = ",", header = T)

r11.xplant$Location <- rep("r11", len.xplant)
uva.xplant$Location <- rep("uva", len.xplant)
duke.xplant$Location <- rep("duke", len.xplant)
mcv.xplant$Location <- rep("mcv", len.xplant)
unc.xplant$Location <- rep("unc", len.xplant)
us.xplant$Location <- rep("us", len.xplant)

xplant <- merge(r11.xplant, uva.xplant, all.x=TRUE, all.y=TRUE)
xplant <- merge(xplant, duke.xplant, all.x=TRUE, all.y=TRUE)
xplant <- merge(xplant, mcv.xplant, all.x=TRUE, all.y=TRUE)
xplant <- merge(xplant, unc.xplant, all.x=TRUE, all.y=TRUE)
#xplant <- merge(xplant, us.xplant, all.x=TRUE, all.y=TRUE)

xplant$Location <- factor(xplant$Location)

duke.eth <- read.table('DukeEthnic.csv', sep = ',', header = T)
mcv.eth <- read.table('MCVethnic.csv', sep = ',', header = T)
r11.eth <- read.table('R11ethnic.csv', sep = ',', header = T)
uva.eth <- read.table('UVAethnic.csv', sep = ',', header = T)

uva.eth$Location <- rep("uva", len.xplant)
duke.eth$Location <- rep("duke", len.xplant)
mcv.eth$Location <- rep("mcv", len.xplant)
r11.eth$Location <- rep("r11", len.xplant)

eth <- merge(uva.eth, duke.eth, all.x=TRUE, all.y=TRUE)
eth <- merge(eth, mcv.eth, all.x=TRUE, all.y=TRUE)
eth <- merge(eth, r11.eth, all.x=TRUE, all.y=TRUE)

eth$Location <- factor(eth$Location)

#setwd("~/Google Drive/UVaGrad/6021/RCode/")
setwd("~/Projects")
#setwd("~/Documents/UVaGrad/Fall2013/SYS6021/")
setwd("./RCode/")
# source("TSbootfunctions.R")
# source("SPM_Panel.R")
# source("Transplant.plots.R")
# source("DemographicFunctions.R")
# source("DemographicFunctions-1.R")
r.code.files <- c("TSbootfunctions.R", "SPM_Panel-1.R", "Transplant.plots.R",
                  "DemographicFunctions.R", "DemographicFunctions-1.R")
for (i in 1:length(r.code.files)) {
  source(r.code.files[i])
  }

setwd("~/Projects")
#setwd("~/Documents/UVaGrad/Fall2013/SYS6021/")
setwd("./Project3/")

uva.xplant <- uva.xplant[-26, c("Year", "Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")]
mcv.xplant <- mcv.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")  ]
duke.xplant <- duke.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD")]
unc.xplant <- unc.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]
r11.xplant <- r11.xplant[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]

r11.donor <- r11.donor[-26, c("Year","Liver", "Liver_DD", "Liver_LD","Kidney", "Kidney_DD", "Kidney_LD") ]
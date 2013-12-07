
#setwd("~/Projects")
setwd("~/Documents/UVaGrad/Fall2013/SYS6021/")
setwd("./Project3/TransplantData/")

r11.donor<-read.table("R11donor.csv", sep = ",", header = T)
us.donor<-read.table("USdonor.csv", sep = ",", header = T)

len.xplant <- 26
r11.xplant <- read.table("R11xplant.csv", sep = ",", header = T)
uva.xplant <- read.table("UVAxplant.csv", sep = ",", header = T)
duke.xplant <- read.table("Dukexplant.csv", sep = ",", header = T)
mcv.xplant <- read.table("MCVxplant.csv", sep = ",", header = T)
unc.xplant <- read.table("UNCxplant.csv", sep = ",", header = T)
us.xplant <- read.table("UStransplant.csv", sep = ",", header = T)

duke.eth <- read.table('DukeEthnic.csv', sep = ',', header = T)
mcv.eth <- read.table('MCVethnic.csv', sep = ',', header = T)
r11.eth <- read.table('R11ethnic.csv', sep = ',', header = T)
uva.eth <- read.table('UVAethnic.csv', sep = ',', header = T)

#setwd("~/Projects")
setwd("~/Documents/UVaGrad/Fall2013/SYS6021/")
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

#setwd("~/Projects")
setwd("~/Documents/UVaGrad/Fall2013/SYS6021/")
setwd("./Project3/")

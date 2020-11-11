
eclipse <- "C:/Users/vanessa.burton.sb/Documents/eclipse-workspace/CRAFTY_OPM/data/csv/"

TestRegion <- read.csv(paste0(eclipse,"TestRegion.csv"))
Capitals <- read.csv(paste0(eclipse,"Capitals.csv"))
Services <- read.csv(paste0(eclipse,"Services.csv"))
#LandUses <- read.csv(paste0(eclipse,"LandUses.csv"))
Demand <- read.csv(paste0(eclipse,"Demand.csv"))

mgmt.high <- read.csv(paste0(eclipse,"mgmt_highInt.csv"))
mgmt.med <- read.csv(paste0(eclipse,"mgmt_medInt.csv"))
mgmt.low <- read.csv(paste0(eclipse,"mgmt_lowInt.csv"))
no.mgmt.unable <- read.csv(paste0(eclipse,"no_mgmt_unable.csv"))
no.mgmt.no.opm <- read.csv(paste0(eclipse,"no_mgmt_NOPM.csv"))

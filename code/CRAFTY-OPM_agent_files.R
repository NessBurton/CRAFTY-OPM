
# date: 05/11/2020
# author: VB
# purpose: create csv files for each OPM management agent

library(tidyverse)

wd <- "~/CRAFTY-opm" # sandbox VM

agentFilepath <- paste0(wd,"/data-processed/for-CRAFTY/")

# define Production levels for Services (0-1, low-high)
# and sensitivity of Production to each capital (0-1, low-high)

# no management (no OPM)
Service <- c("biodiversity","recreation","management")
Production <- c(1,1,0) # if no OPM, assume maximum amount of Services can be produced (excl. management)
OPMpresence <- c(NA,NA,NA) # no reliance on opm presence
riskPerc <- c(NA,NA,NA) # no reliance on risk perception
budget <- c(NA,NA,NA) # no reliance on budget
knowledge <- c(NA,NA,NA) # no reliance on knowledge
nature <- c(1,NA,NA) # Production of biodiversity fully reliant on nature capital level, 1:1
access <- c(NA,1,NA) # Production of recreation fully reliant on access capital level, 1:1

no.mgmt.no.opm <- tibble(Service,Production,OPMpresence,riskPerc,budget,knowledge,nature,access)
write.csv(no.mgmt.no.opm, paste0(agentFilepath,"no_mgmt_NOPM.csv"), row.names=F)

# no management (unable)
Service <- c("biodiversity","recreation","management")
Production <- c(0.4,0.4,0) # if OPM present but no management, Service provision compromised
OPMpresence <- c(1,1,1) # should only appear when OPM is present 
riskPerc <- c(NA,NA,NA) # no reliance
budget <- c(NA,NA,NA) # no reliance 
knowledge <- c(NA,NA,NA) # no reliance
nature <- c(1,NA,NA) # Production of biodiversity dependent on nature capital
access <- c(NA,1,NA) # Production of recreation dependent on access capital

no.mgmt.unable <- tibble(Service,Production,OPMpresence,riskPerc,budget,knowledge,nature,access)
write.csv(no.mgmt.unable, paste0(agentFilepath,"no_mgmt_unable.csv"), row.names=F)

# manage (low intensity)
Service <- c("biodiversity","recreation","management")
Production <- c(1,0.6,1) # focus is on biodiversity, so provides maximum amount - recreation compromised by reduced access
OPMpresence <- c(1,1,1) # should only appear when OPM is present 
riskPerc <- c(NA,NA,0.2) # lower risk perceptions, skeptical about human health impacts, worried about biodiversity. does lower sensitivity to risk capital achieve this?
budget <- c(NA,NA,0.2) # some budget required
knowledge <- c(NA,NA,0.8) # management requires knowledge
nature <- c(1,NA,NA) # Production of biodiversity dependent on nature capital
access <- c(NA,1,NA) # Production of recreation dependent on access capital

mgmt.low <- tibble(Service,Production,OPMpresence,riskPerc,budget,knowledge,nature,access)
write.csv(mgmt.low, paste0(agentFilepath,"mgmt_lowInt.csv"), row.names=F)

# manage (med intensity)
Service <- c("biodiversity","recreation","management")
Production <- c(0.6,0.6,1) # attempting balance of objectives
OPMpresence <- c(1,1,1) # should only appear when OPM is present 
riskPerc <- c(NA,NA,0.5) # medium risk perception
budget <- c(NA,NA,0.5) # requires more budget for spraying etc. 
knowledge <- c(NA,NA,0.8) # management requires knowledge
nature <- c(1,NA,NA) # Production of biodiversity dependent on nature capital
access <- c(NA,1,NA) # Production of recreation dependent on access capital

mgmt.med <- tibble(Service,Production,OPMpresence,riskPerc,budget,knowledge,nature,access)
write.csv(mgmt.med, paste0(agentFilepath,"mgmt_medInt.csv"), row.names=F)

# manage (high intensity)
Service <- c("biodiversity","recreation","management")
Production <- c(0.5,1,1) # focus is on reducing risk to public health and allowing continued access
OPMpresence <- c(1,1,1) # should only appear when OPM is present 
riskPerc <- c(NA,NA,1) # this kind of management only possible where risk perceptions...
budget <- c(NA,NA,1) # and budget are high
knowledge <- c(NA,NA,0.8) # management requires knowledge
nature <- c(1,NA,NA) # Production of biodiversity dependent on nature capital
access <- c(NA,1,NA) # Production of recreation dependent on access capital

mgmt.high <- tibble(Service,Production,OPMpresence,riskPerc,budget,knowledge,nature,access)
write.csv(mgmt.high, paste0(agentFilepath,"mgmt_highInt.csv"), row.names=F)


##### Also capitals, Services index tables, + demand

# Capitals
Name <- c("OPMpresence","riskPerc","budget","knowledge","nature","access")
Index <- c(0,1,2,3,4,5)
Capitals <- tibble(Name,Index)
write.csv(Capitals, paste0(agentFilepath,"Capitals.csv"), row.names=F)

# Services
Name <- c("biodiversity","recreation","management")
Index <- c(0,1,2)
Services <- tibble(Name,Index)
write.csv(Services, paste0(agentFilepath,"Services.csv"), row.names=F)

# Demand

Year <- 1
biodiversity <- 1000
recreation <- 1000
management <- 1000
Demand <- tibble(Year,biodiversity,recreation,management)
write.csv(Demand, paste0(agentFilepath,"Demand.csv"), row.names=F)

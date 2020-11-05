
# date: 05/11/2020
# author: VB
# purpose: create csv files for each OPM management agent

library(tidyverse)

agentFilepath <- ""

# define production levels for services (0-1, low-high)
# and sensitivity of production to each capital (0-1, low-high)

# no management (no OPM)
service <- c("biodiversity","recreation","management")
production <- as.integer(c(1,1,NA))
opm.presence <- as.integer(c(NA,NA,NA))
risk.perception <- as.integer(c(NA,NA,NA))
budget <- as.integer(c(NA,NA,NA))
knowledge <- as.integer(c(NA,NA,NA))
nature <- as.integer(c(1,NA,NA))
access <- as.integer(c(NA,1,NA))

no.mgmt.no.opm <- tibble(service,production,opm.presence,risk.perception,budget,knowledge,nature,access)
write.csv(no.mgmt.no.opm, agentFilepath)


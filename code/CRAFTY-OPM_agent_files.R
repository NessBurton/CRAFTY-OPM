
# date: 05/11/2020
# author: VB
# purpose: create csv files for each OPM management agent

library(tidyverse)

agentFilepath <- ""

# define production levels for services (0-1, low-high)
# and sensitivity of production to each capital (0-1, low-high)

# no management (no OPM)
service <- c("biodiversity","recreation","management")
production <- c(1,1,NA) # if no OPM, assume maximum amount of services can be produced (excl. management)
opm.presence <- c(NA,NA,NA) # no reliance on opm presence
risk.perception <- c(NA,NA,NA) # no reliance on risk perception
budget <- c(NA,NA,NA) # no reliance on budget
knowledge <- c(NA,NA,NA) # no reliance on knowledge
nature <- c(1,NA,NA) # production of biodiversity fully reliant on nature capital level, 1:1
access <- c(NA,1,NA) # production of recreation fully reliant on access capital level, 1:1

no.mgmt.no.opm <- tibble(service,production,opm.presence,risk.perception,budget,knowledge,nature,access)
write.csv(no.mgmt.no.opm, agentFilepath)

# no management (unable)
service <- c("biodiversity","recreation","management")
production <- c(0.4,0.4,NA) # if OPM present but no management, service provision compromised
opm.presence <- c(1,1,1) # should only appear when OPM is present 
risk.perception <- c(NA,NA,NA) # no reliance
budget <- c(NA,NA,NA) # no reliance 
knowledge <- c(NA,NA,NA) # no reliance
nature <- c(1,NA,NA) # production of biodiversity dependent on nature capital
access <- c(NA,1,NA) # production of recreation dependent on access capital

no.mgmt.unable <- tibble(service,production,opm.presence,risk.perception,budget,knowledge,nature,access)
write.csv(no.mgmt.unable, agentFilepath)

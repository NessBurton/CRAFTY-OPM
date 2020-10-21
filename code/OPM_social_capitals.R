
# date: 21/10/20
# author: VB
# description: work out how to allocate spatial data across the landscape to represent risk perception and budget

# load libraries
library(tidyverse)
library(sf)
library(tmap)

# file paths
#wd <- "~/Documents/crafty-opm" # mac
wd <- "~/R/CRAFTY-OPM" # FR
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

# join files with type and ownerIDs
hexType <- st_read(paste0(dirOut, "/hexGrids/hexGrid40m_types2.shp"))
hexOwner <- st_read(paste0(dirOut,"/capitals/hexG_ownerIDs2.shp"))
hexType <- hexType[,c(1,21)]
hexType <- as.data.frame(hexType)
hexType$geometry <- NULL
hexSocial <- merge(hexType,hexOwner,by="joinID")
hexSocial <- st_as_sf(hexSocial)

# check
ggplot()+
  geom_sf(hexSocial, mapping = aes(fill = type), col = NA)+
  scale_fill_manual(values=type.pal)

# new risk perception variable
hexSocial$riskPerc <- NA 

# start test with private gardens
# 16% strongly agree OPM is a risk - value = 1
# 54% agree - value = 0.8
# 9% neutral - value = 0.5
# 21%  disagree, strongly disagree or don't know - value = 0
gardensAll <- as.numeric(row.names(hexSocial[which(hexSocial$type == "Private.garden"),]))
perc16 <- length(sample(gardensAll,(0.16*length(gardensAll))))
perc54 <- length(sample(gardensAll,(0.54*length(gardensAll))))
perc9 <- length(sample(gardensAll,(0.09*length(gardensAll))))
perc21 <- length(sample(gardensAll,(0.21*length(gardensAll))))
# random sample 16%
gardens16 <- sample(gardensAll,perc16,replace = F)
hexSocial$riskPerc[gardens16] <- 1
# random sample 54% from gardens not already sampled
gardens84 <- as.numeric(row.names(hexSocial[which(hexSocial$type=="Private.garden" & is.na(hexSocial$riskPerc)),]))
gardens54 <- sample(gardens84,perc54,replace = F)
hexSocial$riskPerc[gardens54] <- 0.8
# random sample 9% from gardens not already sampled
gardens30 <- as.numeric(row.names(hexSocial[which(hexSocial$type=="Private.garden" & is.na(hexSocial$riskPerc)),]))
gardens9 <- sample(gardens30,perc9,replace = F)
hexSocial$riskPerc[gardens9] <- 0.5
# final 21%
gardens21 <- as.numeric(row.names(hexSocial[which(hexSocial$type=="Private.garden" & is.na(hexSocial$riskPerc)),]))
hexSocial$riskPerc[gardens21] <- 0

summary(hexSocial$riskPerc)

ggplot()+
  geom_sf(hexSocial, mapping = aes(fill = riskPerc), col = NA)+
  scale_fill_viridis()

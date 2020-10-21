
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


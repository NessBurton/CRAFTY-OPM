
# date: 05/11/2020
# author: VB
# description: collate all capital data processed in other scripts and write to a single test region csv for CRAFTY

library(tidyverse)
library(ggplot2)
library(sf)

wd <- "C:/Users/vanessa.burton/OneDrive - Forest Research/Documents/R/CRAFTY-OPM"
wd <- "~/CRAFTY-opm" # sandbox VM

#####
# load in capital files
#####

#opm <- st_read()
natural <- sf::st_read(paste0(wd,"/data-processed/capitals/hexG_bio_access.shp"))
social <- st_read(paste0(wd,"/data-processed/capitals/hexG_social.shp"))

#####
# merge
#####

social <- social %>% st_drop_geometry()
TestRegion <- merge(natural,social,by="joinID")

#####
# add empty OPM presence column for now

TestRegion$OPMpresence <- 0

#####
# extract geometry into x and y
#####

TestRegion <- TestRegion %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2]) %>% 
  st_drop_geometry()

# check plot
ggplot(TestRegion)+
  geom_raster(aes(X,Y,fill=nature))

#####
# edit coordinates so CRAFTY can deal with them (java not a fan of long coordinates)
#####

# subtract minX from X and minY from Y

xmin <- min(TestRegion$X)
ymin <- min(TestRegion$Y)

TestRegion$X <- TestRegion$X - xmin
TestRegion$Y <- TestRegion$Y - ymin

TestRegion$X <- TestRegion$X / 1000
TestRegion$Y <- round(TestRegion$Y / 1000, digits=2)

ggplot(TestRegion)+
  geom_tile(aes(X,Y,fill=type))

# note to self. where 'type' is non-greenspace, mask out/remove from CRAFTY?
# e.g.

TestRegion <- TestRegion %>%
  filter(type != "Non.greenspace") 

TestRegion %>%
  ggplot()+
  geom_tile(aes(X,Y,fill=type))

#####
# initial agent allocation - no management everywhere
#####

TestRegion$Agent <- "no_mgmt_NOPM"

#####
# edit order of columns and write to csv
#####

head(TestRegion)
head(TestRegion[,c(1,11,12,13,10,6,8,9,2,3)])
TestRegion <- TestRegion[,c(1,11,12,13,10,6,8,9,2,3)]

# make NAs 0 for CRAFTY?
TestRegion[is.na(TestRegion)] <- 0

TestRegion %>%
  ggplot()+
  geom_tile(aes(X,Y,fill=riskPerc))

write.csv(TestRegion, paste0(wd,"/data-processed/for-CRAFTY/TestRegion.csv"), row.names = F)

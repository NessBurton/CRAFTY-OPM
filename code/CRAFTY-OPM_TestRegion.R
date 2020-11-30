
# date: 05/11/2020
# author: VB
# description: collate all capital data processed in other scripts and write to a single test region csv for CRAFTY

library(tidyverse)
library(ggplot2)
library(sf)

#wd <- "C:/Users/vanessa.burton/OneDrive - Forest Research/Documents/R/CRAFTY-OPM"
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
# row and column ids for hex
#####

#install.packages("hexbin")
#library(hexbin)



#####
# extract geometry into x and y
#####

TestRegion <- TestRegion %>%
  mutate(Long = st_coordinates(st_centroid(.))[,1],
         Lat = st_coordinates(st_centroid(.))[,2]) %>% 
  st_drop_geometry()

# check plot
ggplot(TestRegion)+
  geom_raster(aes(Long,Lat,fill=nature))+
  coord_cartesian(xlim = c(525000, 527500),ylim = c(180000, 182500))
# issues with gaps.
# due to hexagonal grid.
# have to re-do with regular grid?

# rasterise
library(raster)
TRxyz <- TestRegion[,c(11,12,4)]
rstTestRegion <- rasterFromXYZ(TRxyz)  #Convert first two columns as lon-lat and third as value                
plot(rstTestRegion)


# where 'type' is non-greenspace, mask out/remove from CRAFTY?

TestRegion <- TestRegion %>%
  filter(type != "Non.greenspace") 

TestRegion %>%
  ggplot()+
  geom_tile(aes(Long,Lat,fill=type))


#####
# cell id and lat long

ids <- TestRegion[,c(1,11,12)]
head(ids)
write.csv(ids, paste0(wd,"/data-processed/for-CRAFTY/Cell_ID_LatLong.csv"), row.names = F )

#####
# edit coordinates so CRAFTY can deal with them (java not a fan of long coordinates)
#####

# subtract minX from X and minY from Y

#xmin <- min(TestRegion$Long)
#ymin <- min(TestRegion$Lat)

#TestRegion$X <- TestRegion$Long - xmin
#TestRegion$Y <- TestRegion$Lat - ymin

#TestRegion$X <- TestRegion$X / 1000
#TestRegion$Y <- round(TestRegion$Y / 1000, digits=2)

#ggplot(TestRegion)+
  #geom_tile(aes(X,Y,fill=type))


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
  geom_tile(aes(Long,Lat,fill=riskPerc))

write.csv(TestRegion, paste0(wd,"/data-processed/for-CRAFTY/LondonBoroughs_latLong.csv"), row.names = F)

write.csv(TestRegion, paste0(wd,"/data-processed/for-CRAFTY/TestRegion.csv"), row.names = F)

# round coordinates to 1 decimal place for CRAFTY
TestRegion <- read.csv(paste0(wd,"/data-processed/for-CRAFTY/LondonBoroughs.csv"))
summary(TestRegion$X)
TestRegion$X <- round(TestRegion$X, digits=2)
TestRegion$Y <- round(TestRegion$Y, digits=2)
summary(TestRegion)
TestRegion[,5:10] <- round(TestRegion[,5:10], digits = 2)
summary (TestRegion)

TestRegion <- tibble::rowid_to_column(TestRegion, "id")

write.csv(TestRegion, paste0(wd,"/data-processed/for-CRAFTY/LondonBoroughs.csv"), row.names = F)

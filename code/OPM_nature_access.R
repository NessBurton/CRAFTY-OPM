
# date: 14/10/20
# author: VB
# description: process GiGL biodiversity data and OS open greenspace data

# load libraries
library(tidyverse)
library(sf)
library(tmap)

# file paths
#wd <- "~/Documents/crafty-opm" # mac
wd <- "~/R/CRAFTY-OPM" # FR
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

# hex points and grid
hexPoints <- st_read(paste0(dirOut,"/hexGrids/hexPoints40m.shp"))

# GiGL dataset (clipped to AOI and rasterised at 2m res in Arc)
bio <- raster(paste0(dirOut,"/rastBHP2m.tif"))
plot(bio)

# OS open greenspace data
access <- raster(paste0(dirOut,"/rstAccess2m.tif"))
plot(access)

# extract values 
# points need to be SpatialPoints
hexPointsSP <- as_Spatial(hexPoints)
hexPoints_bio <- extract(bio, hexPointsSP)
hexPoints_access <- extract(access, hexPointsSP)
hexPoints_access[which(is.na(hexPoints_access))] <- 0

# combined
hexP_capitals <- cbind(hexPointsSP,hexPoints_bio, hexPoints_access)
head(hexP_capitals)
colnames(hexP_capitals@data)[2] = "nature"
colnames(hexP_capitals@data)[3] = "access"
hexP_capitals <- st_as_sf(hexP_capitals)

# join to hexagons
hexGrid <- st_read(paste0(dirOut,"/hexGrids/hexGrid40m.shp"))
hexGrid <- as.data.frame(hexGrid)
hexGrid <- merge(hexGrid, hexP_capitals, by="joinID")
hexGrid$geometry.y<-NULL # remove poin geometry
colnames(hexGrid)[2] <- "geometry"
hexGrid <- st_as_sf(hexGrid)

# check
ggplot() +
  geom_sf(hexGrid, mapping = aes(fill = access), col = NA)

# write
st_write(hexGrid, paste0(dirOut,"/capitals/hexG_bio_access.shp"))

# normalise bio values 0-1
summary(hexGrid)

data <- hexGrid$nature
normalised <- (data-min(data))/(max(data)-min(data))
hist(data)
hist(normalised)

hexGrid$nature <- normalised

st_write(hexGrid, paste0(dirOut,"/capitals/hexG_bio_access_norm.shp"))

hexGrid <- st_read(paste0(dirOut,"/capitals/hexG_bio_access_norm.shp"))

library(viridis)

png(paste0(wd,"/figures/nature_capital.png"), width = 800, height = 800)
ggplot() +
  geom_sf(hexGrid, mapping = aes(fill = nature), col = NA)+
  scale_fill_viridis()
dev.off()

png(paste0(wd,"/figures/access_capital.png"), width = 800, height = 800)
ggplot() +
  geom_sf(hexGrid, mapping = aes(fill = access), col = NA)+
  scale_fill_viridis()
dev.off()



# date: 21/08/20
# author: VB
# purpose: process and plot a number of different spatial datasets which will be used to develop CRAFTY-OPM

# load libraries
library(tidyverse)
library(sf)
library(tmap)

setwd("~/R/CRAFTY-OPM")

#####
# OSMM greenspace for hammersmith/kensington/westminster/camden + 1km buffer

gspace <- st_read("./spatial-data/OSMM_greenspace/OSMM_gspaceAOI.shp")
head(gspace)
unique(gspace$priFunc)

# 300,000 obvs!
# may be an issue... but may reduce if i merge park polygons within a distance (e.g. 10m?)

gspace_summary <- gspace %>% 
  group_by(priFunc) %>% 
  summarise(area=sum(SHAPE_Area))

gspace_summary <- gspace_summary[with(gspace_summary, order(-area)),]
gspace_summary$geometry<-NULL
tot<-sum(gspace_summary$area)
gspace_summary <- gspace_summary %>% 
  mutate(proportion = area/tot*100)
#sum(gspace_summary$proportion)
# 39.9% private gardens
# 24.3% public park or garden
# 13.2% amenity - residential or business
# 3.7% school grounds
# 3.2% natural
# 2.3% playing field
# 2.1% cemetery
write.csv(gspace_summary,"./data-processed/AOI_greenspace_area_summaries.csv")

#plot(gspace["priFunc"], key.pos = 4)

tm_shape(gspace) +
  tm_polygons("priFunc")

#####
# GiGL biodiversity dataset

bio <- st_read("./spatial-data/GiGL_BHP_SHP/GiGL_BHP_1km_buffer.shp")
head(bio)
unique(bio$BHP_Score)

tm_shape(bio)+
  tm_polygons("BHP_Score")

#####
# example code for plotting - maybe useful later
ggplot() +
  geom_sf(data = lscape, aes(fill = biodiversity)) +
  scale_y_continuous(breaks = 1:10)

lscape2 <- lscape %>% select(biodiversity, recreation, knowledge, opm.pres, geometry) %>% 
  gather(resource, value, -geometry)
ggplot() +
  geom_sf(data = lscape2, aes(fill = value), color=NA) +
  facet_wrap(~resource, ncol = 1) +
  scale_y_continuous(breaks = 1:10)+
  scale_fill_viridis_c()

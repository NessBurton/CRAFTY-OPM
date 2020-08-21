
library(tidyverse)
library(sf)
library(tmap)

#####
# OSMM greenspace camden/hammersmith

camden <- st_read("~/FPPH_Defra-OPM/spatial-data/OSMM_gspace_Camden.shp")
head(camden)
unique(camden$priFunc)

camden_summary <- camden %>% 
  group_by(priFunc) %>% 
  summarise(area=sum(SHAPE_Area))

camden_summary <- camden_summary[with(camden_summary, order(-area)),]
camden_summary$geometry<-NULL
tot<-sum(camden_summary$area)
camden_summary <- camden_summary %>% 
  mutate(proportion = area/tot*100)
#sum(camden_summary$proportion)
# 39% private gardens
# 35% public park or garden
# 12% amenity - residential or business
# 2% school grounds
# 2% cemetery

#plot(camden["priFunc"], key.pos = 4)

tm_shape(camden) +
  tm_polygons("priFunc")

#####
# geofabrik data

poisLDN <- st_read("~/FPPH_Defra-OPM/spatial-data/greater-london-latest-free.shp/gis_osm_pois_a_free_1.shp")
head(poisLDN)
unique(poisLDN$fclass)

parks <- poisLDN %>% filter(fclass=="park")
plot(parks)

# CRAFTY explainer/example

lscape <- st_read("~/FPPH_Defra-OPM/spatial-data/CRAFTY-exp.shp")
plot(lscape)

lscape$objectid<-NULL
lscape$toid<-NULL
lscape$version<-NULL
lscape$secfunc<-NULL
lscape$priform<-NULL
lscape$secform<-NULL

plot(lscape)
unique(lscape$prifunc)
colnames(lscape)[1]<-"owner"
plot(lscape)

lscape$biodiversity<-NA
lscape$recreation<-NA
lscape$knowledge<-NA
lscape$opm.pres<-NA

lscape$biodiversity[which(lscape$owner=="Private Garden")]<-6
lscape$biodiversity[which(lscape$owner=="Public Park Or Garden")]<-8
lscape$biodiversity[which(lscape$owner=="School Grounds")]<-3
lscape$biodiversity[which(lscape$owner=="Playing Field")]<-5
lscape$biodiversity[which(lscape$owner=="Residential Or Business Amenity")]<-4
lscape$biodiversity[which(lscape$owner=="Transport")]<-2

plot(lscape)

lscape$recreation[which(lscape$owner=="Private Garden")]<-1
lscape$recreation[which(lscape$owner=="Public Park Or Garden")]<-10
lscape$recreation[which(lscape$owner=="School Grounds")]<-1
lscape$recreation[which(lscape$owner=="Playing Field")]<-8
lscape$recreation[which(lscape$owner=="Residential Or Business Amenity")]<-0
lscape$recreation[which(lscape$owner=="Transport")]<-0

lscape$knowledge[which(lscape$owner=="Private Garden")]<-2
lscape$knowledge[which(lscape$owner=="Public Park Or Garden")]<-8
lscape$knowledge[which(lscape$owner=="School Grounds")]<-6
lscape$knowledge[which(lscape$owner=="Playing Field")]<-5
lscape$knowledge[which(lscape$owner=="Residential Or Business Amenity")]<-5
lscape$knowledge[which(lscape$owner=="Transport")]<-7

lscape$opm.pres[which(lscape$owner=="Private Garden")]<-0
lscape$opm.pres[which(lscape$owner=="Public Park Or Garden")]<-5
lscape$opm.pres[which(lscape$owner=="School Grounds")]<-4
lscape$opm.pres[which(lscape$owner=="Playing Field")]<-4
lscape$opm.pres[which(lscape$owner=="Residential Or Business Amenity")]<-0
lscape$opm.pres[which(lscape$owner=="Transport")]<-10

plot(lscape)
lscape$biodiversity<-as.integer(lscape$biodiversity)
plot(lscape["biodiversity"], key.pos = 2)

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

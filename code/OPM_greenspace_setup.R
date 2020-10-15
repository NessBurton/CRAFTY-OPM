
# date: 12/10/20
# author: VB
# description: process OSMM greenspace data. simplify typology and merge adjacent types (parks)

# load libraries
library(tidyverse)
library(sf)
library(tmap)
library(rgeos)
library(units)
library(tictoc) # for timing

# file paths
#wd <- "~/Documents/crafty-opm" # mac
wd <- "~/R/CRAFTY-OPM" # FR
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

#####
# OSMM greenspace for hammersmith/kensington/westminster/camden + 1km buffer

gspace <- st_read(paste0(dirData,"/OSMM_greenspace/OSMM_gspaceAOI.shp"))
head(gspace)
unique(gspace$priFunc)
unique(gspace$secFunc)

gspace_summary <- read.csv("./data-processed/AOI_greenspace_area_summaries.csv")
# look at stats and work out a simpler typology

#####
# testing/ working out process
# start with just camden
camden <- st_read(paste0(dirData,"/OSMM_greenspace/gspaceCamden.shp"))
ggplot(camden) + geom_sf()

# create new binary column for whether polygon is a park or not (T/F)
#camden$park <- camden$priFunc == "Public Park Or Garden"
#tm_shape(camden) + tm_fill(col="park") +tm_borders(col = "white")

# st_union
parks <- camden %>% 
  filter(priFunc == "Public Park Or Garden") %>% 
  st_union(by_feature = TRUE)
#head(parks)
#ggplot(parks)+geom_sf()  
#tm_shape(parks) + tm_fill(col='OBJECTID')
# still have 1680 features

# group park features within a distance of each other and give each group a unique id
# then aggregate based on that id?

# a function that takes an sf polygons object and clusters all features within a threshold distance, then merges the features
# https://gis.stackexchange.com/questions/254519/group-and-union-polygons-that-share-a-border-in-r
clusterSF <- function(sfpolys, thresh){
  dmat = st_distance(sfpolys)
  hc = hclust(as.dist(dmat>thresh), method="single")
  groups = cutree(hc, h=0.5)
  d = st_sf(
    geom = do.call(c,
                   lapply(1:max(groups), function(g){
                     st_union(sfpolys[groups==g,])
                   })
    )
  )
  d$group = 1:nrow(d)
  d
}

# function works on test
#library(rgeos)
#set.seed(123)
#pols  = st_as_sf(
#gBuffer(SpatialPoints(cbind(100*runif(20),100*runif(20))), width=12, byid=TRUE)
#)
#plot(pols)
#polcluster = clusterSF(pols, 1)

# test on smaller area
#parksTest <- parks[100:200,]
#ggplot(parksTest)+geom_sf()
# cluster features within 5 metres, define units using set_unit()
#parksClust <- clusterSF(parksTest, set_units(5, "m"))
#plot(parksClust, col=parksClust$group)
# seems to work
tic("clustering")
parksClust <- clusterSF(parks, set_units(5, "m"))
toc() # takes about 50 mins to run
plot(parksClust, col=parksClust$group)
st_write(parksClust, paste0(dirData,"/OSMM_greenspace/gspaceCamden_parksClust.shp"))

# join original greenspace data to new parks layer, dropping original features within new park features?
#parksJoin <- st_join(camden,parksClust)
#parksJoin
# this doesn't drop the original polygons.

# filter parks out of camden
camden2 <-  camden %>% filter(priFunc != "Public Park Or Garden")
# check
ggplot(camden2)+geom_sf()
# union simplified parks to this?
#parksJoin <- st_union(camden2, parksClust)
# error Error in CPL_geos_op2(op, x, y) : Evaluation error: std::bad_alloc.
#ggplot(parksJoin)+geom_sf() 
# do this part in ArcGIS

#####
# loop to select each priFunc to be clustered/merged, write new clustered shapefile for input to Arc

unique(gspace$priFunc)

# list of types that if polygons are within 5m of each other, are likely to be under same ownership
type <- c("School Grounds","Religious Grounds", "Institutional Grounds", "Play Space", "Other Sports Facility","Playing Field", "Public Park Or Garden",
          "Cemetery", "Golf Course")

for (i in type){
  
  i <- "Religious Grounds"
  
  x <- gspace %>% 
    filter(priFunc == i) %>% 
    st_union(by_feature = TRUE)
  
  xClust <- clusterSF(x, set_units(5, "m"))
  
  st_write(xClust, paste0(dirData,"/OSMM_greenspace/gspace_",i,"_Clust.shp"))
  
}


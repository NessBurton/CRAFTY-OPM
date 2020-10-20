
# date: 12/10/20
# author: VB
# description: process OSMM greenspace data. simplify typology and merge adjacent types (parks)

# load libraries
library(tidyverse)
library(sf)
library(tmap)
library(rgeos)
library(raster)
library(units)
library(tictoc) # for timing

# file paths
#wd <- "~/Documents/crafty-opm" # mac
wd <- "~/R/CRAFTY-OPM" # FR
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

#####
# OSMM greenspace for hammersmith/kensington/westminster/camden + 1km buffer

#gspace <- st_read(paste0(dirData,"/OSMM_greenspace/OSMM_gspaceAOI.shp"))
#head(gspace)
#unique(gspace$priFunc)
#unique(gspace$secFunc)

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
  
  #i <- "Religious Grounds"
  
  x <- gspace %>% 
    filter(priFunc == i) %>% 
    st_union(by_feature = TRUE)
  
  xClust <- clusterSF(x, set_units(5, "m"))
  
  st_write(xClust, paste0(dirData,"/OSMM_greenspace/gspace_",i,"_Clust.shp"))
  
}

# check
plot(xClust, col=xClust$group)


#####
# hexagonal grid over gspace
library(sp) #  can only be done using sp

# read in as sf
studyArea <- st_read(paste0(dirData,"/borough_boundaries/case_studies.shp"))
# convert to sp
studyArea <- as_Spatial(studyArea)

size <- 40 # 40m2

hex_points <- spsample(studyArea, type = "hexagonal", cellsize = size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
#plot(spGspace, col = "grey50", bg = "light blue", axes = TRUE) # DON'T PLOT THIS!! takes sooo long
plot(studyArea, col = "grey50", bg = "white", dx = size)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)

# convert hex back to sf and write shapefile
#hex_grid <- st_as_sf(hex_grid)
#st_write(hex_grid, paste0(dirOut,"/hexGrids/hexGrid40m.shp"), append=F)
#hex_points <- st_as_sf(hex_points)
#st_write(hex_points, paste0(dirOut,"/hexGrids/hexPoints40m.shp"), append = F)

# commented out so i don't overwrite

# read in from arc
# (in arc, used Zonal Statistics to table to calculate majority raster value in each hex)

#hexGspace <- st_read(paste0(dirOut, "/hexGrids/hex30_gspaceMaj.shp"))
#head(hexGspace)

# new type field
#hexGspace$type<-NA
#hexGspace$type[which(hexGspace$MAJORITY==18)] <- "Non-greenspace"
#hexGspace$type[which(hexGspace$MAJORITY==1)] <- "Natural"
#hexGspace$type[which(hexGspace$MAJORITY==2)] <- "Private garden"
#hexGspace$type[which(hexGspace$MAJORITY==3)] <- "Amenity - residential or business"
#hexGspace$type[which(hexGspace$MAJORITY==4)] <- "School"
#hexGspace$type[which(hexGspace$MAJORITY==5)] <- "Religious grounds"
#hexGspace$type[which(hexGspace$MAJORITY==6)] <- "Amenity - transport"
#hexGspace$type[which(hexGspace$MAJORITY==7)] <- "Institutional grounds"
#hexGspace$type[which(hexGspace$MAJORITY==8)] <- "Land use changing"
#hexGspace$type[which(hexGspace$MAJORITY==9)] <- "Play space"
#hexGspace$type[which(hexGspace$MAJORITY==10)] <- "Other sports facility"
#hexGspace$type[which(hexGspace$MAJORITY==11)] <- "Playing field"
#hexGspace$type[which(hexGspace$MAJORITY==12)] <- "Public park or garden"
#hexGspace$type[which(hexGspace$MAJORITY==13)] <- "Tennis court"
#hexGspace$type[which(hexGspace$MAJORITY==14)] <- "Cemetery"
#hexGspace$type[which(hexGspace$MAJORITY==15)] <- "Allotments"
#hexGspace$type[which(hexGspace$MAJORITY==16)] <- "Bowling green"
#hexGspace$type[which(hexGspace$MAJORITY==17)] <- "Golf course"

#plot(hexGspace, col=hexGspace$MAJORITY)

# load shp with tabulated areas
hexGspace <- st_read(paste0(dirOut, "/hexGrids/hexGrid40m_tab.shp"))
head(hexGspace)
head(hexGspace[,4:20])
colnames(hexGspace)[4] <- "Natural"
colnames(hexGspace)[5] <- "Garden"
colnames(hexGspace)[6] <- "AmenityRB"
colnames(hexGspace)[7] <- "School"
colnames(hexGspace)[8] <- "Religious"
colnames(hexGspace)[9] <- "AmenityT"
colnames(hexGspace)[10] <- "Institutional"
colnames(hexGspace)[11] <- "Changing"
colnames(hexGspace)[12] <- "Playspace"
colnames(hexGspace)[13] <- "OtherSports"
colnames(hexGspace)[14] <- "PlayingField"
colnames(hexGspace)[15] <- "PublicPark"
colnames(hexGspace)[16] <- "Tennis"
colnames(hexGspace)[17] <- "Cemetery"
colnames(hexGspace)[18] <- "Allotments"
colnames(hexGspace)[19] <- "BowlingGreen"
colnames(hexGspace)[20] <- "Non-greenspace"


# convert to data frame
dfHex <- as.data.frame(hexGspace)
# calculate total area across rows
#nrows <- 52947
nrows <- length(dfHex[,1])
for (i in c(1:nrows)){
  dfHex$area[i] <- sum(dfHex[i,4:20])
}
# function to divide each row by total area
divideRow <- function(x){
  x / dfHex$area
}

# apply function across all rows for specific colums
dfHex[,4:20] <- lapply(dfHex[,4:20], FUN = divideRow)

# assign types

type <- rep("NA",nrows)

for (i in c(1:nrows)) {
  
  # mask out natural (either inland water or shore front)
  if (dfHex$Natural[i]>0){
    type[i] <- "Natural" 
  }
  
  # prioritise bits of amenity
  if (dfHex$AmenityRB[i]>=0.2){
    type[i] <- "Amenity.residential.business"
  }
  if (dfHex$AmenityT[i]>=0.2){
    type[i] <- "Amenity.transport"
  }
  
  # now private gardens
  if (dfHex$Garden[i]>=0.4){ # this underestimates overall area of private gardens, but
    type[i]<- "Private.garden" # is probably better at estimating number of households, and
  }                           # picking up areas of road between private gardens
  
    # parks
  if (dfHex$PublicPark[i]>=0.45){
    type[i] <- "Public.park"
  }
  
  if (dfHex$School[i]>=0.2){
    type[i] <- "School.grounds"
  }
  
  if (dfHex$Religious[i]>=0.2){
    type[i] <- "Religious.grounds"
  }
  
  if (dfHex$Institutional[i]>=0.2){
    type[i] <- "Institutional.grounds"
  }
  
  if (dfHex$Changing[i]>=0.5){
    type[i] <- "Non.greenspace"
  }
  
  if (dfHex$Playspace[i]>=0.4){
    type[i] <- "Play.space"
  }
  
  if (dfHex$OtherSports[i] >= 0.4){
    type[i] <- "Other.sports"
  }
  
  if (dfHex$PlayingField[i] >= 0.4){
    type[i] <- "Playing.field"
  }
  
  if (dfHex$Tennis[i] >= 0.4){
    type[i] <- "Tennis.court"
  }
  
  if (dfHex$Cemetery[i] >= 0.4){
    type[i] <- "Cemetery"
  }
  
  if (dfHex$Allotments[i] >= 0.2) {
    type[i] <- "Allotments"
  }
  
  if(dfHex$BowlingGreen[i] >= 0.2) {
    type[i] <- "Bowling.green"
  }
  
  if(type[i] == "NA") {
    
    # catch smaller garden areas
    if (dfHex$Garden[i] >= 0.3){
      type[i] <- "Private.garden"
    }
    
    # now assign urban with high threshold first
    if (dfHex$`Non-greenspace`[i]>=0.7){
      type[i] <- "Non.greenspace"
    }
    
    # if still na, check tiny areas of amenity
    if (dfHex$AmenityRB[i]>0){
      type[i] <- "Amenity.residential.business"
    }
    if (dfHex$AmenityT[i]>0){
      type[i] <- "Amenity.transport"
    }
    # but otherwise make urban
    if (dfHex$`Non-greenspace`[i]>0.1){
      type[i] <- "Non.greenspace"
    }
    
  }
  
  # final NA catch
  if (type[i]=="NA"){
    type[i] <- "Non.greenspace"
  }
  
}

# check
hexGspace$type <- type
dfHex$type <- type

# check summaries against OSMM greenspace summaries
gspace_summary <- read.csv("./data-processed/AOI_greenspace_area_summaries.csv")

tot<-sum(dfHex$area[which(dfHex$type!="Non.greenspace")])
type_summary <- dfHex %>% 
  group_by(type) %>% 
  filter(type != "Non.greenspace") %>% # OSMM greenspace summaries don't include non-greenspace
  summarise(area=sum(area)) %>% 
  mutate(proportion = area/tot*100)

# underestimating private gardens and overestimating public parks, but broadly representative of the landscape
# go with this

ggplot() +
  geom_sf(hexGspace, mapping = aes(fill = type), col = NA)+
  scale_fill_manual(values=type.pal)

type.pal <- c("Amenity.residential.business" = "grey",
              "Amenity.transport" = "darkgrey",
              "Private.garden" = "hotpink",
              "Public.park" = "green",
              "School.grounds" = "blue",
              "Religious.grounds" = "blue",
              "Institutional.grounds" = "blue",
              "Non.greenspace" = "white",
              "Play.space" = "orange",
              "Playing.field" = "orange",
              "Other.sports" = "yellow",
              "Tennis.court" = "yellow",
              "Bowling.green" = "yellow",
              "Allotments" = "brown",
              "Cemetery" = "black",
              "Natural" = "darkgreen",
              "NA" = "red")

# write to shape
st_write(hexGspace, paste0(dirOut, "/hexGrids/hexGrid40m_types.shp"), append = FALSE) # append false overwrites layer

#####
# generate cluster IDs

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

unique(hexGspace$type)
# list of types that if polygons are within 5m of each other, are likely to be under same ownership
# essentially everything except private gardens (and non.greenspace/natural)
cluster <- c("School.grounds","Religious.grounds", "Institutional.grounds", 
          "Play.space", "Other.sports","Tennis.court","Bowling.green","Playing.field", "Public.park",
          "Cemetery", "Allotments")

for (i in cluster){
  
  #i <- "Religious.grounds" # test
  
  x <- hexGspace %>% 
    filter(type == i) %>% 
    st_union(by_feature = TRUE)
  
  xClust <- clusterSF(x, set_units(5, "m"))
  
  #xClust$uniqueID <- paste0(i,"-",xClust$group)
  
  xRast <- rasterize(x=xClust,
                     y=raster(extent(xClust), res=1),
                     field='group')
  
  st_write(xClust, paste0(dirOut,"/hexClusters/hexGspace_",i,"_Clust.shp"), append=F)
  writeRaster(xRast, file.path(paste0(dirOut, "/hexClusters/clust",i,".tif")), format="GTiff", overwrite=TRUE)
  
}


# hex points and grid
hexPoints <- st_read(paste0(dirOut,"/hexGrids/hexPoints40m.shp"))
hexGrid <- st_read(paste0(dirOut,"/hexGrids/hexGrid40m.shp"))


# to extract values, need to use sp points
hexPointsSP <- as_Spatial(hexPoints)

nrows <- nrow(hexGrid)

# empty dataframe for values
df <- data.frame(School.grounds = rep(NA, nrows),
                 Religious.grounds = rep(NA, nrows), 
                 Institutional.grounds = rep(NA, nrows), 
                 Play.space = rep(NA, nrows), 
                 Other.sports = rep(NA, nrows),
                 Tennis.court = rep(NA, nrows),
                 Bowling.green = rep(NA, nrows),
                 Playing.field = rep(NA, nrows), 
                 Public.park = rep(NA, nrows),
                 Cemetery = rep(NA, nrows), 
                 Allotments = rep(NA, nrows))

# read in rasters which show group id and extract values
for (i in cluster){
  
  #i <- "Allotments"
  
  x <- raster(paste0(dirOut, "/hexClusters/clust",i,".tif"))
  
  value <- extract(x, hexPointsSP)
  
  df[,i] <- value
  
}

summary(df)




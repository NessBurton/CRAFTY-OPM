
library(tidyverse)
library(sf)
library(tmap)
library(rgeos)
library(raster)

mstMap <- st_read(paste0(dirData,"/MasterMap/mstMapAOI.shp"))
head(mstMap)

# select from "" field where non-coniferous is mentioned
broadleaf <- filter(mstMap, grepl("Nonconiferous", descterm)) 
head(broadleaf)
unique(broadleaf$descterm)

st_write(broadleaf, paste0(dirData,"/MasterMap/mstMap_broadleaf.shp"))


# read in shp with tabulated areas from both OSMM priForm and MasterMap nonConiferous
hexSuit <- st_read(paste0(dirOut, "/hexGrids/hexGrid40m_suit.shp"))
head(hexSuit)
# rename values

colnames(hexSuit)[4:10] <- c("inland.water",
                             "multisurface",
                             "manmade",
                             "open.seminat",
                             "woodland",
                             "foreshore",
                             "non.greenspace")

colnames(hexSuit)[13:16] <- c("nonconiferous.primary",
                              "nonconiferous.secondary",
                              "nonconiferous.tertiary",
                              "non.woodland")

head(hexSuit)

hexSuit$Rowid_1<-NULL
hexSuit$HEXGRID4_1<-NULL

# convert to data frame
dfHex <- as.data.frame(hexSuit)


nrows <- length(dfHex[,1])

# function to divide each row by total area
divideRow <- function(x){
  x / 1388
}

# apply function across all rows for specific colums
dfHex[,4:10] <- lapply(dfHex[,4:10], FUN = divideRow)
dfHex[,11:14] <- lapply(dfHex[,11:14], FUN = divideRow)

# assign suitability
# this should range from 0-100% for rangeshiftR

suit <- rep("NA",nrows)

for (i in c(1:nrows)) {
  
  # mask out inland water/foreshore/manmade
  if (dfHex$inland.water[i]>0){
    suit[i] <- 0
  }
  
  if (dfHex$foreshore[i]>0){
    suit[i] <- 0
  }
  
  if (dfHex$manmade[i]>0.5){
    suit[i] <- 0
  }
  
  # now most suitable
  
  if (dfHex$nonconiferous.primary[i]>0.5){
    suit[i] <- 100
  }
  
  if (dfHex$nonconiferous.secondary[i]>0.5){
    suit[i] <- 95
  }
  
  if (dfHex$nonconiferous.tertiary[i]>0.5){
    suit[i] <- 90
  }
  
  if(suit[i] == "NA") {
    
    if (dfHex$woodland[i]>0.45){ # same threshold as for public parks in type allocation
      suit[i] <- 85
    }
    
    if (dfHex$open.seminat[i]>0.45){ # same threshold as for public parks in type allocation
      suit[i] <- 60
    }
    
    if (dfHex$multisurface[i]>0.35){ # same threshold as for private gardens in type allocation
      suit[i] <- 50
    }
    
  }
  
  if(suit[i] == "NA") {
    suit[i] <- 0
    }
}

# check
hexSuit$suit <- as.numeric(suit)
dfHex$suit <- as.numeric(suit)

library(viridis)
ggplot() +
  geom_sf(hexSuit, mapping = aes(fill = suit), col = NA)+
  scale_fill_viridis()

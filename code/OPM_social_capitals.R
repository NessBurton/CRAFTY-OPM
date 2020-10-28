
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
type.pal <- c("Amenity.residential.business" = "grey",
              "Amenity.transport" = "darkgrey",
              "Private.garden" = "#483D8B",
              "Public.park" = "#008000",
              "School.grounds" = "#2F4F4F",
              "Religious.grounds" = "#2F4F4F",
              "Institutional.grounds" = "#2F4F4F",
              "Non.greenspace" = "white",
              "Play.space" = "#008080",
              "Playing.field" = "#008080",
              "Other.sports" = "#00FA9A",
              "Tennis.court" = "#00FA9A",
              "Bowling.green" = "#00FA9A",
              "Allotments" = "#B8860B",
              "Cemetery" = "#696969",
              "Natural" = "white",
              "NA" = "red")
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

# now gets a little trickier.
# for other types i need to apply to ownerIDs, so can't use row indexing in the same way

# parks
# 50% neutral = 0.5, 50% agree = 0.8
parks <- filter(hexSocial, grepl("park", ownerID))
parkIDs <- unique(parks$ownerID)
perc50 <- length(sample(parkIDs,(0.5*length(parkIDs)))) # 50% of parkIDs
parkIDhalf <- sample(parkIDs,perc50,replace=F) # randomly sample
library(Hmisc) # for %nin% (not in)
index <- parkIDs %nin% parkIDhalf
parkIDhalf2 <- parkIDs[index==T] # extract the other half
# check
summary(parkIDhalf %in% parkIDhalf2) # all false so all good

#parksAll <- as.numeric(row.names(hexSocial[which(hexSocial$type == "Public.park"),]))

hexSocial$riskPerc[which(hexSocial$ownerID %in% parkIDhalf == T)] <- 0.8
hexSocial$riskPerc[which(hexSocial$ownerID %in% parkIDhalf2 == T)] <- 0.5


# all other types, by cluster ID
# 30% agree OPM is a risk - value 0.8
# 50% neutral - value 0.5
# 2% disagree - value 0.2
# 18% strongly disagree - value 0
others <- filter(hexSocial, grepl("schl|rlgs|inst|plysp|plyfd|othsp|ten|bwl|cmtry|altmt|amnrb|amnt", ownerID)) 
otherIDs <- unique(others$ownerID)


# example
#png(paste0(wd,"/figures/riskPerc_example.png"), width = 800, height = 600)
library(viridis)
ggplot()+
  geom_sf(hexSocial, mapping = aes(fill = riskPerc), col = NA)+
  scale_fill_viridis()
#dev.off()

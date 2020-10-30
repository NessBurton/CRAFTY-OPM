
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

##### Risk Perception
# new risk perception variable
hexSocial$riskPerc <- NA 

# start test with private gardens
# 16% strongly agree OPM is a risk - value = 1
# 54% agree - value = 0.8
# 9% neutral - value = 0.5
# 21%  disagree, strongly disagree or don't know - value = 0
gardensAll <- as.numeric(row.names(hexSocial[which(hexSocial$type == "Private.garden"),]))
# random sample 16%                       
perc16 <- length(sample(gardensAll,(0.16*length(gardensAll))))
gardens16 <- sample(gardensAll,perc16,replace = F)
hexSocial$riskPerc[gardens16] <- 1
# random sample 54% from gardens not already sampled
gardens84 <- as.numeric(row.names(hexSocial[which(hexSocial$type=="Private.garden" & is.na(hexSocial$riskPerc)),]))
perc54 <- length(sample(gardens84,(0.54*length(gardensAll))))
gardens54 <- sample(gardens84,perc54,replace = F)
hexSocial$riskPerc[gardens54] <- 0.8
# random sample 9% from gardens not already sampled
gardens30 <- as.numeric(row.names(hexSocial[which(hexSocial$type=="Private.garden" & is.na(hexSocial$riskPerc)),]))
perc9 <- length(sample(gardens30,(0.09*length(gardensAll))))
gardens9 <- sample(gardens30,perc9,replace = F)
hexSocial$riskPerc[gardens9] <- 0.5
# final 21%
gardens21 <- as.numeric(row.names(hexSocial[which(hexSocial$type=="Private.garden" & is.na(hexSocial$riskPerc)),]))
perc21 <- length(sample(gardens21,(0.21*length(gardensAll))))
hexSocial$riskPerc[gardens21] <- 0

summary(hexSocial$riskPerc)
#check
length(hexSocial$riskPerc[which(hexSocial$riskPerc==0.8)])/length(gardensAll)*100 # 54%
length(hexSocial$riskPerc[which(hexSocial$riskPerc==0.5)])/length(gardensAll)*100 # 9%
length(hexSocial$riskPerc[which(hexSocial$riskPerc==0)])/length(gardensAll)*100 #21%
# all good

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
others30 <- sample(otherIDs,(0.3*length(otherIDs)),replace = F)
# check
length(others30)/length(otherIDs)*100
hexSocial$riskPerc[which(hexSocial$ownerID %in% others30 == T)] <- 0.8
# select ids which haven't been selected already
index2 <- otherIDs %nin% others30 
others70 <- otherIDs[index2==T]
# check
length(others70)/length(otherIDs)*100
# now sample 50% (of all otherIDs) from these ids
others50 <- sample(others70, (0.5*length(otherIDs)))
# check
length(others50)/length(otherIDs)*100
hexSocial$riskPerc[which(hexSocial$ownerID %in% others50 == T)] <- 0.5
# now need to select 18% from IDs which haven't been sampled already
index3 <- others70 %nin% others50
summary(index3==T)
others20 <- others70[index3==T]
# check
length(others20)/length(otherIDs)*100
# sample 18% 
others18 <- sample(others20, (0.18*length(otherIDs)))
length(others18)/length(otherIDs)*100
hexSocial$riskPerc[which(hexSocial$ownerID %in% others18==T)] <- 0
index4 <- others20 %nin% others18
others2 <- others20[index4==T]
length(others2)/length(otherIDs)*100
hexSocial$riskPerc[which(hexSocial$ownerID %in% others2==T)] <-0.2

# example
#png(paste0(wd,"/figures/riskPerc_example.png"), width = 800, height = 600)
library(viridis)
ggplot()+
  geom_sf(hexSocial, mapping = aes(fill = riskPerc), col = NA)+
  scale_fill_viridis()
#dev.off()

# last written to file 28/10/20
st_write(hexSocial, paste0(dirOut,"/capitals/hexG_social.shp"), append=F)
hexSocial <- st_read(paste0(dirOut,"/capitals/hexG_social.shp"))

##### Budget
# apply randomly to ownerIDs? or just apply to whole boroughs

# read in hexGrid with boroughs
hexGrid <- st_read(paste0(dirOut, "/hexGrids/hexGrid40m.shp"))
head(hexGrid)
hexSocial <- cbind(hexSocial,hexGrid$borough)
colnames(hexSocial)[5] <- "borough"

ggplot() +
  geom_sf(hexSocial, mapping = aes(fill = borough), col = NA)

# modelled median household income from London Atlas
boroughs <- c("camden","westminster","kensington","hammersmith")
income <- c(43750,47510,55620,43820)

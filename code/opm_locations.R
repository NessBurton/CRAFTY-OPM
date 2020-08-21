
library(tidyverse)
library(ggplot2)

setwd("~/R/CRAFTY-OPM/")

# both datasets opm_sites and opm_trees come from the raw dataset
# opm_sites-locations_FRExport_20200629.xlsx in data-raw
# exported separate tabs as csv files

sites <- read.csv("data-processed/opm_sites.csv",na.strings = "NA")
trees <- read.csv("data-processed/opm_trees.csv")

str(sites) ; colnames(sites)[1]<-"siteID"
str(trees) ; colnames(trees)[1]<-"treeID"

unique(sites$Site.type)
sites$Site.type<-as.character(sites$Site.type)
sites$Site.type[which(sites$Site.type=="Street Tree")]<-"Street tree"
sites$Site.type[which(sites$Site.type=="street tree")]<-"Street tree"
sites$Site.type[which(sites$Site.type=="Residential Garden")]<-"Residential garden"
sites$Site.type[which(sites$Site.type=="residential garden")]<-"Residential garden"
sites$Site.type[which(sites$Site.type=="Park / public garden")]<-"Park / Public garden"
sites$Site.type[which(sites$Site.type=="Park / Public Garden")]<-"Park / Public garden"
sites$Site.type[which(sites$Site.type=="Golf Course")]<-"Golf course"
sites$Site.type<-factor(sites$Site.type)
unique(sites$Site.type)

unique(sites$Owner.type)
sites$Owner.type<-as.character(sites$Owner.type)
sites$Owner.type[which(sites$Owner.type=="n/a")]<-NA
sites$Owner.type[which(sites$Owner.type=="Public / private organisationv")]<-"Public / private organisation"
sites$Owner.type[which(sites$Owner.type=="Public / privateo organisation")]<-"Public / private organisation"
sites$Owner.type<-factor(sites$Owner.type)
unique(sites$Owner.type)

unique(sites$Status_2015) # status columns all factors, "not infested", "infested", "previously infested"
summary(sites)
# need to tidy
# columns for year, surveyed, infested, SPHN, sprayed, nest.removed
# have true/false or codes 1/0


status <- sites[,c(1:7,9:17)]
survey <- sites[,c(1:7,18:26)]
SPHN <- sites[,c(1:7,27:34)]
spray <- sites[,c(1:7,35:42)]
nest <- sites[,c(1:7,43:48)]

status_summary <- gather(status, year, status, -siteID, -Site.name, -Site.type, -Owner.type, -Company, -Easting, -Northing) %>%
  group_by(Site.type, year, status) %>%
  tally %>% 
  spread(status, n, fill = 0)

SPHN_summary <- gather(SPHN, year, SPHN, -siteID, -Site.name, -Site.type, -Owner.type, -Company, -Easting, -Northing) %>%
  group_by(Site.type, year, SPHN) %>%
  tally %>% 
  spread(SPHN, n, fill = 0)

spray_summary <- gather(spray, year, sprayed, -siteID, -Site.name, -Site.type, -Owner.type, -Company, -Easting, -Northing) %>%
  group_by(Site.type, year, sprayed) %>%
  tally %>% 
  spread(sprayed, n, fill = 0)

nest_summary <- gather(nest, year, nest, -siteID, -Site.name, -Site.type, -Owner.type, -Company, -Easting, -Northing) %>%
  group_by(Site.type, year, nest) %>%
  tally %>% 
  spread(nest, n, fill = 0)

spray_tot<-sum(spray_summary$Spraying)
spray_summary <- spray_summary %>% mutate(perc=Spraying/spray_tot*100)
nest_tot<-sum(nest_summary$`Nest removal`)
nest_summary <- nest_summary %>% mutate(perc=`Nest removal`/nest_tot*100)

# actually do i want percentages as a percentage per type (i.e. number of each type surveyed, percentage who spray/remove nests within that)
# but what is this really telling me
# these are OPM control programme records?
# if SPHN issued, then control (usually spraying) occurs

spray_summary %>% 
  ggplot(aes(Site.type,perc))+geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~year)

nest_summary %>% 
  ggplot(aes(Site.type,perc))+geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~year)


#####
# inefficient wrangling

status <- status %>% 
  pivot_longer(c(8:16), names_to = "year", values_to = "status")
head(status)
status$year[which(status$year=="Status_2012")]<-2012
status$year[which(status$year=="Status_2013")]<-2013
status$year[which(status$year=="Status_2014")]<-2014
status$year[which(status$year=="Status_2015")]<-2015
status$year[which(status$year=="Status_2016")]<-2016
status$year[which(status$year=="Status_2017")]<-2017
status$year[which(status$year=="Status_2018")]<-2018
status$year[which(status$year=="Status_2019")]<-2019
status$year[which(status$year=="Status_2020")]<-2020
status$year<-as.factor(status$year)
summary(status)
levels(status$status)
status$status<-as.character(status$status)
status$status[which(status$status=="")]<-NA
status$status<-as.factor(status$status)
#library(plyr)
#status$status <- mapvalues(status$status, from = c("", "Infested", "Not infested", "Previously infested"), to = c("0", "1","0", "0"))
summary(status)

survey <- survey %>% 
  pivot_longer(c(8:16), names_to = "year", values_to = "survey")
head(survey)
survey$year[which(survey$year=="Survey_2012")]<-2012
survey$year[which(survey$year=="Survey_2013")]<-2013
survey$year[which(survey$year=="Survey_2014")]<-2014
survey$year[which(survey$year=="Survey_2015")]<-2015
survey$year[which(survey$year=="Survey_2016")]<-2016
survey$year[which(survey$year=="Survey_2017")]<-2017
survey$year[which(survey$year=="Survey_2018")]<-2018
survey$year[which(survey$year=="Survey_2019")]<-2019
survey$year[which(survey$year=="Survey_2020")]<-2020
survey$year<-as.factor(survey$year)
summary(survey)
levels(survey$survey)
survey$survey<-as.character(survey$survey)
survey$survey[which(survey$survey=="")]<-NA
survey$survey<-as.factor(survey$survey)
#library(plyr)
#survey$survey <- mapvalues(survey$survey, from = c("", "Infested", "Not infested", "Previously infested"), to = c("0", "1","0", "0"))
summary(survey)

SPHN <- SPHN %>% 
  pivot_longer(c(8:15), names_to = "year", values_to = "SPHN")
head(SPHN)
#SPHN$year[which(SPHN$year=="SPHN_2012")]<-2012
SPHN$year[which(SPHN$year=="SPHN_2013")]<-2013
SPHN$year[which(SPHN$year=="SPHN_2014")]<-2014
SPHN$year[which(SPHN$year=="SPHN_2015")]<-2015
SPHN$year[which(SPHN$year=="SPHN_2016")]<-2016
SPHN$year[which(SPHN$year=="SPHN_2017")]<-2017
SPHN$year[which(SPHN$year=="SPHN_2018")]<-2018
SPHN$year[which(SPHN$year=="SPHN_2019")]<-2019
SPHN$year[which(SPHN$year=="SPHN_2020")]<-2020
SPHN$year<-as.factor(SPHN$year)
summary(SPHN)
levels(SPHN$SPHN)
SPHN$SPHN<-as.character(SPHN$SPHN)
SPHN$SPHN[which(SPHN$SPHN=="")]<-NA
SPHN$SPHN<-as.factor(SPHN$SPHN)
#library(plyr)
#SPHN$SPHN <- mapvalues(SPHN$SPHN, from = c("", "Infested", "Not infested", "Previously infested"), to = c("0", "1","0", "0"))
summary(SPHN)

head(spray)
spray <- spray %>% 
  pivot_longer(c(8:15), names_to = "year", values_to = "sprayed")
head(spray)
spray$year[which(spray$year=="Spray_2013")]<-2013
spray$year[which(spray$year=="Spray_2014")]<-2014
spray$year[which(spray$year=="Spray_2015")]<-2015
spray$year[which(spray$year=="Spray_2016")]<-2016
spray$year[which(spray$year=="Spray_2017")]<-2017
spray$year[which(spray$year=="Spray_2018")]<-2018
spray$year[which(spray$year=="Spray_2019")]<-2019
spray$year[which(spray$year=="Spray_2020")]<-2020
spray$year<-as.factor(spray$year)
summary(spray)
levels(spray$sprayed)
spray$sprayed<-as.character(spray$sprayed)
spray$sprayed[which(spray$sprayed=="")]<-NA
spray$sprayed<-as.factor(spray$sprayed)
#library(plyr)
#spray$sprayed <- mapvalues(spray$sprayed, from = c("", "Spraying"), to = c("0", "1"))
summary(spray)

nest <- nest %>% 
  pivot_longer(c(8:13), names_to = "year", values_to = "nest")
head(nest)
#nest$year[which(nest$year=="Nest.removal_2012")]<-2012
nest$year[which(nest$year=="Nest.removal_2013")]<-2013
nest$year[which(nest$year=="Nest.removal_2014")]<-2014
nest$year[which(nest$year=="Nest.removal_2015")]<-2015
nest$year[which(nest$year=="Nest.removal_2016")]<-2016
nest$year[which(nest$year=="Nest.removal_2017")]<-2017
nest$year[which(nest$year=="Nest.removal_2018")]<-2018
nest$year[which(nest$year=="Nest.removal_2019")]<-2019
nest$year[which(nest$year=="Nest.removal_2020")]<-2020
nest$year<-as.factor(nest$year)
summary(nest)
levels(nest$nest)
nest$nest<-as.character(nest$nest)
nest$nest[which(nest$nest=="")]<-NA
nest$nest<-as.factor(nest$nest)
#library(plyr)
#nest$nest <- mapvalues(nest$nest, from = c("", "Infested", "Not infested", "Previously infested"), to = c("0", "1","0", "0"))
summary(nest)

t1 <- left_join(survey,status)
summary(t1)
t2 <- left_join(t1, SPHN)
t3 <- left_join(t2,spray)
sites2 <- left_join(t3,nest)
summary(sites2)
sites2$year<-as.factor(sites2$year)
levels(sites2$year)
summary(sites2)

# get stats per type?
# i.e. get combinations of site type/owner type, n(), %sprayed, %nest removed

str(sites)
sites_summary <- sites %>%
  na.omit() %>% 
  group_by(Site.type,Owner.type) %>% 
  summarise(n = n())
sites_summary <- sites_summary[with(sites_summary, order(-n)),]
sitesTot <- sum(sites_summary$n)
sites_summary <- sites_summary %>% 
  mutate(prop = n/sitesTot*100)

sites2summary <- sites2 %>% #gather(spray, year, sprayed, -siteID, -Site.name, -Site.type, -Owner.type, -Company, -Easting, -Northing) %>%
  group_by(Site.type,Owner.type, year, status) %>%
  tally() #%>% 
  spread(status, n, fill = 0)





#####
# mapping
#####
library(ggplot2)
ggplot(sites)+
  geom_point(aes(Easting,Northing,colour=Site.type))

library(ggmap)
# find locations googling "location coordinates"
london <- c(lon = 0.12, lat = 51.5)
# Get map at zoom level 5: map_5
map <- get_map(london, zoom = 10, scale = 1)
ggmap(map)
# nicer styles
stamen.lon <- get_map(london, zoom=10,scale=1,
                        maptype = "watercolor",
                        source = "stamen")
ggmap(stamen.lon)

# convert easting/northing to lat/long
## libraries
require(rgdal) # for spTransform
require(stringr)

### shortcuts
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

### Create coordinates variable
sites <- na.omit(sites)
coords <- cbind(Easting = as.numeric(as.character(sites$Easting)),
                Northing = as.numeric(as.character(sites$Northing)))

### Create the SpatialPointsDataFrame
dat_SP <- SpatialPointsDataFrame(coords,
                                 data = sites,
                                 proj4string = CRS("+init=epsg:27700"))

### Convert
dat_SP_LL <- spTransform(dat_SP, CRS(latlong))

## replace Lat, Long
dat_SP_LL@data$Long <- coordinates(dat_SP_LL)[, 1]
dat_SP_LL@data$Lat <- coordinates(dat_SP_LL)[, 2]

sitesLL <- as.data.frame(dat_SP_LL)

# plot
ggmap(map)+
  geom_point(aes(Long,Lat,colour=Site.type), data=sitesLL)

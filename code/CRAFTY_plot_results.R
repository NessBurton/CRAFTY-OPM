
library(tidyverse)

dirCRAFTY <- "C:/Users/vanessa.burton.sb/Documents/eclipse-workspace/CRAFTY_RangeshiftR/"
dirOut <- paste0(dirCRAFTY, "output")
setwd(dirCRAFTY)

# read in all results ----------------------------------------------------------

results <-
  list.files(path = "./output/",
             pattern = "*.csv", 
             full.names = T) %>% 
  grep("-Cell-", value=TRUE, .) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

# match back to hex grid -------------------------------------------------------

cellIDs <- read.csv(paste0(dirCRAFTY,"data-processed/Cell_ID_XY_Borough.csv"))
hexGrid <- st_read(paste0(dirCRAFTY,"data-processed/hexgrids/hexGrid40m.shp"))

hexGrid %>%
  mutate(Long = st_coordinates(st_centroid(.))[,1],
         Lat = st_coordinates(st_centroid(.))[,2]) %>% 
  st_drop_geometry()

results$joinID  = cellIDs$Cell_ID[match(results$X, cellIDs$X)]

results$geometry  = hexGrid$geometry[match(results$joinID, hexGrid$geometry)]

results2 <- filter(results, Tick==2020)
test <- merge(hexGrid, results2, by='joinID')

# plot -------------------------------------------------------------------------

ggplot(results)+
  #geom_raster(aes(X,Y,fill=Agent))+
  geom_bar(aes(Agent, fill=Agent))+
  facet_wrap(~Tick)

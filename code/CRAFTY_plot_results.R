
library(tidyverse)
library(foreach)
library(sf)
library(viridis)

dirCRAFTY <- "C:/Users/vanessa.burton.sb/Documents/eclipse-workspace/CRAFTY_RangeshiftR/"
dirOut <- paste0(dirCRAFTY, "output")
setwd(dirCRAFTY)

# figure directory
dirFigs <- "C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/figures/"

# read in all results ----------------------------------------------------------

dfResults <-
  list.files(path = "./output/",
             pattern = "*.csv", 
             full.names = T) %>% 
  grep("-Cell-", value=TRUE, .) %>% 
  #map_df(~read_csv(., col_types = cols(.default = "c")))
  map_df(~read.csv(.))

head(dfResults)
summary(dfResults)
dfResults$Tick <- factor(dfResults$Tick)
dfResults$Agent <- factor(dfResults$Agent)

# undo OPM inversion -----------------------------------------------------------

# inverted OPM presence capital 
invert <- dfResults$Capital.OPMinverted - 1
z <- abs(invert)
dfResults$OPMpresence <- z
# check OPM values/distribution
summary(dfResults$OPMpresence)
hist(dfResults$OPMpresence[which(dfResults$OPMpresence!=0)])

# bar plot agents --------------------------------------------------------------

agentSummary <- dfResults %>% 
  group_by(Tick,Agent) %>% 
  summarise(agentCount = length(Agent))

agentSummary %>% 
  #filter(Agent != "no_mgmt") %>% 
  ggplot()+
  geom_col(aes(x=Tick,y=agentCount, fill=Agent), position = "stack")#+
  #facet_wrap(~Tick)

# plot service provision through time ------------------------------------------

serviceSummary <- dfResults %>% 
  group_by(Tick,Agent) %>% 
  #group_by(Tick) %>% 
  summarise(biodiversity = mean(Service.biodiversity),
            recreation = mean(Service.recreation)) %>% 
  pivot_longer(., cols=3:4, names_to="service",values_to="provision")

serviceSummary$Tick <- as.numeric(as.character(serviceSummary$Tick))

serviceSummary %>% 
  ggplot()+
  geom_line(aes(x=Tick,y=provision,col=Agent))+
  facet_wrap(~service)

# plot capital levels through time ---------------------------------------------

capitalSummary <- dfResults %>% 
  group_by(Tick,Agent) %>% 
  #group_by(Tick) %>% 
  summarise(OPM = mean(OPMpresence),
            riskPerc = mean(Capital.riskPerc),
            budget = mean(Capital.budget),
            knowledge = mean(Capital.knowledge),
            nature = mean(Capital.nature),
            access = mean(Capital.access)) %>% 
  pivot_longer(., cols=3:8, names_to="capital",values_to="level")

capitalSummary$Tick <- as.numeric(as.character(capitalSummary$Tick))

capitalSummary %>% 
  ggplot()+
  geom_line(aes(x=Tick,y=level,col=capital))+
  facet_wrap(~Agent)

# plot competitiveness through time --------------------------------------------

AFTcomp <- read.csv(paste0(dirOut,"/Baseline-0-99-LondonBoroughs-AggregateAFTCompetitiveness.csv"))


# match back to hex grid -------------------------------------------------------

hexGrid <- st_read(paste0(dirCRAFTY,"data-processed/hexgrids/hexGrid40m.shp"))
london_xy_df <- read.csv(paste0(dirCRAFTY,"data-processed/Cell_ID_XY_Borough.csv"))
tick1 <- filter(dfResults, Tick==1)
val_xy <- data.frame(tick1$X,tick1$Y)
colnames(val_xy) <- c("X", "Y")
x_coord <- london_xy_df[match(val_xy$X, london_xy_df$X), "x_coord"]
y_coord <- london_xy_df[match(val_xy$Y, london_xy_df$Y), "y_coord"]

cellid <- foreach(rowid = 1:nrow(val_xy), .combine = "c") %do% { 
  which((as.numeric(val_xy[rowid, 1]) == london_xy_df$X) & (as.numeric(val_xy[rowid, 2]) == london_xy_df$Y))
}

tick1$joinID <- cellid
sfResult <- left_join(hexGrid, tick1, by="joinID")

# plot -------------------------------------------------------------------------

ggplot() +
  geom_sf(sfResult, mapping = aes(fill = Agent), col = NA)+
  scale_fill_brewer(palette="Dark2")
ggplot() +
  geom_sf(sfResult, mapping = aes(fill = Capital.OPMinverted), col = NA)+
  scale_fill_viridis()
ggplot() +
  geom_sf(sfResult, mapping = aes(fill = OPMpresence), col = NA)+
  scale_fill_viridis()+theme_minimal()

# facet plots

sfResult_lg <- pivot_longer(sfResult, cols = 6:7, names_to = "service", values_to = "provision") %>%
  pivot_longer(., cols=c(7:11,14), names_to="capital", values_to="level") %>% 
  st_as_sf()

# plot capital levels
png(paste0(dirFigs,"capitals_V4_tick1.png"), units="cm", width = 20, height = 18, res=1000)
ggplot(sfResult_lg) +
  geom_sf(mapping = aes(fill = level), col = NA)+
  scale_fill_viridis()+
  facet_wrap(~capital)+
  theme_minimal()
dev.off()

# plot service provision
png(paste0(dirFigs,"services_V4_tick1.png"), units="cm", width = 20, height = 18, res=1000)
ggplot(sfResult_lg) +
  geom_sf(mapping = aes(fill = provision), col = NA)+
  scale_fill_viridis()+
  facet_wrap(~service)+
  theme_minimal()
dev.off()

# plot agents at 3 timesteps
#brewer.pal(3, name = "Dark2")
agent.pal <- c("no_mgmt" = "grey",
               "mgmt_lowInt" = "#1B9E77",
               "mgmt_highInt" = "#D95F02")

# tick 1
png(paste0(dirFigs,"agents_V4_tick1.png"), units="cm", width = 20, height = 18, res=1000)
ggplot() +
  geom_sf(sfResult, mapping = aes(fill = Agent), col = NA)+
  scale_fill_manual(values=agent.pal)+
  theme_minimal()
dev.off()

tick5 <- filter(dfResults, Tick==5)
val_xy <- data.frame(tick5$X,tick5$Y)
colnames(val_xy) <- c("X", "Y")
x_coord <- london_xy_df[match(val_xy$X, london_xy_df$X), "x_coord"]
y_coord <- london_xy_df[match(val_xy$Y, london_xy_df$Y), "y_coord"]

cellid <- foreach(rowid = 1:nrow(val_xy), .combine = "c") %do% { 
  which((as.numeric(val_xy[rowid, 1]) == london_xy_df$X) & (as.numeric(val_xy[rowid, 2]) == london_xy_df$Y))
}

tick5$joinID <- cellid
sfTick5 <- left_join(hexGrid, tick5, by="joinID")

# tick 5
png(paste0(dirFigs,"agents_V4_tick5.png"), units="cm", width = 20, height = 18, res=1000)
ggplot() +
  geom_sf(sfTick5, mapping = aes(fill = Agent), col = NA)+
  scale_fill_manual(values=agent.pal)+
  theme_minimal()
dev.off()

tick10 <- filter(dfResults, Tick==10)
val_xy <- data.frame(tick10$X,tick10$Y)
colnames(val_xy) <- c("X", "Y")
x_coord <- london_xy_df[match(val_xy$X, london_xy_df$X), "x_coord"]
y_coord <- london_xy_df[match(val_xy$Y, london_xy_df$Y), "y_coord"]

cellid <- foreach(rowid = 1:nrow(val_xy), .combine = "c") %do% { 
  which((as.numeric(val_xy[rowid, 1]) == london_xy_df$X) & (as.numeric(val_xy[rowid, 2]) == london_xy_df$Y))
}

tick10$joinID <- cellid
sfTick10 <- left_join(hexGrid, tick10, by="joinID")

# tick 5
png(paste0(dirFigs,"agents_V4_tick10.png"), units="cm", width = 20, height = 18, res=1000)
ggplot() +
  geom_sf(sfTick10, mapping = aes(fill = Agent), col = NA)+
  scale_fill_manual(values=agent.pal)+
  theme_minimal()
dev.off()

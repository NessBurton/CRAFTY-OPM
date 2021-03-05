
library(tidyverse)
library(foreach)
library(sf)
library(viridis)

dirCRAFTY <- "C:/Users/vanessa.burton.sb/Documents/eclipse-workspace/CRAFTY_RangeshiftR/"
dirOut <- paste0(dirCRAFTY, "output")
setwd(dirCRAFTY)

# figure directory
dirFigs <- "C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/figures/"

agent.pal <- c("no_mgmt" = "grey",
               "mgmt_lowInt" = "#1B9E77",
               "mgmt_highInt" = "#D95F02")

# read in all results ----------------------------------------------------------

# scenarios
scenarioList <- c("Baseline","de-regulation","govt-intervention")
#scenarioList <- "de-regulation"

for (i in scenarioList){
  
  i <- scenarioList[2]
  
  dfResults <-
    list.files(path = paste0(dirOut,"/V4/",i,"/"),
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
  
  # bar plot agents --------------------------------------------------------------
  
  agentSummary <- dfResults %>% 
    group_by(Tick,Agent) %>% 
    summarise(agentCount = length(Agent)) %>% 
    ungroup() %>% 
    group_by(Tick) %>% 
    mutate(tot=sum(agentCount),
           perc=agentCount/tot*100)
  
  p1 <- agentSummary %>% 
    #filter(Agent != "no_mgmt") %>% 
    ggplot()+
    geom_col(aes(x=Tick,y=perc, fill=Agent), position = "stack")+
    scale_fill_manual(values=agent.pal)+
    ylab("Percentage of area (%)")+xlab("Year")+
    theme_bw()
  
  png(paste0(dirFigs,"agentBarPlot_",i,".png"), units="cm", width = 12, height = 8, res=1000)
  print(p1)
  dev.off()
  
  # plot service provision through time ------------------------------------------
  
  serviceSummary <- dfResults %>% 
    group_by(Tick,Agent) %>% 
    #group_by(Tick) %>% 
    summarise(biodiversity = mean(Service.biodiversity),
              recreation = mean(Service.recreation)) %>% 
    pivot_longer(., cols=3:4, names_to="service",values_to="provision")
  
  serviceSummary$Tick <- as.numeric(as.character(serviceSummary$Tick))
  
  p2 <- serviceSummary %>% 
    ggplot()+
    geom_line(aes(x=Tick,y=provision,col=Agent))+
    scale_color_manual(values=agent.pal)+
    facet_wrap(~service)+
    ylim(c(0,1))+ylab("Service provision")+
    scale_x_continuous("Year",n.breaks = 10)+
    theme_bw()
  
  png(paste0(dirFigs,"servicesLinePlot_",i,".png"), units="cm", width = 12, height = 6, res=1000)
  print(p2)
  dev.off()
  
  p2a <- serviceSummary %>% 
    ggplot()+
    geom_smooth(aes(x=Tick,y=provision,col=Agent))+
    scale_color_manual(values=agent.pal)+
    facet_wrap(~service)+
    ylim(c(0,1))+ylab("Service provision")+
    scale_x_continuous("Year",n.breaks = 10)+
    theme_bw()
  
  png(paste0(dirFigs,"servicesLinePlot_",i,"2.png"), units="cm", width = 12, height = 6, res=1000)
  print(p2a)
  dev.off()
  
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
  
  p3 <- capitalSummary %>% 
    ggplot()+
    geom_line(aes(x=Tick,y=level,col=capital))+
    scale_color_brewer(palette = "Dark2")+
    facet_wrap(~Agent)+
    ylim(c(0,1))+ylab("Capital level")+
    scale_x_continuous("Year",n.breaks = 10)+
    theme_bw()
  
  png(paste0(dirFigs,"capitalsLinePlot_",i,".png"), units="cm", width = 18, height = 8, res=1000)
  print(p3)
  dev.off()
  
  
  # plot competitiveness through time --------------------------------------------
  
  #AFTcomp <- read.csv(paste0(dirOut,"/V4/Baseline/Baseline-0-99-LondonBoroughs-AggregateAFTCompetitiveness.csv"))
  # issue with competitiveness file?
  
  # plot agents at 3 timesteps -------------------------------------------------
  
  hexGrid <- st_read(paste0(dirCRAFTY,"data-processed/hexgrids/hexGrid40m.shp"))
  london_xy_df <- read.csv(paste0(dirCRAFTY,"data-processed/Cell_ID_XY_Borough.csv"))
  
  # tick 1
  tick1 <- filter(dfResults, Tick==1)
  val_xy <- data.frame(tick1$X,tick1$Y)
  colnames(val_xy) <- c("X", "Y")
  x_coord <- london_xy_df[match(val_xy$X, london_xy_df$X), "x_coord"]
  y_coord <- london_xy_df[match(val_xy$Y, london_xy_df$Y), "y_coord"]
  
  cellid <- foreach(rowid = 1:nrow(val_xy), .combine = "c") %do% { 
    which((as.numeric(val_xy[rowid, 1]) == london_xy_df$X) & (as.numeric(val_xy[rowid, 2]) == london_xy_df$Y))
  }
  
  tick1$joinID <- cellid
  sfTick1 <- left_join(hexGrid, tick1, by="joinID")
  
  png(paste0(dirFigs,"agents_",i,"_tick1.png"), units="cm", width = 12, height = 10, res=1000)
  print(ggplot() +
    geom_sf(sfTick1, mapping = aes(fill = Agent), col = NA)+
    scale_fill_manual(values=agent.pal)+
    theme_bw())
  dev.off()
  
  
  # tick 5
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
  
  png(paste0(dirFigs,"agents_",i,"_tick5.png"), units="cm", width = 12, height = 10, res=1000)
  print(ggplot() +
    geom_sf(sfTick5, mapping = aes(fill = Agent), col = NA)+
    scale_fill_manual(values=agent.pal)+
    theme_bw())
  dev.off()
  
  
  # tick 10
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
  
  png(paste0(dirFigs,"agents_",i,"_tick10.png"), units="cm", width = 12, height = 10, res=1000)
  print(ggplot() +
    geom_sf(sfTick10, mapping = aes(fill = Agent), col = NA)+
    scale_fill_manual(values=agent.pal)+
    theme_bw())
  dev.off()
  
  png(paste0(dirFigs,"capitals_",i,".png"), units="cm", width = 18, height = 14, res=1000)
  print(tick10 %>% 
    pivot_longer(cols = starts_with("Capital"),
                 names_to="Capital",values_to="Level") %>% 
    left_join(., hexGrid, by="joinID") %>% 
    st_as_sf() %>% 
    ggplot()+
    geom_sf(aes(fill=Level),col=NA)+
    scale_fill_viridis()+
    facet_wrap(~ Capital)+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()))
  dev.off()
  
  # plot capitals through time per region
  dfResults$joinID <- rep(cellid, 10)
  sfResults <- left_join(dfResults, hexGrid, by="joinID") 
  sfResults$Tick <- as.numeric(sfResults$Tick)
  
  png(paste0(dirFigs,"capitalsPerBorough_",i,".png"), units="cm", width = 18, height = 12, res=1000)
  print(sfResults %>% 
    group_by(borough,Tick) %>% 
    summarise(OPM = mean(OPMpresence),
              riskPerc = mean(Capital.riskPerc),
              budget = mean(Capital.budget),
              knowledge = mean(Capital.knowledge),
              nature = mean(Capital.nature),
              access = mean(Capital.access)) %>% 
    pivot_longer(., cols=3:8, names_to="capital",values_to="level") %>% 
    ggplot(aes(Tick,level,colour=capital))+
    geom_line(position=position_jitter(w=0, h=0.01))+
    scale_color_brewer(palette = "Dark2")+
    facet_wrap(~borough)+
    theme_bw()+
    ylim(c(0,1))+ylab("Capital level")+
    scale_x_continuous("Year",n.breaks = 10))
  dev.off()
  
}

### RangeshiftR populations ----------------------------------------------------

dfRS_stlone <- read.csv(paste0(dirOut,"/development/dfRangeshiftR_output_RsftR_standalone.csv"))
dfRS_stlone$models <- "Uncoupled"
dfRS_stlone$scenario <- "No management"

dfRS_baseline <- read.csv(paste0(dirOut,"/dfRangeshiftR_output_coupled_Baseline.csv"))
dfRS_baseline$models <- "Coupled"
dfRS_baseline$scenario <- "Baseline"

dfRS_dereg <- read.csv(paste0(dirOut,"/dfRangeshiftR_output_coupled_de-regulation.csv"))
dfRS_dereg$models <- "Coupled"
dfRS_dereg$scenario <- "De-regulation"

dfRS_govt <- read.csv(paste0(dirOut,"/dfRangeshiftR_output_coupled_govt-intervention.csv"))
dfRS_govt$models <- "Coupled"
dfRS_govt$scenario <- "Govt-Intervention"

#dfRsftR_all <- rbind(dfRS_stlone,dfRS_baseline,dfRS_dereg,dfRS_govt)
dfRsftR_all <- rbind(dfRS_baseline,dfRS_dereg,dfRS_govt)
head(dfRsftR_all)
dfRsftR_all$models <- factor(dfRsftR_all$models, ordered = T, levels = c("Uncoupled","Coupled"))
#dfRsftR_all$scenario <- factor(dfRsftR_all$scenario, ordered = T, levels = c("No management","Baseline","De-regulation","Govt-Intervention"))
dfRsftR_all$scenario <- factor(dfRsftR_all$scenario, ordered = T, levels = c("Baseline","De-regulation","Govt-Intervention"))

png(paste0(dirFigs,"rangeshiftR_individuals_per_scenario.png"), units="cm", width = 12, height = 8, res=1000)
dfRsftR_all %>% filter(Year==2) %>%
  filter(!is.na(scenario)) %>% 
  ggplot(aes(timestep,NInds,color=scenario))+
  geom_smooth(position=position_jitter(w=0.02, h=0.05))+
  #facet_wrap(~scenario)+
  scale_x_continuous(breaks=seq(1,10,1))+
  xlab("Year")+ylab("Total number of individuals in landscape")+
  theme_bw()+theme(text = element_text(size=12, family = "Roboto"),
                   axis.text=element_text(size=8, family = "Roboto"),
                   axis.title=element_text(size=10,face="bold", family = "Roboto"),
                   axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                   axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
dev.off()


### raster pops ----------------------------------------------------------------

library(raster)

rstBaseline <- stack(paste0(dirOut,"/rstRangeshiftR_output_coupled_Baseline.tif"))
rstDereg <- stack(paste0(dirOut,"/rstRangeshiftR_output_coupled_de-regulation.tif"))
rstGovt <- stack(paste0(dirOut,"/rstRangeshiftR_output_coupled_govt-intervention.tif"))

names(rstBaseline) <- c("Yr1","Yr2","Yr3","Yr4","Yr5","Yr6","Yr7","Yr8","Yr9","Yr10")
clrs.viridis <- colorRampPalette(viridis::viridis(10))
png(paste0(dirFigs,"/rsftr_pops_CRAFTY-coupled_baseline.png"), width = 800, height = 600)
spplot(rstBaseline, layout = c(5,2), col.regions=clrs.viridis(14), at = seq(0,70,10))
dev.off()

names(rstDereg) <- c("Yr1","Yr2","Yr3","Yr4","Yr5","Yr6","Yr7","Yr8","Yr9","Yr10")
clrs.viridis <- colorRampPalette(viridis::viridis(10))
png(paste0(dirFigs,"/rsftr_pops_CRAFTY-coupled_de-regulation.png"), width = 800, height = 600)
spplot(rstDereg, layout = c(5,2), col.regions=clrs.viridis(14), at = seq(0,70,10))
dev.off()

names(rstGovt) <- c("Yr1","Yr2","Yr3","Yr4","Yr5","Yr6","Yr7","Yr8","Yr9","Yr10")
clrs.viridis <- colorRampPalette(viridis::viridis(10))
png(paste0(dirFigs,"/rsftr_pops_CRAFTY-coupled_govt-intervention.png"), width = 800, height = 600)
spplot(rstGovt, layout = c(5,2), col.regions=clrs.viridis(14), at = seq(0,70,10))
dev.off()


### service prod per scenario --------------------------------------------------

# scenarios
scenarioList <- c("Baseline","de-regulation","govt-intervention")
#scenarioList <- "de-regulation"

dfMaster <- data.frame()

for (i in scenarioList){
  
  #i <- scenarioList[3]
  dfResults <-
    list.files(path = paste0(dirOut,"/V4/",i,"/"),
               pattern = "*.csv", 
               full.names = T) %>% 
    grep("-Cell-", value=TRUE, .) %>% 
    #map_df(~read_csv(., col_types = cols(.default = "c")))
    map_df(~read.csv(.))
  
  dfResults$Tick <- factor(dfResults$Tick)
  dfResults$Agent <- factor(dfResults$Agent)
  dfResults$scenario <- i
  
  dfMaster <- rbind(dfMaster,dfResults)
  
}

head(dfMaster)
dfMaster$scenario <- factor(dfMaster$scenario, ordered = T, levels=scenarioList)
summary(dfMaster)

dfMaster$Tick <- as.numeric(dfMaster$Tick)

ggplot(dfMaster)+
  geom_col(aes(Tick,Service.biodiversity,fill=Agent))+
  facet_wrap(~scenario)

png(paste0(dirFigs,"services_per_scenario.png"), units="cm", width = 16, height = 8, res=1000)
dfMaster %>% pivot_longer(cols = Service.biodiversity:Service.recreation,
                          names_to = "Benefit", values_to = "Value") %>% 
  group_by(scenario, Benefit, Tick) %>% 
  summarise(Value = mean(Value)) %>% 
  ggplot(aes(Tick,Value,col=scenario))+
  geom_line(lwd=1)+
  facet_wrap(~Benefit)+
  ylim(c(0,1))+ylab("Service level")+
  scale_x_continuous("Year",n.breaks = 10)+
  theme_bw()
dev.off()

dfMaster %>% pivot_longer(cols = Capital.OPMinverted:Capital.access,
                          names_to = "Capital", values_to = "Value") %>% 
  group_by(scenario, Capital, Tick) %>% 
  summarise(Value = mean(Value)) %>% 
  ggplot(aes(Tick,Value,col=scenario))+
  geom_line(position=position_jitter(),lwd=1)+
  facet_wrap(~Capital)+
  ylim(c(0,1))+ylab("Capital level")+
  scale_x_continuous("Year",n.breaks = 10)+
  theme_bw()


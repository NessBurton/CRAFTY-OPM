
library(RangeshiftR)
library(AlgDesign)
library(raster)
library(tidyverse)

wd <- "~/CRAFTY-opm"# sandbox VM
dirOut <- file.path(wd, 'data-processed')
dirData <- file.path(dirOut, 'for-rangeshiftR') 

# paths for folders required by rangeshiftR
dirRsftr <- file.path(wd, 'rangeshiftR')
dirRsftrInput <- file.path(dirRsftr,"Inputs")
dirRsftrOutput <- file.path(dirRsftr,"Outputs")
dirRsftrOutputMaps <- file.path(dirRsftr,"Output_Maps")
dirRsftr <- file.path('C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/rangeshiftR/') # need to add the / for this path to work in RunRS

# ---------------------------------------------------
# high/low options for each parameter

dfSensitivity <- gen.factorial(levels = 2, nVars = 3, varNames=c("K","Rmax","Dispersal"))
dfSensitivity
dfSensitivity$K[which(dfSensitivity$K==-1)] <- 20 # from Synes et al. 2020 supplementary for invertebrates
dfSensitivity$K[which(dfSensitivity$K==1)] <- 50 # from Synes et al. 2020 supplementary for invertebrates
dfSensitivity$Rmax[which(dfSensitivity$Rmax==-1)] <- 10 # from Synes et al. 2020 supplementary for invertebrates
dfSensitivity$Rmax[which(dfSensitivity$Rmax==1)] <- 25 # from Synes et al. 2020 supplementary for invertebrates
dfSensitivity$Dispersal[which(dfSensitivity$Dispersal==-1)] <- 800 # from Cowley et al. 2015
dfSensitivity$Dispersal[which(dfSensitivity$Dispersal==1)] <- 1000 # higher option

dfSensitivity <- tibble::rowid_to_column(dfSensitivity, "ID")

write.csv(dfSensitivity, paste0(dirOut,"/sensitivity_analysis_rangeshiftR/rsftr_parameters_2level_factorial.csv"))

# ----------------------------------------------------
# set up parameters

rstHabitat <- raster(file.path(dirRsftrInput, 'Habitat-2m.tif'))
rasterizeRes <- 2
habitatRes <- 100
rstModal <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=modal)
rstHabitat <- rstModal

rangeshiftrYears <- 10

# initial individuals file.
# created lots of options for this (2012-2015, varying numbers of Ninds)
init <- Initialise(InitType=2, InitIndsFile='initial_inds_2014_n25.txt')

# ----------------------------------------------------
# Create empty data frame and raster stack to store the output data
dfRangeShiftrData <- data.frame()
outRasterStack <- stack()

# loop through
for (i in c(1:nrow(dfSensitivity))) {
  
  #params <- dfSensitivity[1,] # test
  params <- dfSensitivity[i,] 
  ID <- params[[1]]
  
  sim <- Simulation(Simulation = ID,
                    Years = rangeshiftrYears,
                    Replicates = 20,
                    OutIntPop = 1, 
                    OutIntInd = 1, 
                    OutIntOcc = 1,
                    ReturnPopRaster = TRUE)
  
  land <- ImportedLandscape(LandscapeFile=sprintf('Habitat-%sm.asc', habitatRes),
                            Resolution=habitatRes,
                            HabitatQuality=TRUE,
                            K=params[[2]]) 
  
  demo <- Demography(Rmax = params[[3]],
                     ReproductionType = 0)
  
  disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                     Transfer   = DispersalKernel(Distances = params[[4]]),
                     Settlement = Settlement() )
  
  s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, init = init)
  
  # run and store raster result
  rstResult <- RunRS(s, sprintf('%s/',dirRsftr))
  
  # work out what i want from raster data
  # average of replications per year?
  
  crs(rstResult) <- rstHabitat
  extent(rstResult) <- rstHabitat
  #names(rstResult)
  
  outRasterStack <- stack()
  
  for (j in c(0:9)){
    
    #j <- 1
    
    annualReps <- subset(rstResult, grep(paste0("year",j), names(rstResult), value = T))
    
    annualMean <- calc(annualReps, fun = mean, na.rm=T)
    #plot(annualMean)
    names(annualMean) <- paste0("year.",j)
    
    outRasterStack <- addLayer(outRasterStack, annualMean)
    outRasterStack
    
  }
  
  # same breaks for comparing between plots
  #library(classInt)
  #library(RColorBrewer)
  breaks <- classIntervals(0:50, n=10, style="quantile",intervalClosure="right")
  #pop.palette <- brewer.pal(n = 8, name = "YlGn")
  
  # save plot per sensitivity simulation
  png(paste0("~/CRAFTY-opm/figures/rsftr_avgPop_20reps_dfSensitity_",ID,".png"), width = 800, height = 800)
  #plot(outRasterStack, zlim=c(0,50), nc=5, nr = 2)
  #print(spplot(outRasterStack, layout=c(5,2), at = breaks$brks))
  print(spplot(outRasterStack, at=breaks$brks, zlim=c(0,50), layout=c(5,2)))
  dev.off()

  # store population data in our output data frame.
  dfRange <- readRange(s, sprintf('%s/',dirRsftr))
  dfRange$ID <- ID
  dfRangeShiftrData <- rbind(dfRangeShiftrData, dfRange)
  
  # occupancy
  #dfOcc <- read.table(file.path(dirRsftrOutput, sprintf('Batch1_Sim%s_Land1_Occupancy_Stats.txt', ID)))
  
}

View(dfRangeShiftrData)
summary(dfRangeShiftrData)
dfRangeShiftrData <- left_join(dfRangeShiftrData,dfSensitivity,by='ID')

write.csv(dfRangeShiftrData, paste0(dirOut,"/sensitivity_analysis_rangeshiftR/df_Sensitivity_anaysis2.csv"))
dfRangeShiftrData <- read.csv(paste0(dirOut,"/sensitivity_analysis_rangeshiftR/df_Sensitivity_anaysis2.csv"))

# look at effect of parameters (including all years, each with 20 reps)

K_labs <- list('20'="K = 20",'50'="K = 50")
K_labeller <- function(variable,value){
  return(K_labs[value])
}

dfRangeShiftrData$K <- as.factor(dfRangeShiftrData$K)
levels(dfRangeShiftrData$K)

ggplot(dfRangeShiftrData)+
  geom_boxplot(aes(x=factor(Rmax), y=NOccupCells, col=factor(Dispersal)))+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~K, labeller = (K=K_labeller))+
  ylab("Number of occupied cells")+xlab("Rmax")+
  labs(col = "Dispersal")

ggplot(dfRangeShiftrData)+
  geom_boxplot(aes(x=factor(Rmax), y=Occup.Suit, col=factor(Dispersal)))+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~factor(K))+
  ylab("Occupancy")


# create test OPM presence for CRAFTY
# i.e. run a single simulation
# extract results per year to hexgrid points


rstHabitat <- raster(file.path(dirRsftrInput, 'Habitat-2m.tif'))
rasterizeRes <- 2
habitatRes <- 100
rstModal <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=modal)
rstHabitat <- rstModal

rangeshiftrYears <- 10

init <- Initialise(InitType=2, InitIndsFile='initial_inds_2014_n25.txt')

sim <- Simulation(Simulation = 3,
                  Years = rangeshiftrYears,
                  Replicates = 1,
                  OutIntPop = 1, # interval for output of population data
                  OutIntInd = 1, # interval for output of individual data
                  ReturnPopRaster=TRUE) 

land <- ImportedLandscape(LandscapeFile=sprintf('Habitat-%sm.asc', habitatRes),
                          Resolution=habitatRes,
                          HabitatQuality=TRUE,
                          K=50) # carrying capacity (individuals per hectare) when habitat at 100% quality

demo <- Demography(Rmax = 25,
                   ReproductionType = 0) # 0 = asexual / only female; 1 = simple sexual; 2 = sexual model with explicit mating system

disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                   Transfer   = DispersalKernel(Distances = 1500), # test getting to top of landscape while keeping other params low
                   Settlement = Settlement() )

# Setup the simulation with the parameters defined above.
s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, init = init)
validateRSparams(s)

# run simulation
result <- RunRS(s, sprintf('%s/', dirpath = dirRsftr))
crs(result) <- crs(rstHabitat)
extent(result) <- extent(rstHabitat)
plot(result[[10]])
spplot(result)

# extract each year to hex points
# make sure ones for CRAFTY with non-greenspace filtered out

hexPoints <- st_read(paste0(dirOut,"/hexGrids/hexPoints40m.shp"))
hexPointsSP <- as_Spatial(hexPoints)

hexPointsOPM <- raster::extract(result, hexPointsSP)

hexPointsOPM <- cbind(hexPointsSP,hexPointsOPM)

# join to hexagons
hexGrid <- st_read(paste0(dirOut,"/hexGrids/hexGrid40m_types2.shp"))
hexGrid <- as.data.frame(hexGrid)
hexGrid <- merge(hexGrid, hexPointsOPM, by="joinID")
hexGrid <- st_as_sf(hexGrid)

# check
hexGrid %>% 
  filter(type != "Non.greenspace") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = rep0_year5), col = NA)

head(hexGrid[,c(1,21:31)])

hexGrid <- hexGrid[,c(1,21:31)] %>% 
  st_drop_geometry()

write.csv(hexGrid, paste0(dirOut,"/hexGrids/hexG_rangeshiftR_test.csv"))

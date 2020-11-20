
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
  
  #params <- dfSensitivity[2,] # test
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
  names(rstResult)
  outRasterStack <- addLayer(outRasterStack, rstResult)
  
  #read in population file
  #dfPop <- read.table(file.path(dirRsftrOutput, sprintf('Batch1_Sim%s_Land1_Pop.txt', ID)), header=TRUE)
  #dfPop <- subset(dfPop, Year == rangeshiftrYears-1)
  
  #dfPop_summary <- dfPop %>% 
    #filter(Year != 0) %>% 
    #group_by(Year,x,y) %>% 
    #summarise(NIndAvg = mean(NInd)) %>% 
    #pivot_wider(id_cols = c("x","y"), names_from=Year, values_from=NIndAvg)
  
  # make a raster from the data frame
  #stackYrs_allReps <- rasterFromXYZ(dfPop_summary)
  #names(stackYrs_allReps) <- c('Year1', 'Year2', "Year3", "Year4", "Year5",
   #                            "Year6", "Year7", "Year8", "Year9", "Year10")
  # Not all years have the same number of populated and thus listed cells. For stacking, we set a common extent with the values used in the landscape module:
  #ext <- extent(rstHabitat)
  #if(sum(as.matrix(extent(stackYrs_allReps))!=as.matrix(ext)) == 0){ 
    #stackYrs_allReps <- extend(stackYrs_allReps,ext)
  #}
    
  #spplot(stackYrs_allReps)#, zlim = c(0,7))
  #outRasterStack <- addLayer(outRasterStack, stackYrs_allReps)

  # store population data in our output data frame.
  dfRange <- readRange(s, sprintf('%s/',dirRsftr))
  dfRange$ID <- ID
  dfRangeShiftrData <- rbind(dfRangeShiftrData, dfRange)
  
  # occupancy
  #dfOcc <- read.table(file.path(dirRsftrOutput, sprintf('Batch1_Sim%s_Land1_Occupancy_Stats.txt', ID)))
  
}

View(dfRangeShiftrData)

dfRangeShiftrData <- left_join(dfRangeShiftrData,dfSensitivity,by='ID')

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

names(outRasterStack)
# name based on ID from dfSensitivity table.
# is it possible to group layers within raster stack and calculate average per year across all reps?
# plot each with title showing parameter values
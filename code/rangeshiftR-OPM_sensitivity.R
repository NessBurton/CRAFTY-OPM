
library(RangeshiftR)
library(AlgDesign)
library(raster)
library(tidyverse)

wd <- "~/CRAFTY-opm"# sandbox VM
#setwd(wd)
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

dfSensitivity <- tibble::rowid_to_column(dfSensitivity, "testID")

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
  
  params <- dfSensitivity[2,] # test
  #params <- dfSensitivity[i,] 
  ID <- params[[1]]
  
  sim <- Simulation(Simulation = ID,
                    Years = rangeshiftrYears,
                    Replicates = 20,
                    OutIntPop = 1, 
                    OutIntInd = 1, 
                    OutIntOcc = 1)
  
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
  
  # run 
  RunRS(s, sprintf('%s/',dirRsftr))
  
  # read in population file
  dfPop <- read.table(file.path(dirRsftrOutput, sprintf('Batch1_Sim%s_Land1_Pop.txt', ID)), header=TRUE)
  #dfPop <- subset(dfPop, Year == rangeshiftrYears-1)
  
  # Make stack of different raster layers for each year and for only one repetition (Rep==0):
  dfPop_rep0 <- reshape(subset(dfPop,Rep==0)[,c('Year','x','y','NInd')], timevar='Year', v.names=c('NInd'), idvar=c('x','y'), direction='wide')
  
  # make a raster from the data frame
  stack_years_rep0 <- rasterFromXYZ(dfPop_rep0)
  #names(stack_years_rep0) <- c('Year.0', 'Year.50')
  spplot(stack_years_rep0)#, zlim = c(0,7))

  
  # Store RangeShiftR's population data in our output data frame.
  dfRange <- readRange(s, sprintf('%s/',dirRsftr))
  dfRange$ID <- ID
  dfRangeShiftrData <- rbind(dfRangeShiftrData, dfRange)
  
}

View(dfRangeShiftrData)

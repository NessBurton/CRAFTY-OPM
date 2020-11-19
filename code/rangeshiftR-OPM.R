
# libraries
library(rgdal)
library(raster)
library(RangeshiftR)
library(sf)
library(tidyverse)
library(ggplot2)

#wd <- "~/R/CRAFTY-OPM" # FR
#wd <- "/Users/Vanessa/Documents/crafty-opm" # mac
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
#dir.create(dirRsftrInput)
#dir.create(dirRsftrOuput)
#dir.create(dirRsftrOutputMaps)

#####
# load in habitat data
#####
# this is a 40m hexagonal grid which has a quality code assigned already ("suit" field)
# 100% = oak tree present (OPM survey data), or woodland, non-coniferous primary category in OSMM
# 95% = woodland, non-coniferous secondary category in OSMM
# 90% = woodland, non-coniferous tertiary category in OSMM
# 85% = other woodland (based on greenspace data, if not covered by OSMM above)
# 0% = open semi-natural (greenspace)
# 0% = multi-surface (greenspace) - this is how most gardens are classified
# 0% = all remaining areas
shpHabitat <- st_read(dirData, layer = "hexGrid_OPM_suitability")

# have a look
ggplot(data = shpHabitat) +
  geom_sf(aes(fill=suit),col=NA)

rasterizeRes <- 2 # resolution at which to rasterise the habitat polygons
habitatRes <- 100 # Habitat resolution for rangeshifter

# rasterise at 2m
# slow process tp rasterise at fine resolution, so check if file already exists
#if ( !file.exists(file.path(dirRsftrInput, sprintf('Habitat-%sm.tif', rasterizeRes)))) {
  #rstHabitat <- rasterize(x=shpHabitat,
                          #y=raster(extent(shpHabitat), res=rasterizeRes),
                          #field='suit',
                          #fun=min, # where there are overlapping polygons, use lowest value (is there a better way to choose?)
                          #update=TRUE)
  #writeRaster(rstHabitat, file.path(dirRsftrInput, sprintf('Habitat-%sm.tif', rasterizeRes)), overwrite=TRUE)
#}

# read in and aggregate
rstHabitat <- raster(file.path(dirRsftrInput, sprintf('Habitat-%sm.tif', rasterizeRes)))
#plot(rstHabitat)
#rstMin <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=min)
#rstMax <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=max)
#rstMean <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=mean)
rstModal <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=modal)
# need to decide whether to use fun=min or fun=max here. min underestimates a lot but max probably overestimates too much
#plot(rstMin)
#plot(rstMax)
#plot(rstMean)
#plot(rstModal) # most frequent value - seems to represent the habitat the best, go with this
# doesn't overestimate and still picks up some smaller areas
# but definitely loses smaller areas

rstHabitat <- rstModal

# export as ascii file for RangeShifter.
# be sure to specify -9999 as the no data value (NAflag argument)
#writeRaster(rstHabitat, file.path(dirRsftrInput, sprintf('Habitat-%sm.asc', habitatRes)), format="ascii", overwrite=TRUE, NAflag=-9999)
#rstHabitat <- raster(file.path(dirRsftrInput, sprintf('Habitat-%sm.asc', habitatRes)))

#####
# species locations
#####
csvSpecies <- read.csv(file=file.path(dirData, 'opm_trees.csv'), header=TRUE, sep=",")

# subset to  trees that were infested or previously infested
# keep previously infested based on assumption also made in Cowley et al. (2015)
# "Given that OPM was continually spreading during our period of study...
# ...we made the assumption that once detected in a location, 
# OPM remained present throughout the period"
# need to choose 'best' year

# write a few different initial individual files and compare
years <- as.numeric(c(2012,2013,2014,2015))
# also test a few options for number of initial individuals
Ninds <- as.numeric(c(10,25,50,75))

for (i in years){ 
  
  #i <- years[1]
  csvSpecies <- read.csv(file=file.path(dirData, 'opm_trees.csv'), header=TRUE, sep=",")
  csvSpecies <- subset(csvSpecies, SurveyYear == i)
  csvSpecies <- subset(csvSpecies, Status == 'Infested' | Status == "Previously infested")
  
  # convert to shapefile
  csvSpecies <- csvSpecies[!is.na(csvSpecies$Easting), ]
  csvSpecies <- csvSpecies[!is.na(csvSpecies$Northing), ]
  shpInitialIndividuals <- csvSpecies
  rm(csvSpecies)
  coordinates(shpInitialIndividuals) <- ~Easting+Northing
  projection(shpInitialIndividuals) <- crs(shpHabitat)
  shapefile(shpInitialIndividuals, file.path(dirData, paste0('opm_trees_',i,'.shp')), overwrite=TRUE)
  
  # crop to species locations only within the study landscape
  shpInitialIndividuals <- crop(shpInitialIndividuals, extent(shpHabitat))
  shpInitialIndividuals$n <- 1
  
  # check and write to file
  shpInitialIndividuals <- st_as_sf(shpInitialIndividuals)
  png(paste0(dirRsftrOutputMaps,'/initial_individuals_',i,'.png'), width = 800, height = 800)
  ggplot(data = shpHabitat) +
    geom_sf(aes(fill=suit),col=NA) +
    geom_sf(data = shpInitialIndividuals)
  dev.off()
  
  # need to rasterise then extract the species locations to get the xy, row/col (not spatial) indices for rangeshifter.
  rstInitIndividuals <- rasterize(shpInitialIndividuals, rstHabitat, field='n', background=0)
  #plot(rstInitIndividuals)
  dfInitialIndividuals <- extract(rasterize(shpInitialIndividuals, rstHabitat, field='n', background=0), shpInitialIndividuals, cellnumbers=T, df=TRUE)
  
  # rangeshiftR requires a specific format for the individuals file, so add the required columns here,
  # and convert 'cells' value to x/y, row/col values.
  # as an example we are just initialising each cell with 100 individuals - we may need to adjust this later.
  dfInitialIndividuals$Year <- 0
  dfInitialIndividuals$Species <- 0
  dfInitialIndividuals$X <- dfInitialIndividuals$cells %% ncol(rstHabitat)
  dfInitialIndividuals$Y <- nrow(rstHabitat) - (floor(dfInitialIndividuals$cells / ncol(rstHabitat)))
  
  for (j in Ninds){
    
    #j <- Ninds[1]
    
    dfInitialIndividuals$Ninds <- j 
    
    dfInitialIndividuals <- dfInitialIndividuals[ , !(names(dfInitialIndividuals) %in% c('ID', 'cells', 'layer'))]
    
    # make sure individuals aren't being counted more than once in the same location (due to multiple tree id points)
    dfInitialIndividuals <- unique(dfInitialIndividuals)
    
    write.table(dfInitialIndividuals, file.path(dirRsftrInput, paste0('initial_inds_',i,'_n',j,'.txt')), row.names = F, quote = F, sep = '\t')
    
  }
  
}


# check
#init2012_n10 <- read.delim(paste0(dirRsftrInput,"/initial_inds_2012_n10.txt"), header = TRUE, sep = "\t", dec = ".",)
#init2012_n75 <- read.delim(paste0(dirRsftrInput,"/initial_inds_2012_n75.txt"), header = TRUE, sep = "\t", dec = ".",)
#init2014_n10 <- read.delim(paste0(dirRsftrInput,"/initial_inds_2014_n10.txt"), header = TRUE, sep = "\t", dec = ".",)
#init2014_n75 <- read.delim(paste0(dirRsftrInput,"/initial_inds_2014_n75.txt"), header = TRUE, sep = "\t", dec = ".",)


#####
# parameter set-up
#####
# we need to simulate at least 2 years of rangeshiftR for each CRAFTY iteration.
rangeshiftrYears <- 10

sim <- Simulation(Simulation = 1,
                  Years = rangeshiftrYears,
                  Replicates = 1,
                  OutIntPop = 1, # interval for output of population data
                  #OutIntInd = 1, # interval for output of individual data
                  ReturnPopRaster=TRUE) # We need RangeShiftR to return a raster with the population so we can use it for CRAFTY

land <- ImportedLandscape(LandscapeFile=sprintf('Habitat-%sm.asc', habitatRes),
                          #LandscapeFile='Habitat-2m_arc.asc',
                          Resolution=habitatRes,
                          HabitatQuality=TRUE,
                          K=100) # carrying capacity (individuals per hectare) when habitat at 100% quality

# We have often used ReproductionType=0 for invertebrate species in RangeShifter.
# It doesn't mean the species is asexual, just that we are modelling only the females - assumming that the species mates
# upon emergence in the natal patch, then fertilised females disperse.
# Rmax (intrinsic growth rate) - we don't have a good basis for this value yet.
demo <- Demography(Rmax = 10,
                   ReproductionType = 0) # 0 = asexual / only female; 1 = simple sexual; 2 = sexual model with explicit mating system

# Basing dispersal kernel distance on the alpha value given in Cowley et al 2015. 
# Would be good to test sensitivty around this value
# and to find other sources for dispersal distance estimates.
disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                   Transfer   = DispersalKernel(Distances = 800),
                   Settlement = Settlement() )

# InitType=2 tells RangeShiftR that we will provide a txt file of individuals to initialise from.
init <- Initialise(InitType=2,
                   InitIndsFile='initial_inds.txt')

# Setup the simulation with the parameters defined above.
s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, init = init)
validateRSparams(s)


#####
# run simulation
#####

result <- RunRS(s, sprintf('%s/', dirpath = dirRsftr))
crs(result) <- crs(rstHabitat)
extent(result) <- extent(rstHabitat)
#result[[1]]
spplot(result)
#spplot(result[[-1]])


# plot abundance and occupancy
range_df <- readRange(s, "C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/rangeshiftR/")
# ...with replicates:
par(mfrow=c(1,2))
plotAbundance(range_df)
plotOccupancy(range_df)
dev.off()

# read population output file into a data frame
pop_df <- readPop(s, "C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/rangeshiftR/")

#####
# sensitivity analysis
#####

# parameters to sensitivity test
# K carring capacity
# Rmax
# Dispersal kernel

# data frame of parameter variations
# full factorial design
# https://www.r-bloggers.com/2009/12/design-of-experiments-–-full-factorial-designs/
# http://www.lithoguru.com/scientist/statistics/DOE%20Factorial%20Design.R
#install.packages("AlgDesign")
library(AlgDesign)

# The common 2^k design (two levels for each factor e.g. high/low):
df2 <- gen.factorial(levels = 2, nVars = 3, varNames=c("K","Rmax","Dispersal"))
df2
# the output is a data frame containing the factorial design
# 8 runs/experiments in this case

# 3^k?
df3 <- gen.factorial(levels = 3, nVars = 3, center = TRUE, varNames = c("K","Rmax","Dispersal"))
df3
# this would give 27 runs/experiments with all possible combinations of med (0?) high (1?) and low (-1) values for each parameter
# for each experiment, store result of interest from rangeshiftR e.g. % occupied cells?

paramSens <- df2
# now define parameters
paramSens$K[which(paramSens$K==-1)] <- 50
paramSens$K[which(paramSens$K==1)] <- 100
paramSens$Rmax[which(paramSens$Rmax==-1)] <- 10
paramSens$Rmax[which(paramSens$R==1)] <- 20
paramSens$Dispersal[which(paramSens$Dispersal==-1)] <- 600
paramSens$Dispersal[which(paramSens$Dispersal==1)] <- 1000

# test for loop to use each set of parameters in turn
for (i in c(1:nrow(paramSens))){
  
  params <- paramSens[i,]
  
  expmt <- as.numeric(row.names(params))
  
  rangeshiftrYears <- 20
  
  sim <- Simulation(Simulation = expmt,
                    Years = rangeshiftrYears,
                    Replicates = 1,
                    OutIntPop = 1)
  
  land <- ImportedLandscape(LandscapeFile=sprintf('Habitat-%sm.asc', habitatRes),
                            Resolution=habitatRes,
                            HabitatQuality=TRUE,
                            K=params[[1]]) # carrying capacity (individuals per hectare) when habitat at 100% quality
  
  demo <- Demography(Rmax = params[[2]],
                     ReproductionType = 0) # 0 = asexual / only female; 1 = simple sexual; 2 = sexual model with explicit mating system
  
  disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                     Transfer   = DispersalKernel(Distances = params[[3]]),
                     Settlement = Settlement() )
  
  init <- Initialise(InitType=2,
                     InitIndsFile='initial_inds.txt')
  
  # Setup the simulation with the parameters defined above.
  s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, init = init)
  validateRSparams(s)
  
  RunRS(s, sprintf('%s/', dirpath = dirRsftr))
  
  # read in range results
  range_df <- readRange(s, sprintf('%s/', dirpath = dirRsftr))
  
  # extract N occupied cells in year 20?
  paramSens$occupied[expmt] <- range_df$NOccupCells[which(range_df$Year==20)]
  
}
# seems to work!

# now set up to run sensitivity analysis for a set of different initial individuals
# list all init individual files

individuals <- list.files(path = dirRsftrInput,
                          pattern='initial_inds_.*\\.txt', recursive=TRUE,
                          full.names = F)

# for each initial individual file, run simulations with sensitivity testing
# make sure sensible batch numbers or sub-folders

dfSensitivity <- tibble(individuals)
dfSensitivity <- tibble::rowid_to_column(dfSensitivity, "indID")

paramSens <- tibble::rowid_to_column(paramSens, "paramID")
library(mefa)
paramSens <- rep(paramSens, times=16)
paramSens$indID <- rep(1:16, each=8)

dfSensitivity <- left_join(dfSensitivity,paramSens,by="indID")
dfSensitivity$batch <- paste0(dfSensitivity$indID,dfSensitivity$paramID) # note. tried as decimal e.g. 1.1, 1.2 but RangeshiftR doesn't like it - unable to read range file later
dfSensitivity$batch <- as.numeric(dfSensitivity$batch)

dfSensitivity$occupied <- NA

rangeshiftrYears <- 10

for (i in c(1:nrow(dfSensitivity))){
  
  #params <- dfSensitivity[1,] #for testing
  params <- dfSensitivity[i,] 

  init <- Initialise(InitType=2, InitIndsFile=params[[2]])
  
  # set up parameters
  expmt <- as.numeric(params[[7]])
  sim <- Simulation(Simulation = expmt,
                    Years = rangeshiftrYears,
                    Replicates = 1,
                    OutIntPop = 1,
                    ReturnPopRaster=TRUE)
  land <- ImportedLandscape(LandscapeFile=sprintf('Habitat-%sm.asc', habitatRes),
                              Resolution=habitatRes,
                              HabitatQuality=TRUE,
                              K=params[[4]])
  demo <- Demography(Rmax = params[[5]],
                       ReproductionType = 0)
  disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                       Transfer   = DispersalKernel(Distances = params[[6]]),
                       Settlement = Settlement())
  
  # setup the simulation with the parameters defined above.
  s <- RSsim(batchnum = expmt, simul = sim, land = land, demog = demo, dispersal = disp, init = init)
  validateRSparams(s)
  
  result <- RunRS(s, sprintf('%s/', dirpath = dirRsftr))
  crs(result) <- crs(rstHabitat)
  extent(result) <- extent(rstHabitat)
  
  # save results
  writeRaster(result, paste0(dirRsftrOutputMaps,"/sensitivityResult_batch",params[[1]],"-",params[[3]],".tif"), options="INTERLEAVE=BAND", overwrite=TRUE)
  
  # read in range results
  range_df <- readRange(s, sprintf('%s/', dirpath = dirRsftr))
  
  # extract N occupied cells in year 10?
  dfSensitivity$occupied[expmt] <- range_df$NOccupCells[which(range_df$Year==10)]
  
}

write.csv(dfSensitivity, paste0(dirOut,"/sensitivity_analysis_rangeshiftR/df_Sensitivity_analysis.csv"),row.names = F)

ggplot(dfSensitivity)+
  geom_boxplot(aes(x=factor(individuals), y=occupied))+ 
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Number of occupied cells")

dfSensitivityLong <- dfSensitivity %>% pivot_longer(cols = K:Dispersal, names_to = "Parameter", values_to = "Value")

dfSensitivityLong$paramSens <- paste0(dfSensitivityLong$Parameter,"-",dfSensitivityLong$Value)

ggplot(dfSensitivityLong)+
  geom_col(aes(factor(paramSens),occupied, fill=Parameter))+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~individuals)

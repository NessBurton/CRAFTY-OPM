
# libraries
library(rgdal)
library(raster)
library(RangeshiftR)
library(sf)
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
dirRsftrOuput <- file.path(dirRsftr,"Outputs")
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
# 60% = open semi-natural (greenspace)
# 50% = multi-surface (greenspace) - this is how most gardens are classified
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

### WRITE RASTER!!!!!
plot(rstHabitat) 
# read in and aggregate
rstHabitat <- raster(file.path(dirRsftrInput, sprintf('Habitat-%sm.tif', rasterizeRes)))
plot(rstHabitat)
rstMin <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=min)
rstMax <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=max)
rstMean <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=mean)
rstModal <- aggregate(rstHabitat, fact=habitatRes/rasterizeRes, fun=modal)
# need to decide whether to use fun=min or fun=max here. min underestimates a lot but max probably overestimates too much
plot(rstMin)
plot(rstMax)
plot(rstMean)
plot(rstModal) # most frequent value - seems to represent the habitat the best, go with this
# doesn't overestimate and still picks up some smaller areas
# but definitely loses smaller areas

rstHabitat <- rstModal

# export as ascii file for RangeShifter.
# be sure to specify -9999 as the no data value (NAflag argument)
writeRaster(rstHabitat, file.path(dirRsftrInput, sprintf('Habitat-%sm.asc', habitatRes)), format="ascii", overwrite=TRUE, NAflag=-9999)

#####
# species locations
#####
csvSpecies <- read.csv(file=file.path(dirData, 'opm_trees.csv'), header=TRUE, sep=",")

# subset to only trees that were infested in 2012
# csvSpecies <- subset(csvSpecies, Status == 'Infested' & SurveyYear == 2012)
# change to trees that were infested or previously infested in 2015
csvSpecies <- subset(csvSpecies, Status == 'Infested' 
                     | Status == "Previously infested" 
                     & SurveyYear == 2015)
# kept previously infested based on assumption also made in Cowley et al. (2015)
# "Given that OPM was continually spreading during our period of study...
# ...we made the assumption that once detected in a location, 
# OPM remained present throughout the period"

# convert to shapefile
csvSpecies <- csvSpecies[!is.na(csvSpecies$Easting), ]
csvSpecies <- csvSpecies[!is.na(csvSpecies$Northing), ]
shpInitialIndividuals <- csvSpecies
rm(csvSpecies)
coordinates(shpInitialIndividuals) <- ~Easting+Northing
projection(shpInitialIndividuals) <- crs(shpHabitat)
shapefile(shpInitialIndividuals, file.path(dirData, "opm_trees.shp"), overwrite=TRUE)

# crop to species locations only within the study landscape
shpInitialIndividuals <- crop(shpInitialIndividuals, extent(shpHabitat))
shpInitialIndividuals$n <- 1

# check
shpInitialIndividuals <- st_as_sf(shpInitialIndividuals)
ggplot(data = shpHabitat) +
  geom_sf(aes(fill=suit),col=NA) +
  geom_sf(data = shpInitialIndividuals)

# need to rasterise then extract the species locations to get the xy, row/col (not spatial) indices for rangeshifter.
rstInitIndividuals <- rasterize(shpInitialIndividuals, rstHabitat, field='n', background=0)
plot(rstInitIndividuals)
dfInitialIndividuals <- extract(rasterize(shpInitialIndividuals, rstHabitat, field='n', background=0), shpInitialIndividuals, cellnumbers=T, df=TRUE)

# rangeshiftR requires a specific format for the individuals file, so add the required columns here,
# and convert 'cells' value to x/y, row/col values.
# as an example we are just initialising each cell with 100 individuals - we may need to adjust this later.
dfInitialIndividuals$Year <- 0
dfInitialIndividuals$Species <- 0
dfInitialIndividuals$X <- dfInitialIndividuals$cells %% ncol(rstHabitat)
dfInitialIndividuals$Y <- nrow(rstHabitat) - (floor(dfInitialIndividuals$cells / ncol(rstHabitat)))
dfInitialIndividuals$Ninds <- 10 #100 # try reducing this to e.g. 10
# 
dfInitialIndividuals <- dfInitialIndividuals[ , !(names(dfInitialIndividuals) %in% c('ID', 'cells', 'layer'))]

# make sure individuals aren't being counted more than once in the same location (due to multiple tree id points)
dfInitialIndividuals <- unique(dfInitialIndividuals)

write.table(dfInitialIndividuals, file.path(dirRsftrInput, 'initial_inds.txt'), row.names = F, quote = F, sep = '\t')

#####
# parameter set-up
#####
# we need to simulate at least 2 years of rangeshiftR for each CRAFTY iteration.
rangeshiftrYears <- 20

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
result[[1]]
spplot(result)
spplot(result[[-1]])


# plot abundance and occupancy
range_df <- readRange(s, "C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/rangeshiftR/")
# ...with replicates:
par(mfrow=c(1,2))
plotAbundance(range_df)
plotOccupancy(range_df)
dev.off()

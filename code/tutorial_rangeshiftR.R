
library(RangeshiftR)

s <- RSsim()
dirpath <- "/Users/Vanessa/Documents/FR/FR_R/tutorial_rangeshiftR/"
dir.create(paste0(dirpath,"Inputs"),showWarnings = TRUE)
dir.create(paste0(dirpath,"Outputs"),showWarnings = TRUE)
dir.create(paste0(dirpath,"Output_Maps"),showWarnings = TRUE)

# run simulation
RunRS(s,getwd())

# look at parameter master
s

# set general simulation parameters
sim <- Simulation(Simulation = 2,
                  Years = 50,
                  Replicates = 2,
                  OutIntPop = 50)
?Simulation

# generate artificial landscape
land <- ArtificialLandscape(Resolution = 10,
                            K = 1500,
                            propSuit = 0.2,
                            dimX=129, dimY=257,
                            fractal=T, hurst=0.3,
                            continuous=F)
# demography
demo <- Demography(Rmax = 2.2, ReproductionType = 1, PropMales = 0.45)
stg <- StageStructure(Stages=3,
                      TransMatrix=matrix(c(0,1,0,5.7,.5,.4,3.4,0,.9), nrow = 3),
                      FecDensDep = T,
                      SurvDensDep = T)
demo <- demo + stg
plotProbs(stg)

# dispersal
disp <- Dispersal(Emigration = Emigration(EmigProb=0.2),
                  Transfer = DispersalKernel(Distances=50),
                  Settlement = Settlement())
plotProbs(DispersalKernel(Distances = matrix(c(0,1,2,70,50,30),nrow = 3), StageDep=T))
disp <-  disp + Settlement(SexDep = T, 
                           Settle = matrix(c(0,1,1,0), nrow = 2))
disp2 <-  disp + Emigration(IndVar = TRUE,
                            EmigProb = matrix(c(.7,.1), nrow = 1),
                            MutationScales = c(.1))

# genetics
gene <- Genetics(NLoci = 3, ProbMutn = .05)

# initialisation
init <- Initialise(FreeType = 0, 
                   NrCells = 2250,
                   InitDens = 2, 
                   IndsHaCell = 3, 
                   PropStages = c(0,0.7,0.3))
init

# parameter master
s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, gene = gene, init = init)
validateRSparams(s)

# run simulation
RunRS(s, dirpath=dirpath)

# plot abundance and occupancy timeseries
range_df <- readRange(s, dirpath)

# ...with replicates:
par(mfrow=c(1,2))
plotAbundance(range_df)
plotOccupancy(range_df)
# ...with standard deviation:
par(mfrow=c(1,2))
plotAbundance(range_df, sd=T, replicates = F)
plotOccupancy(range_df, sd=T, replicates = F)

# spatial distribution of abundance
# read population output file into a dataframe
pop_df <- readPop(s, dirpath)

# Not all years have the same number of populated and thus listed cells. 
# For stacking, we set a common extent with the values used in the landscape module:
ext <- c(0,1290,0,2570)
res <- 10

# Make stack of different raster layers for each year and for only one repetition (Rep==0):
pop_wide_rep0 <- reshape(subset(pop_df,Rep==0)[,c('Year','x','y','NInd')], timevar='Year', v.names=c('NInd'), idvar=c('x','y'), direction='wide')

# use raster package to make a raster from the data frame
library(raster)
stack_years_rep0 <- rasterFromXYZ(pop_wide_rep0)
names(stack_years_rep0) <- c('Year.0', 'Year.50')
spplot(stack_years_rep0, zlim = c(0,7))

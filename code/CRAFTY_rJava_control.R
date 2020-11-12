
library(raster)
library(sp)
library(jdx)
library(xml2)
library(doSNOW) # doMC does not work in Windows

# location of this script 
#setwd("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB/Calibration/")
setwd("C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/code")
source("Functions_CRAFTY_rJava.R")
source("Functions_CRAFTY_common.R")
# source("../RScripts/Functions_CRAFTY_WEB.R")

# location of the downloaded data
#path_crafty_package = "~/Dropbox/KIT_Modelling/CRAFTY/"
path_crafty_package = "C:/Users/vanessa.burton.sb/Documents/CRAFTY_R_package/"
#path_crafty_data = "~/Dropbox/KIT_Modelling/CRAFTY/crafty_cobra_impressions_kit_data/" # is this the eclipse data folder? e.g.
path_crafty_data = "C:/Users/vanessa.burton.sb/Documents/eclipse-workspace/CRAFTY-OPM/data/"
# Location of the CRAFTY Jar file
path_crafty_jar = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/CRAFTY_KIT_engine2020.jar"))
# Location of the CRAFTY lib files
path_crafty_libs = path.expand(paste0(path_crafty_package, "lib/"))
crafty_libs = list.files(paste0(path_crafty_libs), pattern = "jar")

# Make sure that in the classpath setting , gt-opengis-9.0.jar must be included before geoapi-20050403.jar. Otherwise it throws an uncatchable error during the giving up process: loading libraries without ordering them particularly, the opengis library is loaded after the geoapi library following alphabetical order.
# Related commit - https://github.com/CRAFTY-ABM/CRAFTY_CoBRA/commit/4ce1041cae349572032fc7e25be49652781f5866

crafty_libs = crafty_libs[crafty_libs != "geoapi-20050403.jar"  ] 
crafty_libs = c(crafty_libs,  "geoapi-20050403.jar")

# Location of the input data
path_crafty_inputdata = path.expand(paste0(path_crafty_data, "data_EU28/"))



path_crafty_batch_run = path.expand(paste0("~/tmp/"))
### Scenario  

# Name of the scenario file
scenarios <- c( "Baseline", "RCP2_6-SSP1","RCP2_6-SSP4","RCP4_5-SSP1","RCP4_5-SSP3","RCP4_5-SSP4","RCP8_5-SSP3","RCP8_5-SSP5")
n.scenario = length(scenarios)
scenario.filenames = paste0("Scenario_", scenarios, "_everyyear_8years_relative_landusecontrol_nodisplay")

scenario.filename.dummy =  paste0("Scenario_Baseline_everyyear_8years_relative_landusecontrol_nodisplay_dummy.xml")


n.paramset = 5


 



#### JAVA configuration 
crafty_jclasspath = c(
  path_crafty_jar
  , paste0(path_crafty_libs, crafty_libs)
  
)



## Parallelisation
parallelize = F

if (parallelize) { 
  
  # Each process roughly occupies 2.8-3.3 G memory (10 arcmin EU-28 version) 
  n_thread = 8 # detectCores()
  cl = makeCluster(n_thread)
  registerDoSNOW(cl)
   
}



start_year_idx = 1 # first year of the input data
end_year_idx = 5 # fifth year of the input data 


doProcessFR = T # process and visualise FR each timestep 


setwd(path_crafty_batch_run)

s.idx = p.idx = 2 

foreach(s.idx = 1:n.scenario, .errorhandling = "stop",.packages = c("doSNOW"), .verbose = T) %dopar% {
  
  scenario = scenarios[s.idx]
    
  # must change to the output folder for getting the output files correctly
  setwd(path_crafty_batch_run) 
  
  # initialise jvm in forked processes / not before parallelism is initiated
  # https://stackoverflow.com/questions/24337383/unloading-rjava-and-or-restarting-jvm
  # "There is a way to run expressions using rJava in parallel based on running the parallel processes to get and assemble all results BEFORE you load the rJava library in the main process. As the main R process has not initiated jvm then java is started in each single subprocess and this particular instance will die together with subprocess as well."
  
  library(rJava)
  
  .jinit(parameters="-Dlog4j.configuration=log4j2020_normal.properties")
  .jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
  .jinit( parameters=paste0("-Xms", java.ms, " -Xmx", java.mx)) # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.
  
  # add java classpath
  .jclassPath() # print out the current class path settings.
  for (i in 1:length(crafty_jclasspath)) { 
    .jaddClassPath(crafty_jclasspath[i])
  }
  # .jinit(parameters = paste0("user.dir=", path_crafty_batch_run )) # does not work.. 
  
  .jcall( 'java/lang/System', 'S', 'setProperty', 'user.dir', path_crafty_batch_run )
  
  print(  .jcall( 'java/lang/System', 'S', 'getProperty', 'user.dir' ))
  
  
  
  foreach(p.idx = 1:n.paramset, .errorhandling = "stop", .verbose = T) %do% { 
    
     
    
    paramset =  paste0("Paramset", p.idx)
    
    scenario.filename = paste0(scenario.filenames[s.idx], "_", paramset, ".xml") 
    
     
    
 
    
    # Read the scenario file
    scenario.xml <- xml2::read_xml(paste0(path_crafty_inputdata, scenario.filename.dummy))
    # str(scenario.xml)
    scenario.l <- xml2::as_list(scenario.xml)
    
    # # Replace utility_beta
    # # for (u.idx in 1:length(utility.b)) {
    # #     print(  attr(competition.l$competition[[u.idx]]$curve, "b") <- as.character(utility.b[u.idx]))
    # # }
    
    # Replace scenario name 
    attr(scenario.l$scenario, "scenario") <- scenario
    # Replace version info 
    attr(scenario.l$scenario, "version") <- paramset
    
    
    
    ## Write the modified competition file
    scenario.xml.modified <- xml2::as_xml_document(scenario.l)
    
    xml2::write_xml(scenario.xml.modified, paste0(path_crafty_inputdata, scenario.filename), options = "no_empty_tags")
    
    
    
    
    ############# CRAFTY configuration
    # Model run 
    
    # Scenario 2019 
    CRAFTY_sargs =   c("-d", path_crafty_inputdata, "-f", scenario.filename, "-o", "99", "-r", "1",  "-n", "1", "-sr", "0") # change the argument as you wish 
    
    
    ########### Model running 
    print(paste0("============CRAFTY JAVA-R API: Create the instance"))
    
    CRAFTY_jobj = new(J(CRAFTY_main_name)) # Create a new instance (to call non-static methods)
    
    # prepares a run and returns run information 
    CRAFTY_RunInfo_jobj = CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
    print(paste0("============CRAFTY JAVA-R API: Run preparation done"))
    
    # running from the first timestep to the fifth
    CRAFTY_loader_jobj = CRAFTY_jobj$EXTsetSchedule(as.integer(start_year_idx), as.integer(end_year_idx))

     
    # print(  .jcall( 'java/lang/System', 'S', 'getProperty', 'user.dir' ))
    
    
    # Yet experimental as rJava frequently hangs.. 
    if (doProcessFR) { 
      
      
      # slower..  
      # system.time({ 
      #     val_fr = sapply(allcells_l, function(c) c$getOwnersFrLabel() )
      # })
      # print("sapply")
      # system.time({
      #     val_fr = sapply(allcells_l2, function(c) c$getOwnersFrLabel() )
      # })
      
      region = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()$'next'()
      
      
      # # alloc_m = region$getAllocationModel()
      # # .jmethods(alloc_m)
      # # btmap = region$getBehaviouralTypeMapByLabel() 
      # 
      # a = region$getFunctionalRoleMapByLabel()
      # a$values()
      # a2 = a$get("Ur")
      # a2$getAlternativeFrId()
      # a2$getSampledGivingInThreshold()
      
      allcells_uset = region$getAllCells() 
      allcells_l =   as.list(allcells_uset)
      
      
      #### Get XY coordinates
      print("Get XY coords")
      
      system.time({
        val_xy =foreach(c = allcells_l, .combine = "rbind") %do% { 
          c(X=c$getX(), Y=c$getY())
        }
        val_xy = data.frame(val_xy)
        colnames(val_xy) = c("X", "Y")
        x_coord = x.lat.v[val_xy$X]
        y_coord = y.lon.v[val_xy$Y]
      })
      
      crafty_sp =SpatialPoints(cbind(x_coord, y_coord))
      proj4string(crafty_sp) = proj4.LL
      
    }
    
     

    # crafty main loop
    for (tick in start_year_idx:end_year_idx) {

    nextTick = CRAFTY_jobj$EXTtick()

    stopifnot(nextTick == (tick + 1 )) # assertion


      # safe to alter capital csv files 




      # Yet experimental as rJava frequently hangs..
      if (doProcessFR) {

        region = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()$'next'()

        # allcells = region$getAllCells()
        allcells_uset = region$getAllCells() # slower
        # allcells_arr = allcells_uset$toArray() # slower # often throws jave execption/warning
        allcells_l =   as.list(allcells_uset)

        print("Process output")

        # visualise something

        # allregions_iter = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()
        # region = allregions_iter$'next'()
        # for all regions

        # allcells_uset = r$getAllCells() # UnmodifiableSet
        # a = allcells_uset$forEach()
        #
        #
        # allcells_stream = allcells_uset$stream()

        # aa = allcells_stream$toArray()
        # str(aa[[1]]$getX())
        # .jmethods(allcells_stream$forEach())
        # x= allcells_stream$sorted()
        # x2 = x$forEach()$getX()

        # allcells_l =  as.list(r$getAllCells()) # faster with as.list
        print(Sys.time())

        if (parallelize) {
          system.time({
            val_df = foreach(c = allcells_l, .combine = rbind, .packages = c("rJava"), .verbose = F, .export = c("region",  "allcells_l") ) %dopar% {
              c(  c$getOwnersFrLabel(), c$getEffectiveCapitals()$getAll(), c$getSupply()$getAll())
            }
            val_fr = val_df[,1]

          })
        } else  {
          system.time({
            val_df = t(sapply(allcells_l, FUN = function(c) c(  c$getOwnersFrLabel()#, c$getEffectiveCapitals()$getAll(), c$getSupply()$getAll()
            )))
            val_fr = val_df[1,]

          })
        }
        print(Sys.time())



        val_fr_fac = factor(val_fr, levels = aft.names.fromzero)


        fr_spdf = SpatialPixelsDataFrame(crafty_sp, data =data.frame( as.numeric(val_fr_fac )), tolerance = 0.0011)
        fr_r = raster(fr_spdf)
        # plot(fr_r)
        # par(mfrow=c(3,3))
        plot(fr_r, main = paste0("Tick=", tick), xlab = "lon", ylab = "lat")

        # rm(allregions_iter)
        rm(allcells_l)
        rm(region)
        
      }


      if (nextTick <= end_year_idx) {
        print(paste0("============CRAFTY JAVA-R API: NextTick=", nextTick))
      } else {
        print(paste0("============CRAFTY JAVA-R API: Simulation done (tick=", tick, ")"))

      }
 
    }



    print("Close run")


    # close the run
    CRAFTY_jobj$EXTcloseRrun() # ignore the MPI initialisation error

    # delete the java objects
    rm(CRAFTY_RunInfo_jobj)
    rm(CRAFTY_loader_jobj)
    
    rm(CRAFTY_jobj)
     
    

  }
  
}
stopCluster(cl)


# 
# 
# a = region$getFunctionalRoles() 
# b = a$toArray()
# # b[[1]]$getMeanGivingInThreshold()
# # b[[1]]$getMeanGivingUpThreshold()
# # b[[1]]$getSampledGivingInThreshold()
# # b[[1]]$getSampledGivingInThreshold()
# # b[[1]]$getProduction()
# # b[[1]]$getAllocationProbability()
# # b[[1]]$getNewFunctionalComp()
# 
# b[[4]]
# b[[4]]$getMeanGivingInThreshold()
# b[[2]]$getMeanGivingUpThreshold()
# b[[2]]$getSampledGivingInThreshold()
# # b[[1]]$getSampledGivingInThreshold()
# # b[[1]]$getProduction()
# # b[[1]]$getAllocationProbability()
# # b[[1]]$getNewFunctionalComp()
# 
# 
# 



# stop("ends here")




library(raster)
library(sp)
library(rJava)



# location of this script 
setwd("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB/Calibration/") 
source("Functions_CRAFTY_rJava.R")

# location of the downloaded data
path_crafty_package = "~/Downloads/"

# Location of the CRAFTY Jar file
path_crafty_jar = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/CRAFTY_KIT_engine2020.jar"))
# Location of the CRAFTY lib files
path_crafty_libs = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/CRAFTY_KIT_engine2020/lib/"))
crafty_libs = list.files(paste0(path_crafty_libs), pattern = "jar")

# Location of the input data
path_crafty_inputdata = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/data_KIT2020/"))
# Name of the scenario file
scenario.filename = "Scenario_KIT2019.xml"
 
# # Location of the output files
# path_batch_root = "~/Downloads/CRAFTY_R_batch/"
# RUN_ID = "CRAFTY_CoBRA_EU_testrun"
# path_batch = paste0(path_batch_root, RUN_ID)
# 
# if (!dir.exists(path_batch)) { 
#   dir.create(path_batch, recursive = T)
# }




#### JAVA configuration 
crafty_jclasspath = c(
  path_crafty_jar
  , paste0(path_crafty_libs, crafty_libs)
  
)


# add java classpath
.jinit() 
for (i in 1:length(crafty_jclasspath)) { 
  .jaddClassPath(crafty_jclasspath[i])
}
# .jclassPath() # print out the current class path settings. 


# .jinit( parameters="-Dfile.encoding=UTF-8 -Dlog4j.configuration=log4j_cluster.properties")
.jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
.jinit( parameters=paste0("-Xms", java.ms, " -Xmx", java.mx)) # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.

# .jinit(parameters = paste0("-Duser.dir=",path_batch ))

## Parallelisation
parallelize = F
if (parallelize) { 
  library(parallel)
  library(doMC)
  n_thread = detectCores()
  registerDoMC(n_thread)
}
############# CRAFTY configuration
# Model run 
start_year_idx = 1 # first year of the input data
end_year_idx = 10 # fifth year of the input data 

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



# crafty main loop 
for (tick in start_year_idx:end_year_idx) { 
  
  nextTick = CRAFTY_jobj$EXTtick()
  
  stopifnot(nextTick == (tick + 1 )) # assertion
  
  
  
  # safe to alter capital csv files
  
  
  # Yet perimental as rJava frequently hangs.. 
  doProcessFR = FALSE
  if (doProcessFR) { 
    
    print("Process output")
    
    # visualise something 
    
    allregions_iter = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()
    r = allregions_iter$'next'()
    # for all regions 
    
    allcells_l = as.list(r$getAllCells()$toArray())
    
    # slower..  
    # system.time({ 
    #     val_fr = sapply(allcells_l, function(c) c$getOwnersFrLabel() )
    # })
    # print("sapply")
    # system.time({
    #     val_fr = sapply(allcells_l2, function(c) c$getOwnersFrLabel() )
    # })
    if (is.na(crafty_sp)) { 
      print("XY coords")
      
      system.time({
        val_x =foreach(c = allcells_l, .combine = "c") %dopar% { 
          c$getX()
        }
        val_y =foreach(c = allcells_l, .combine = "c") %dopar% { 
          c$getY()
        }
      })
      
      crafty_sp =SpatialPoints(cbind(val_x, val_y))
    }
    
    ### Process functional roles (AFT)
    system.time({
      val_fr =foreach(c = allcells_l, .combine = "c") %dopar% { 
        c$getOwnersFrLabel()
      }
    })
    
    val_fr_fac = factor(val_fr, levels = aft.names.fromzero)
    
    
    fr_spdf = SpatialPixelsDataFrame(crafty_sp, data =data.frame( as.numeric(val_fr_fac )))
    fr_r = raster(fr_spdf)
    # plot(fr_r)
    # par(mfrow=c(3,3))
    plot(fr_r, main = paste0("Tick=", tick))
    
    rm(allregions_iter)
  }
  
  
  if (nextTick <= end_year_idx) { 
    print(paste0("============CRAFTY JAVA-R API: NextTick=", nextTick))
  }
}



print("Close run")


# close the run 
CRAFTY_jobj$EXTcloseRrun() # ignore the MPI initialisation error

# delete the java objects
rm(CRAFTY_loader_jobj)
rm(CRAFTY_jobj)
rm(CRAFTY_jclass)
rm(CRAFTY_RunInfo_jobj)



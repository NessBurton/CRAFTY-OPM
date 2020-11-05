
# libraries
library(rgdal)
library(raster)
library(RangeshiftR)
library(sf)

wd <- "~/R/CRAFTY-OPM" # FR
wd <- "~/Documents/crafty-opm" # mac
setwd(wd)
dirOut <- file.path(wd, 'data-processed')
dirData <- file.path(dirOut, 'for-rangeshiftR') 
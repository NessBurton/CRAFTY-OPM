
# date: 12/10/20
# author: VB
# description: process OSMM greenspace data. simplify typology and merge adjacent types (parks)

# load libraries
library(tidyverse)
library(sf)
library(tmap)

# file paths
wd <- "~/Documents/crafty-opm" # mac
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

#####
# OSMM greenspace for hammersmith/kensington/westminster/camden + 1km buffer

gspace <- st_read("./data-raw/OSMM_greenspace/OSMM_gspaceAOI.shp")
head(gspace)
unique(gspace$priFunc)

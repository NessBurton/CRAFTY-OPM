
# date: 14/10/20
# author: VB
# description: process GiGL biodiversity data and OS open greenspace data

# load libraries
library(tidyverse)
library(sf)
library(tmap)

# file paths
#wd <- "~/Documents/crafty-opm" # mac
wd <- "~/R/CRAFTY-OPM" # FR
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

# GiGL dataset
GiGL <- st_read(paste0(dirData,"/GiGL_BHP_SHP/GiGL_BHP_region.shp"))
# clip to AOI
# rasterise (fine res)
# sum to gspace polygons
# extract value per polygon to centroid point
# normalise values 0-1

# OS open greenspace data
openGS <- st_read(paste0(dirData,"/OS_open_greenspace_TQ/data/TQ_GreenspaceSite.shp"))
# join to gspace polygons
# should be simple new access attribute, 1 if within openGS, 0 if not





# Cell ID and cooridnates 
#ctry.ids <- read.csv("~/Dropbox/KIT/CLIMSAVE/IAP/Cell_ID_LatLong.csv")
ctry.ids <- read.csv("~/CRAFTY-opm/data-processed/for-CRAFTY/Cell_ID_LatLong.csv")
x.lat.v = sort(unique(ctry.ids$Long))
y.lon.v = sort(unique(ctry.ids$Lat))


# Lon-Lat projection 
#proj4.LL <- CRS("+proj=longlat +datum=WGS84")
proj4.LL <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

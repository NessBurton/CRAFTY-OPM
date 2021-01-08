
# desc: for presentation to Defra 12/01/21
# author: VB

# plots to make:
# - normal map of area
# - greenspace map --> hex grid
# - capitals map
# - services map
# - agent map (yr1, yr 5, yr 10)
# - rangeshiftR map

# figure directory
dirFigs <- "C:/Users/vanessa.burton.sb/Documents/CRAFTY-opm/figures/"

# normal map -------------------------------------------------------------------

wd <- "~/CRAFTY-opm" # sandbox
dirData <- file.path(wd, 'data-raw')
dirOut <- file.path(wd, 'data-processed')

library(ggmap)
register_google(key = "AIzaSyB_Sft1bVREQZ8T7BsTUzyg44L_MMgqM6I", write=TRUE)

studyArea <- st_read(paste0(dirData,"/borough_boundaries/case_studies.shp"))
plot(studyArea)
studyArea$Borough <- NA

studyArea$Borough[which(studyArea$name=="Hammersmith and Fulham London Boro")] <- "Hammersmith & Fulham"
studyArea$Borough[which(studyArea$name=="Kensington and Chelsea London Boro")] <- "Kensington"
studyArea$Borough[which(studyArea$name=="City of Westminster London Boro")] <- "City of Westminster"

# Transform studyArea to EPSG 3857 (Pseudo-Mercator, what Google uses)
studyArea_3857 <- st_transform(studyArea, 3857)studyArea$Borough[which(studyArea$name=="Camden London Boro")] <- "Camden"

centre <- c(lon = -0.17,lat = 51.51)
#bbox <- st_bbox(studyArea_3857)
map <- get_map(centre, zoom = 12, scale = 1)
#map <- get_openstreetmap(bbox, zoom = 12, scale = 1, source = "osm")
#bbox
#map2 <- get_stamenmap(bbox = c(left=-0.25, bottom=51.46,right=-0.1,top=51.56), maptype = "watercolor", crop = FALSE, zoom = 12)
ggmap(map)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- ggmap_bbox(map)
#map2 <- ggmap_bbox(map2)

png(paste0(dirFigs,"case_study_area.png"), units="cm", width = 20, height = 20, res=1000)
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = studyArea_3857, aes(col = Borough), fill=NA, size=0.8, inherit.aes = FALSE)+
  scale_color_brewer(palette="Dark2")+
  ylab("")+xlab("")
dev.off()


# greenspace maps --------------------------------------------------------------

sfGspace <- st_read(paste0(dirData,"/OSMM_greenspace/OSMM_gspaceAOI.shp"))

#png(paste0(dirFigs,"greenspace_map.png"), units="cm", width = 20, height = 20, res=1000)
ggplot(sfGspace)+
  geom_sf(aes(fill=priFunc), col=NA)
#dev.off()

hexGspace <- st_read(paste0(dirOut, "/hexGrids/hexGrid40m_types2.shp"))

#png(paste0(dirFigs,"hex_greenspace_map.png"), units="cm", width = 20, height = 20, res=1000)
ggplot() +
  geom_sf(hexGspace, mapping = aes(fill = type), col = NA)+
  scale_fill_manual(values=type.pal)
#dev.off()

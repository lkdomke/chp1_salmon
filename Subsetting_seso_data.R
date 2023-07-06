#' Goal: subset a noaa site shapefile to include just the seso sites 
#' (all sites regardless of salmon present). 
#' 
#' Required libraries
library(rgdal)
library(rgdal)
library(ggplot2)
library(rgeos)
library(raster)
library(maptools)
library(sf)
library(dplyr)
#' 
#' First load noaa data for seso
seso <- read.csv("Data/noaa_region_seso.csv")
# Then the shapefile
noaa_shp <- readOGR(dsn = "../../../../K Nielson thmb drive/", layer = "NOAA_SE_sites")
#' 
#' Need to transform the sites to a SF object to filter with dplyr
noaa_sf <- st_as_sf(noaa_shp)
#' 
#' Should be able to use EventID to filter data
glimpse(seso)                          
EventIDs <- seso %>%
  dplyr::select(EventID) %>%
  distinct()


seso_loc <- subset(noaa_sf, EventID %in% EventIDs$EventID)

seso_sp <- as(seso_loc, Class = "Spatial")

seso_utm <- spTransform(seso_sp, CRS("+proj=utm +zone=8 +ellps=GRS80 +towgs84=0,0,0"))

# write out the new reduced shapefile to the data section
writeOGR(seso_utm, dsn = "Data", "seso_sites_utm", driver="ESRI Shapefile") 

# lets also write out the reduced shapefile in WGS
seso_wgs <- spTransform(seso_sp, CRS("+init=epsg:4326"))
writeOGR(seso_wgs, dsn = "Data", "seso_sites_wgs", driver = "ESRI Shapefile")


# out of curiousity what projection are the shorezone data in?
seak_wgs <- readOGR(dsn = "Data/shorezone_APECSDrive", layer = "AK_SEAK_WGS")
seak_wgs@proj4string

# get string of lat/long for only seso
head(seso)
library(dplyr)
seso_latlong <- seso %>%
  dplyr::select(c(SiteID, Region, Locale, Habitat, Lat1, Long1)) %>%
  distinct()

#write.csv(seso_latlong, "../GIS data_WR/seso_latlong_site.csv")

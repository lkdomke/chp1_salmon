#' We know that Johnson et al., 2012 used four regions to divide out southeast Alaska (minus Yakutat). 
#' Southeast Northern Outside (SENO), Southeast Northern Inside (SENI), Southeast Southern Outside (SESO),
#' and Southeast Southern Inside (SESI). However the noaa dataset doesn't have an identifier to divide out the
#' regions. So we have to do this by hand. NOTE: highly prone to errors and should regularily be checked using 
#' leaflet and maps to make sure the sites fall in the correct area. 
#' 
#' load libraries
library(tidyverse)
library(leaflet)

##### Load data #####

getwd() # because this is within chp1 project should be easy to load
noaa <- read.csv("../APECS Master repository/APECS Master repo/ALL_DATA/Lia_fish/noaa_seak_fish_atlas_CLEAN_LD.csv")

# create list of SESI SiteID
sesi <- c(171,172,166,167,154,153,170,341,182,181,149,150,187,188,622,620,611,168,169,179,180,151,152,147,148,185,
          186,593,619,610,594,621,612,609,612)

# create list of SENO SiteID
seno <- c(688,696,695,125,126,240,239,238,237,692,690,691,530,545,193,194,199,542,544,218,213,200,211,532,533,538,
          687,697,123,124,689,694,222,529,204,201,203,202,189,217,190,191,192,546,197,198,219,541,212,214,207,208,
          209,210,535,536,537,686,223,224,205,206,196,195,540,692,271,272,286,285,269,318,315,316,317,274,277,270,
          276,275,531,292,290,298,288,291,294,287,305,302,314,306,313,307,308,309,311,310,547,299,301,543,281,282,
          284,534,539,320,321,322,323,324,326,331,332,330,329,333,328,327,693,221,215,289,297,303,312,280,325)

# create list of SESO SiteID 
seso <- c(177,178,183,184,158,157,155,156,131,132,133,134,142,141,340,339,338,335,146,145,135,136,139,140,137,138,
          337,336,108,109,110,107,598,599,600,601,596,595,597,616,617,618,164,165,163,613,614,615,607,606,608,603,
          602,604,605,111,112,144,143,162)

##### Subset NOAA data by region #####
#' We want to make sure we don't miss any sites, so we need to know how many total sites in Southeast Alaska 
#' there are minus the seines that occured in Yakutat. 

yak <- noaa %>%
  filter(Gear == "BSEINE") %>%
  filter(Locale == "Yakutat Bay" | Locale == "Situk River estuary") %>%
  dplyr::select(SiteID, EventID, Date, Gear, Locale, Location, SubLocale, Habitat) %>%
  distinct()
unique(yak$SiteID) # 19 unique sites. same as map
yak_sites <- unique(yak$SiteID)
# there are 7434 observations in yak 


# lets drop yakutat from the noaa data
noaa_seak <- noaa[!noaa$SiteID %in% yak_sites,]

# there were 46193 obs minus 7434 yak so there should be 38759 observations now
# lets remove all the non bseine 
noaa_seak_bseine <- noaa_seak %>%
  filter(Gear == "BSEINE") %>%
  filter(Region == "southeastern Alaska")

# Whats the total number of sites seined? 
noaa_seak_bseine %>%
  select(SiteID) %>%
  distinct() %>%
  dplyr::summarise(total = n()) # 377 sites

# Alright lets subset the observations by siteID by region now
sesi_fish <- subset(noaa_seak_bseine, SiteID %in% sesi) #35 site
seso_fish <- subset(noaa_seak_bseine, SiteID %in% seso) #55 sites
seno_fish <- subset(noaa_seak_bseine, SiteID %in% seno) #120 sites

# whatever is left should have happened in the juneau region (seni)
siteID_total <- noaa_seak_bseine %>%
  dplyr::select(SiteID) %>%
  distinct()

ntp1 <- noaa_seak_bseine[!noaa_seak_bseine$SiteID %in% seno_fish$SiteID,]
ntp2 <- ntp1[!ntp1$SiteID %in% seso_fish$SiteID,]
noaa_seni <- ntp2[!ntp2$SiteID %in% sesi_fish$SiteID,]

###### Lets check this with a map ######

# lets get rid of all the extra info
seni_siteInfo <- noaa_seni %>%
  dplyr::select(SiteID, EventID, Date, Lat1, Long1, Gear, Locale, Location, SubLocale, Habitat) %>%
  distinct()

# make sure the habitat column is a factor with only 4 levels
seni_siteInfo$Habitat <- as.factor(seni_siteInfo$Habitat)
levels(seni_siteInfo$Habitat)[levels(seni_siteInfo$Habitat)=="Sand-Gravel"] <- "Sand-gravel"
levels(seni_siteInfo$Habitat)[levels(seni_siteInfo$Habitat)=="Surfgrass"] <- "Kelp"

# Set up icons
icon.hab <- awesomeIcons(icon = "map_pin", 
                         markerColor = ifelse(seni_siteInfo$Habitat == "Kelp", "orange", 
                                              ifelse(seni_siteInfo$Habitat == "Eelgrass", "green",
                                                     ifelse(seni_siteInfo$Habitat == "Bedrock", "gray",
                                                            ifelse(seni_siteInfo$Habitat == "Sand-gravel", 
                                                                   "red", "blue")))), library = "fa", 
                         iconColor = "black")

# Create the map
require(leaflet)
map <- leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(data = seni_siteInfo, ~Long1, ~Lat1, label = ~SiteID, icon = icon.hab)
require(htmlwidgets)
saveWidget(map, file="m.html")
#map

write.csv(noaa_seni, "Data/noaa_region_seni.csv")
write.csv(seso_fish, "Data/noaa_region_seso.csv")
write.csv(sesi_fish, "Data/noaa_region_sesi.csv")
write.csv(seno_fish, "Data/noaa_region_seno.csv")
write.csv(yak, "Data/noaa_yakutat_only.csv")








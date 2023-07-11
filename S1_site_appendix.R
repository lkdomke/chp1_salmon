# Quick r script to pull all the chapter 1 site coordinates, dates sampled, and data source

pow <- read.csv("Data/salmon_env_1719.csv", stringsAsFactors=FALSE, header = TRUE)
ids <- read.csv("../../../../../Desktop/Site_metadata_allyears.csv", stringsAsFactors = FALSE, header = TRUE) 

library(tidyverse)
library(stringr)
pow$site <- trimws(pow$site, which = "both")

pow.loc <- pow %>%
  dplyr::select(c(site,  habitat, latitude, longitude, date, year)) %>%
  mutate(year = as.character(year)) %>%
  mutate(site = ifelse(site == "Shakan - eel", "Calder Bay", site),
         site = ifelse(site == "Heceta", "Heceta Island", site),
         site = ifelse(site == "Klawock airport", "Klawock Airport", site),
         site = ifelse(site == "Baker Island - eel", "Baker Island", site)) 

sites17 <- ids %>%
  filter(str_detect(site_2017, "2017")) %>%
  unite(bay_id, bay_code:bay_sample) %>%
  mutate(year = "2017")

sites19 <- ids %>%
  filter(str_detect(site_2019, ".")) %>%
  filter(habitat == "eelgrass") %>%
  unite(bay_id, bay_code:bay_sample) %>%
  mutate(year = "2019")

sites2 <- sites17 %>%
  rbind(sites19) %>%
  dplyr::select(place_name, bay_id, habitat, latitude, longitude, year) %>%
  left_join(pow.loc, by = c("latitude", "longitude", "habitat", "year"), relationship = "many-to-many") %>%
  filter(!(is.na(site))) %>%
  left_join(pow.loc, by = c("place_name" = "site"), relationship = "many-to-many") %>%
  ungroup() %>%
  dplyr::select(-c(date.x, latitude.x, longitude.x, year.x)) %>%
  distinct() %>%
  filter(!(bay_id == "CHUS_B" & year.y == "2017"),
         !(bay_id == "CHUS_A" & year.y == "2019")) %>%
  dplyr::rename(habitat = habitat.x, latitude = latitude.y,
                longitude = longitude.y, date = date.y, 
                year = year.y) %>%
  dplyr::select(-c(place_name, habitat.y))

#these are the ones that are missing
missing <- anti_join(pow.loc, sites2, by = c("latitude", "longitude"))  

missing2 <- missing %>%
  left_join(ids, by = c("site" = "place_name"), relationship = "many-to-many") %>%
  dplyr::select(site, habitat.x, latitude.x, longitude.x, date, year, bay_code,
                bay_sample) %>%
  distinct() %>%
  filter(!(site == "Salt Lake Bay" & bay_sample == "B"),
         !(site == "North Pass" & year == "2017" & bay_sample == "B"),
         !(site == "Heceta Island"  & bay_sample == "B"),
         !(site == "Salt Lake Bay" & bay_sample == "B"),
         !(site == "North Pass" & year == "2019" & bay_sample == "A")) %>%
  unite(bay_id, bay_code:bay_sample) %>%
  dplyr::rename(habitat = habitat.x, latitude = latitude.x,
                longitude = longitude.x)

allsite <- rbind(sites2, missing2) %>%
  dplyr::select(site, bay_id, habitat, date, year, latitude, longitude) %>%
  mutate(objective = "Objective 2")

anti_join(pow.loc, allsite, by = c("site", "year", "date")) # nice

# objective 1
seso <- read.csv("Data/noaa_region_seso.csv", stringsAsFactors=FALSE, header = TRUE) %>%
  filter(Habitat == "Eelgrass" | Habitat == "Kelp") %>%
  filter(Mon == "Jul" | Mon == "Apr" | Mon == "Jun" | Mon == "May") %>%
  dplyr::select(Lat1, Long1, Habitat, Date, Year, SiteID, Location) %>%
  distinct() %>%
  dplyr::rename(site = Location, bay_id = SiteID, habitat = Habitat,
                date = Date, year = Year, latitude = Lat1, 
                longitude = Long1) %>%
  mutate(habitat = ifelse(habitat == "Eelgrass", "eelgrass", habitat),
         habitat = ifelse(habitat == "Kelp", "understory kelp", habitat)) %>%
  mutate(objective = "Objective 1")

both <- rbind(seso, allsite)

#write.csv(both, "Appendix_sites.csv")

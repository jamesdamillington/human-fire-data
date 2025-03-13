library(readxl)
library(foreign)  #to read .dbf
library(sf)
library(tidyverse)
library(ggplot2)

## Read data

#lookup tables to create consistent classifications
lookup.use <- read_excel("DB-lookups.xlsx", sheet = "Practices")
lookup.gov <- read_excel("DB-lookups.xlsx", sheet = "Governance")
lookup.prev <- read_excel("DB-lookups.xlsx", sheet = "Prevention")


#DAFI data from https://doi.org/10.6084/m9.figshare.c.5290792.v4
DAFI.case <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Record info", na='ND')

DAFI.shp <- DAFI.case %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude","Latitude"), remove = FALSE)

plot(st_geometry(DAFI.shp))

DAFI.use <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Land use")
DAFI.gov <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Fire Policy")

#col_type for DAFI.prev
DAFI.prev.ct <- c('text', 'skip',  'skip', 'skip', 'skip', 'skip', 'skip',
                  'numeric', 'skip', 'numeric', 'skip', 'numeric', 'skip', 'skip')
DAFI.prev <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Fire suppression", na='ND', col_types=DAFI.prev.ct)


#LIFE data from https://doi.org/10.17637/rh.c.5469993
LIFE.case <- read_excel("data\\Database_V6.xlsx", sheet = "Case")
LIFE.gov <- read_excel("data\\Database_V6.xlsx", sheet = "Governance")  #column TY
LIFE.prev <- read_excel("data\\Database_V6.xlsx", sheet = "Fire practices")  #column COTYPE


#GFUS data from https://doi.org/10.5281/zenodo.10671047
#shapefile data (for spatial plotting)
GFUS.shp <- st_read("data\\Shiny_app\\data\\Meta_data.shp")

GFUS.shp <- GFUS.shp %>%
  select(regnum, ecoregion, political, CONTINENT, area, season, geometry)

#GFUS raw data
#because of double header rows, read twice, per this trick: https://stackoverflow.com/a/62530050 
#there were errors of data entry for Q1b on lines (numeric when should be csv) for the following response ids, 
#required manual fix (format cell as text) before reading:   R_paQ4ZC0Mfse7VrH, R_10IaJz5ysz22Sdd, R_3KOYbd6O8dJu9RT
myCols <- as.character(read_excel("data\\Shiny_app\\Global_human_fire_data\\raw_data\\Survey\\Global fire use survey data.xlsx", sheet="Data", n_max = 1, col_names = FALSE))
GFUS.raw <- read_excel("data\\Shiny_app\\Global_human_fire_data\\raw_data\\Survey\\Global fire use survey data.xlsx", sheet="Data", skip = 2, col_names = myCols)

GFUS.raw.split <- separate_longer_delim(GFUS.raw, 'Q1b', delim=",")




## Analysis

#prevention 
DAFI.prev <- DAFI.prev %>% 
  mutate(`Max Prev` = pmax(`Fire control (0-3)`,
                           `Fire prevention (0-3)`,
                           `Fire extinction (0-3)`,
                           na.rm=T))
DAFI.prev <- DAFI.prev %>%
  right_join(DAFI.shp) %>%
  st_as_sf() %>% 
  st_set_crs(st_crs(GFUS.shp))
  

plot(DAFI.prev['Max Prev'])  #this has >2000 points which is quite messy - use raster like in FIRE paper?



GFUS.Q15a.regions <- GFUS.raw.split %>%
  select(Q1b, Q15a) %>%
  group_by(Q1b) %>%
  summarise(mean15a = mean(Q15a), max15a=max(Q15a), min15a=min(Q15a), count=n()) %>%
  mutate(Q1b = as.numeric(Q1b))


GFUS.15a.shp <- GFUS.Q15a.regions %>%
  right_join(GFUS.shp, join_by('Q1b'=='regnum')) %>%
  st_as_sf()

plot(GFUS.15a.shp['mean15a'])

g <- ggplot() + geom_sf(data = GFUS.15a.shp, aes(fill = mean15a)) + geom_sf(data = DAFI.prev)

g


#practices



#governance









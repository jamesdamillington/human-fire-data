library(readxl)
library(dplyr)
library(foreign)  #to read .dbf
library(sf)


#DAFI data from https://doi.org/10.6084/m9.figshare.c.5290792.v4
DAFI.case <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Record info")
DAFI.use <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Land use")
DAFI.gov <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Fire Policy")

#col_type for DAFI.prev
DAFI.prev.ct <- c('text', 'skip',  'skip', 'skip', 'skip', 'skip', 'skip',
                  'numeric', 'skip', 'numeric', 'skip', 'numeric', 'skip', 'skip')

DAFI.prev <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Fire suppression", na='ND', col_types=DAFI.prev.ct)

DAFI.prev %>% 
  mutate(`Max Prev` = pmax(`Fire control (0-3)`,
                           `Fire prevention (0-3)`,
                           `Fire extinction (0-3)`,
                           na.rm=T))
  



#LIFE data from https://doi.org/10.17637/rh.c.5469993
LIFE.case <- read_excel("data\\Database_V6.xlsx", sheet = "Case")
LIFE.gov <- read_excel("data\\Database_V6.xlsx", sheet = "Governance")  #column TY
LIFE.prev <- read_excel("data\\Database_V6.xlsx", sheet = "Fire practices")  #column COTYPE



#GFUS data from https://doi.org/10.5281/zenodo.10671047
GFUS.shp <- st_read("data\\Shiny_app\\data\\Meta_data.shp")
plot(st_geometry(GFUS.shp))


#GFUS.use <- st_read("data\\Shiny_app\\data\\Overall_fire_use_shape.shp")
GFUS.use <- read.dbf("data\\Shiny_app\\data\\SH_fire_use_LIFE_SURVEY.dbf")
GFUS.use <- as_tibble(GFUS.use)

glimpse(GFUS.use)





#lookup tables to create consistent classification
lookup.use <- read_excel("DB-lookups.xlsx", sheet = "Practices")
lookup.gov <- read_excel("DB-lookups.xlsx", sheet = "Governance")
lookup.prev <- read_excel("DB-lookups.xlsx", sheet = "Prevention")




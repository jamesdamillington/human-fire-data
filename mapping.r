library(readxl)
library(sf)

#DAFI data from https://doi.org/10.6084/m9.figshare.c.5290792.v4
DAFI.dat <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Land use")


#LIFE data from https://doi.org/10.17637/rh.c.5469993
LIFE.dat <- read_excel("data\\Database_V6.xlsx", sheet = "Case")

#GFUS data from https://doi.org/10.5281/zenodo.10671047
GFUS.shp <- st_read("data\\Shiny_app\\data\\Meta_data.shp")
plot(st_geometry(GFUS.shp))


GFUS.use <- st_read("data\\Shiny_app\\data\\Overall_fire_use_shape.shp")

#GFUS user type:  columns we want: Q4a_1	Q4a_2	Q4a_3 from fire_use_shape (for User Type)
#GFUS fire use: There are 26 specific fire uses in SH_fire_use_LIFE_SURVEY.shp  but 30 in GFUS methods summary.docx  (which were dropped/combined?)
#GFUS governance:



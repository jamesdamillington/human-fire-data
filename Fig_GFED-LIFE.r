library(terra)
library(colorspace)
library(sf)
library(readxl)
library(tidyverse)
library(ggplot2)

#produced by GFED.r
GFED_2016dBF <- rast("data\\GFED_dBF_2016.nc")
GFED_2016dBA <- rast("data\\GFED_dBA_2016.nc")


#spatial zoom
Afbb <- c(-20,-35,55,35)
Asbb <- c(30,-10,160,80)
SAbb <- c(-85, -60, -30, 10)
Ozbb <- c(110, -50, 180, -10)
NAbb <- c(-170, 15, -50, 70)
EUbb <- c(-10, 35, 45, 70)

SAsiabb <- c(70, -10, 160, 35)
SAsiaNOzbb <- c(70, -20, 150, 30)
NAsiabb <- c(45, 20, 170, 70)
EASbb <- c(70, 0, 140, 60)
SEASbb <- c(70,5,110,35)

worldbb <- c(-170, -60, 178, 80)

bboxes <- list("Africa"=Afbb,
               "Asia"=Asbb,
               "SAme"=SAbb,
               "Aus"=Ozbb,
               "NAAme"=NAbb,
               "EU"=EUbb,
               "SAsia"=SAsiabb,
               "NAsia"=NAsiabb,
               "World"=worldbb
)


#GFUS data from https://doi.org/10.5281/zenodo.10671047
#shapefile data (for spatial plotting)
GFUS.shp <- st_read("data\\Shiny_app\\data\\Meta_data.shp")

GFUS.shp <- GFUS.shp %>%
  select(regnum, ecoregion, political, CONTINENT, area, season, geometry)



#LIFE data from https://doi.org/10.17637/rh.c.5469993
LIFE.use.ct <- c('text', 'text', 'numeric', 'numeric', rep('skip', 3), 'text', rep('skip', 33)) 


LIFE.use <- read_excel("data\\Database_V6.xlsx", 
                       sheet = "Fire practices", na='U',
                       col_types=LIFE.use.ct) %>%   #column FUPU2 
  mutate(FUPU2 = na_if(FUPU2, 'X'))

# Function to count distinct words in a string
count_distinct_words <- function(x) {
  str_split(x, ",") %>%
    unlist() %>%
    unique() %>%
    length()
}



LIFE.use <- LIFE.use %>%
  #work on cases not individual practices 
  mutate(CaseID = sub("\\.*\\d*$", "", FID)) %>%
  group_by(CaseID) %>%
  #replaces NA in Q5a with the first non-NA value within each group
  mutate(FUPU2 = replace_na(FUPU2, first(na.omit(FUPU2))))  %>%
  #combine all strings in the group into a single string 
  mutate(allA = paste0(FUPU2, collapse = ",")) %>%
  #count unique 'words' (i.e. uses)
  mutate(LIFE_SH = sapply(allA, count_distinct_words)) %>%
  #mutate_at(c("Q5a", "Q5c", "Q5e"), ~sapply(., count_distinct_words)) %>%
  distinct(CaseID, .keep_all=T) 


#bb <- worldbb

for (i in seq_along(bboxes)) {
  bb <- bboxes[[i]]
  nm <- names(bboxes)[i]
  
  g <- ggplot() +
    geom_sf(data = st_geometry(GFUS.shp), color='lightgrey') +
    geom_raster(data = as.data.frame(GFED_2016dBF, xy = TRUE), 
                aes(x = x, y = y, fill = dBF)) + 
    scale_fill_continuous_divergingx(palette='RdYlBu') +
    #coord_quickmap() +
    #geom_sf(data = st_geometry(GFUS.shp), color='lightgrey', fill=NA) +
    geom_point(data = filter(LIFE.use, !is.na(LIFE_SH)),
               aes(x=LONGITUDE, y=LATITUDE), 
               #size=1,shape=3,fill='red',colour='red', alpha=0.5, stroke=0.2) +
               size=1.5,shape=3,fill='red',colour='red', alpha=0.75, stroke=0.75) +
    coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4])) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("2016 GFED Difference (cell fraction) - LIFE Practices Data")
  
  plot(g)
  
}


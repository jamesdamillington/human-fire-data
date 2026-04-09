library(readxl)
#library(foreign)  #to read .dbf
library(sf)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
#library(terra)
#library(colorspace)


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

bboxes <- list(Afbb, Asbb,SAbb,Ozbb, NAbb, EUbb, SAsiabb, NAsiabb)


#DAFI data from https://doi.org/10.6084/m9.figshare.c.5290792.v4
DAFI.case <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Record info", na='ND')

DAFI.shp <- DAFI.case %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude","Latitude"), remove = FALSE)


#col_type for DAFI.supp
DAFI.supp.ct <- c('text', 'skip',  'skip', 'skip', 'text', 'skip', 'skip',
                  'numeric', 'skip', 'numeric', 'skip', 'numeric', 'skip', 'skip')

DAFI.supp <- read_excel("data\\DAFI version 1.01.xlsx", 
                        sheet = "Fire suppression", na='ND', 
                        col_types=DAFI.supp.ct)


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

#suppression (all actors)
DAFI.supp.all <- DAFI.supp %>% 
  #get the maximum level from across the different types of prevention
  mutate(MaxPrev = pmax(`Fire control (0-3)`,
                        `Fire prevention (0-3)`,
                        `Fire extinction (0-3)`,
                        na.rm=T)) %>%
  #reverse the scale to match GFUS (1 is high)
  mutate(MaxPrev = case_when(MaxPrev == 1 ~ 3,
                             MaxPrev == 2 ~ 2,
                             MaxPrev == 3 ~ 1,
                             MaxPrev == 0 ~ NA)) %>%
  filter(!is.na(MaxPrev)) %>%
  #summarise by Case Study
  group_by(`Case Study ID`) %>%
  summarise(meanMP = mean(MaxPrev, na.rm=T), 
            maxMP=max(MaxPrev, na.rm=T), 
            minMP=min(MaxPrev, na.rm=T), 
            count=n()) %>%
  #add spatial data 
  full_join(DAFI.shp) %>%
  filter(!is.na(count)) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  rename(ID=`Case Study ID`) %>%
  select(ID, Latitude, Longitude, meanMP, maxMP, minMP, count)




#suppression (state and non-state agencies)
#DAFI.supp.AFTs <- unique(DAFI.supp$AFT)

DAFI.supp.sngo <- DAFI.supp %>% 
  #get actors we want
  filter(AFT == "Fire suppression agent" |
           AFT == "Conservationist (Fire exclusion)" |
           AFT == "State land manager" |
           AFT == "Conservationist" |
           AFT == "Conservationist (Pyro-diversity)") %>%
  #get the maximum level from across the different types of prevention
  mutate(MaxSupp = pmax(`Fire control (0-3)`,
                        `Fire prevention (0-3)`,
                        `Fire extinction (0-3)`,
                        na.rm=T)) %>%
  #reverse the scale to match GFUS (where 1 is high)
  mutate(MaxSupp = case_when(MaxSupp == 1 ~ 3,
                             MaxSupp == 2 ~ 2,
                             MaxSupp == 3 ~ 1,
                             MaxSupp == 0 ~ NA)) %>%
  filter(!is.na(MaxSupp)) %>%
  #summarise by Case Study
  group_by(`Case Study ID`) %>%
  summarise(meanMS = mean(MaxSupp, na.rm=T), 
            maxMS=max(MaxSupp, na.rm=T), 
            minMS=min(MaxSupp, na.rm=T), 
            count=n()) %>%
  #add spatial data 
  full_join(DAFI.shp) %>%
  filter(!is.na(count)) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  rename(ID=`Case Study ID`) %>%
  select(ID, Latitude, Longitude, meanMS, maxMS, minMS, count)


#calc summaries of maximum suppression level in a given region
GFUS.Q15a.regions <- GFUS.raw.split %>%
  select(Q1b, Q15a, Q15b) %>%
  filter(Q15b > 3) %>%   #only use records with high or very high confidence
  group_by(Q1b) %>%
  summarise(mean15a = mean(Q15a), max15a=max(Q15a), min15a=min(Q15a), count=n()) %>%
  mutate(Q1b = as.numeric(Q1b)) 

GFUS.15a.shp <- GFUS.Q15a.regions %>%
  right_join(GFUS.shp, join_by('Q1b'=='regnum')) %>%
  st_as_sf()


mapbbox <- EUbb
mapbbox <- worldbb

#for(mapbbox in bboxes){
  #plot
  g <- ggplot() + 
    geom_sf(data = st_geometry(GFUS.15a.shp), color='lightgrey') +
    geom_sf(data = filter(GFUS.15a.shp, !is.na(mean15a)), aes(fill = mean15a), color=NA) + 
    #geom_sf(data = filter(GFUS.15a.shp, !is.na(count)), aes(fill = count), color=NA) + 
    scale_fill_distiller(palette='Reds') +
    new_scale_fill() +
    #geom_point(data = filter(DAFI.supp.all, !is.na(meanMP)), 
    geom_point(data = filter(DAFI.supp.sngo, !is.na(meanMS)), 
               aes(fill=meanMS, x=Longitude, y=Latitude),
               size=2,shape=21,,colour='black', alpha=1) +
    scale_fill_distiller(palette='Reds') +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    coord_sf(xlim=c(mapbbox[1],mapbbox[3]),ylim=c(mapbbox[2],mapbbox[4]))  +
    #ggtitle("Supp/Prev/Control (1 high) - GFUS Polys - DAFI Points (All)") +
    ggtitle("Supp/Prev/Control (1 high) - GFUS Polys - DAFI Points (State, NGO)")
#}


g
  
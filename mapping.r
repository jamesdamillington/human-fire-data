library(readxl)
library(foreign)  #to read .dbf
library(sf)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(terra)
library(colorspace)


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


## Read data

#DAFI data from https://doi.org/10.6084/m9.figshare.c.5290792.v4
DAFI.case <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Record info", na='ND')

DAFI.shp <- DAFI.case %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude","Latitude"), remove = FALSE)

plot(st_geometry(DAFI.shp))

DAFI.use <- read_excel("data\\DAFI version 1.01.xlsx", sheet = "Land use")

DAFI.gov.ct <- c('text', rep('skip', 8),'text', 'skip', 'skip',
                 'text',rep('skip',4),'text',rep('skip',5))

DAFI.gov <- read_excel("data\\DAFI version 1.01.xlsx", 
                       sheet = "Fire Policy", na='ND',
                       col_types=DAFI.gov.ct)


#col_type for DAFI.supp
DAFI.supp.ct <- c('text', 'skip',  'skip', 'skip', 'text', 'skip', 'skip',
                  'numeric', 'skip', 'numeric', 'skip', 'numeric', 'skip', 'skip')

DAFI.supp <- read_excel("data\\DAFI version 1.01.xlsx", 
                        sheet = "Fire suppression", na='ND', 
                        col_types=DAFI.supp.ct)


#LIFE data from https://doi.org/10.17637/rh.c.5469993
LIFE.case <- read_excel("data\\Database_V6.xlsx", sheet = "Case")
LIFE.gov <- read_excel("data\\Database_V6.xlsx", sheet = "Governance")  #column TY

LIFE.supp.ct <- c('text', 'text', 'numeric', 'numeric', rep('skip', 20), 'text', rep('skip', 16))

LIFE.supp <- read_excel("data\\Database_V6.xlsx", 
                        sheet = "Fire practices", na='U',
                        col_types=LIFE.supp.ct) #column COTYPE

LIFE.use.ct <- c('text', 'text', 'numeric', 'numeric', rep('skip', 3), 'text', rep('skip', 33)) 


LIFE.use <- read_excel("data\\Database_V6.xlsx", 
                        sheet = "Fire practices", na='U',
                        col_types=LIFE.use.ct) %>%   #column FUPU2 
  mutate(FUPU2 = na_if(FUPU2, 'X'))

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

#write data if needed
#DAFI.supp.all.sf <-st_as_sf(DAFI.supp.all, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
#st_write(DAFI.supp.all.sf, "data\\DAFI_supp_all.shp", driver = "ESRI Shapefile")

#DAFI.supp.sngo.sf <- st_as_sf(DAFI.supp.sngo, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
#st_write(DAFI.supp.sngo.sf, "data\\DAFI_supp_sngo.shp", driver = "ESRI Shapefile")

#old check
#DAFI.supp.sngo.n306 <- DAFI.supp.sngo %>%
#  filter(!grepl("306",ID))


LIFE.supp.summ <- LIFE.supp %>% 
  #set the scale to match GFUS
  mutate(COTYPE = str_replace_all(COTYPE, "[MD]", "0")) %>%
  mutate(COTYPE = str_replace_all(COTYPE, "[RS]", "1")) %>%
  mutate(COTYPE = str_replace_all(COTYPE, c("B" = "2", 
                                            "TC" = "2",
                                            "TE" = "2",
                                            "FC" = "2", 
                                            "G" = "2"))) %>%
  mutate(MaxPrev = ifelse(grepl("2",COTYPE),2,
                             ifelse(grepl("1", COTYPE), 1,
                                    ifelse(grepl("0", COTYPE), 0, NA)))) %>%
  mutate(DB="LIFE") %>%
  rename(ID=FID, Latitude=LATITUDE, Longitude=LONGITUDE) %>%
  select(ID, Latitude, Longitude, MaxPrev, DB) %>%
  filter(!is.na(Latitude))

#append DAFI and LIFE
pointDBs <- DAFI.supp.shp %>% bind_rows(LIFE.supp.summ)


#calc summaries of maximum prevention level in a given region
GFUS.Q15a.regions <- GFUS.raw.split %>%
  select(Q1b, Q15a, Q15b) %>%
  filter(Q15b > 3) %>%   #only use records with high or very high confidence
  group_by(Q1b) %>%
  summarise(mean15a = mean(Q15a), max15a=max(Q15a), min15a=min(Q15a), count=n()) %>%
  mutate(Q1b = as.numeric(Q1b)) 

GFUS.15a.shp <- GFUS.Q15a.regions %>%
  right_join(GFUS.shp, join_by('Q1b'=='regnum')) %>%
  st_as_sf()

plot(GFUS.15a.shp['mean15a'])

st_write(GFUS.15a.shp, "data\\GFU_15a.shp", driver = "ESRI Shapefile")

mapbbox <- EUbb

for(mapbbox in bboxes){
#plot
g <- ggplot() + 
  geom_sf(data = st_geometry(GFUS.15a.shp), color='lightgrey') +
  geom_sf(data = filter(GFUS.15a.shp, !is.na(mean15a)), aes(fill = mean15a), color=NA) + 
  #geom_sf(data = filter(GFUS.15a.shp, !is.na(count)), aes(fill = count), color=NA) + 
  scale_fill_distiller(palette='Reds') +
  new_scale_fill() +
  #geom_point(data = filter(DAFI.supp.all, !is.na(meanMP)), 
  geom_point(data = filter(DAFI.supp.sngo, !is.na(meanMP)), 
             aes(fill=meanMP, x=Longitude, y=Latitude),
             size=2,shape=21,,colour='black', alpha=1) +
  scale_fill_distiller(palette='Reds') +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_sf(xlim=c(mapbbox[1],mapbbox[3]),ylim=c(mapbbox[2],mapbbox[4]))  +
  #ggtitle("Supp/Prev/Control (1 high) - GFUS Polys - DAFI Points (All)") +
  ggtitle("Supp/Prev/Control (1 high) - GFUS Polys - DAFI Points (State, NGO)") 
  
  

#old plot for both LIFE and DAFI
#g <- ggplot() + 
#  geom_sf(data = st_geometry(GFUS.15a.shp), color='lightgrey') +
#  geom_sf(data = filter(GFUS.15a.shp, !is.na(mean15a)), aes(fill = mean15a), 
#          color=NA) + 
#  scale_fill_distiller(palette='Reds', guide= "none") +
#  new_scale_fill() +
#  geom_point(data = filter(pointDBs, !is.na(MaxPrev)), 
#          aes(fill=MaxPrev, x=Longitude, y=Latitude, shape=DB),colour='darkblue',size=2) +
#  scale_fill_distiller(palette='Reds') +
#  scale_shape_manual(values=c(21,22)) +
#  theme_light() +
#  coord_sf(xlim=c(mapbbox[1],mapbbox[3]),ylim=c(mapbbox[2],mapbbox[4]))

print(g)
}

#practices

# Function to count distinct words in a string
count_distinct_words <- function(x) {
  str_split(x, ",") %>%
    unlist() %>%
    unique() %>%
    length()
}


#count unique practices by user
#does not check if practice is the same between users as practice IDs are not consistent across users - see data_dictionary.xlsx)
GFUS.Q5.regions <- GFUS.raw.split %>%
  select(Q1b, Q5a,Q5c, Q5e) %>%
  group_by(Q1b) %>%
  #replaces NA in Q5a with the first non-NA value within each group
  mutate(Q5a = replace_na(Q5a, first(na.omit(Q5a)))) %>%  
  #mutate_at(c("Q5a", "Q5c", "Q5e"), ~replace_na(., first(na.omit(.)))) %>%
  #combine all strings in the group into a single string 
  mutate(allA = paste0(Q5a, collapse = ",")) %>%
  #mutate_at(c("Q5a", "Q5c", "Q5e"), ~paste0(., collapse = ",")) %>%
  #re-assign 'NA' characters that have been created to NA
  mutate(allA = na_if(allA, 'NA')) %>%
  #mutate_at(c("Q5a", "Q5c", "Q5e"), ~na_if(., 'NA')) %>%
  #count unique 'words' (i.e. uses)
  mutate(GFUS_SH = sapply(allA, count_distinct_words)) %>%
  #mutate_at(c("Q5a", "Q5c", "Q5e"), ~sapply(., count_distinct_words)) %>%
  distinct(Q1b, .keep_all=T) %>%
  #mutate(practices = rowSums(across(c("Q5a", "Q5c", "Q5e")))) %>%
  mutate_at("Q1b", as.numeric )
  

GFUS.Q5.shp <- GFUS.Q5.regions %>%
  right_join(GFUS.shp, join_by('Q1b'=='regnum')) %>%
  st_as_sf()



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


mapbbox <- SAbbox

pmap <- ggplot() + 
  geom_sf(data = st_geometry(GFUS.Q5.shp), color='lightgrey') +
  geom_sf(data = filter(GFUS.Q5.shp, !is.na(GFUS_SH)), aes(fill = GFUS_SH), color=NA) + 
  scale_fill_distiller(palette='Reds') +
  #new_scale_fill() +
  geom_point(data = filter(LIFE.use, !is.na(LIFE_SH)), 
             aes(size=LIFE_SH, x=LONGITUDE, y=LATITUDE),colour='blue', alpha=0.4) +
  theme_light() +
  coord_sf(xlim=c(mapbbox[1],mapbbox[3]),ylim=c(mapbbox[2],mapbbox[4]))

pmap


#governance

#should Regulations trump Incentives trump Voluntary??  
#or can we find some way to show when there are multiple  
DAFI.gov.summ <- DAFI.gov %>%
  mutate(governance = case_when(grepl('Yes',`Fire restricted`) ~ 'Regulation',
                                grepl('Yes',`Fire banned`) ~ 'Regulation',
                                grepl('Yes',`Economic incentives`) ~ 'Incentive',
                                .default=NA)) %>%
  filter(!is.na(governance))


#should Regulations trump Incentives trump Voluntary??   
#or can we find some way to show when there are multiple
GFUS.Q16a.regions <- GFUS.raw.split %>%
  select(Q1b, Q16a) %>%
  mutate(Q1b = as.numeric(Q1b)) %>%
  mutate(gov = case_when(grepl('1|2|3|4',Q16a) ~ 'Regulation',
                         grepl('5|6',Q16a) ~ 'Incentive',
                         grepl('7',Q16a) ~ 'Voluntary',
                         .default=NA)) %>%
  filter(!is.na(gov))  %>%
  group_by(Q1b) %>%
  summarise("RegN" = sum(gov=='Regulation'),
            "IncN" = sum(gov=='Incentive'),
            "VolN" = sum(gov=='Voluntary')) %>%
  mutate(governance = ifelse(RegN > 0, "Regulation",
                             ifelse(IncN > 0, "Incentive",
                                    ifelse(VolN > 0 , "Voluntary", NA))))

GFUS.16a.shp <- GFUS.Q16a.regions %>%
  right_join(GFUS.shp, join_by('Q1b'=='regnum')) %>%
  st_as_sf()

plot(GFUS.16a.shp['governance'])

#CHALLENGE!
#LIFE looks like governance instances can be linked only to sources, not to cases
#and it is cases that has point locations... 



##GFED  - see GFED.r

GFED16_dBF <- rast("data\\GFED_dBF_2016.nc", drivers="NETCDF")
GFED16_dBF



mapbbox <- Afbb

for(mapbbox in bboxes){
  
  g <- ggplot() +
    geom_sf(data = st_geometry(GFUS.shp), color='lightgrey') +
    geom_raster(data = as.data.frame(GFED16_dBF, xy = TRUE), 
                aes(x = x, y = y, fill = dBF)) + 
    scale_fill_continuous_divergingx(palette='RdYlBu') +
    #coord_quickmap() +
    #geom_sf(data = st_geometry(GFUS.shp), color='lightgrey', fill=NA) +
    geom_point(data = filter(LIFE.use, !is.na(LIFE_SH)),
               aes(x=LONGITUDE, y=LATITUDE), 
               size=1,shape=3,fill='red',colour='black', alpha=0.5, stroke=0.2) +
    coord_sf(xlim=c(mapbbox[1],mapbbox[3]),ylim=c(mapbbox[2],mapbbox[4])) +
    theme_light() +
    theme(axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
    ggtitle("2016 GFED Difference (cell fraction) - LIFE Practices Data")

  plot(g)
 
}

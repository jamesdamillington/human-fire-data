library(terra)
library(rhdf5)  #I needed to install via BiocManager::install("rhdf5") for R 4.4.3 (on 20/03/25) 
library(quickPlot)   #for divergent colout palette
library(ncdf4)
library(colorspace)


#GFED4s data for 2020 seems to be missing burned_area variables
#GFED4_2020 <- rast("data\\GFED4\\GFED4.1s_2020_beta.hdf5", subds="//burned_area/04/burned_fraction")
#names(GFED4_2020)


##GFED5
GFED5_BA <- rast("data\\GFED5\\BA201601.nc", lyrs="Total")  #check total is the layer we want!

for(i in 2:12){
  mon <- formatC(i,digits=1,format = "d", flag = "0")
  lyr <- rast(paste0("data\\GFED5\\BA2016",mon,".nc"), lyrs="Total")
  
  GFED5_BA <- c(GFED5_BA, lyr)
}

#plot(GFED5_BA)
#summary(GFED5_BA)

GFED5_sumBA <- sum(GFED5_BA)
GFED5_sumBA[GFED5_sumBA==0] <- NA
#plot(GFED5_sumBA, main="GFED5 BA")


#this was used to check if BurnableArea was same as GFED4s ancil grid_cell_area below
#it is not, so use cellSize
#GFED5_b <- rast("data\\GFED5\\BurnableArea.nc")  
#plot(GFED5_b)

#calc cell size for burned fraction calc
GFED5_CS <- cellSize(GFED5_sumBA)
GFED5_CSkm <- GFED5_CS / 1000000
#plot(GFED5_CSkm, main="GFED5 CS")

GFED5_sumBF <- GFED5_sumBA / GFED5_CSkm
#plot(GFED5_sumBF, main="GFED5 2016, Burned Fraction")

##GFED4
#with help from https://www.neonscience.org/resources/learning-hub/tutorials/create-raster-stack-hsi-hdf5-r
GFED4_BF <- h5read("data\\GFED4\\GFED4.1s_2016.hdf5", name="//burned_area/01/burned_fraction")
GFED4_BF <- t(GFED4_BF)
GFED4_BF <- rast(GFED4_BF, crs="epsg:4326")
ext(GFED4_BF) <- ext(GFED5_BA)

for(i in 2:12){
  mon <- formatC(i,digits=1,format = "d", flag = "0")
  lyr <- h5read("data\\GFED4\\GFED4.1s_2016.hdf5", name=paste0("//burned_area/",mon,"/burned_fraction"))
  lyr <- t(lyr)
  lyr <- rast(lyr, crs="epsg:4326")
  ext(lyr) <- ext(GFED5_BA)
  
  GFED4_BF <- c(GFED4_BF, lyr)
}

#plot(GFED4_BF)
#summary(GFED4_BF)

GFED4_sumBF <- sum(GFED4_BF)
GFED4_sumBF[GFED4_sumBF==0] <- NA
#plot(GFED4_sumBF, main="GFED4 2016, Burned Fraction")


GFED_dBF <- GFED5_sumBF - GFED4_sumBF
#plot(GFED_dBF)
#writeCDF(GFED_dBF, "data\\GFED_dBF_2016.nc", varname="dBF", 
#         longname="difference between GFED5 BA and GFED4s BF", unit="fraction")



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



mapbbox <- Afbb
mapbbox <- SEASbb
mapbbox <- worldbb

for(mapbbox in bboxes){
  
  g <- ggplot() +
    geom_sf(data = st_geometry(GFUS.shp), color='lightgrey') +
    geom_raster(data = as.data.frame(GFED_dBF, xy = TRUE), 
                aes(x = x, y = y, fill = sum)) + 
    scale_fill_continuous_divergingx(palette='RdYlBu') +
    #coord_quickmap() +
    #geom_sf(data = st_geometry(GFUS.shp), color='lightgrey', fill=NA) +
    geom_point(data = filter(LIFE.use, !is.na(LIFE_SH)),
               aes(x=LONGITUDE, y=LATITUDE), 
               #size=1,shape=3,fill='red',colour='red', alpha=0.5, stroke=0.2) +
               size=1.5,shape=3,fill='red',colour='red', alpha=0.75, stroke=0.75) +
    coord_sf(xlim=c(mapbbox[1],mapbbox[3]),ylim=c(mapbbox[2],mapbbox[4])) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("2016 GFED Difference (cell fraction) - LIFE Practices Data")
  
  plot(g)
  
}


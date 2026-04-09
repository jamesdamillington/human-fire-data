library(terra)
library(colorspace)   #for divergent colout palette
library(sf)
library(dplyr)
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

#bb <- Afbb

#burned fraction
for (i in seq_along(bboxes)) {
  bb <- bboxes[[i]]
  nm <- names(bboxes)[i]
  
  g <- ggplot() +
    geom_sf(data = st_geometry(GFUS.shp), color='lightgrey') +
    geom_raster(data = as.data.frame(GFED_2016dBF, xy = TRUE), 
                aes(x = x, y = y, fill = dBF)) + 
    scale_fill_continuous_divergingx(palette='RdYlBu') +
    coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4])) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("2016 GFED Difference (cell fraction)")
  
  ggsave(paste0("png/gfed/Fig_GFED-2016-dBF-",nm,".png"),
         plot=g,dpi=300,device = ragg::agg_png)
}


#burned area
for (i in seq_along(bboxes)) {
  bb <- bboxes[[i]]
  nm <- names(bboxes)[i]

  g <- ggplot() +
    geom_sf(data = st_geometry(GFUS.shp), color='lightgrey') +
    geom_raster(data = as.data.frame(GFED_2016dBA, xy = TRUE), 
                aes(x = x, y = y, fill = dBA)) + 
    scale_fill_continuous_divergingx(palette='RdYlBu') +
    coord_sf(xlim=c(bb[1],bb[3]),ylim=c(bb[2],bb[4])) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle("2016 GFED Difference (BA, sq km)")
  
  ggsave(paste0("png/gfed/Fig_GFED-2016-dBA-",nm,".png"),
         plot=g,dpi=300,device = ragg::agg_png)
}



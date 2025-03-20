library(terra)
library(rhdf5)  #I needed to install via BiocManager::install("rhdf5") for R 4.4.3 (on 20/03/25) 
library(quickPlot)   #for divergent colout palette
library(ncdf4)

#GFED4s data for 2020 seems to be missing burned_area variables
#GFED4_2020 <- rast("data\\GFED4\\GFED4.1s_2020_beta.hdf5", subds="//burned_area/04/burned_fraction")
#names(GFED4_2020)


##GFED5
GFED5 <- rast("data\\GFED5\\BA201601.nc", lyrs="Total")  #check total is the layer we want!

for(i in 2:12){
  mon <- formatC(i,digits=1,format = "d", flag = "0")
  lyr <- rast(paste0("data\\GFED5\\BA2016",mon,".nc"), lyrs="Total")
  
  GFED5 <- c(GFED5, lyr)
}

plot(GFED5)
summary(GFED5)

GFED5sum <- sum(GFED5)
GFED5sum[GFED5sum==0] <- NA
plot(GFED5sum, main="GFED5")


##GFED4
#with help from https://www.neonscience.org/resources/learning-hub/tutorials/create-raster-stack-hsi-hdf5-r
GFED4 <- h5read("data\\GFED4\\GFED4.1s_2016.hdf5", name="//burned_area/01/burned_fraction")
GFED4 <- t(GFED4)
GFED4 <- rast(GFED4, crs="epsg:4326")
ext(GFED4) <- ext(GFED5)

for(i in 2:12){
  mon <- formatC(i,digits=1,format = "d", flag = "0")
  lyr <- h5read("data\\GFED4\\GFED4.1s_2016.hdf5", name=paste0("//burned_area/",mon,"/burned_fraction"))
  lyr <- t(lyr)
  lyr <- rast(lyr, crs="epsg:4326")
  ext(lyr) <- ext(GFED5)
  
  GFED4 <- c(GFED4, lyr)
}

plot(GFED4)
summary(GFED4)

#get cell area (convert to km2) to multiply by burned_fraction
ancil <- h5read("data\\GFED4\\GFED4.1s_2016.hdf5", name="//ancill/grid_cell_area")
ancil <- t(ancil)
ancil <- rast(ancil, crs="epsg:4326")
ext(ancil) <- ext(GFED5)
ancil <- ancil / 1000000
GFED4 <- GFED4 * ancil


GFED4sum <- sum(GFED4)
GFED4sum[GFED4sum==0] <- NA
plot(GFED4sum, main="GFED4")


GFEDdiff <- GFED5sum - GFED4sum
#writeCDF(GFEDdiff, "data\\GFEDdiff_2016.nc", varname="BAdiffc", 
#        longname="difference between GFED5 BA and GFED4s BA", unit="sq km")


##plotting
plot(GFEDdiff, main="GFED Difference", col=divergentColors("blue", "red",
                                                           min.value=minmax(GFEDdiff)[1], max.value=minmax(GFEDdiff)[2],
                                                           mid.value=0, mid.color="lightgrey"))

GFEDpos <- GFEDdiff
GFEDpos[GFEDpos<=0] <- NA
plot(GFEDpos, col="grey", main="Increased")


GFEDneg <- GFEDdiff
GFEDneg[GFEDneg>=0] <- NA
plot(GFEDneg, col="grey", main="Decreased")

      
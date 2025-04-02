library(terra)
library(rhdf5)  #I needed to install via BiocManager::install("rhdf5") for R 4.4.3 (on 20/03/25) 
library(quickPlot)   #for divergent colout palette
library(ncdf4)

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

plot(GFED5_BA)
summary(GFED5_BA)

GFED5_sumBA <- sum(GFED5_BA)
GFED5_sumBA[GFED5_sumBA==0] <- NA
plot(GFED5_sumBA, main="GFED5 BA")


#this was used to check if BurnableArea was same as GFED4s ancil grid_cell_area below
#it is not, so use cellSize
#GFED5_b <- rast("data\\GFED5\\BurnableArea.nc")  
#plot(GFED5_b)

#calc cell size for burned fraction calc
GFED5_CS <- cellSize(GFED5_sumBA)
GFED5_CSkm <- GFED5_CS / 1000000
plot(GFED5_CSkm, main="GFED5 CS")

GFED5_sumBF <- GFED5_sumBA / GFED5_CSkm
plot(GFED5_sumBF, main="GFED5 2016, Burned Fraction")

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

plot(GFED4_BF)
summary(GFED4_BF)

GFED4_sumBF <- sum(GFED4_BF)
GFED4_sumBF[GFED4_sumBF==0] <- NA
plot(GFED4_sumBF, main="GFED4 2016, Burned Fraction")

#get cell area (convert to km2) to multiply by burned_fraction
ancil <- h5read("data\\GFED4\\GFED4.1s_2016.hdf5", name="//ancill/grid_cell_area")
ancil <- t(ancil)
ancil <- rast(ancil, crs="epsg:4326")
ext(ancil) <- ext(GFED5_BA)
ancilkm <- ancil / 1000000
GFED4_BA <- GFED4_BF * ancilkm

#plot(GFED5_b, ancil)  #BurnableArea and ancil are not identical 
plot(GFED5_CS, ancil) #calculated cellsize and ancil are identical

GFED4_sumBA <- sum(GFED4_BA)
GFED4_sumBA[GFED4_sumBA==0] <- NA
plot(GFED4_sumBA, main="GFED4 2016, Burned Area (sq km)")





GFED_dBA <- GFED5_sumBA - GFED4_sumBA
plot(GFED_dBA)
writeCDF(GFED_dBA, "data\\GFED_dBA_2016.nc", varname="dBA", 
        longname="difference between GFED5 BA and GFED4s BA", unit="sq km",
        overwrite=T)


GFED_dBF <- GFED5_sumBF - GFED4_sumBF
plot(GFED_dBF)
writeCDF(GFED_dBF, "data\\GFED_dBF_2016.nc", varname="dBF", 
         longname="difference between GFED5 BA and GFED4s BF", unit="fraction")





##plotting
plot(GFED_dBA, main="GFED dBA", col=divergentColors("blue", "red",
                                                    min.value=minmax(GFED_dBA)[1], 
                                                    max.value=minmax(GFED_dBA)[2],
                                                    mid.value=0, mid.color="lightgrey"))

##plotting
plot(GFED_dBF, main="GFED dBF", col=divergentColors("blue", "red",
                                                    min.value=minmax(GFED_dBF*100)[1], 
                                                    max.value=minmax(GFED_dBF*100)[2],
                                                    mid.value=0, mid.color="lightgrey"))


GFEDpos <- GFEDdiff
GFEDpos[GFEDpos<=0] <- NA
plot(GFEDpos, col="grey", main="Increased")


GFEDneg <- GFEDdiff
GFEDneg[GFEDneg>=0] <- NA
plot(GFEDneg, col="grey", main="Decreased")

      
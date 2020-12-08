#Libraries
library("spgwr")
library("spatstat")
library("tmap")
library("gstat")
library("sf")
library("raster")
library("rgdal")
library("e1071")
library("spdep")
library("lubridate")
library("gtable")
library("gridExtra")
library("grid")
library("ggplot2")
library("dplyr")
library("bcmaps")
library('bcmapsdata', repos='https://bcgov.github.io/drat/')
library("raster")
library("maps")
library("rgeos")


#Set working directory
dir <- "C:/Users/nicol/Documents/Year_1_school/518/VRI_FINAL_PROJECT"
setwd(dir)

#Reading in elevation dataset
elev <- readOGR("ElevSample.shp")
elev <- spTransform(elev, CRS("+init=epsg:26910"))

#Rename columns in elev
colnames(elev@data) <- c("FID", "Elevation")
head(elev)

#Reading in VRI data
VRI <- readOGR("WatershedVRI.shp") #Read in shapefile
VRI <- spTransform(VRI, CRS("+init=epsg:26910"))
head(VRI@data)

vriCleanCols <- c("FID_VEG_CO", "POLYGON_ID", "PROJ_AGE_1",
                  "SITE_INDEX", "SPECIES__4", "SPECIES__5",
                  "PROJ_HEI_1", "SPECIES_PC", "SPECIES__6",
                  "VRI_LIVE_S", "BASAL_AREA", "WHOLE_STEM",
                  "CROWN_CL_1")

## Make a subset with only the columns of interest
vriClean <- VRI[,vriCleanCols]


## Rename the columns in the subset data to understandable names
newNames <- c("FID", "PolyID", "Stand_Age", "Site_Index",
              "CoDom_Sp", "Dom_Sp", "Stand_HT", "DomSP_Perc", 
              "CDomSP_Perc", "Stand_Dens", "Stand_BA", "Stand_StemBio", "Stand_CrownCl")

## The new column names are explained below: 
# FID = Field ID
# PolyID = VRI Polygon ID
# Stand_Age = Estimated stand age projected to 2020 from estimated establishment date
# Site_Index = A value to estimate site quality. This describes the height that the stand could grow to by age 50 in meters.
# CoDom_Sp = The species code for the co-dominant tree species. Full list of codes: https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm
# Dom_Sp = The species code for the dominant tree species. Full list of codes: https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm
# Stand_HT = The estimated height for the stand
# DomSP_Perc = The estimated percentage of the dominent species
# CDomSP_Perc = The estimated percentage of the co-dominent species
# Stand_Dens = Estimated density of stand (Stems per hectare)
# Stand_BA = Estimated Basal area of the stand (square meters)
# Stand_StemBio = Estimated stand level stem biomass (tonnes per hectare)
# Stand_CrownCl = The percentage of ground area covered by tree crowns


## Add the new column names to the cleaned data set and view the new columns 
colnames(vriClean@data) <- newNames
head(vriClean@data) 


## Choose a Variable and select only the observations that have a value in BOTH columns (remove NA values with "!is.na()")
vriClean <- vriClean[!is.na(vriClean@data$Stand_StemBio),]

#Remove all polygons with 0 values for stem biomass
vriClean <-  vriClean[which(vriClean$Stand_StemBio > 0), ]

#Confirm type of data 
typeof(vriClean$Stand_StemBio)
typeof(elev$Elevation)

# Process elevation data 

#create an elevation subset that falls within the VRI data (all polygons, not just the ones with stembio>0)
elev_subset <- elev[VRI, ]

#plot the extent of both shape files (elevation and VRI), notice how points fall outside of VRI
plot(vriClean)
plot(elev, col="blue", add=TRUE)
plot(elev_subset, col="red", add=TRUE)

###########
#create a study area map
#Change tmap to view to create an interactive map 
tmap_mode("view")                           

#### Isolate lake for text label

#Create Study area map of stem biomass 
map_Bio <-  tm_shape(VRI) +tm_polygons(palette= "Greys", title= "VRI ")+
  tm_shape(vriClean) +
  tm_polygons(col = "Stand_StemBio",
              title = "Stand Stem Biomass (tonnes/ha)",
              style = "fixed",
              breaks= c(0, 100, 200, 300, 400, 900),
              palette = "Greens", n = 6) +
  tm_shape(elev) + tm_dots(size=0.1, col = "black", title= "All Elevation observations")+
  tm_legend()
map_Bio
tmap_mode(plot)
###########################################################
#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(elev, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(elev)

#########################################################





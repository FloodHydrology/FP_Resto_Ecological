#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Name: Flow to stage relationshiop 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 2/1/2022
#Purpose: Create script to inundate DEM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Semi Code (for each landscape)
# -Load DEM
# -Create 10 XS
# -Describe average stage-area and stage-wp for XS
# -Guestimate mannings N (maybe using existing stage-discharge relationship?)
# -estimate stage-discharge based parameteriziation

#Note, snag XS code from TBS work
# -https://github.com/bamaecohydro/CP_LandUse_Legacies/blob/main/R/0_demo.R

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())


#Load required packages
library(tidyverse) #join the cult!
library(raster)
library(sf)
library(whitebox)
library(stars)
library(fasterize)
library(mapview)
library(parallel)

#load data of interest
dem<-raster("data/spatialdata/dem_contemp")
stream<-st_read("data/spatialdata/centerline.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: XS Analysis ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Crop stream layer to dem
stream<-st_crop(stream, dem)

#Define distance between cross sections
dist<- 10 #m

#Estimate number of cross sections to create based on distance
n_points<-sum(st_length(stream))/dist 
n_points<-n_points %>% as.numeric(.) %>% round()

#Create points along flow lines
stream_pnts<-st_union(stream)
stream_pnts<-st_line_merge(stream_pnts)
stream_pnts<-as_Spatial(stream_pnts, cast=FALSE)
stream_pnts<-spsample(stream_pnts, n = n_points, type="regular")
stream_pnts<-st_as_sf(stream_pnts)

##################--------------------
#Start here bozo ---------------------
##################--------------------


xs_fun<-function(n, width=200){
  #For testing
  pnt<-stream_pnts[n,]
  
  #Define flowline segment
  reach<-st_intersection(stream, st_buffer(pnt, dist = 1))
  reach_backup<-reach
  
  #Estimate planar slope
  reach<-st_coordinates(reach)
  reach_slope<-(reach[1,"Y"]-reach[nrow(reach),"Y"])/(reach[1,"X"]-reach[nrow(reach),"X"])
  
  #Estimate inverse slope
  xs_slope <- -1/reach_slope
  
  #Estimate endpoints of XS
  xs_coord <- st_coordinates(pnt)
  xs_coord <-rbind(
    xs_coord, 
    matrix(0, nrow=2, ncol=2)
  )
  xs_coord[2,"X"] <- xs_coord[1,"X"] + width/2*cos(atan(xs_slope))
  xs_coord[2,"Y"] <- xs_coord[1,"Y"] + width/2*sin(atan(xs_slope))
  xs_coord[3,"X"] <- xs_coord[1,"X"] - width/2*cos(atan(xs_slope))
  xs_coord[3,"Y"] <- xs_coord[1,"Y"] - width/2*sin(atan(xs_slope))
  xs_coord<-xs_coord[-1,]
  
  #Create XS
  xs<-xs_coord %>%  
    as_tibble() %>% 
    st_as_sf(coords = c("X","Y")) %>% 
    st_coordinates() %>% 
    st_linestring() %>% 
    st_sfc(.) %>% 
    st_set_crs(st_crs(dem@crs)) %>% 
    st_as_sf() 
  
  #Export XS Shape
  xs
}


#plots for funzies
plot(dem)
stream %>% st_geometry() %>% plot(add=T)
stream_pnts %>% st_geometry() %>% plot(add=T)




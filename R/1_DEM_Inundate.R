#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Name: Inundation Script
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 2/4/2021
#Purpose: Create script to inundate DEM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
rm(list=ls(all=TRUE))

#Download packages
library(tidyverse)
library(raster)
library(stars)
library(gstat)
library(sf)

#set relevant directories
dir.create('data//temp')
temp_dir<-"data//temp//"
spatial_dir<-"data//spatialdata//"

#download data
dem_rest<-raster(paste0(spatial_dir, "dem_restored"))
dem_cont<-raster(paste0(spatial_dir, "dem_contemp"))
cnt<-st_read(paste0(spatial_dir,"CenterLine.shp"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: DEM Inundate Fun-----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Setup function workspace
fun<-function(
  dem, #Floodplain DEM
  output_dir #where the inundation maps are stored
){

#Download packages
library(tidyverse)
library(raster)
library(stars)
library(gstat)
library(sf)
  
#2.2 DEM Processing ----------------------------------------
#Create Clip
fp_clip<-dem*0
fp_clip_poly<-fp_clip %>% st_as_stars() %>% st_as_sf(., merge=T)

#Clip centerline
cnt<-st_intersection(cnt, fp_clip_poly)

#Create raster
cnt<-rasterize(cnt, fp_clip)

#Extract Elevation
cnt<-cnt*0+dem

#create points
cnt<-rasterToPoints(cnt, spatial=F)
cnt<-data.frame(cnt)
colnames(cnt)<-c("x","y", "ele")

#Create IDW Raster
IDW<-gstat(id="layer", formula=ele~1, locations=~x+y, data=cnt, nmax=7, set=list(idp=4.2))
IDW<-interpolate(fp_clip, IDW)
IDW<-mask(IDW, fp_clip)

#Correct dem for valley slope
dem_norm<-dem-IDW

#Create Minimum Raster
dem_min<-dem*0+minValue(dem_norm)

#2.2 Inundation Estimate------------------------------------
#Define max increase and step increase heights
zmax<-3
dz<-0.1

#Create function to return conditional raster 
Con<-function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)
}

#Create Dataframe to house information
df<-data.frame(matrix(0, ncol=3, nrow=zmax/dz))
colnames(df)<-c("relative_ele", "area", "volume")
df$relative_ele<-seq(dz,zmax, dz)

#Loop through inundation sims
for(i in 1:(zmax/dz)){
  #define satge increase
  z<-dz*i
  
  #calculate area and volume rasters
  area<-Con(dem_norm>(dem_min+z),0,1)
  volume<-(((z+dem_min)-dem_norm)*area)*res(dem)[1]*res(dem)[2]
  
  #add to df
  df$area[i]<-cellStats(area, 'sum')*res(area)[1]*res(area)[2]
  df$volume[i]<-cellStats(volume, 'sum')
  
  #Export volume raster
  fun <- function(x) { x[x<0.001] <- NA; return(x) }
  volume<-calc(volume, fun)
  writeRaster(volume,paste0(output_dir,"Inundate",i), format="GTiff", overwrite=T)
}

#Export df into global env
df
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Execute function-----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create output dir
cont_dir<-paste0(temp_dir, "cont//")
rest_dir<-paste0(temp_dir, "rest//")
dir.create(cont_dir)
dir.create(rest_dir)

#Apply function
rest<-fun(dem_rest, rest_dir)
cont<-fun(dem_cont, cont_dir)

#Export data
rest<-rest %>% mutate(dem='rest')
cont<-cont %>% mutate(dem='cont')
df<-bind_rows(rest, cont)
write_csv(df, paste0(temp_dir, "stage_relationships.csv"))

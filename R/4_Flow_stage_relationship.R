#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Name: Flow to stage relationshiop 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/4/2023
#Purpose: Create rating curve for reach based on Zhang et al., 2018
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This code is roughly based on Zheng et al., 2018
#     http://doi.org/10.1111/1752-1688.12661

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
rm(list=ls(all=TRUE))

#Download packages
library(tidyverse)
library(raster)
library(stars)
library(gstat)
library(sf)
library(mapview)

#set relevant directories
dir.create('data//temp')
temp_dir <- "data//temp//"
spatial_dir <- "data//spatialdata//"

#download data
dem_rest <- raster(paste0(spatial_dir, "dem_restored"))
dem_cont <- raster(paste0(spatial_dir, "dem_contemp"))
cnt <- st_read(paste0(spatial_dir,"CenterLine.shp"))

#pull in dem inundate result
df <- read_csv(paste0(temp_dir, "stage_relationships.csv"))

#Plot for funzies
mapview(dem_rest) + mapview(cnt)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Create stage-flow relationships for each floodplain surface -----------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define centerline length
cnt<-st_crop(cnt, dem_rest)
reach_length <- st_length(cnt)
df$reach_length <- as.numeric(reach_length)

#Define reach slope (restored)
cnt_rest_grd <- rasterize(cnt, dem_rest)
cnt_rest_grd <- cnt_rest_grd*dem_rest
slope_rest <- (cellStats(cnt_rest_grd, max, na.rm=T) - cellStats(cnt_rest_grd, min, na.rm=T))/reach_length

#Define reach slope (contemporary)
cnt_cont_grd <- rasterize(cnt, dem_cont)
cnt_cont_grd <- cnt_cont_grd*dem_cont
slope_cont <- (cellStats(cnt_cont_grd, max, na.rm=T) - cellStats(cnt_cont_grd, min, na.rm=T))/reach_length

#Define reach slope
df$slope <- slope_cont #ifelse(df$dem=="rest", slope_rest, slope_cont)
df$slope <- as.numeric(df$slope)

#Estimate average XS area
df$A_xs <- df$volume/df$reach_length
df$A_xs <- as.numeric(df$A_xs)

#Estimate bed area
df$A_bed <- df$area*(1 + (df$slope)^2)^0.5

#Estiamte wetted perimeter
df$wetted_perimeter <- df$A_bed/df$reach_length

#Estiamte average top width
df$width_average <- df$area/df$reach_length

#estimate hydraulic radius
df$R0 <- df$A_xs/df$wetted_perimeter

#Defiune mannings roughness
df$n <- 0.035

#Estimate Flow
df$Q <- (1/df$n)*df$A_xs*((df$R0)^(2/3))*((df$slope)^0.5)
df$Q <- as.numeric(df$Q)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Plot and Export -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Tidy data
df <- df %>% 
  select(relative_ele, Q, dem) %>% 
  pivot_wider(values_from = Q, names_from = dem) 

#Export
write_csv(df,paste0(temp_dir, "stage_Q.csv"))

#Plot
df %>%  
  #Filter to below 2
  filter(relative_ele<2) %>% 
  ggplot() +
  #add line data
  geom_line(
    aes(x=relative_ele, y=rest), 
    lwd=2, 
    col="steelblue") +
  geom_line(
    aes(x=relative_ele, y=cont), 
    lwd=2, 
    col="darkorange") +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Stage [m]") + 
  ylab("Flow [cms]") 
  


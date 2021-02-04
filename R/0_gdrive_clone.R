#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Google Drive Clone
#Coder: Nate Jones (cnjones7@ua.edu)
  #Date: 1/28/2021
#Purpose: Download gdrive with complete control
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Helpful link
#   https://community.rstudio.com/t/how-to-download-a-google-drives-contents-based-on-drive-id-or-url/16896/12


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear memory
remove(list=ls())

#Add libraries of interest
library('tidyverse')
library("googledrive")
library('parallel')

#Define google folder of interest 
#  (https://drive.google.com/drive/folders/[gdrive_id])
gdrive_id<-'1JF5ug6B4QIiIDnxne8Rcaz_pq5V5ecvg'

#Initiate connection with gdrive
drive_find(n_max = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Clone folder schema----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create data file
dir.create("data")

#Define total number of folders in gdrive
n_folders<-drive_ls(
  path = as_id(gdrive_id), 
  type='folder', 
  recursive = T) %>% 
  nrow()

#Create function to identify files and create path name recursively
file_ls<-function(gid){
  #Define google drive folders
  drive_ls(
    path = as_id(gid), 
    type='folder', 
    recursive  = TRUE) %>% 
    #Add local folder name
    mutate(local_name = paste0("data//",name))
}

#Start files list
files<-file_ls(gdrive_id)

#Create while loop to identify embedded folders
i<-1
n<-nrow(files)
while(i<=n){
  #Identify file
  temp<-file_ls(files$id[i])
  
  #create local name
  temp$local_name<-paste0(files$local_name[i],"//", temp$name)
  
  #bind to file
  files <- bind_rows(files, temp)
  
  #Add counter
  n<-nrow(files)
  i<-i+1
  print(i)
}

#Create local copy of folder schema
lapply(files$local_name, dir.create)

#Add root dir file
files<-
  tibble(
    name = NA,
    id = gdrive_id, 
    drive_resource=NA, 
    local_name = "data") %>% 
  bind_rows(., files) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Create function to copy files in each folder---------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fun<-function(n){
  
  #Define required libraries
  library('tidyverse')
  library("googledrive")
  
  #Create list of objects in folder
  temp<-drive_ls(as_id(files$id[n]))
  
  #Add output file path
  temp<-temp %>% 
    mutate(local_name = paste0(files$local_name[n],"//",name))
  
  #Create inner download function
  inner_fun<-function(m){
    tryCatch(
      drive_download(
        file = as_id(temp$id[m]),
        path = temp$local_name[m]
      ), 
      error = function(e) NA
    )
  }
  
  #Apply inner function to list of objects
  lapply(seq(1,nrow(temp)), inner_fun)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Apply function in parrallel -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define number of cores available
n_cores<-3

#Create clusters
cl <- makeCluster(n_cores)

#Send environmental vars to clusters
clusterExport(cl,list('files','fun'))

#Execute!
parLapply(
  cl = cl, 
  fun = fun, 
  X = seq(1, nrow(files)))

#Remove working cores
stopCluster(cl)
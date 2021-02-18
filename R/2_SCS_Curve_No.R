#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#Tile: SCS Curve Number Estimate
#Title: SCS Curve Number 
#Coder: C. Nathan Jones
#Date: 1/8/2014
#Purpose: Estimate CN for Stroulbes Creek  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Set up code environment and input data --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
rm(list=ls(all=TRUE))  

#load libraries of interest
library(tidyverse)
library(lubridate)

#Download pump and flume flow data (Note, flow needs to be in L/s)
stage<-read_csv("data//stream_lab_data//CR1000Bridge1_Stage.dat", skip=3) %>% 
  select(date = X1, stage_m = Smp) %>% 
  arrange(date)
colnames<-names(read_csv("data//stream_lab_data//CR1000Met_HourlySample.dat", n_max=0))
precip<-read_csv(
    file = "data//stream_lab_data//CR1000Met_HourlySample.dat", 
    col_names = colnames, 
    skip=4) %>% 
  select(
    date = TOA5,
    precip_mm = `37156`) %>% 
  arrange(date) %>% 
  distinct()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2:Define Rainfall and Runoff Volumes for individual storms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Calculate Discharge from Stage-Discharge Relationship
Q<-matrix(0, nrow=length(stage[,2]), ncol=1) #Create flow value matrix

for(i in 1:length(stage[,2])){  #Calculate discharge with stage discharge curve
  ifelse(stage[i,2]<0.68, Q[i]<-4.8292*stage[i,2]^1.9887, Q[i]<-13.559*stage[i,2]-7.0945)
}

#create dataframe of flow data
flow.data<-data.frame(stage[,1], Q, Q/1.4164e7)
colnames(flow.data)<-c("data", "Q_cms", "q_m")

#breakpoint flow
bp<-4.8292*0.68^1.9887

#Create a vector to count the lengths of connection
count<-data.frame(matrix(0,nrow=length(flow.data[,1]), ncol=2))
for(i in 1:length(flow.data[,1])){
  #Count the number of consecutive days
  ifelse(flow.data[i,2]>bp, ifelse(count[i-1,1]>0, count[i,1]<-count[i-1,1]+1, count[i,1]<- 1), 0)
  #Calculate the Volume of floodwater 
  ifelse(count[i,1]>0, count[i,2]<-flow.data[i,2],0)
  }
count<-data.frame(count, round(flow.data[,1], units="hours"))

#Filter for overbank events
count<-subset(count, count[,2]>0) 

#Find 24 hour rainfall values
rainfall<-data.frame(matrix(0, nrow=length(count[,1]), ncol=1))
for(i in 1:length(count[,1])){
  n<-which(precip[,1]==count[i,3])
  m<-n-48
  try(rainfall[i,1]<-sum(precip[n:m,2]))
}

#Create a vector of flood volumes, duration, and storage index for each event
volume<-data.frame(matrix(0,nrow=length(subset(count, count[,1]==1)[,1]), ncol=3)) #Create volume vector
n<-1 #counter for volume matrix
m<-1
for(i in 1:length(count[,1])){
  #define postion in volume dataframe
  ifelse((count[i,1]==1 & i>1), n<-n+1, n<-n)
  #define duration count
  ifelse((count[i,1]==1 & i>1), m<-1, m<-m+1)
  #populate volume matrix with maxima
  ifelse(count[i,1]==1,volume[n,1]<-count[i,2],volume[n,1]<-count[i,2]+volume[n,1])
  #populate voume matrix with duration
  volume[n,2]<-m
  #populate volume matrix with date
  volume[n,3]<-rainfall[i,1]
}

#Calculate Runoff (cm)
runoff<-volume[,1]*volume[,2]*10*60/1.4164e8
runoff<-data.frame(volume[,3]/10, runoff*100)
colnames(runoff)<-c("P","R")

#delete events where runoff>rainfall
runoff<-runoff[runoff$R<runoff$P,]

#run least square fit
model<-nls(R~((P-0.05*S)^2)/(P+0.95*S), runoff)

nrcs<-function(P){((P-0.05*11.2)^2)/(P+0.95*11.2)}
x<-seq(from=0,to=6,length.out=100)

plot(runoff$R~runoff$P, 
     #points
     cex=1.8, pch=19, col="grey70", 
     #labels
     xlab="Precip (cm)", ylab="Runoff (cm)", 
     #label options
     cex.lab=1.4, cex.axis=1.2)
points(x,nrcs(x), type="l", lty=2, lwd=5)
text(2, 1.8,"S=11.2 cm", cex=2.2)


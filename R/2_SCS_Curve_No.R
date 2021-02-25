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

#Download gage data
flow_data<-read_csv("data//stream_lab_data//CR1000Bridge1_Stage.dat", skip=3) %>% 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2:Define Rainfall and Runoff Volumes for individual storms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Calculate Discharge from Stage-Discharge Relationship
flow_data<-flow_data %>% 
  mutate(Q_cms = if_else(stage_m<0.68, 
            4.8292*stage_m^1.9887, 
            13.559*stage_m-7.0945)) %>% 
  mutate(q_m = Q_cms/1.4164e7) %>% 
  drop_na()

#Estimate flow at breakpoint (i.e., bankful)
bp<-4.8292*0.68^1.9887

#Estimate duration of connection
events<-flow_data %>% 
  #define events
  mutate(flood = if_else(Q_cms>bp, 1,0)) %>% 
  #Define points when flood initiate
  mutate(flood_start=if_else(flood==1 & lag(flood, default = 0)==0,1, 0)) %>% 
  #Define flood_id by cummulative sum
  mutate(flood_id = cumsum(flood_start)) %>% 
  #Limit to just overbank events
  mutate(flood_id = flood_id*flood) %>% 
  filter(flood_id>0) %>% 
  #Summarize each event
  group_by(flood_id) %>% 
  summarise(
    dt = as.numeric(as.POSIXct(max(date)) - as.POSIXct(min(date)), units="secs"), 
    runoff_m = mean(q_m), 
    date = as.POSIXct(mean(date))) %>% 
  mutate(runoff_mm = dt*runoff_m*1000) %>%
  select(date, runoff_mm)

#Estimate 24 hour rainfall
precip<-precip %>% 
  #group by date
  mutate(date = ceiling_date(date, unit="day")) %>% 
  group_by(date) %>% 
  summarise(precip_mm=sum(precip_mm, na.rm=T)) %>% 
  #Sum day + previous day
  mutate(precip_mm = precip_mm + lag(precip_mm, default = 0))
  
#join events and precip
events<-events %>% 
  mutate(date = ceiling_date(date, unit="day")) %>% 
  left_join(., precip)

#delete events where runoff>rainfall
events<-events %>% filter(precip_mm>runoff_mm)

#Estimate initial abstraction
model<-nls(runoff_mm~((precip_mm-0.05*S)^2)/(precip_mm+0.95*S), data=events, start=list(S = 120))
s_mm<-summary(model)$coefficients[1]

#Estimate CN
CN<-1000/((s_mm/25.4) + 10)

#Plot  --------------
par(mar=c(5,5,0.5, 0.5))
nrcs<-function(P){((P-0.05*138.7)^2)/(P+0.95*s_mm)}
x<-seq(from=0,to=100,length.out=100)
plot(events$runoff_mm~events$precip_mm, 
     #points
     cex=1.8, pch=19, col="grey70", 
     #labels
     xlab="Precip [mm]", ylab="Runoff [mm]", 
     #label options
     ps = 12, cex.lab=14/12, cex.axis=10/12)
points(x,nrcs(x), type="l", lty=2, lwd=5)
text(20, 20,paste0("S=",round(s_mm,0), " mm"), cex=2.2)
text(20, 16,paste0("CN=",round(CN,0)), cex=2.2)


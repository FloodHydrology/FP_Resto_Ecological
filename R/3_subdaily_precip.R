#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Name: Precipitation Timeseries Modeling 
#Coder: C. Nathan Jones
#Date: 11/20/2016
#Purpose: Simulate subdaily rainfall timeseries as input into HEC-HMS 
#See http://www.itia.ntua.gr/en/softinfo/3/ for more details
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For this script, you MUST load version 3.5 of R: https://cran.r-project.org/bin/windows/base/old/3.5.3/

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1:  Setup Workspace-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
rm(list=ls(all=TRUE))

#add appropriate libarary
library(HyetosMinute)
library(lubridate)
library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2:  Format Historic Data--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download historic data
df<-read.csv("data\\stream_lab_data\\Precip_Observed_StREAMLab.csv")
df$date_time<-strptime(df$date_time, "%m/%d/%Y %H:%M")

#Create time series to populate
ts<-seq(strptime("10/1/2011 1",  "%m/%d/%Y %H"), 
        strptime("9/30/2016 24", "%m/%d/%Y %H"), 
        by="hour"
)
ts<-data.frame(ts)
colnames(ts)<-"date_time"
ts$date_time<-strptime(ts$date_time,"%Y-%m-%d %H")
ts<-data.frame(ts[format(ts$date_time,"%m-%d")!="02-29",])
colnames(ts)<-"date_time"

#Aggregate df by hour
df$date_time<-format(df$date_time,"%m/%d/%Y %H")
df<-aggregate(df$precip_mm, list(df$date_time), sum)
colnames(df)<-c("date_time","precip_mm")
df$date_time<-strptime(df$date_time, "%m/%d/%Y %H")

#Populate timeseries df
df<-merge(ts, df, by='date_time', all.x=T)
df[is.na(df)]<-0
remove(ts)

#define time collumns
df$day<-as.POSIXlt(df$date_time)$yday
df$day<-ifelse(df$day>=273, df$day-272, df$day+93)
df$month<-1
df$year<-rep(seq(1,5),each=(365*24))
df$hour<-seq(1,24)

#Remove dat_time collumn
df$date_time<-NULL

#Reshape dataframe
df<-reshape(df,timevar="hour", direction="wide", idvar=c("day","month","year"))

#export 
write.table(df,"HistHourlyData.txt", sep="\t", row.names=F,col.names=F, quote=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3:  Develop Synthetic Flow Record-----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run simulation
n.years<-1000
SequentialSimul(Length=(365*n.years),
                BLpar=list(lambda=0.324,
                           phi=0.0318,
                           kappa=0.0751,
                           alpha=99,
                           v=2.32,
                           mx=99,
                           sx=NA),
                CellIntensityProp=list(Weibull=FALSE,iota=NA),
                TimeScale=1,
                ExportSynthData=list(exp=TRUE,
                                     FileContent=c("AllDays"),
                                     DaysPerSeason=365,
                                     file="SynthRPBLM.txt"),
                ImportHistData=list(imp=TRUE,
                                    file="HistHourlyData.txt",
                                    ImpDataTimeScale=1,
                                    na.values="NA",
                                    FileContent=c("AllDays"),
                                    DaysPerSeason=365,
                                    DailyValues=TRUE),
                PlotTs=FALSE,
                Statistics=list(print=TRUE,plot=FALSE),
                RandSeed=5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4:  Organize Data---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read exported data
df<-read.csv("SynthRPBLM.txt",sep="\t")
colnames(df)<-c("day","month","year",seq(1,25))
df[,28]<-NULL
df$month<-NULL

#Reorganize to longitudinal
df<-melt(df,id.vars=c('year','day'))
df<-df[order(df$year, df$day, df$variable),]
colnames(df)<-c("year","day","hour", "precip")

#Export
write.csv(df, "data//temp//Precip_Modeled_1000yrs.csv")

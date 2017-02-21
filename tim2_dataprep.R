setwd("~/assignments/biogeo_practicum/flux_data")    #set working directory

#install packages needed
#install.packages(lubridate)
#install.packages("REddyProc", repos=c("http://R-Forge.R-project.org","@CRAN@"))

#load installed packages
library(lubridate)
library(REddyProc)


bar<-read.csv("AMF_US-Bar_BASE_HH_4-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
ha1<-read.csv("AMF_US-Ha1_BASE_HR_10-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
ha2<-read.csv("AMF_US-Ha2_BASE_HH_2-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
ho1<-read.csv("AMF_US-Ho1_BASE_HH_4-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
lph<-read.csv("AMF_US-LPH_BASE_HH_1-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
mms<-read.csv("AMF_US-MMS_BASE_HR_8-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
umb1<-read.csv("AMF_US-UMB_BASE_HH_10-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)
umb2<-read.csv("AMF_US-UMB_BASE_HR_10-1.csv", sep=",", header=T, colClasses=c(rep('character',38)),skip=2)








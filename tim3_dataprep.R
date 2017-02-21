
setwd("~/assignments/biogeo_practicum/flux_data")    #set working directory

library(lubridate)
#require(lubridate)
#require(geosphere)

# Read in data
Bar_HH <- read.csv('AMF_US-Bar_BASE_HH_4-1.csv', skip = 2)

#get needed data out of table and emliminate rest
Bar_HH <- data.frame(cbind(as.vector(as.character(Bar_HH$TIMESTAMP_START)), Bar_HH$NEE_PI, 
                           Bar_HH$LE, Bar_HH$H, Bar_HH$SW_IN, Bar_HH$TA, Bar_HH$TS_1, 
                           Bar_HH$TS_2, Bar_HH$RECO_PI, Bar_HH$VPD_PI, Bar_HH$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(Bar_HH) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
Bar_HH$Year<-sub('^(\\d{4}).*$',"\\1",Bar_HH$TIMESTAMP)
#Month
Bar_HH$Month<-substr(Bar_HH$TIMESTAMP,5,6)
#DoY
################  write function to pull out julian date  #########################
get_jd <- function(x) {
  jd <- yday(x)   
}

# function to formate as yyyy-mm-dd in order to conver to Julian Date
format_date_time <- function(x) {
  ymd_hm_dates <- ymd_hm(as.character(x))
}

######################### test with a subset
#Bar_HH_subset <- Bar_HH[1:192,]
#Bar_HH_date_time <- as.vector(as.character(Bar_HH_subset[,1]))
#l_Bar_HH <- length(Bar_HH_subset[,1])
#Bar_HH_dates <- matrix(ncol = 1, nrow = l_Bar_HH, "NA")

##apply function over all timestamp
#Bar_HH_dates <- lapply(Bar_HH_date_time, format_date_time)
#get jd from yyyy-mm-dd
#Bar_DoY<-lapply(Bar_HH_dates, get_jd)

##put into data table as column
#Bar_HH_subset$DoY<-Bar_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

Bar_HH_date_time <- as.vector(as.character(Bar_HH[,1]))

l_Bar_HH <- length(Bar_HH[,1])

Bar_HH_dates <- matrix(ncol = 1, nrow = l_Bar_HH, "NA")

Bar_HH_dates <- lapply(Bar_HH_date_time, format_date_time)

Bar_DoY<-lapply(Bar_HH_dates, get_jd)

Bar_HH$DoY<-Bar_DoY

#Hour
Bar_HH$Hour<-substr(Bar_HH$TIMESTAMP,9,10)

#Minute
Bar_HH$Minute<-substr(Bar_HH$TIMESTAMP,11,12)
Bar_HH$Minute<-as.numeric(as.character(Bar_HH$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
Bar_HH$Hour<-(as.numeric(as.character(Bar_HH$Hour))+as.numeric(as.character(Bar_HH$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
Bar_HH_ready <- data.frame(cbind(as.vector(as.character(Bar_HH$Year)),as.vector(as.character(Bar_HH$DoY))
                           ,as.vector(as.character(Bar_HH$Hour)),as.vector(as.character(Bar_HH$NEE))
                           ,as.vector(as.character(Bar_HH$LE)),as.vector(as.character(Bar_HH$H))
                           ,as.vector(as.character(Bar_HH$SW_IN)),as.vector(as.character(Bar_HH$Tair))
                           ,as.vector(as.character(Bar_HH$Tsoil1)),as.vector(as.character(Bar_HH$Tsoil2))
                           ,as.vector(as.character(Bar_HH$Re)),as.vector(as.character(Bar_HH$VPD))
                           ,as.vector(as.character(Bar_HH$USTAR))))
                           
#give columns names that reasonable match those needed for REddyProc
colnames(Bar_HH_ready) <- c("Year", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                      "VPD","Ustar")






setwd("~/assignments/biogeo_practicum/flux_data")    #set working directory
wd<-setwd("~/assignments/biogeo_practicum/flux_data")    #set working directory

######################   This is working for Bartlett, can be adapted to other sites

library(REddyProc)
library(lubridate)
#require(lubridate)
#require(geosphere)

# Read in all data
Bar_HH <- read.csv('AMF_US-Bar_BASE_HH_4-1.csv', skip = 2)
Ha1_HR <- read.csv('AMF_US-Ha1_BASE_HR_10-1.csv', skip = 2)
Ha2_HH <- read.csv('AMF_US-Ha2_BASE_HH_2-1.csv', skip = 2)
LPH_HH <- read.csv('AMF_US-LPH_BASE_HH_1-1.csv', skip = 2)
MMS_HR <- read.csv('AMF_US-MMS_BASE_HR_8-1.csv', skip = 2)
UMB_HH <- read.csv('AMF_US-UMB_BASE_HH_10-1.csv', skip = 2)
UMB_HR <- read.csv('AMF_US-UMB_BASE_HR_10-1.csv', skip = 2)
Ho1_HH <- read.csv('AMF_US-Ho1_BASE_HH_4-1.csv', skip = 2)


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

#Bar_HH_date_time <- as.vector(as.character(Bar_HH[,1]))

#l_Bar_HH <- length(Bar_HH[,1])

#Bar_HH_dates <- matrix(ncol = 1, nrow = l_Bar_HH, "NA")

#Bar_HH_dates <- lapply(Bar_HH_date_time, format_date_time)

#Bar_DoY<-lapply(Bar_HH_dates, get_jd)

#Bar_HH$DoY<-Bar_DoY
################### Getting Julian date without lubridate is faster
Bar_HH$date<-strptime(Bar_HH$TIMESTAMP,format="%Y%m%d%H%M")
Bar_HH$DoY<-format(Bar_HH$date,"%j")
Bar_HH$date1<-format(Bar_HH$date,"%m-%d-%Y")
Bar_HH$date2<-format(Bar_HH$date,"%m-%Y")

#Hour
Bar_HH$Hour<-substr(Bar_HH$TIMESTAMP,9,10)

#Minute
Bar_HH$Minute<-substr(Bar_HH$TIMESTAMP,11,12)
Bar_HH$Minute<-as.numeric(as.character(Bar_HH$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
Bar_HH$Hour<-(as.numeric(as.character(Bar_HH$Hour))+as.numeric(as.character(Bar_HH$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
Bar_HH_ready <- data.frame(cbind(as.vector(as.character(Bar_HH$Year)),as.vector(as.character(Bar_HH$Month)),as.vector(as.character(Bar_HH$DoY))
                           ,as.vector(as.character(Bar_HH$Hour)),as.vector(as.character(Bar_HH$NEE))
                           ,as.vector(as.character(Bar_HH$LE)),as.vector(as.character(Bar_HH$H))
                           ,as.vector(as.character(Bar_HH$SW_IN)),as.vector(as.character(Bar_HH$Tair))
                           ,as.vector(as.character(Bar_HH$Tsoil1)),as.vector(as.character(Bar_HH$Tsoil2))
                           ,as.vector(as.character(Bar_HH$Re)),as.vector(as.character(Bar_HH$VPD))
                           ,as.vector(as.character(Bar_HH$USTAR)), as.vector(as.character(Bar_HH$date1))
                           ,as.vector(as.character(Bar_HH$date2))))
                           
#give columns names that reasonable match those needed for REddyProc
colnames(Bar_HH_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                      "VPD","Ustar","Date1","Date2")

#save output
write.csv(Bar_HH_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/Bar_HH.csv",
          row.names=FALSE)



###########################################################################################
###################################################  Ho1  ##################################################################
#############################################################################################

#get needed data out of table and emliminate rest
Ho1_HH <- data.frame(cbind(as.vector(as.character(Ho1_HH$TIMESTAMP_START)), Ho1_HH$NEE_PI, 
                           Ho1_HH$LE, Ho1_HH$H, Ho1_HH$SW_IN, Ho1_HH$TA, Ho1_HH$TS_1, 
                           Ho1_HH$TS_2, Ho1_HH$RECO_PI, Ho1_HH$VPD_PI, Ho1_HH$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(Ho1_HH) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
Ho1_HH$Year<-sub('^(\\d{4}).*$',"\\1",Ho1_HH$TIMESTAMP)
#Month
Ho1_HH$Month<-substr(Ho1_HH$TIMESTAMP,5,6)
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
#Ho1_HH_subset <- Ho1_HH[1:192,]
#Ho1_HH_date_time <- as.vector(as.character(Ho1_HH_subset[,1]))
#l_Ho1_HH <- length(Ho1_HH_subset[,1])
#Ho1_HH_dates <- matrix(ncol = 1, nrow = l_Ho1_HH, "NA")

##apply function over all timestamp
#Ho1_HH_dates <- lapply(Ho1_HH_date_time, format_date_time)
#get jd from yyyy-mm-dd
#Ho1_DoY<-lapply(Ho1_HH_dates, get_jd)

##put into data table as column
#Ho1_HH_subset$DoY<-Ho1_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#Ho1_HH_date_time <- as.vector(as.character(Ho1_HH[,1]))

#l_Ho1_HH <- length(Ho1_HH[,1])

#Ho1_HH_dates <- matrix(ncol = 1, nrow = l_Ho1_HH, "NA")

#Ho1_HH_dates <- lapply(Ho1_HH_date_time, format_date_time)

#Ho1_DoY<-lapply(Ho1_HH_dates, get_jd)

#Ho1_HH$DoY<-Ho1_DoY
################### Getting Julian date without lubridate is faster
Ho1_HH$date<-strptime(Ho1_HH$TIMESTAMP,format="%Y%m%d%H%M")
Ho1_HH$DoY<-format(Ho1_HH$date,"%j")
Ho1_HH$date1<-format(Ho1_HH$date,"%m-%d-%Y")
Ho1_HH$date2<-format(Ho1_HH$date,"%m-%Y")


#Hour
Ho1_HH$Hour<-substr(Ho1_HH$TIMESTAMP,9,10)

#Minute
Ho1_HH$Minute<-substr(Ho1_HH$TIMESTAMP,11,12)
Ho1_HH$Minute<-as.numeric(as.character(Ho1_HH$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
Ho1_HH$Hour<-(as.numeric(as.character(Ho1_HH$Hour))+as.numeric(as.character(Ho1_HH$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
Ho1_HH_ready <- data.frame(cbind(as.vector(as.character(Ho1_HH$Year)),as.vector(as.character(Ho1_HH$Month)),as.vector(as.character(Ho1_HH$DoY))
                                 ,as.vector(as.character(Ho1_HH$Hour)),as.vector(as.character(Ho1_HH$NEE))
                                 ,as.vector(as.character(Ho1_HH$LE)),as.vector(as.character(Ho1_HH$H))
                                 ,as.vector(as.character(Ho1_HH$SW_IN)),as.vector(as.character(Ho1_HH$Tair))
                                 ,as.vector(as.character(Ho1_HH$Tsoil1)),as.vector(as.character(Ho1_HH$Tsoil2))
                                 ,as.vector(as.character(Ho1_HH$Re)),as.vector(as.character(Ho1_HH$VPD))
                                 ,as.vector(as.character(Ho1_HH$USTAR)), as.vector(as.character(Ho1_HH$date1))
                                 ,as.vector(as.character(Ho1_HH$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(Ho1_HH_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(Ho1_HH_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/Ho1_HH.csv",
          row.names=FALSE)



#######################################################################################
############################################ Ha1 ######################################################
#######################################################################################

#get needed data out of table and emliminate rest
Ha1_HR <- data.frame(cbind(as.vector(as.character(Ha1_HR$TIMESTAMP_START)), Ha1_HR$NEE_PI, 
                           Ha1_HR$LE, Ha1_HR$H, Ha1_HR$SW_IN, Ha1_HR$TA, Ha1_HR$TS_1, 
                           Ha1_HR$TS_2, Ha1_HR$RECO_PI, Ha1_HR$VPD_PI, Ha1_HR$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(Ha1_HR) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
Ha1_HR$Year<-sub('^(\\d{4}).*$',"\\1",Ha1_HR$TIMESTAMP)
#Month
Ha1_HR$Month<-substr(Ha1_HR$TIMESTAMP,5,6)
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
#Ha1_HR_subset <- Ha1_HR[1:192,]
#Ha1_HR_date_time <- as.vector(as.character(Ha1_HR_subset[,1]))
#l_Ha1_HR <- length(Ha1_HR_subset[,1])
#Ha1_HR_dates <- matrix(ncol = 1, nrow = l_Ha1_HR, "NA")

##apply function over all timestamp
#Ha1_HR_dates <- lapply(Ha1_HR_date_time, format_date_time)
#get jd from yyyy-mm-dd
#Ha1_DoY<-lapply(Ha1_HR_dates, get_jd)

##put into data table as column
#Ha1_HR_subset$DoY<-Ha1_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#Ha1_HR_date_time <- as.vector(as.character(Ha1_HR[,1]))

#l_Ha1_HR <- length(Ha1_HR[,1])

#Ha1_HR_dates <- matrix(ncol = 1, nrow = l_Ha1_HR, "NA")

#Ha1_HR_dates <- lapply(Ha1_HR_date_time, format_date_time)

#Ha1_DoY<-lapply(Ha1_HR_dates, get_jd)

#Ha1_HR$DoY<-Ha1_DoY
################### Getting Julian date without lubridate is faster
Ha1_HR$date<-strptime(Ha1_HR$TIMESTAMP,format="%Y%m%d%H%M")
Ha1_HR$DoY<-format(Ha1_HR$date,"%j")
Ha1_HR$date1<-format(Ha1_HR$date,"%m-%d-%Y")
Ha1_HR$date2<-format(Ha1_HR$date,"%m-%Y")

#Hour
Ha1_HR$Hour<-substr(Ha1_HR$TIMESTAMP,9,10)

#Minute
Ha1_HR$Minute<-substr(Ha1_HR$TIMESTAMP,11,12)
Ha1_HR$Minute<-as.numeric(as.character(Ha1_HR$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
Ha1_HR$Hour<-(as.numeric(as.character(Ha1_HR$Hour))+as.numeric(as.character(Ha1_HR$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
Ha1_HR_ready <- data.frame(cbind(as.vector(as.character(Ha1_HR$Year)),as.vector(as.character(Ha1_HR$Month)),as.vector(as.character(Ha1_HR$DoY))
                                 ,as.vector(as.character(Ha1_HR$Hour)),as.vector(as.character(Ha1_HR$NEE))
                                 ,as.vector(as.character(Ha1_HR$LE)),as.vector(as.character(Ha1_HR$H))
                                 ,as.vector(as.character(Ha1_HR$SW_IN)),as.vector(as.character(Ha1_HR$Tair))
                                 ,as.vector(as.character(Ha1_HR$Tsoil1)),as.vector(as.character(Ha1_HR$Tsoil2))
                                 ,as.vector(as.character(Ha1_HR$Re)),as.vector(as.character(Ha1_HR$VPD))
                                 ,as.vector(as.character(Ha1_HR$USTAR)), as.vector(as.character(Ha1_HR$date1))
                                 ,as.vector(as.character(Ha1_HR$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(Ha1_HR_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(Ha1_HR_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/Ha1_HR.csv",
          row.names=FALSE)



########################################################################################
#####################################  Ha2  ##############################################
#######################################################################################

#get needed data out of table and emliminate rest
Ha2_HH <- data.frame(cbind(as.vector(as.character(Ha2_HH$TIMESTAMP_START)), Ha2_HH$NEE_PI, 
                           Ha2_HH$LE, Ha2_HH$H, Ha2_HH$SW_IN, Ha2_HH$TA, Ha2_HH$TS_1, 
                           Ha2_HH$TS_2, Ha2_HH$RECO_PI, Ha2_HH$VPD_PI, Ha2_HH$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(Ha2_HH) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
Ha2_HH$Year<-sub('^(\\d{4}).*$',"\\1",Ha2_HH$TIMESTAMP)
#Month
Ha2_HH$Month<-substr(Ha2_HH$TIMESTAMP,5,6)
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
#Ha2_HH_subset <- Ha2_HH[1:192,]
#Ha2_HH_date_time <- as.vector(as.character(Ha2_HH_subset[,1]))
#l_Ha2_HH <- length(Ha2_HH_subset[,1])
#Ha2_HH_dates <- matrix(ncol = 1, nrow = l_Ha2_HH, "NA")

##apply function over all timestamp
#Ha2_HH_dates <- lapply(Ha2_HH_date_time, format_date_time)
#get jd from yyyy-mm-dd
#Ha2_DoY<-lapply(Ha2_HH_dates, get_jd)

##put into data table as column
#Ha2_HH_subset$DoY<-Ha2_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#Ha2_HH_date_time <- as.vector(as.character(Ha2_HH[,1]))

#l_Ha2_HH <- length(Ha2_HH[,1])

#Ha2_HH_dates <- matrix(ncol = 1, nrow = l_Ha2_HH, "NA")

#Ha2_HH_dates <- lapply(Ha2_HH_date_time, format_date_time)

#Ha2_DoY<-lapply(Ha2_HH_dates, get_jd)

#Ha2_HH$DoY<-Ha2_DoY
################### Getting Julian date without lubridate is faster
Ha2_HH$date<-strptime(Ha2_HH$TIMESTAMP,format="%Y%m%d%H%M")
Ha2_HH$DoY<-format(Ha2_HH$date,"%j")
Ha2_HH$date1<-format(Ha2_HH$date,"%m-%d-%Y")
Ha2_HH$date2<-format(Ha2_HH$date,"%m-%Y")

#Hour
Ha2_HH$Hour<-substr(Ha2_HH$TIMESTAMP,9,10)

#Minute
Ha2_HH$Minute<-substr(Ha2_HH$TIMESTAMP,11,12)
Ha2_HH$Minute<-as.numeric(as.character(Ha2_HH$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
Ha2_HH$Hour<-(as.numeric(as.character(Ha2_HH$Hour))+as.numeric(as.character(Ha2_HH$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
Ha2_HH_ready <- data.frame(cbind(as.vector(as.character(Ha2_HH$Year)),as.vector(as.character(Ha2_HH$Month)),as.vector(as.character(Ha2_HH$DoY))
                                 ,as.vector(as.character(Ha2_HH$Hour)),as.vector(as.character(Ha2_HH$NEE))
                                 ,as.vector(as.character(Ha2_HH$LE)),as.vector(as.character(Ha2_HH$H))
                                 ,as.vector(as.character(Ha2_HH$SW_IN)),as.vector(as.character(Ha2_HH$Tair))
                                 ,as.vector(as.character(Ha2_HH$Tsoil1)),as.vector(as.character(Ha2_HH$Tsoil2))
                                 ,as.vector(as.character(Ha2_HH$Re)),as.vector(as.character(Ha2_HH$VPD))
                                 ,as.vector(as.character(Ha2_HH$USTAR)), as.vector(as.character(Ha2_HH$date1))
                                 ,as.vector(as.character(Ha2_HH$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(Ha2_HH_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(Ha2_HH_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/Ha2_HH.csv",
          row.names=FALSE)



#######################################################################################
#######################################  LPH  ############################################
#######################################################################################

#get needed data out of table and emliminate rest
LPH_HH <- data.frame(cbind(as.vector(as.character(LPH_HH$TIMESTAMP_START)), LPH_HH$NEE_PI, 
                           LPH_HH$LE, LPH_HH$H, LPH_HH$SW_IN, LPH_HH$TA, LPH_HH$TS_1, 
                           LPH_HH$TS_2, LPH_HH$RECO_PI, LPH_HH$VPD_PI, LPH_HH$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(LPH_HH) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
LPH_HH$Year<-sub('^(\\d{4}).*$',"\\1",LPH_HH$TIMESTAMP)
#Month
LPH_HH$Month<-substr(LPH_HH$TIMESTAMP,5,6)
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
#LPH_HH_subset <- LPH_HH[1:192,]
#LPH_HH_date_time <- as.vector(as.character(LPH_HH_subset[,1]))
#l_LPH_HH <- length(LPH_HH_subset[,1])
#LPH_HH_dates <- matrix(ncol = 1, nrow = l_LPH_HH, "NA")

##apply function over all timestamp
#LPH_HH_dates <- lapply(LPH_HH_date_time, format_date_time)
#get jd from yyyy-mm-dd
#LPH_DoY<-lapply(LPH_HH_dates, get_jd)

##put into data table as column
#LPH_HH_subset$DoY<-LPH_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#LPH_HH_date_time <- as.vector(as.character(LPH_HH[,1]))

#l_LPH_HH <- length(LPH_HH[,1])

#LPH_HH_dates <- matrix(ncol = 1, nrow = l_LPH_HH, "NA")

#LPH_HH_dates <- lapply(LPH_HH_date_time, format_date_time)

#LPH_DoY<-lapply(LPH_HH_dates, get_jd)

#LPH_HH$DoY<-LPH_DoY
################### Getting Julian date without lubridate is faster
LPH_HH$date<-strptime(LPH_HH$TIMESTAMP,format="%Y%m%d%H%M")
LPH_HH$DoY<-format(LPH_HH$date,"%j")
LPH_HH$date1<-format(LPH_HH$date,"%m-%d-%Y")
LPH_HH$date2<-format(LPH_HH$date,"%m-%Y")

#Hour
LPH_HH$Hour<-substr(LPH_HH$TIMESTAMP,9,10)

#Minute
LPH_HH$Minute<-substr(LPH_HH$TIMESTAMP,11,12)
LPH_HH$Minute<-as.numeric(as.character(LPH_HH$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
LPH_HH$Hour<-(as.numeric(as.character(LPH_HH$Hour))+as.numeric(as.character(LPH_HH$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
LPH_HH_ready <- data.frame(cbind(as.vector(as.character(LPH_HH$Year)),as.vector(as.character(LPH_HH$Month)),as.vector(as.character(LPH_HH$DoY))
                                 ,as.vector(as.character(LPH_HH$Hour)),as.vector(as.character(LPH_HH$NEE))
                                 ,as.vector(as.character(LPH_HH$LE)),as.vector(as.character(LPH_HH$H))
                                 ,as.vector(as.character(LPH_HH$SW_IN)),as.vector(as.character(LPH_HH$Tair))
                                 ,as.vector(as.character(LPH_HH$Tsoil1)),as.vector(as.character(LPH_HH$Tsoil2))
                                 ,as.vector(as.character(LPH_HH$Re)),as.vector(as.character(LPH_HH$VPD))
                                 ,as.vector(as.character(LPH_HH$USTAR)), as.vector(as.character(LPH_HH$date1))
                                 ,as.vector(as.character(LPH_HH$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(LPH_HH_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(LPH_HH_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/LPH_HH.csv",
          row.names=FALSE)



########################################################################################
#####################################  MMS  #################################################
########################################################################################

#get needed data out of table and emliminate rest
MMS_HR <- data.frame(cbind(as.vector(as.character(MMS_HR$TIMESTAMP_START)), MMS_HR$NEE_PI, 
                           MMS_HR$LE, MMS_HR$H, MMS_HR$SW_IN, MMS_HR$TA, MMS_HR$TS_1, 
                           MMS_HR$TS_2, MMS_HR$RECO_PI, MMS_HR$VPD_PI, MMS_HR$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(MMS_HR) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
MMS_HR$Year<-sub('^(\\d{4}).*$',"\\1",MMS_HR$TIMESTAMP)
#Month
MMS_HR$Month<-substr(MMS_HR$TIMESTAMP,5,6)
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
#MMS_HR_subset <- MMS_HR[1:192,]
#MMS_HR_date_time <- as.vector(as.character(MMS_HR_subset[,1]))
#l_MMS_HR <- length(MMS_HR_subset[,1])
#MMS_HR_dates <- matrix(ncol = 1, nrow = l_MMS_HR, "NA")

##apply function over all timestamp
#MMS_HR_dates <- lapply(MMS_HR_date_time, format_date_time)
#get jd from yyyy-mm-dd
#MMS_DoY<-lapply(MMS_HR_dates, get_jd)

##put into data table as column
#MMS_HR_subset$DoY<-MMS_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#MMS_HR_date_time <- as.vector(as.character(MMS_HR[,1]))

#l_MMS_HR <- length(MMS_HR[,1])

#MMS_HR_dates <- matrix(ncol = 1, nrow = l_MMS_HR, "NA")

#MMS_HR_dates <- lapply(MMS_HR_date_time, format_date_time)

#MMS_DoY<-lapply(MMS_HR_dates, get_jd)

#MMS_HR$DoY<-MMS_DoY
################### Getting Julian date without lubridate is faster
MMS_HR$date<-strptime(MMS_HR$TIMESTAMP,format="%Y%m%d%H%M")
MMS_HR$DoY<-format(MMS_HR$date,"%j")
MMS_HR$date1<-format(MMS_HR$date,"%m-%d-%Y")
MMS_HR$date2<-format(MMS_HR$date,"%m-%Y")

#Hour
MMS_HR$Hour<-substr(MMS_HR$TIMESTAMP,9,10)

#Minute
MMS_HR$Minute<-substr(MMS_HR$TIMESTAMP,11,12)
MMS_HR$Minute<-as.numeric(as.character(MMS_HR$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
MMS_HR$Hour<-(as.numeric(as.character(MMS_HR$Hour))+as.numeric(as.character(MMS_HR$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
MMS_HR_ready <- data.frame(cbind(as.vector(as.character(MMS_HR$Year)),as.vector(as.character(MMS_HR$Month)),as.vector(as.character(MMS_HR$DoY))
                                 ,as.vector(as.character(MMS_HR$Hour)),as.vector(as.character(MMS_HR$NEE))
                                 ,as.vector(as.character(MMS_HR$LE)),as.vector(as.character(MMS_HR$H))
                                 ,as.vector(as.character(MMS_HR$SW_IN)),as.vector(as.character(MMS_HR$Tair))
                                 ,as.vector(as.character(MMS_HR$Tsoil1)),as.vector(as.character(MMS_HR$Tsoil2))
                                 ,as.vector(as.character(MMS_HR$Re)),as.vector(as.character(MMS_HR$VPD))
                                 ,as.vector(as.character(MMS_HR$USTAR)), as.vector(as.character(MMS_HR$date1))
                                 ,as.vector(as.character(MMS_HR$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(MMS_HR_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(MMS_HR_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/MMS_HR.csv",
          row.names=FALSE)



#######################################################################################
######################################  UMB HH  ##########################################
######################################################################################

#get needed data out of table and emliminate rest
UMB_HH <- data.frame(cbind(as.vector(as.character(UMB_HH$TIMESTAMP_START)), UMB_HH$NEE_PI, 
                           UMB_HH$LE, UMB_HH$H, UMB_HH$SW_IN, UMB_HH$TA, UMB_HH$TS_1, 
                           UMB_HH$TS_2, UMB_HH$RECO_PI, UMB_HH$VPD_PI, UMB_HH$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(UMB_HH) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
UMB_HH$Year<-sub('^(\\d{4}).*$',"\\1",UMB_HH$TIMESTAMP)
#Month
UMB_HH$Month<-substr(UMB_HH$TIMESTAMP,5,6)
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
#UMB_HH_subset <- UMB_HH[1:192,]
#UMB_HH_date_time <- as.vector(as.character(UMB_HH_subset[,1]))
#l_UMB_HH <- length(UMB_HH_subset[,1])
#UMB_HH_dates <- matrix(ncol = 1, nrow = l_UMB_HH, "NA")

##apply function over all timestamp
#UMB_HH_dates <- lapply(UMB_HH_date_time, format_date_time)
#get jd from yyyy-mm-dd
#UMB_DoY<-lapply(UMB_HH_dates, get_jd)

##put into data table as column
#UMB_HH_subset$DoY<-UMB_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#UMB_HH_date_time <- as.vector(as.character(UMB_HH[,1]))

#l_UMB_HH <- length(UMB_HH[,1])

#UMB_HH_dates <- matrix(ncol = 1, nrow = l_UMB_HH, "NA")

#UMB_HH_dates <- lapply(UMB_HH_date_time, format_date_time)

#UMB_DoY<-lapply(UMB_HH_dates, get_jd)

#UMB_HH$DoY<-UMB_DoY
################### Getting Julian date without lubridate is faster
UMB_HH$date<-strptime(UMB_HH$TIMESTAMP,format="%Y%m%d%H%M")
UMB_HH$DoY<-format(UMB_HH$date,"%j")
UMB_HH$date1<-format(UMB_HH$date,"%m-%d-%Y")
UMB_HH$date2<-format(UMB_HH$date,"%m-%Y")

#Hour
UMB_HH$Hour<-substr(UMB_HH$TIMESTAMP,9,10)

#Minute
UMB_HH$Minute<-substr(UMB_HH$TIMESTAMP,11,12)
UMB_HH$Minute<-as.numeric(as.character(UMB_HH$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
UMB_HH$Hour<-(as.numeric(as.character(UMB_HH$Hour))+as.numeric(as.character(UMB_HH$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
UMB_HH_ready <- data.frame(cbind(as.vector(as.character(UMB_HH$Year)),as.vector(as.character(UMB_HH$Month)),as.vector(as.character(UMB_HH$DoY))
                                 ,as.vector(as.character(UMB_HH$Hour)),as.vector(as.character(UMB_HH$NEE))
                                 ,as.vector(as.character(UMB_HH$LE)),as.vector(as.character(UMB_HH$H))
                                 ,as.vector(as.character(UMB_HH$SW_IN)),as.vector(as.character(UMB_HH$Tair))
                                 ,as.vector(as.character(UMB_HH$Tsoil1)),as.vector(as.character(UMB_HH$Tsoil2))
                                 ,as.vector(as.character(UMB_HH$Re)),as.vector(as.character(UMB_HH$VPD))
                                 ,as.vector(as.character(UMB_HH$USTAR)), as.vector(as.character(UMB_HH$date1))
                                 ,as.vector(as.character(UMB_HH$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(UMB_HH_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(UMB_HH_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/UMB_HH.csv",
          row.names=FALSE)



#######################################################################################
###################################  UMB HR  #############################################
#######################################################################################

#get needed data out of table and emliminate rest
UMB_HR <- data.frame(cbind(as.vector(as.character(UMB_HR$TIMESTAMP_START)), UMB_HR$NEE_PI, 
                           UMB_HR$LE, UMB_HR$H, UMB_HR$SW_IN, UMB_HR$TA, UMB_HR$TS_1, 
                           UMB_HR$TS_2, UMB_HR$RECO_PI, UMB_HR$VPD_PI, UMB_HR$USTAR))

#give columns names that reasonable match those needed for REddyProc
colnames(UMB_HR) <- c("TIMESTAMP", "NEE", "LE", "H", "SW_IN", "Tair", "Tsoil1","Tsoil2", "Re", "VPD", "USTAR")

##Separate out dates
#Year
UMB_HR$Year<-sub('^(\\d{4}).*$',"\\1",UMB_HR$TIMESTAMP)
#Month
UMB_HR$Month<-substr(UMB_HR$TIMESTAMP,5,6)
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
#UMB_HR_subset <- UMB_HR[1:192,]
#UMB_HR_date_time <- as.vector(as.character(UMB_HR_subset[,1]))
#l_UMB_HR <- length(UMB_HR_subset[,1])
#UMB_HR_dates <- matrix(ncol = 1, nrow = l_UMB_HR, "NA")

##apply function over all timestamp
#UMB_HR_dates <- lapply(UMB_HR_date_time, format_date_time)
#get jd from yyyy-mm-dd
#UMB_DoY<-lapply(UMB_HR_dates, get_jd)

##put into data table as column
#UMB_HR_subset$DoY<-UMB_DoY

########################### done testing with subset

############## Julian date with full table  COMPUTATIONALLY HEAVY ###############

#UMB_HR_date_time <- as.vector(as.character(UMB_HR[,1]))

#l_UMB_HR <- length(UMB_HR[,1])

#UMB_HR_dates <- matrix(ncol = 1, nrow = l_UMB_HR, "NA")

#UMB_HR_dates <- lapply(UMB_HR_date_time, format_date_time)

#UMB_DoY<-lapply(UMB_HR_dates, get_jd)

#UMB_HR$DoY<-UMB_DoY
################### Getting Julian date without lubridate is faster
UMB_HR$date<-strptime(UMB_HR$TIMESTAMP,format="%Y%m%d%H%M")
UMB_HR$DoY<-format(UMB_HR$date,"%j")
UMB_HR$date1<-format(UMB_HR$date,"%m-%d-%Y")
UMB_HR$date2<-format(UMB_HR$date,"%m-%Y")

#Hour
UMB_HR$Hour<-substr(UMB_HR$TIMESTAMP,9,10)

#Minute
UMB_HR$Minute<-substr(UMB_HR$TIMESTAMP,11,12)
UMB_HR$Minute<-as.numeric(as.character(UMB_HR$Minute)) * (.5/30)
#add minutes back onto hours to get proper formatting for REddyProc
UMB_HR$Hour<-(as.numeric(as.character(UMB_HR$Hour))+as.numeric(as.character(UMB_HR$Minute)))+0.5


############ Convert table to match Data input format for REddyProc
#make new dataframe
UMB_HR_ready <- data.frame(cbind(as.vector(as.character(UMB_HR$Year)),as.vector(as.character(UMB_HR$Month)),as.vector(as.character(UMB_HR$DoY))
                                 ,as.vector(as.character(UMB_HR$Hour)),as.vector(as.character(UMB_HR$NEE))
                                 ,as.vector(as.character(UMB_HR$LE)),as.vector(as.character(UMB_HR$H))
                                 ,as.vector(as.character(UMB_HR$SW_IN)),as.vector(as.character(UMB_HR$Tair))
                                 ,as.vector(as.character(UMB_HR$Tsoil1)),as.vector(as.character(UMB_HR$Tsoil2))
                                 ,as.vector(as.character(UMB_HR$Re)),as.vector(as.character(UMB_HR$VPD))
                                 ,as.vector(as.character(UMB_HR$USTAR)), as.vector(as.character(UMB_HR$date1))
                                 ,as.vector(as.character(UMB_HR$date2))))

#give columns names that reasonable match those needed for REddyProc
colnames(UMB_HR_ready) <- c("Year", "Month", "DoY", "Hour", "NEE", "LE", "H", "Rg","Tair", "Tsoil", "Tsoil2","rH",
                            "VPD","Ustar","Date1","Date2")

#save output
write.csv(UMB_HR_ready,"C:/Users/condo/Documents/assignments/biogeo_practicum/flux_data/workingtables/UMB_HR.csv",
          row.names=FALSE)












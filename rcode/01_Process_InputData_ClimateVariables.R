# Household Air Conditioning Analysis, Part 1
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

rm(list = ls())
#datadir <- "" # set directory for CMIP5 climate data
#datadir2 <- "" # set director for NARR climate data
#rdatadir <- "" # set directory for rdata files

############################# LOAD CMIP5/NARR DATA #############################################

# function for reading files
readFilesIn <- function(dir_path = paste(datadir,'/DI/',sep='')){
  myfun <- function(fl=paste(datadir,'/DI/DI_gfdl-esm2m_1950-2099_CONUS_monthly_avg.csv',sep='')){
    df<-read.csv(fl)
    df <- df[which(df[,1] > 2019),] # only read in years after 2019
    return(df)
  }
  filelist <- list.files(path = dir_path, pattern = ".*.csv", full.names = TRUE)
  datalist <- lapply(filelist, FUN =  myfun)
  climateVar <- do.call("rbind", datalist) 
  return(datalist)
}

# read CMIP5 files
DI_f <- readFilesIn(paste(datadir,'/DI/',sep=''))             # discomfort index (degC)
HIA_f <- readFilesIn(paste(datadir,'/HIA/',sep=''))           # heat index (--)
HUMIDEX_f <- readFilesIn(paste(datadir,'/HUMIDEX/',sep=''))   # humidex (--)
sWBGT_f <- readFilesIn(paste(datadir,'/sWBGT/',sep=''))       # simplified wet bulb globe temperature (degC)
tas_f <- readFilesIn(paste(datadir,'/tas/',sep=''))           # 2 m air temperature (K)
TDEW_f <- readFilesIn(paste(datadir,'/TDEW/',sep=''))         # dew point temperature (K)
WBA_f <- readFilesIn(paste(datadir,'/WBA/',sep=''))           # wet bulb temperature (degC)

# read NARR files
DI <- read.csv(paste(datadir2,'/DI_daily_avg.csv',sep=''))
HIA <- read.csv(paste(datadir2,'/HIA_daily_avg.csv',sep=''))
HUMIDEX <- read.csv(paste(datadir2,'/HUMIDEX_daily_avg.csv',sep=''))
sWBGT <- read.csv(paste(datadir2,'/sWBGT_daily_avg.csv',sep=''))
tas <- read.csv(paste(datadir2,'/air_temp_daily_avg.csv',sep=''))
TDEW <- read.csv(paste(datadir2,'/TDEW_daily_avg.csv',sep=''))
WBA <- read.csv(paste(datadir2,'/WBA_daily_avg.csv',sep=''))

# convert from daily to monthly mean
DI <- aggregate(DI[,-3], by=list(DI$YYYYMM), FUN = mean)
HIA <- aggregate(HIA[,-3], by=list(HIA$YYYYMM), FUN = mean)
HUMIDEX <- aggregate(HUMIDEX[,-3], by=list(HUMIDEX$YYYYMM), FUN = mean)
sWBGT <- aggregate(sWBGT[,-3], by=list(sWBGT$YYYYMM), FUN = mean)
tas <- aggregate(tas[,-3], by=list(tas$YYYYMM), FUN = mean)
TDEW <- aggregate(TDEW[,-3], by=list(TDEW$YYYYMM), FUN = mean)
WBA <- aggregate(WBA[,-3], by=list(WBA$YYYYMM), FUN = mean)

# save climate variables in rdata file
setwd(rdatadir)
save(list=c("DI_f","HIA_f","HUMIDEX_f", "sWBGT_f", "tas_f", "TDEW_f","WBA_f",
            "DI","HIA","HUMIDEX", "sWBGT", "tas", "TDEW","WBA"), 
     file="01_Processed_InputData_ClimateVaraiables.RDATA")

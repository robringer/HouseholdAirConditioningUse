# Household Air Conditioning Analysis, Part 2
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

rm(list = ls())
datadir <- "" # set directory for EIA data
rdatadir <- "" # set directory for rdata files
scriptdir <- "" # set directory for R scripts

####### Electricity Sales Data read from .csv file
####### Pick out data for all states from 2005 to 2019.
####### ================================================
electricity = read.csv(paste(datadir,'/sales_revenue.csv',sep=''), stringsAsFactors = F, skip = 2)
electricity <- subset(electricity, Year>2004 & Year <2020, select=c(Year, Month, State, Megawatthours, Cents.kWh) )
electricity <- electricity[!(electricity$State=="AK"|electricity$State=="HI"),] # remove states not present in predictor data
monthElec<- paste0(as.character(electricity$Year), as.character(sprintf("%02d", electricity$Month)))

###### Sales Vector
salesR <- as.numeric(gsub(",","",electricity$Megawatthours)) * 1e3 # change to kWh

###### Convert a state's aggregate electricity sales into per capita electricity sales
###### Monthly population data for the years 2005 to 2019 is processed in separate R Script (02A_PopulationDataProcessing.R)
######=================================================================================
setwd(scriptdir)
source(paste(scriptdir,'/02A_PopulationDataProcessing.R',sep=''))
# NOTE: Errors may relate to 02A_PopulationDataProcessing.R 
salesR <- salesR/pop

###### Deterending the Sales data. Specifically, the response varaiable: Per capita sales.
###### Detrending methodology as explained in the paper: Sailor and Munoz (1997)
######==========================================================================
yearly <- aggregate(salesR, by=list(electricity$State, electricity$Year), sum) # sales in each year for every state
annualmean <- aggregate(yearly[,3], by=list(yearly[,1]), mean)
adjfactor_yearlist <-  split( yearly[,3]/rep(annualmean[,2], length.out = nrow(yearly)) , yearly[,2] ) # Every year's adjustment factor of 49 states fall into one list element.
adjfactor_yearlist <- lapply(adjfactor_yearlist, FUN = rep, 12) # Every year's adjustment factor of 49 states, repreated 12 times. First 49 elements inside first list element has 2008 Jan's adjFactor for 49 states.
adjfactor <- Reduce(c, adjfactor_yearlist) #unlist(adjfactor_yearlist) #unlist returns named vector  # Combines all months data for all 49 states into single vector

salesR <- salesR/adjfactor

# save important variables for next script
setwd(rdatadir)
save(list=c("salesR","monthElec","electricity"), file="02_Processed_InputData_ElectricitySales.RDATA")

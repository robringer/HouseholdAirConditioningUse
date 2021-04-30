# Household Air Conditioning Analysis, Part 2a
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

setwd("") # set directory to population data directory

##### Run 02_Process_InputData_ElectricitySales.R before executing this code

##### Goal: Create a population vector in the same format as electricity sales vector
#####       i.e. 1st 49 entries must correspond to population data for the month 200810 (2008 October)
#####            Next 49 entries correspond to the month 200811 
#####            ....
#####            Last 49 entries correspond to the month 201612
#####       Therfore, POP vector contains 49 states * 99 months = 4851 data points

##### Population data from 2011 to 2016 for all states into a data frame
pop1020 <- read.csv(file = "nst-est2019-alldata.csv", header=TRUE, stringsAsFactors = F)
pop1020 <- subset(pop1020, STATE >= 1, select=c(NAME,POPESTIMATE2010:POPESTIMATE2019))
pop1020 <- pop1020[-52,] #Remove: Puerto Rico

##### Population data from 2000 to 2009 for all states into a data frame. 
pop0010 <- read.csv(file = "st-est00int-01.csv", header=TRUE, stringsAsFactors = F)
pop0010 <- subset(pop0010, STATE >= 1, select=c(NAME,X2005.00:X2009.00))
pop0010 <- pop0010[-52,] #Remove: Puerto Rico
names(pop0010) <- c('NAME','POPESTIMATE2005','POPESTIMATE2006','POPESTIMATE2007','POPESTIMATE2008','POPESTIMATE2009')

##### Combine 2000-10 and 2011-2016 data frames
##### Used 2010 data from 2000 to 2010 series to be consistent with the data used in 8states model
popul <- cbind(pop0010, pop1020[,-1])
popul <- popul[!(popul$NAME=="Alaska"|popul$NAME=="Hawaii"),]#Remove States that we don't consider in our analysis


##### Note: Alphabetical order of StateCodes is different from alphabetical order of StateName 
#####       So, get StateCodes order in Electricity Sales data file and reorder population data accordingly
sNames <- read.csv("StateCodes.csv", header = TRUE, stringsAsFactors = FALSE)
popul <- merge(sNames, popul, by = "NAME")
popul <- popul[order(popul$StateCode),]

##### Population data is in yearly format.
##### Therefore to obtain monthly population data we interpolate values between the years
##### According to census bureau annual-population implies the population of the state as of the month July in that year.
##### Therefore, we consider 2008 population data value, as the population in the month of 200807 (i.e.July 2008)
#####            And, to obtain 200810 data we interpolate between 200807 and 200907. 
#####                 to obtain 200911 data we interpolate between 200907 and 200907... so on
#####==============================================================================================================================
#For ease of interpolation, consider months from 200501-200503 and from 201905 till 201911
#After interplation, remove those months data from pop vector.
pop <- vector(mode = "numeric", length = 8820) # 15 years * 12 months * 49 states = 8820
yea <- vector(mode = "numeric", length = 8820) #Note: census => pop as on July 1st 
incre_prev <- 0
mont = unique(monthElec)
for(c in 3:ncol(popul)){ 
  cp1 = c+1
  for(k in 1:12){
    begins <- 1+49*(k-1)+(c-3)*588; ends <- begins+48;#print(begins)
    if( c==ncol(popul) ) increment <- incre_prev else increment <- (popul[, cp1] - popul[, c])/12
    pop[begins:ends] <- popul[, c] + (k-1)*increment
    y = k+12*(c-3); #print(mont[y]); print(ends); 
    yea[begins:ends] <- rep(mont[y], 49)
  }
  incre_prev <- increment
}


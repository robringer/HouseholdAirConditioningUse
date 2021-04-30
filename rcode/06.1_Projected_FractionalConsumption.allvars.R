# Household Air Conditioning Analysis, Part 6
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

rm(list = ls())
rdatadir <- ""  # set directory for rdata files

load("03_ProcessedInputData.RDATA")
load("03_Results_alldomains.RDATA") 
load("05.1_GeneratedFractionalDistributions.RDATA")

#####=================================================================================================
#### Compute Projected household fractional consumption for Summer
#####=================================================================================================


computeProjFrac<- function(generatedFractions, dnum, projectedData){
  basefrac_distributn <- mean(generatedFractions[[dnum]])*projectedData
}

finaldata <- list()
actdata <- list()
domain <- as.numeric(dfusaR$domain)
for(dnum in unique(domain)) {
  Domain_avgHHMembers <- list_avgHHMembers[[dnum]]
  summerobs <- subset(list_AllResults_R[[dnum]][,3], list_AllResults_R[[dnum]][,2] %in% c("06","07","08","09"))
  summerdata <- subset(AllPredData[[dnum]][,3:7], AllPredData[[dnum]][,2] %in% c("06","07","08","09"))
  coolingfrac <- list()
  for (i in 1:5) {  
    alldata <- summerdata[,i]
    # Multiply aggregrate domain consumption with the projected fractions for that domain
    projectedData_perDomain_perHousehold <- alldata * Domain_avgHHMembers
    coolingfrac[[i]] <- computeProjFrac(list_COLFractions, dnum, projectedData_perDomain_perHousehold)
    }
  finaldata[[dnum]] <- coolingfrac
  actcoolingfrac <- summerobs * Domain_avgHHMembers
  actdata[[dnum]] <- computeProjFrac(list_COLFractions, dnum, actcoolingfrac)
}

obsmonths <- subset(list_AllResults_R[[1]][,1], list_AllResults_R[[1]][,2] %in% c("06","07","08","09"))
futuremonths <- subset(AllPredData[[1]][,1], AllPredData[[1]][,2] %in% c("06","07","08","09"))
allmonths <- c(obsmonths, futuremonths)

# save final a/c projections
setwd(rdatadir)
save(list=c("finaldata", "allmonths","actdata"), file="06_allvars_All_ProjectedFractions.RDATA")



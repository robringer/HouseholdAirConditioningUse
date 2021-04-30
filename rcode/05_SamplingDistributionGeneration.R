# Household Air Conditioning Analysis, Part 5
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

rm(list = ls())
rdatadir <- ""  # set directory for rdata files
datadir <- "" # set directory for EIA RECS data

# libraries
library(MASS)
library(CDFt)
library(ggplot2)

# load RECS data
setwd(datadir)
recs2009 <- read.csv(file = "05_recs2009_public.csv")

# initialize variables
list_COLFractions <- list()
list_avgHHMembers <- list()

set.seed(12)
###### At every iteration, the loop prints domainNum, TRUE if Normal distribution else FALSE for all generated/simulated fractionalConsumption Vectors 
for(rd in sort(unique(recs2009[, "REPORTABLE_DOMAIN"]))){ ## rd =3 is New York
  print(rd)
  recs_state = subset(recs2009, REPORTABLE_DOMAIN == rd, select = c("NHSLDMEM","BTUEL", "BTUELSPH", "BTUELCOL", "BTUELWTH")) # "NWEIGHT","KWH",
  fracCOL <- recs_state[, "BTUELCOL"]/recs_state[, "BTUEL"]

  avg_HouseholdMembers <- mean(recs_state[, "NHSLDMEM"])
  avg_HouseholdMembers <- round(avg_HouseholdMembers) ### rounded off to nearest integer. For example, 2.50 will be 2 and 2.51 will be 3. 
  list_avgHHMembers[[paste0("avgHouseholdMembers_rd",format(rd))]] <- avg_HouseholdMembers
  
  ###=================================
  ### Fraction of Space Cooling
  ###=================================
  fracCOL_xbar <- vector()
  fracCOL_x <- matrix(nrow=1000, ncol=length(fracCOL))
  
  for(i in 1:1000){
    fracCOL_x[i,] <- sample(fracCOL, size = length(fracCOL), replace = TRUE)
    fracCOL_xbar[i] <- mean(fracCOL_x[i,])
  }
  
  #print(shapiro.test(fracCOL_xbar)$p.value > 0.05)
  
  # fitdist_COL <- fitdistr(fracCOL_xbar, "gamma")
  fitdist_COL <- fitdistr(fracCOL_xbar, "normal")
  
  # fracCOL_Simul <- rgamma(1000, shape = fitdist_COL$estimate[[1]], rate = fitdist_COL$estimate[[2]]) # simulated dataset with defined distribution
  fracCOL_Simul <- rnorm(1000, fitdist_COL$estimate[[1]], fitdist_COL$estimate[[2]]) # simulated dataset with defined distribution               
  
  res.2<- CramerVonMisesTwoSamples(fracCOL_xbar, fracCOL_Simul)
  p_value = 1/6*exp(-res.2)
  print(p_value > 0.05)  ### if > >0.05, it indicates that we fail to reject the null hypothesis

  list_COLFractions[[paste0("fracCOL_rd",format(rd))]] <- fracCOL_Simul
  
}

setwd(rdatadir)
save(list=ls(pattern ="^list"), file="05.1_GeneratedFractionalDistributions.RDATA")

# Household Air Conditioning Analysis, Part 3
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

rm(list = ls())
rdatadir <- ""  # set directory for rdata files
scriptdir <- "" # set directory for R scripts
outputdir <- "" # set directory for non-rdata outputs

setwd(rdatadir)

library(stringr)

# load rdata files
load("01_Processed_InputData_ClimateVaraiables.RDATA")  
load("02_Processed_InputData_ElectricitySales.RDATA")

#####======================================================
##### Build final input dataframe
# NOTE: If you have 03_ProcessedInputData.RDATA, skip to line 94 to run model

# separate data by CMIP5 model
allmods <- list()
for (i in 1:5){
  # convert units
  tas_f[[i]][,-(1:2)] <- tas_f[[i]][,-(1:2)] - 273.15  # K --> degrees C
  TDEW_f[[i]][,-(1:2)] <- TDEW_f[[i]][,-(1:2)] - 273.15  # K --> degrees C
  
  # combine into one list
  allmods[[i]] <- list(DI_f[[i]],HIA_f[[i]],HUMIDEX_f[[i]],sWBGT_f[[i]],tas_f[[i]],TDEW_f[[i]],WBA_f[[i]])
}


# create vectors for each variable with all domain IDs
allvars <- list()
for (i in 1:5) {
  var <- list()
  for (v in 1:7){
    var[[v]] <- allmods[[i]][[v]][,3]
    for (d in 4:29) {
      var[[v]] <- append(var[[v]], allmods[[i]][[v]][,d])
    }
  }
  allvars[[i]] <- var
}

# extract historical climate data

# convert units
tas[,-(1:4)] <- tas[,-(1:4)] - 273.15  # K --> degrees C
TDEW[,-(1:4)] <- TDEW[,-(1:4)] - 273.15  # K --> degrees C

climvar <- list(DI, HIA, HUMIDEX, sWBGT, tas, TDEW, WBA)
hist <- list()
for (v in 1:7){
    hist[[v]] <- climvar[[v]][,5]
    for (d in 6:31) {
      hist[[v]] <- append(hist[[v]], climvar[[v]][,d])
    }
}

# convert sales and price data to domains
domains <- c(18, 20, 24, 26, 22, 1, 14, 14, 17, 15, 10, 23, 6, 7, 11, 18, 20, 2, 14, 1, 8, 10, 12, 18, 23, 16, 10, 11, 1, 4, 25, 25, 3, 7, 20, 27, 5, 1, 16, 10, 19, 21, 23, 13, 1, 27, 9, 14, 23)
alldoms <- rep(domains, (15*12))
ids <- paste0(as.character(sprintf("%02d", alldoms)),'-',as.character(monthElec))
data <- cbind.data.frame(ids, as.numeric(salesR), as.numeric(electricity$Cents.kWh))
domaindata <- aggregate(data[,2:3], by=list(ids), FUN = mean)

# get months and domain labels
domainmonths <- sapply(strsplit(as.character(domaindata$Group.1), "-"), "[", 2)
domainlist <- sapply(strsplit(as.character(domaindata$Group.1), "-"), "[", 1)

# final dataframe
dfusaR <- data.frame(domainlist, domainmonths, domaindata[,2], hist[[1]], hist[[2]], hist[[3]], hist[[4]], hist[[5]], hist[[6]], hist[[7]], domaindata[,3])
colnames(dfusaR) <- c("domain", "month","sales", "DI", "HIA", "HUMIDEX","sWBGT","tas","TDEW","WBA", "Price")

# CMIP5 data - get months and domain labels
falldoms <- sort(rep(domains, each=1548)) # 129 years * 12 months
months<- paste0(as.character(allmods[[1]][[1]][,1]), as.character(sprintf("%02d", allmods[[1]][[1]][,2])))
allmonths <- rep(months, 49)
ids <- paste0(as.character(sprintf("%02d", falldoms)),'-',allmonths)
futids <- unique(ids)
fdomainmonths <- sapply(strsplit(as.character(futids), "-"), "[", 2)
fdomainlist <- sapply(strsplit(as.character(futids), "-"), "[", 1)

# CMIP5 data - final dataframe
futdata <- list()
for (i in 1:5) {
  futdata[[i]] <- data.frame(fdomainlist, fdomainmonths, allvars[[i]][[1]], allvars[[i]][[2]], allvars[[i]][[3]], allvars[[i]][[4]], allvars[[i]][[5]], allvars[[i]][[6]], allvars[[i]][[7]])
  colnames(futdata[[i]]) <- c("domain", "month", "DI", "HIA", "HUMIDEX","sWBGT","tas","TDEW","WBA")
}

# save data for model run
save(list=c("dfusaR", "futdata"), file="03_ProcessedInputData.RDATA")


#####========================================================
##### Create BART Model
#####========================================================

load("03_ProcessedInputData.RDATA")

options(java.parameters = "-Xmx1000m")
library("bartMachine")
set_bart_machine_num_cores(4)
library(ggplot2)
options(warn=-1)

source(paste(scriptdir,'/03A_bart_package_cross_validation.R',sep=''))
source(paste(scriptdir,'/03B_bart_package_vimp.R',sep=''))

setwd(outputdir)
dfusaR$domain <- as.numeric(as.character(dfusaR$domain))

df_stateModel_metrics <- data.frame(unique(dfusaR$domain), character(27), numeric(27), numeric(27), numeric(27), numeric(27), numeric(27), numeric(27), numeric(27), numeric(27), numeric(27), numeric(27),  stringsAsFactors = FALSE) 
colnames(df_stateModel_metrics) <- c("Domain","Tuned Hyperparameters","BART model: Rsq_adj","BART model: In-sample RMSE", "Null model: In-sample RMSE", "BART model: In-sample MAE", "Null model: In-sample MAE", "BART model: Out-of-sample RMSE", "Null model: Out-of-sample RMSE", "BART model: Out-of-sample MAE", "Null model: Out-of-sample MAE", "OOS NRMSE")
counter = 1
list_AllResults_R <- list()
AllPredData <- list()

for(domaincode in unique(dfusaR$domain)){
  
  df <- subset(dfusaR, domain==domaincode); dftest=NULL; 
  name_df <-paste0("df",domaincode,"R"); #### Used for plot titles
  justmonths <- str_sub(df$month, -2)
  #####==================================
  ##### BART Model: Model Build
  #####==================================
  bart <- bartMachine(df[,c(4:10)], df[,"sales"], use_missing_data=T, num_trees=200, q=0.99, nu=1, k=0.5); 
  hyperParameters <- c(paste(paste("k=",0.5), paste("num_trees=",200), paste("q=",0.99),paste("nu=",1)))
  tR <- my_k_fold_cv(df[,c(4:10)], df[,"sales"], k_folds = 5, use_missing_data=T, num_trees=200, q=0.99, nu=1, k=0.5); 
  nrmse_test <- bart$rmse_train/(max(bart$y)-min(bart$y))
  
  ### NUll model
  NullModel_MAE_train = sum(abs(df$sales-mean(df$sales)))/length(df$sales)
  NullModel_RMSE_train = sqrt(sum((df$sales-mean(df$sales))^2)/length(df$sales))
  
  df_stateModel_metrics[counter,] <- c(domaincode, hyperParameters, bart$PseudoRsq, bart$rmse_train, NullModel_RMSE_train, bart$L1_err_train/bart$n, NullModel_MAE_train, tR$rmse,  tR$NullModel_RMSE_test, tR$L1_err/length(tR$y_hat), tR$NullModel_MAE_test, nrmse_test)
  #df_stateModel_metrics[counter,] <- c(domaincode, hyperParameters, bart$PseudoRsq, bart$rmse_train, NullModel_RMSE_train, bart$L1_err_train/bart$n, NullModel_MAE_train, 0,  0, 0, 0 )
  counter <- counter+1
  
  #####===========================================================================
  ##### BART Model: Predict increase in Electricity demand  
  #####===========================================================================
  predictR <- list()
  for (i in 1:5) {
    futdata[[i]]$domain <- as.numeric(as.character(futdata[[i]]$domain))
    dfp <- subset(futdata[[i]], domain==domaincode)
    justmonths2 <- str_sub(dfp$month, -2)
    
    predictR[[i]] <- predict(bart, new_data = dfp[,c(3:9)])
  }
  
  #####==========================================================================================
  
  #### Store all results i.e. predicted values from 2 scenarios and historical/base values., in one dataframe.
  AllObsData_R <- data.frame(df$month, justmonths,df$sales, bart$y_hat_train)
  AllPredData_R <- data.frame(dfp$month, justmonths2, predictR[[1]], predictR[[2]], predictR[[3]], predictR[[4]], predictR[[5]])
  
  list_AllResults_R[[domaincode]] <- AllObsData_R
  AllPredData[[domaincode]] <- AllPredData_R
  print(paste0("Analyzed state - ",counter,": ", domaincode))
}

# save model performance results
setwd(outputdir)
write.csv(df_stateModel_metrics, file = "BART_Model_Metrics.csv")

# save results for next script
setwd(rdatadir)
save(list=c("list_AllResults_R", "AllPredData"), file="03_Results_alldomains.RDATA")


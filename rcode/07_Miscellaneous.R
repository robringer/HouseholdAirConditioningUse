# Household Air Conditioning Analysis, Part 7
# Written By: Vineeth CR, Renee Obringer
# Last Ran: 30 April 2021

rm(list = ls())
rdatadir <- ""  # set directory for rdata files
outputdir <- "" # set directory for non-rdata output files
datadir <- ""   # set directory for input data (efficiency/HHD analysis)
datadir2 <- ""  # set directory for RECS dataset
datadir3 <- ""  # set directory for census data

# libraries
library(ggplot2)
library(gridExtra)
library(cowplot)

# load rdata files
load("06_allvars_All_ProjectedFractions.RDATA")
load("03_ProcessedInputData.RDATA")
load("05.1_GeneratedFractionalDistributions.RDATA")

########################### PERCENT CHANGE #####################################

# percent change after 1.5 degrees (pos == % increase) [new-old/old]

indices <- list(c(125,184),c(61,120),c(65,124),c(65,124),c(105,164))
# indices based on years each model reaches threshold, to check: allmonths[c(1,60)] == 200506, 201909 (baseline)
# models: GDFL (2036-2050), HADGEM (2020-2034), IPSL (2021-2035), MIROC (2021-2035), NORESM (2031-2045)

perc15 <- list()
for (d in 1:27) {
  change <- c()
  for (m in 1:5) {
    base <- mean(finaldata[[d]][[m]][1:60]) 
    change[m] <- ((mean(finaldata[[d]][[m]][indices[[m]][1]:indices[[m]][2]])-base)/base)*100
  }
  perc15[[d]] <- change
}

# percent change after 2.0 degrees (pos == % increase) [new-old/old]

indices <- list(c(193,252),c(105,164),c(113,172),c(109,168),c(165,224))
# models: GDFL (2053-2067), HADGEM (2031-2045), IPSL (2033-2047), MIROC (2032-2046), NORESM (2046-2060)

perc20 <- list()
for (d in 1:27) {
  change <- c()
  for (m in 1:5) {
    base <- mean(finaldata[[d]][[m]][1:60]) 
    change[m] <- ((mean(finaldata[[d]][[m]][indices[[m]][1]:indices[[m]][2]])-base)/base)*100
  }
  perc20[[d]] <- change
}

# plots

# percent change per seasons (all models combined)
avgs15 <- c(); meds15 <- c(); min15 <- c(); max15 <- c(); sd15 <- c()
avgs20 <- c(); meds20 <- c(); min20 <- c(); max20 <- c(); sd20 <- c()

for (i in 1:27) {
  avgs15[i] <- mean(perc15[[i]])
  meds15[i] <- median(perc15[[i]])
  min15[i] <- min(perc15[[i]])
  max15[i] <- max(perc15[[i]])
  sd15[i] <- sd(perc15[[i]])
  
  avgs20[i] <- mean(perc20[[i]])
  meds20[i] <- median(perc20[[i]])
  min20[i] <- min(perc20[[i]])
  max20[i] <- max(perc20[[i]])
  sd20[i] <- sd(perc20[[i]])
}

pchange <- c(meds15,meds20)
mins <- c(min15,min20) 
maxs <- c(max15,max20)
sds <- c(sd15,sd20)

Threshold <- c(rep("1.5 degC",27),rep("2.0 degC",27))
domains <- c(rep(1:27, 2))
domainnames <- c('CT, ME, NH, RI, VT','MA','NY','NJ','PA','IL','IN, OH', 'MI','WI','IA, MN, ND, SD',
                 'KS, NE','MO','VA','DE, DC, MD, WV','GA','NC, SC','FL','AL, KY, MS','TN','AR, LA, OK',
                 'TX','CO','ID, MT, UT, WY','AZ','NV, NM','CA','OR, WA')
dnames <- rep(domainnames,2)

pcdata <- data.frame(pchange,Threshold,mins,maxs,dnames, sds)

ggplot(pcdata, aes(x=dnames, y=pchange, fill=Threshold)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(x=dnames, ymin=pchange-sds, ymax=pchange+sds), width=0.5, position=position_dodge(.9)) +
  xlab('Domain') + ylab('Percent Change (%)') +
  theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  theme(axis.text.x = element_text(angle=45,hjust=1))

########################### EFFICIENCY/HOUSEHOLD DAYS ANALYSIS ########################

# EFFICIENCY

base <- c()
for (d in 1:27) {
  basem <- c()
  for (m in 1:5) {
    basem[m] <- mean(finaldata[[d]][[m]][1:60])
  }
  base[d] <- mean(basem)
}

indices <- list(c(193,252),c(105,164),c(113,172),c(109,168),c(165,224))
# models: GDFL (2053-2067), HADGEM (2031-2045), IPSL (2033-2047), MIROC (2032-2046), NORESM (2046-2060)

kwh20 <- c()
for (d in 1:27) {
  mods <- c()
  for (m in 1:5) {
    mods[m] <- mean(finaldata[[d]][[m]][indices[[m]][1]:indices[[m]][2]])
  }
  kwh20[d] <- mean(mods)
}

delt_base20 <- kwh20 - base

# Equations from: McNeil & Letschert (2008) Future air conditioning energy consumption...

# load data
setwd(datadir)
gspdata <- read.csv('gsp_percap2009.csv') # from Bureau of Economic Analysis

setwd(datadir2)
recs2009 <- read.csv(file = "05_recs2009_public.csv")
CDDdata <- cbind.data.frame(recs2009$REPORTABLE_DOMAIN, recs2009$CDD65)
cdd <- aggregate(CDDdata[,2], by=list(CDDdata[,1]), FUN = mean)

# convert from states to domains
domains <- c(18, 20, 24, 26, 22, 1, 14, 14, 17, 15, 10, 23, 6, 7, 11, 18, 20, 2, 14, 1, 8, 10, 12, 18, 23, 16, 10, 11, 1, 4, 25, 25, 3, 7, 20, 27, 5, 1, 16, 10, 19, 21, 23, 13, 1, 27, 9, 14, 23)
data <- cbind.data.frame(domains, gspdata)
incomedata <- aggregate(data[,4], by=list(domains), FUN = mean)

# adjust annual per capita GSP for Purchase Power Parity (cost of living)
income_adj <- 20.9 * incomedata[,2]^0.7088

# convert annual per capita GSP to monthly per household GSP
Domain_avgHHMembers <- unlist(list_avgHHMembers)
income <- income_adj/12 * unname(Domain_avgHHMembers)

# annual baseline unit energy consumption (kwh)
UEC <- 0.345*income + 1.44*cdd[,2] - 823

# monthly UEC
monUEC <- UEC/12

# baseline summer efficiency
eff_baseline <- base/monUEC*100

# future summer efficiency 
eff_future <- kwh20/monUEC*100 # 2.0 degrees

# difference
eff_difference <- eff_future - eff_baseline 

# figure 

domainnames <- c('CT, ME, NH, RI, VT','MA','NY','NJ','PA','IL','IN, OH', 'MI','WI','IA, MN, ND, SD',
                 'KS, NE','MO','VA','DE, DC, MD, WV','GA','NC, SC','FL','AL, KY, MS','TN','AR, LA, OK',
                 'TX','CO','ID, MT, UT, WY','AZ','NV, NM','CA','OR, WA')

plotdata <- data.frame(domainnames, eff_difference)

p1 <- ggplot(plotdata, aes(x=domainnames, y=eff_difference)) + geom_bar(stat = 'identity' , position = 'dodge') +
  xlab('') + ylab('Required Increase\nin Efficiency (%)') + theme_light() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme(axis.text.x = element_blank())

# calculating sd for efficiency
mod1 <- c(); mod2 <- c(); mod3 <- c(); mod4 <- c(); mod5 <- c()
for (d in 1:27) {
  basem <- c()
  for (m in 1:5) {
    basem[m] <- mean(finaldata[[d]][[m]][1:60])
  }
  mod1[d] <- mean(basem[1]); mod2[d] <- mean(basem[2])
  mod3[d] <- mean(basem[3]); mod4[d] <- mean(basem[4])
  mod5[d] <- mean(basem[5])
}

Fmod1 <- c(); Fmod2 <- c(); Fmod3 <- c(); Fmod4 <- c(); Fmod5 <- c()
for (d in 1:27) {
  mods <- c()
  for (m in 1:5) {
    mods[m] <- mean(finaldata[[d]][[m]][indices[[m]][1]:indices[[m]][2]])
  }
  Fmod1[d] <- mean(mods[1]); Fmod2[d] <- mean(mods[2])
  Fmod3[d] <- mean(mods[3]); Fmod4[d] <- mean(mods[4])
  Fmod5[d] <- mean(mods[5])
}

mods <- cbind(mod1, mod2, mod3, mod4, mod5)
Fmods <- cbind(Fmod1, Fmod2, Fmod3, Fmod4, Fmod5)
eff_d <- matrix(ncol = 5, nrow = 27)
for (i in 1:5) {
  eff_b <- mods[,i]/monUEC*100
  eff_f <- Fmods[,i]/monUEC*100 # 2.0 degrees
  eff_d[,i] <- eff_f - eff_b
}

eff_sd <- apply(eff_d,1, sd, na.rm = TRUE)
eff_sd[which(rowMeans(eff_d) == max(rowMeans(eff_d)))]

# HOUSEHOLD DAYS

# number of days without air conditioning per summer per household
hhd <- delt_base20/(base/30)*4

setwd(datadir3)
pop2019 <- read.csv(file = "nst-est2019-alldata.csv", header=TRUE, stringsAsFactors = F)
pop2019 <- subset(pop2019, STATE >= 1, select=c(NAME,POPESTIMATE2019))
pop2019 <- pop2019[-c(2,12,52),] #Remove: Alaska, Hawaii, Puerto Rico

stateids <- c('AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','ID','IL','IN','IA','KS','KY',
              'LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
              'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
data <- cbind.data.frame(stateids,pop2019)
data <- data[order(data$stateids),]

domains <- c(18, 20, 24, 26, 22, 1, 14, 14, 17, 15, 10, 23, 6, 7, 11, 18, 20, 2, 14, 1, 8, 10, 12, 18, 23, 16, 10, 11, 1, 4, 25, 25, 3, 7, 20, 27, 5, 1, 16, 10, 19, 21, 23, 13, 1, 27, 9, 14, 23)
popdata <- cbind.data.frame(domains,data)
domainpop <- aggregate(popdata[,4], by=list(domains), FUN = sum)

hhnum <- round(domainpop[,2]/unlist(list_avgHHMembers))

hhd_total <- hhd*hhnum

setwd(datadir)
povdata <- read.csv('povrate_data.csv')

povrate <- cbind.data.frame(domains,povdata)
domainpov <- aggregate(povdata[,c(3,4)], by=list(domains), FUN=sum)
dompovrate <- domainpov$Under.200./domainpov$Total

hhd_pov <- hhd_total*dompovrate
hhd_other <- hhd_total*(1-dompovrate)

# figure

plotdata <- data.frame(domainnames, hhd)

p2 <- ggplot(plotdata, aes(x=domainnames, y=hhd, label = round(hhd))) + #geom_bar(stat = 'identity' , position = 'dodge') +
  geom_col() + geom_text(nudge_y = 1, color = 'black') +
  xlab('') + ylab('Days without Air Conditioning per\nSummer per Household') + theme_light() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1))

hhd2 <- c(hhd_pov, hhd_other)/1000000
class <- c(rep('zImpoverished',27),rep('Other',27))
plotdata2 <- data.frame(domainnames, hhd2, class)

options(scipen=10000)
p3 <- ggplot(plotdata2, aes(x=domainnames, y=hhd2, fill=class)) + geom_bar(stat = 'identity' , position = 'stack') +
  xlab('') + ylab('Millions of Household-Days\nwithout Air Conditioning') + theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 18)) +
  scale_fill_manual(name = 'Household Income', labels = c('Above 200% of the PL','Below 200% of the PL'), values = c('#a6cee3','#1f78b4'))+
  theme(legend.position = 'bottom')

# COMBINE FIGURES

plot_grid(p1,p2,p3,align = 'v',nrow = 3, rel_heights = c(2/7,2/7,3/7))

########################### ACTUAL VS PREDICTED DATA ########################

actual <- c()
for (i in 1:27) {
  actual[i] <- mean(actdata[[i]])
}

domainnames <- c('CT, ME, NH, RI, VT','MA','NY','NJ','PA','IL','IN, OH', 'MI','WI','IA, MN, ND, SD',
                 'KS, NE','MO','VA','DE, DC, MD, WV','GA','NC, SC','FL','AL, KY, MS','TN','AR, LA, OK',
                 'TX','CO','ID, MT, UT, WY','AZ','NV, NM','CA','OR, WA')

anomalies <- base - actual
anom_perc <- (base-actual)/actual*100

errors <- c(modperf[,9],modperf[,11])
Measure <- rep(c('RMSE','MAE'),each = 27)
domain2 <- rep(domainnames, 2)

kwhdata <- data.frame(domainnames, actual)
preddata <- data.frame(domainnames,base)
diffdata <- data.frame(domainnames, anomalies)
diffdata2 <- data.frame(domainnames, anom_perc)
errdata <- data.frame(errors, Measure, domain2)


p1 <- ggplot(kwhdata, aes(x=domainnames, y=actual)) + geom_bar(stat='identity') +
  theme_light() + xlab('') + ylab('Actual Values\n(kwh/household)') + theme(axis.text.x=element_blank())

p2 <- ggplot(preddata, aes(x=domainnames, y=base)) + geom_bar(stat='identity') +
  theme_light() + xlab('') + ylab('Predicted Values\n(kwh/household)') + theme(axis.text.x=element_blank()) 

p3 <- ggplot(diffdata, aes(x=domainnames, y=anomalies, fill = anomalies > 0)) + geom_bar(stat='identity') +
  theme_light() + xlab('') + ylab('Difference\n(kwh/household)') + theme(axis.text.x=element_blank()) +
  theme(legend.position = "none")

p3b <- ggplot(diffdata2, aes(x=domainnames, y=anom_perc, fill = anomalies > 0)) + geom_bar(stat='identity') +
  theme_light() + xlab('') + ylab('Difference (%)') + theme(axis.text.x=element_blank()) +
  theme(legend.position = "none") 

p4 <- ggplot(errdata, aes(x=domain2, y=errors, fill = Measure)) + geom_bar(stat='identity', position = 'dodge') +
  theme_light() + xlab('') + ylab('Errors\n(kwh/household)') + #theme(axis.text.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p1,p2,p3b,p4,align = 'v',nrow = 4, rel_heights = c(1/5,1/5,1/5,2/5))




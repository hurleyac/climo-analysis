install.packages("stringi")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("splines")
install.packages("stats")
library(stringi)
library(lubridate)
library(dplyr)
library(tidyverse)
library(splines)
library(stats)

# {.tabset .active}
## Introduction
# Domain 1

variables <- data.frame(
  Variable	= c("Average Temperature","Average Dewpoint Temperature","Average Wind Direction","Average Wind Speed","Average Wind Gust Speed",
               "Average Cloud Ceiling","Average Cloud Cover","Average Visibility","Average Altimeter Setting","Average Precipitation"),
  Description = c("The air temperature (Celsius)","The measure of moisture in the air at the station (Celsius)",
                  "The direction the wind comes from (degrees)","The 2-minute average speed of the wind (m/s)","The 3-second maximum wind (m/s)",
                  "The height of the lowest ceiling – a ceiling is 5/8 or more of sky coverage (ft above ground level)",
                  "The amount of the sky that is covered by clouds (octants [eighths])","The distance you can see before 95% of light is scattered (meters)",
                  "The measure of the atmospheric pressure at the station (inHg)","The average amount of precipitation  (mm)")
)

## Domain 2
#Snow Climate:
krdm_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KRDM.csv"))
kbuf_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KBUF.csv"))

#Warm Summer Climate:
kfoe_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KFOE.csv"))
kmsn_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KMSN.csv"))
ktri_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KTRI.csv"))
pajn_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/PAJN.csv"))

#Arid
kelp_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KELP.csv"))
ksgu_data = data.frame(read.csv("C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KSGU.csv"))


## Domain 3 {.tabset}
### Handling NULL Values: {.tabset}
#### Robert's Field, OR
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
krdm_toKeep <- krdm_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(krdm_toKeep[,i]==5)))&length(which(krdm_toKeep[,i]==5))>0) {
    rows <- which(krdm_toKeep[,i]==5)
    krdm_toKeep <- krdm_toKeep[-rows,]
  }
}

#remove qc columns
krdm_toKeep <- krdm_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
krdm_toKeep <- transform(krdm_toKeep,date = as.Date(krdm_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(krdm_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#krdm_toKeep = krdm_toKeep[order(krdm_toKeep$date),]

#set variables
krdm_avgTemp10 <- c()
krdm_avgDPTemp10 <- c()
krdm_avgWDIR10 <- c()
krdm_avgWSPD10 <- c()
krdm_avgWGSP10 <- c()
krdm_avgCCIG10 <- c()
krdm_avgVIS10 <- c()
krdm_avgALTS10 <- c()
krdm_avgCCOV10 <- c()
krdm_precipData10 <- c()
JD10 <- c()
krdm_avgTemp20 <- c()
krdm_avgDPTemp20 <- c()
krdm_avgWDIR20 <- c()
krdm_avgWSPD20 <- c()
krdm_avgWGSP20 <- c()
krdm_avgCCIG20 <- c()
krdm_avgVIS20 <- c()
krdm_avgALTS20 <- c()
krdm_avgCCOV20 <- c()
krdm_precipData20 <- c()
JD20 <- c()
krdm_avgTemp30 <- c()
krdm_avgDPTemp30 <- c()
krdm_avgWDIR30 <- c()
krdm_avgWSPD30 <- c()
krdm_avgWGSP30 <- c()
krdm_avgCCIG30 <- c()
krdm_avgVIS30 <- c()
krdm_avgALTS30 <- c()
krdm_avgCCOV30 <- c()
krdm_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(krdm_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(krdm_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      krdm_toKeep[rows[i],cols[i]] <- 0
    } else {
      krdm_toKeep[rows[i],cols[i]] <- krdm_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    krdm_toKeep[rows[i],cols[i]] <- 0
  }
}

### get precip amounts by date
krdm_precipAmounts <- aggregate(krdm_toKeep$PRECIPAMOUNT1, by=list(krdm_toKeep$date), sum)
colnames(krdm_precipAmounts) <- c("date","total")
krdm_precipAmounts <- transform(krdm_precipAmounts,JD = yday(as.Date(krdm_precipAmounts$date, "%m/%d/%Y")))
krdm_precipAmounts
krdm_precipCounts10 <-c()
krdm_precipCounts20 <-c()
krdm_precipCounts30 <-c()

### loop through and get averages by julian day

krdm_toKeep10 <- filter(krdm_toKeep, year(date) >= 2014)
krdm_precip10 <- filter(krdm_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  krdm_filtered <- filter(krdm_toKeep10, JD == i)
  krdm_precipFilt <- filter(krdm_precip10, JD == i)
  krdm_avgTemp10[i] <- mean(krdm_filtered$AIRTEMPERATURE, na.rm = TRUE)
  krdm_avgDPTemp10[i] <- mean(krdm_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  krdm_avgWDIR10[i] <- mean(krdm_filtered$WINDDIRECTION, na.rm = TRUE)
  krdm_avgWSPD10[i] <- mean(krdm_filtered$WINDSPEED, na.rm = TRUE)
  krdm_avgCCIG10[i] <- mean(krdm_filtered$CLOUDCEILING, na.rm = TRUE)
  krdm_avgVIS10[i] <- mean(krdm_filtered$VISIBILITY, na.rm = TRUE)
  krdm_avgWGSP10[i] <- mean(krdm_filtered$WINDGUSTSPEED, na.rm = TRUE)
  krdm_avgALTS10[i] <- mean(krdm_filtered$ALTIMETERSETTING, na.rm = TRUE)
  krdm_avgCCOV10[i] <- mean(krdm_filtered$CLOUDCOVER, na.rm = TRUE)
  #krdm_precipCounts10[i] <- sum(krdm_precipFilt$total)
  c_p10[i] <- sum(krdm_precipFilt$total)
  JD10[i] <- i
}

krdm_toKeep20 <- filter(krdm_toKeep,year(date) >= 2004)
krdm_precip20 <- filter(krdm_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  krdm_filtered <- filter(krdm_toKeep20, JD == i)
  krdm_precipFilt <- filter(krdm_precip20, JD == i)
  krdm_avgTemp20[i] <- mean(krdm_filtered$AIRTEMPERATURE, na.rm = TRUE)
  krdm_avgDPTemp20[i] <- mean(krdm_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  krdm_avgWDIR20[i] <- mean(krdm_filtered$WINDDIRECTION, na.rm = TRUE)
  krdm_avgWSPD20[i] <- mean(krdm_filtered$WINDSPEED, na.rm = TRUE)
  krdm_avgCCIG20[i] <- mean(krdm_filtered$CLOUDCEILING, na.rm = TRUE)
  krdm_avgVIS20[i] <- mean(krdm_filtered$VISIBILITY, na.rm = TRUE)
  krdm_avgWGSP20[i] <- mean(krdm_filtered$WINDGUSTSPEED, na.rm = TRUE)
  krdm_avgALTS20[i] <- mean(krdm_filtered$ALTIMETERSETTING, na.rm = TRUE)
  krdm_avgCCOV20[i] <- mean(krdm_filtered$CLOUDCOVER, na.rm = TRUE)
  #krdm_precipCounts20[i] <- sum(krdm_precipFilt$total)
  c_p20[i] <- sum(krdm_precipFilt$total)
  JD20[i] <- i
}

krdm_toKeep30 <- filter(krdm_toKeep,year(date) >= 1994)
krdm_precip30 <- filter(krdm_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  krdm_filtered <- filter(krdm_toKeep30, JD == i)
  krdm_precipFilt <- filter(krdm_precip30, JD == i)
  krdm_avgTemp30[i] <- mean(krdm_filtered$AIRTEMPERATURE, na.rm = TRUE)
  krdm_avgDPTemp30[i] <- mean(krdm_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  krdm_avgWDIR30[i] <- mean(krdm_filtered$WINDDIRECTION, na.rm = TRUE)
  krdm_avgWSPD30[i] <- mean(krdm_filtered$WINDSPEED, na.rm = TRUE)
  krdm_avgCCIG30[i] <- mean(krdm_filtered$CLOUDCEILING, na.rm = TRUE)
  krdm_avgVIS30[i] <- mean(krdm_filtered$VISIBILITY, na.rm = TRUE)
  krdm_avgWGSP30[i] <- mean(krdm_filtered$WINDGUSTSPEED, na.rm = TRUE)
  krdm_avgALTS30[i] <- mean(krdm_filtered$ALTIMETERSETTING, na.rm = TRUE)
  krdm_avgCCOV30[i] <- mean(krdm_filtered$CLOUDCOVER, na.rm = TRUE)
  #krdm_precipCounts30[i] <- sum(krdm_precipFilt$total)
  c_p30[i] <- sum(krdm_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
krdm_precipCounts10 = c_p10/10
krdm_precipCounts20 = c_p20/20
#round(krdm_precipCounts20,1)
krdm_precipCounts30 = c_p30/30
#round(krdm_precipCounts30,1)

## handle avg precip amount by rain event
krdm_amountByRainEvent10 <- round(krdm_precipCounts10,1)
krdm_amountByRainEvent20 <- round(krdm_precipCounts20,1)
krdm_amountByRainEvent30 <- round(krdm_precipCounts30,1)


#get avg array for years of data
krdm_avgArray10 <- data.frame(0,0,0,0,0,0,0,krdm_avgWDIR10,krdm_avgWSPD10,krdm_avgWGSP10,krdm_avgCCIG10,krdm_avgVIS10,krdm_avgTemp10,krdm_avgDPTemp10,krdm_amountByRainEvent10,krdm_avgCCOV10,krdm_avgALTS10,JD10)
krdm_avgArray20 <- data.frame(0,0,0,0,0,0,0,krdm_avgWDIR20,krdm_avgWSPD20,krdm_avgWGSP20,krdm_avgCCIG20,krdm_avgVIS20,krdm_avgTemp20,krdm_avgDPTemp20,krdm_amountByRainEvent20,krdm_avgCCOV20,krdm_avgALTS20,JD20)
krdm_avgArray30 <- data.frame(0,0,0,0,0,0,0,krdm_avgWDIR30,krdm_avgWSPD30,krdm_avgWGSP30,krdm_avgCCIG30,krdm_avgVIS30,krdm_avgTemp30,krdm_avgDPTemp30,krdm_amountByRainEvent30,krdm_avgCCOV30,krdm_avgALTS30,JD30)

#10 years
krdm_toKeep10 <- filter(krdm_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(krdm_toKeep10[,j]))) {
    JDrow <- krdm_toKeep10$JD[i]
    krdm_toKeep10[i,j] <- krdm_avgArray10[JDrow,j]
  }
}

#20 years
krdm_toKeep20 <- filter(krdm_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(krdm_toKeep20[,j]))) {
    JDrow <- krdm_toKeep20$JD[i]
    krdm_toKeep20[i,j] <- krdm_avgArray20[JDrow,j]
  }
}

#30 years
krdm_toKeep30 <- filter(krdm_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(krdm_toKeep30[,j]))) {
    JDrow <- krdm_toKeep$JD[i]
    krdm_toKeep30[i,j] <- krdm_avgArray30[JDrow,j]
  }
}

# write.csv(krdm_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm10.csv")
# write.csv(krdm_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm20.csv")
# write.csv(krdm_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm30.csv")
# write.csv(krdm_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm10_avg.csv")
# write.csv(krdm_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm20_avg.csv")
# write.csv(krdm_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm30_avg.csv")
# write.csv(krdm_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/krdm_data.csv")


#### Buffalo, NY {.tabset}
##### 10 Year Splines
##Temperature
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
kbuf_toKeep <- kbuf_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(kbuf_toKeep[,i]==5)))&length(which(kbuf_toKeep[,i]==5))>0) {
    rows <- which(kbuf_toKeep[,i]==5)
    kbuf_toKeep <- kbuf_toKeep[-rows,]
  }
}

#remove qc columns
kbuf_toKeep <- kbuf_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
kbuf_toKeep <- transform(kbuf_toKeep,date = as.Date(kbuf_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(kbuf_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#kbuf_toKeep = kbuf_toKeep[order(kbuf_toKeep$date),]

#set variables
kbuf_avgTemp10 <- c()
kbuf_avgDPTemp10 <- c()
kbuf_avgWDIR10 <- c()
kbuf_avgWSPD10 <- c()
kbuf_avgWGSP10 <- c()
kbuf_avgCCIG10 <- c()
kbuf_avgVIS10 <- c()
kbuf_avgALTS10 <- c()
kbuf_avgCCOV10 <- c()
kbuf_precipData10 <- c()
JD10 <- c()
kbuf_avgTemp20 <- c()
kbuf_avgDPTemp20 <- c()
kbuf_avgWDIR20 <- c()
kbuf_avgWSPD20 <- c()
kbuf_avgWGSP20 <- c()
kbuf_avgCCIG20 <- c()
kbuf_avgVIS20 <- c()
kbuf_avgALTS20 <- c()
kbuf_avgCCOV20 <- c()
kbuf_precipData20 <- c()
JD20 <- c()
kbuf_avgTemp30 <- c()
kbuf_avgDPTemp30 <- c()
kbuf_avgWDIR30 <- c()
kbuf_avgWSPD30 <- c()
kbuf_avgWGSP30 <- c()
kbuf_avgCCIG30 <- c()
kbuf_avgVIS30 <- c()
kbuf_avgALTS30 <- c()
kbuf_avgCCOV30 <- c()
kbuf_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(kbuf_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(kbuf_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      kbuf_toKeep[rows[i],cols[i]] <- 0
    } else {
      kbuf_toKeep[rows[i],cols[i]] <- kbuf_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    kbuf_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
kbuf_precipAmounts <- aggregate(kbuf_toKeep$PRECIPAMOUNT1, by=list(kbuf_toKeep$date), sum)
colnames(kbuf_precipAmounts) <- c("date","total")
kbuf_precipAmounts <- transform(kbuf_precipAmounts,JD = yday(as.Date(kbuf_precipAmounts$date, "%m/%d/%Y")))
kbuf_precipAmounts
kbuf_precipCounts10 <-c()
kbuf_precipCounts20 <-c()
kbuf_precipCounts30 <-c()

### loop through and get averages by julian day

kbuf_toKeep10 <- filter(kbuf_toKeep, year(date) >= 2014)
kbuf_precip10 <- filter(kbuf_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  kbuf_filtered <- filter(kbuf_toKeep10, JD == i)
  kbuf_precipFilt <- filter(kbuf_precip10, JD == i)
  kbuf_avgTemp10[i] <- mean(kbuf_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kbuf_avgDPTemp10[i] <- mean(kbuf_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kbuf_avgWDIR10[i] <- mean(kbuf_filtered$WINDDIRECTION, na.rm = TRUE)
  kbuf_avgWSPD10[i] <- mean(kbuf_filtered$WINDSPEED, na.rm = TRUE)
  kbuf_avgCCIG10[i] <- mean(kbuf_filtered$CLOUDCEILING, na.rm = TRUE)
  kbuf_avgVIS10[i] <- mean(kbuf_filtered$VISIBILITY, na.rm = TRUE)
  kbuf_avgWGSP10[i] <- mean(kbuf_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kbuf_avgALTS10[i] <- mean(kbuf_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kbuf_avgCCOV10[i] <- mean(kbuf_filtered$CLOUDCOVER, na.rm = TRUE)
  #kbuf_precipCounts10[i] <- sum(kbuf_precipFilt$total)
  c_p10[i] <- sum(kbuf_precipFilt$total)
  JD10[i] <- i
}

kbuf_toKeep20 <- filter(kbuf_toKeep,year(date) >= 2004)
kbuf_precip20 <- filter(kbuf_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  kbuf_filtered <- filter(kbuf_toKeep20, JD == i)
  kbuf_precipFilt <- filter(kbuf_precip20, JD == i)
  kbuf_avgTemp20[i] <- mean(kbuf_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kbuf_avgDPTemp20[i] <- mean(kbuf_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kbuf_avgWDIR20[i] <- mean(kbuf_filtered$WINDDIRECTION, na.rm = TRUE)
  kbuf_avgWSPD20[i] <- mean(kbuf_filtered$WINDSPEED, na.rm = TRUE)
  kbuf_avgCCIG20[i] <- mean(kbuf_filtered$CLOUDCEILING, na.rm = TRUE)
  kbuf_avgVIS20[i] <- mean(kbuf_filtered$VISIBILITY, na.rm = TRUE)
  kbuf_avgWGSP20[i] <- mean(kbuf_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kbuf_avgALTS20[i] <- mean(kbuf_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kbuf_avgCCOV20[i] <- mean(kbuf_filtered$CLOUDCOVER, na.rm = TRUE)
  #kbuf_precipCounts20[i] <- sum(kbuf_precipFilt$total)
  c_p20[i] <- sum(kbuf_precipFilt$total)
  JD20[i] <- i
}

kbuf_toKeep30 <- filter(kbuf_toKeep,year(date) >= 1994)
kbuf_precip30 <- filter(kbuf_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  kbuf_filtered <- filter(kbuf_toKeep30, JD == i)
  kbuf_precipFilt <- filter(kbuf_precip30, JD == i)
  kbuf_avgTemp30[i] <- mean(kbuf_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kbuf_avgDPTemp30[i] <- mean(kbuf_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kbuf_avgWDIR30[i] <- mean(kbuf_filtered$WINDDIRECTION, na.rm = TRUE)
  kbuf_avgWSPD30[i] <- mean(kbuf_filtered$WINDSPEED, na.rm = TRUE)
  kbuf_avgCCIG30[i] <- mean(kbuf_filtered$CLOUDCEILING, na.rm = TRUE)
  kbuf_avgVIS30[i] <- mean(kbuf_filtered$VISIBILITY, na.rm = TRUE)
  kbuf_avgWGSP30[i] <- mean(kbuf_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kbuf_avgALTS30[i] <- mean(kbuf_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kbuf_avgCCOV30[i] <- mean(kbuf_filtered$CLOUDCOVER, na.rm = TRUE)
  #kbuf_precipCounts30[i] <- sum(kbuf_precipFilt$total)
  c_p30[i] <- sum(kbuf_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
kbuf_precipCounts10 <- c_p10/10
kbuf_precipCounts20 <- c_p20/20
#round(kbuf_precipCounts20,1)
kbuf_precipCounts30 <- c_p30/30
#round(kbuf_precipCounts30,1)

## handle avg precip amount by rain event
kbuf_amountByRainEvent10 <- round(kbuf_precipCounts10,1)
kbuf_amountByRainEvent20 <- round(kbuf_precipCounts20,1)
kbuf_amountByRainEvent30 <- round(kbuf_precipCounts30,1)


#get avg array for years of data
kbuf_avgArray10 <- data.frame(0,0,0,0,0,0,0,kbuf_avgWDIR10,kbuf_avgWSPD10,kbuf_avgWGSP10,kbuf_avgCCIG10,kbuf_avgVIS10,kbuf_avgTemp10,kbuf_avgDPTemp10,kbuf_amountByRainEvent10,kbuf_avgCCOV10,kbuf_avgALTS10,JD10)
kbuf_avgArray20 <- data.frame(0,0,0,0,0,0,0,kbuf_avgWDIR20,kbuf_avgWSPD20,kbuf_avgWGSP20,kbuf_avgCCIG20,kbuf_avgVIS20,kbuf_avgTemp20,kbuf_avgDPTemp20,kbuf_amountByRainEvent20,kbuf_avgCCOV20,kbuf_avgALTS20,JD20)
kbuf_avgArray30 <- data.frame(0,0,0,0,0,0,0,kbuf_avgWDIR30,kbuf_avgWSPD30,kbuf_avgWGSP30,kbuf_avgCCIG30,kbuf_avgVIS30,kbuf_avgTemp30,kbuf_avgDPTemp30,kbuf_amountByRainEvent30,kbuf_avgCCOV30,kbuf_avgALTS30,JD30)

#10 years
kbuf_toKeep10 <- filter(kbuf_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(kbuf_toKeep10[,j]))) {
    JDrow <- kbuf_toKeep10$JD[i]
    kbuf_toKeep10[i,j] <- kbuf_avgArray10[JDrow,j]
  }
}

#20 years
kbuf_toKeep20 <- filter(kbuf_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(kbuf_toKeep20[,j]))) {
    JDrow <- kbuf_toKeep20$JD[i]
    kbuf_toKeep20[i,j] <- kbuf_avgArray20[JDrow,j]
  }
}

#30 years
kbuf_toKeep30 <- filter(kbuf_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(kbuf_toKeep30[,j]))) {
    JDrow <- kbuf_toKeep$JD[i]
    kbuf_toKeep30[i,j] <- kbuf_avgArray30[JDrow,j]
  }
}

# write.csv(kbuf_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf10.csv")
# write.csv(kbuf_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf20.csv")
# write.csv(kbuf_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf30.csv")
# write.csv(kbuf_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf10_avg.csv")
# write.csv(kbuf_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf20_avg.csv")
# write.csv(kbuf_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf30_avg.csv")
# write.csv(kbuf_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kbuf_data.csv")



#### Topeka, KS {.tabset}

##### 10 Year Splines
##Temperature
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
kfoe_toKeep <- kfoe_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(kfoe_toKeep[,i]==5)))&length(which(kfoe_toKeep[,i]==5))>0) {
    rows <- which(kfoe_toKeep[,i]==5)
    kfoe_toKeep <- kfoe_toKeep[-rows,]
  }
}

#remove qc columns
kfoe_toKeep <- kfoe_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
kfoe_toKeep <- transform(kfoe_toKeep,date = as.Date(kfoe_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(kfoe_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#kfoe_toKeep = kfoe_toKeep[order(kfoe_toKeep$date),]

#set variables
kfoe_avgTemp10 <- c()
kfoe_avgDPTemp10 <- c()
kfoe_avgWDIR10 <- c()
kfoe_avgWSPD10 <- c()
kfoe_avgWGSP10 <- c()
kfoe_avgCCIG10 <- c()
kfoe_avgVIS10 <- c()
kfoe_avgALTS10 <- c()
kfoe_avgCCOV10 <- c()
kfoe_precipData10 <- c()
JD10 <- c()
kfoe_avgTemp20 <- c()
kfoe_avgDPTemp20 <- c()
kfoe_avgWDIR20 <- c()
kfoe_avgWSPD20 <- c()
kfoe_avgWGSP20 <- c()
kfoe_avgCCIG20 <- c()
kfoe_avgVIS20 <- c()
kfoe_avgALTS20 <- c()
kfoe_avgCCOV20 <- c()
kfoe_precipData20 <- c()
JD20 <- c()
kfoe_avgTemp30 <- c()
kfoe_avgDPTemp30 <- c()
kfoe_avgWDIR30 <- c()
kfoe_avgWSPD30 <- c()
kfoe_avgWGSP30 <- c()
kfoe_avgCCIG30 <- c()
kfoe_avgVIS30 <- c()
kfoe_avgALTS30 <- c()
kfoe_avgCCOV30 <- c()
kfoe_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(kfoe_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(kfoe_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      kfoe_toKeep[rows[i],cols[i]] <- 0
    } else {
      kfoe_toKeep[rows[i],cols[i]] <- kfoe_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    kfoe_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
kfoe_precipAmounts <- aggregate(kfoe_toKeep$PRECIPAMOUNT1, by=list(kfoe_toKeep$date), sum)
colnames(kfoe_precipAmounts) <- c("date","total")
kfoe_precipAmounts <- transform(kfoe_precipAmounts,JD = yday(as.Date(kfoe_precipAmounts$date, "%m/%d/%Y")))
kfoe_precipAmounts
kfoe_precipCounts10 <-c()
kfoe_precipCounts20 <-c()
kfoe_precipCounts30 <-c()

### loop through and get averages by julian day

kfoe_toKeep10 <- filter(kfoe_toKeep, year(date) >= 2014)
kfoe_precip10 <- filter(kfoe_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  kfoe_filtered <- filter(kfoe_toKeep10, JD == i)
  kfoe_precipFilt <- filter(kfoe_precip10, JD == i)
  kfoe_avgTemp10[i] <- mean(kfoe_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kfoe_avgDPTemp10[i] <- mean(kfoe_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kfoe_avgWDIR10[i] <- mean(kfoe_filtered$WINDDIRECTION, na.rm = TRUE)
  kfoe_avgWSPD10[i] <- mean(kfoe_filtered$WINDSPEED, na.rm = TRUE)
  kfoe_avgCCIG10[i] <- mean(kfoe_filtered$CLOUDCEILING, na.rm = TRUE)
  kfoe_avgVIS10[i] <- mean(kfoe_filtered$VISIBILITY, na.rm = TRUE)
  kfoe_avgWGSP10[i] <- mean(kfoe_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kfoe_avgALTS10[i] <- mean(kfoe_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kfoe_avgCCOV10[i] <- mean(kfoe_filtered$CLOUDCOVER, na.rm = TRUE)
  #kfoe_precipCounts10[i] <- sum(kfoe_precipFilt$total)
  c_p10[i] <- sum(kfoe_precipFilt$total)
  JD10[i] <- i
}

kfoe_toKeep20 <- filter(kfoe_toKeep,year(date) >= 2004)
kfoe_precip20 <- filter(kfoe_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  kfoe_filtered <- filter(kfoe_toKeep20, JD == i)
  kfoe_precipFilt <- filter(kfoe_precip20, JD == i)
  kfoe_avgTemp20[i] <- mean(kfoe_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kfoe_avgDPTemp20[i] <- mean(kfoe_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kfoe_avgWDIR20[i] <- mean(kfoe_filtered$WINDDIRECTION, na.rm = TRUE)
  kfoe_avgWSPD20[i] <- mean(kfoe_filtered$WINDSPEED, na.rm = TRUE)
  kfoe_avgCCIG20[i] <- mean(kfoe_filtered$CLOUDCEILING, na.rm = TRUE)
  kfoe_avgVIS20[i] <- mean(kfoe_filtered$VISIBILITY, na.rm = TRUE)
  kfoe_avgWGSP20[i] <- mean(kfoe_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kfoe_avgALTS20[i] <- mean(kfoe_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kfoe_avgCCOV20[i] <- mean(kfoe_filtered$CLOUDCOVER, na.rm = TRUE)
  #kfoe_precipCounts20[i] <- sum(kfoe_precipFilt$total)
  c_p20[i] <- sum(kfoe_precipFilt$total)
  JD20[i] <- i
}

kfoe_toKeep30 <- filter(kfoe_toKeep,year(date) >= 1994)
kfoe_precip30 <- filter(kfoe_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  kfoe_filtered <- filter(kfoe_toKeep30, JD == i)
  kfoe_precipFilt <- filter(kfoe_precip30, JD == i)
  kfoe_avgTemp30[i] <- mean(kfoe_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kfoe_avgDPTemp30[i] <- mean(kfoe_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kfoe_avgWDIR30[i] <- mean(kfoe_filtered$WINDDIRECTION, na.rm = TRUE)
  kfoe_avgWSPD30[i] <- mean(kfoe_filtered$WINDSPEED, na.rm = TRUE)
  kfoe_avgCCIG30[i] <- mean(kfoe_filtered$CLOUDCEILING, na.rm = TRUE)
  kfoe_avgVIS30[i] <- mean(kfoe_filtered$VISIBILITY, na.rm = TRUE)
  kfoe_avgWGSP30[i] <- mean(kfoe_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kfoe_avgALTS30[i] <- mean(kfoe_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kfoe_avgCCOV30[i] <- mean(kfoe_filtered$CLOUDCOVER, na.rm = TRUE)
  #kfoe_precipCounts30[i] <- sum(kfoe_precipFilt$total)
  c_p30[i] <- sum(kfoe_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
kfoe_precipCounts10 <- c_p10/10
kfoe_precipCounts20 <- c_p20/20
#round(kfoe_precipCounts20,1)
kfoe_precipCounts30 <- c_p30/30
#round(kfoe_precipCounts30,1)

## handle avg precip amount by rain event
kfoe_amountByRainEvent10 <- round(kfoe_precipCounts10,1)
kfoe_amountByRainEvent20 <- round(kfoe_precipCounts20,1)
kfoe_amountByRainEvent30 <- round(kfoe_precipCounts30,1)


#get avg array for years of data
kfoe_avgArray10 <- data.frame(0,0,0,0,0,0,0,kfoe_avgWDIR10,kfoe_avgWSPD10,kfoe_avgWGSP10,kfoe_avgCCIG10,kfoe_avgVIS10,kfoe_avgTemp10,kfoe_avgDPTemp10,kfoe_amountByRainEvent10,kfoe_avgCCOV10,kfoe_avgALTS10,JD10)
kfoe_avgArray20 <- data.frame(0,0,0,0,0,0,0,kfoe_avgWDIR20,kfoe_avgWSPD20,kfoe_avgWGSP20,kfoe_avgCCIG20,kfoe_avgVIS20,kfoe_avgTemp20,kfoe_avgDPTemp20,kfoe_amountByRainEvent20,kfoe_avgCCOV20,kfoe_avgALTS20,JD20)
kfoe_avgArray30 <- data.frame(0,0,0,0,0,0,0,kfoe_avgWDIR30,kfoe_avgWSPD30,kfoe_avgWGSP30,kfoe_avgCCIG30,kfoe_avgVIS30,kfoe_avgTemp30,kfoe_avgDPTemp30,kfoe_amountByRainEvent30,kfoe_avgCCOV30,kfoe_avgALTS30,JD30)

#10 years
kfoe_toKeep10 <- filter(kfoe_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(kfoe_toKeep10[,j]))) {
    JDrow <- kfoe_toKeep10$JD[i]
    kfoe_toKeep10[i,j] <- kfoe_avgArray10[JDrow,j]
  }
}

#20 years
kfoe_toKeep20 <- filter(kfoe_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(kfoe_toKeep20[,j]))) {
    JDrow <- kfoe_toKeep20$JD[i]
    kfoe_toKeep20[i,j] <- kfoe_avgArray20[JDrow,j]
  }
}

#30 years
kfoe_toKeep30 <- filter(kfoe_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(kfoe_toKeep30[,j]))) {
    JDrow <- kfoe_toKeep$JD[i]
    kfoe_toKeep30[i,j] <- kfoe_avgArray30[JDrow,j]
  }
}

# write.csv(kfoe_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe10.csv")
# write.csv(kfoe_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe20.csv")
# write.csv(kfoe_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe30.csv")
# write.csv(kfoe_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe10_avg.csv")
# write.csv(kfoe_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe20_avg.csv")
# write.csv(kfoe_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe30_avg.csv")
# write.csv(kfoe_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kfoe_data.csv")



#### Madison, WI {.tabset}
#### 10 Year Splines
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
kmsn_toKeep <- kmsn_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(kmsn_toKeep[,i]==5)))&length(which(kmsn_toKeep[,i]==5))>0) {
    rows <- which(kmsn_toKeep[,i]==5)
    kmsn_toKeep <- kmsn_toKeep[-rows,]
  }
}

#remove qc columns
kmsn_toKeep <- kmsn_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
kmsn_toKeep <- transform(kmsn_toKeep,date = as.Date(kmsn_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(kmsn_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#kmsn_toKeep = kmsn_toKeep[order(kmsn_toKeep$date),]

#set variables
kmsn_avgTemp10 <- c()
kmsn_avgDPTemp10 <- c()
kmsn_avgWDIR10 <- c()
kmsn_avgWSPD10 <- c()
kmsn_avgWGSP10 <- c()
kmsn_avgCCIG10 <- c()
kmsn_avgVIS10 <- c()
kmsn_avgALTS10 <- c()
kmsn_avgCCOV10 <- c()
kmsn_precipData10 <- c()
JD10 <- c()
kmsn_avgTemp20 <- c()
kmsn_avgDPTemp20 <- c()
kmsn_avgWDIR20 <- c()
kmsn_avgWSPD20 <- c()
kmsn_avgWGSP20 <- c()
kmsn_avgCCIG20 <- c()
kmsn_avgVIS20 <- c()
kmsn_avgALTS20 <- c()
kmsn_avgCCOV20 <- c()
kmsn_precipData20 <- c()
JD20 <- c()
kmsn_avgTemp30 <- c()
kmsn_avgDPTemp30 <- c()
kmsn_avgWDIR30 <- c()
kmsn_avgWSPD30 <- c()
kmsn_avgWGSP30 <- c()
kmsn_avgCCIG30 <- c()
kmsn_avgVIS30 <- c()
kmsn_avgALTS30 <- c()
kmsn_avgCCOV30 <- c()
kmsn_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(kmsn_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(kmsn_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      kmsn_toKeep[rows[i],cols[i]] <- 0
    } else {
      kmsn_toKeep[rows[i],cols[i]] <- kmsn_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    kmsn_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
kmsn_precipAmounts <- aggregate(kmsn_toKeep$PRECIPAMOUNT1, by=list(kmsn_toKeep$date), sum)
colnames(kmsn_precipAmounts) <- c("date","total")
kmsn_precipAmounts <- transform(kmsn_precipAmounts,JD = yday(as.Date(kmsn_precipAmounts$date, "%m/%d/%Y")))
kmsn_precipAmounts
kmsn_precipCounts10 <-c()
kmsn_precipCounts20 <-c()
kmsn_precipCounts30 <-c()

### loop through and get averages by julian day

kmsn_toKeep10 <- filter(kmsn_toKeep, year(date) >= 2014)
kmsn_precip10 <- filter(kmsn_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  kmsn_filtered <- filter(kmsn_toKeep10, JD == i)
  kmsn_precipFilt <- filter(kmsn_precip10, JD == i)
  kmsn_avgTemp10[i] <- mean(kmsn_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kmsn_avgDPTemp10[i] <- mean(kmsn_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kmsn_avgWDIR10[i] <- mean(kmsn_filtered$WINDDIRECTION, na.rm = TRUE)
  kmsn_avgWSPD10[i] <- mean(kmsn_filtered$WINDSPEED, na.rm = TRUE)
  kmsn_avgCCIG10[i] <- mean(kmsn_filtered$CLOUDCEILING, na.rm = TRUE)
  kmsn_avgVIS10[i] <- mean(kmsn_filtered$VISIBILITY, na.rm = TRUE)
  kmsn_avgWGSP10[i] <- mean(kmsn_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kmsn_avgALTS10[i] <- mean(kmsn_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kmsn_avgCCOV10[i] <- mean(kmsn_filtered$CLOUDCOVER, na.rm = TRUE)
  #kmsn_precipCounts10[i] <- sum(kmsn_precipFilt$total)
  c_p10[i] <- sum(kmsn_precipFilt$total)
  JD10[i] <- i
}

kmsn_toKeep20 <- filter(kmsn_toKeep,year(date) >= 2004)
kmsn_precip20 <- filter(kmsn_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  kmsn_filtered <- filter(kmsn_toKeep20, JD == i)
  kmsn_precipFilt <- filter(kmsn_precip20, JD == i)
  kmsn_avgTemp20[i] <- mean(kmsn_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kmsn_avgDPTemp20[i] <- mean(kmsn_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kmsn_avgWDIR20[i] <- mean(kmsn_filtered$WINDDIRECTION, na.rm = TRUE)
  kmsn_avgWSPD20[i] <- mean(kmsn_filtered$WINDSPEED, na.rm = TRUE)
  kmsn_avgCCIG20[i] <- mean(kmsn_filtered$CLOUDCEILING, na.rm = TRUE)
  kmsn_avgVIS20[i] <- mean(kmsn_filtered$VISIBILITY, na.rm = TRUE)
  kmsn_avgWGSP20[i] <- mean(kmsn_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kmsn_avgALTS20[i] <- mean(kmsn_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kmsn_avgCCOV20[i] <- mean(kmsn_filtered$CLOUDCOVER, na.rm = TRUE)
  #kmsn_precipCounts20[i] <- sum(kmsn_precipFilt$total)
  c_p20[i] <- sum(kmsn_precipFilt$total)
  JD20[i] <- i
}

kmsn_toKeep30 <- filter(kmsn_toKeep,year(date) >= 1994)
kmsn_precip30 <- filter(kmsn_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  kmsn_filtered <- filter(kmsn_toKeep30, JD == i)
  kmsn_precipFilt <- filter(kmsn_precip30, JD == i)
  kmsn_avgTemp30[i] <- mean(kmsn_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kmsn_avgDPTemp30[i] <- mean(kmsn_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kmsn_avgWDIR30[i] <- mean(kmsn_filtered$WINDDIRECTION, na.rm = TRUE)
  kmsn_avgWSPD30[i] <- mean(kmsn_filtered$WINDSPEED, na.rm = TRUE)
  kmsn_avgCCIG30[i] <- mean(kmsn_filtered$CLOUDCEILING, na.rm = TRUE)
  kmsn_avgVIS30[i] <- mean(kmsn_filtered$VISIBILITY, na.rm = TRUE)
  kmsn_avgWGSP30[i] <- mean(kmsn_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kmsn_avgALTS30[i] <- mean(kmsn_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kmsn_avgCCOV30[i] <- mean(kmsn_filtered$CLOUDCOVER, na.rm = TRUE)
  #kmsn_precipCounts30[i] <- sum(kmsn_precipFilt$total)
  c_p30[i] <- sum(kmsn_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
kmsn_precipCounts10 <- c_p10/10
kmsn_precipCounts20 <- c_p20/20
#round(kmsn_precipCounts20,1)
kmsn_precipCounts30 <- c_p30/30
#round(kmsn_precipCounts30,1)

## handle avg precip amount by rain event
kmsn_amountByRainEvent10 <- round(kmsn_precipCounts10,1)
kmsn_amountByRainEvent20 <- round(kmsn_precipCounts20,1)
kmsn_amountByRainEvent30 <- round(kmsn_precipCounts30,1)


#get avg array for years of data
kmsn_avgArray10 <- data.frame(0,0,0,0,0,0,0,kmsn_avgWDIR10,kmsn_avgWSPD10,kmsn_avgWGSP10,kmsn_avgCCIG10,kmsn_avgVIS10,kmsn_avgTemp10,kmsn_avgDPTemp10,kmsn_amountByRainEvent10,kmsn_avgCCOV10,kmsn_avgALTS10,JD10)
kmsn_avgArray20 <- data.frame(0,0,0,0,0,0,0,kmsn_avgWDIR20,kmsn_avgWSPD20,kmsn_avgWGSP20,kmsn_avgCCIG20,kmsn_avgVIS20,kmsn_avgTemp20,kmsn_avgDPTemp20,kmsn_amountByRainEvent20,kmsn_avgCCOV20,kmsn_avgALTS20,JD20)
kmsn_avgArray30 <- data.frame(0,0,0,0,0,0,0,kmsn_avgWDIR30,kmsn_avgWSPD30,kmsn_avgWGSP30,kmsn_avgCCIG30,kmsn_avgVIS30,kmsn_avgTemp30,kmsn_avgDPTemp30,kmsn_amountByRainEvent30,kmsn_avgCCOV30,kmsn_avgALTS30,JD30)

#10 years
kmsn_toKeep10 <- filter(kmsn_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(kmsn_toKeep10[,j]))) {
    JDrow <- kmsn_toKeep10$JD[i]
    kmsn_toKeep10[i,j] <- kmsn_avgArray10[JDrow,j]
  }
}

#20 years
kmsn_toKeep20 <- filter(kmsn_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(kmsn_toKeep20[,j]))) {
    JDrow <- kmsn_toKeep20$JD[i]
    kmsn_toKeep20[i,j] <- kmsn_avgArray20[JDrow,j]
  }
}

#30 years
kmsn_toKeep30 <- filter(kmsn_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(kmsn_toKeep30[,j]))) {
    JDrow <- kmsn_toKeep$JD[i]
    kmsn_toKeep30[i,j] <- kmsn_avgArray30[JDrow,j]
  }
}

# write.csv(kmsn_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn10.csv")
# write.csv(kmsn_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn20.csv")
# write.csv(kmsn_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn30.csv")
# write.csv(kmsn_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn10_avg.csv")
# write.csv(kmsn_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn20_avg.csv")
# write.csv(kmsn_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn30_avg.csv")
# write.csv(kmsn_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kmsn_data.csv")


#### Tri-cities Airport, TN {.tabset}
##### 10 Year Splines
#Temperature
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
ktri_toKeep <- ktri_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(ktri_toKeep[,i]==5)))&length(which(ktri_toKeep[,i]==5))>0) {
    rows <- which(ktri_toKeep[,i]==5)
    ktri_toKeep <- ktri_toKeep[-rows,]
  }
}

#remove qc columns
ktri_toKeep <- ktri_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
ktri_toKeep <- transform(ktri_toKeep,date = as.Date(ktri_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(ktri_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#ktri_toKeep = ktri_toKeep[order(ktri_toKeep$date),]


#set variables
ktri_avgTemp10 <- c()
ktri_avgDPTemp10 <- c()
ktri_avgWDIR10 <- c()
ktri_avgWSPD10 <- c()
ktri_avgWGSP10 <- c()
ktri_avgCCIG10 <- c()
ktri_avgVIS10 <- c()
ktri_avgALTS10 <- c()
ktri_avgCCOV10 <- c()
ktri_precipData10 <- c()
JD10 <- c()
ktri_avgTemp20 <- c()
ktri_avgDPTemp20 <- c()
ktri_avgWDIR20 <- c()
ktri_avgWSPD20 <- c()
ktri_avgWGSP20 <- c()
ktri_avgCCIG20 <- c()
ktri_avgVIS20 <- c()
ktri_avgALTS20 <- c()
ktri_avgCCOV20 <- c()
ktri_precipData20 <- c()
JD20 <- c()
ktri_avgTemp30 <- c()
ktri_avgDPTemp30 <- c()
ktri_avgWDIR30 <- c()
ktri_avgWSPD30 <- c()
ktri_avgWGSP30 <- c()
ktri_avgCCIG30 <- c()
ktri_avgVIS30 <- c()
ktri_avgALTS30 <- c()
ktri_avgCCOV30 <- c()
ktri_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(ktri_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(ktri_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      ktri_toKeep[rows[i],cols[i]] <- 0
    } else {
      ktri_toKeep[rows[i],cols[i]] <- ktri_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    ktri_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
ktri_precipAmounts <- aggregate(ktri_toKeep$PRECIPAMOUNT1, by=list(ktri_toKeep$date), sum)
colnames(ktri_precipAmounts) <- c("date","total")
ktri_precipAmounts <- transform(ktri_precipAmounts,JD = yday(as.Date(ktri_precipAmounts$date, "%m/%d/%Y")))
ktri_precipAmounts
ktri_precipCounts10 <-c()
ktri_precipCounts20 <-c()
ktri_precipCounts30 <-c()

### loop through and get averages by julian day

ktri_toKeep10 <- filter(ktri_toKeep, year(date) >= 2014)
ktri_precip10 <- filter(ktri_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  ktri_filtered <- filter(ktri_toKeep10, JD == i)
  ktri_precipFilt <- filter(ktri_precip10, JD == i)
  ktri_avgTemp10[i] <- mean(ktri_filtered$AIRTEMPERATURE, na.rm = TRUE)
  ktri_avgDPTemp10[i] <- mean(ktri_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ktri_avgWDIR10[i] <- mean(ktri_filtered$WINDDIRECTION, na.rm = TRUE)
  ktri_avgWSPD10[i] <- mean(ktri_filtered$WINDSPEED, na.rm = TRUE)
  ktri_avgCCIG10[i] <- mean(ktri_filtered$CLOUDCEILING, na.rm = TRUE)
  ktri_avgVIS10[i] <- mean(ktri_filtered$VISIBILITY, na.rm = TRUE)
  ktri_avgWGSP10[i] <- mean(ktri_filtered$WINDGUSTSPEED, na.rm = TRUE)
  ktri_avgALTS10[i] <- mean(ktri_filtered$ALTIMETERSETTING, na.rm = TRUE)
  ktri_avgCCOV10[i] <- mean(ktri_filtered$CLOUDCOVER, na.rm = TRUE)
  #ktri_precipCounts10[i] <- sum(ktri_precipFilt$total)
  c_p10[i] <- sum(ktri_precipFilt$total)
  JD10[i] <- i
}

ktri_toKeep20 <- filter(ktri_toKeep,year(date) >= 2004)
ktri_precip20 <- filter(ktri_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  ktri_filtered <- filter(ktri_toKeep20, JD == i)
  ktri_precipFilt <- filter(ktri_precip20, JD == i)
  ktri_avgTemp20[i] <- mean(ktri_filtered$AIRTEMPERATURE, na.rm = TRUE)
  ktri_avgDPTemp20[i] <- mean(ktri_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ktri_avgWDIR20[i] <- mean(ktri_filtered$WINDDIRECTION, na.rm = TRUE)
  ktri_avgWSPD20[i] <- mean(ktri_filtered$WINDSPEED, na.rm = TRUE)
  ktri_avgCCIG20[i] <- mean(ktri_filtered$CLOUDCEILING, na.rm = TRUE)
  ktri_avgVIS20[i] <- mean(ktri_filtered$VISIBILITY, na.rm = TRUE)
  ktri_avgWGSP20[i] <- mean(ktri_filtered$WINDGUSTSPEED, na.rm = TRUE)
  ktri_avgALTS20[i] <- mean(ktri_filtered$ALTIMETERSETTING, na.rm = TRUE)
  ktri_avgCCOV20[i] <- mean(ktri_filtered$CLOUDCOVER, na.rm = TRUE)
  #ktri_precipCounts20[i] <- sum(ktri_precipFilt$total)
  c_p20[i] <- sum(ktri_precipFilt$total)
  JD20[i] <- i
}

ktri_toKeep30 <- filter(ktri_toKeep,year(date) >= 1994)
ktri_precip30 <- filter(ktri_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  ktri_filtered <- filter(ktri_toKeep30, JD == i)
  ktri_precipFilt <- filter(ktri_precip30, JD == i)
  ktri_avgTemp30[i] <- mean(ktri_filtered$AIRTEMPERATURE, na.rm = TRUE)
  ktri_avgDPTemp30[i] <- mean(ktri_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ktri_avgWDIR30[i] <- mean(ktri_filtered$WINDDIRECTION, na.rm = TRUE)
  ktri_avgWSPD30[i] <- mean(ktri_filtered$WINDSPEED, na.rm = TRUE)
  ktri_avgCCIG30[i] <- mean(ktri_filtered$CLOUDCEILING, na.rm = TRUE)
  ktri_avgVIS30[i] <- mean(ktri_filtered$VISIBILITY, na.rm = TRUE)
  ktri_avgWGSP30[i] <- mean(ktri_filtered$WINDGUSTSPEED, na.rm = TRUE)
  ktri_avgALTS30[i] <- mean(ktri_filtered$ALTIMETERSETTING, na.rm = TRUE)
  ktri_avgCCOV30[i] <- mean(ktri_filtered$CLOUDCOVER, na.rm = TRUE)
  #ktri_precipCounts30[i] <- sum(ktri_precipFilt$total)
  c_p30[i] <- sum(ktri_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
ktri_precipCounts10 <- c_p10/10
ktri_precipCounts20 <- c_p20/20
#round(ktri_precipCounts20,1)
ktri_precipCounts30 <- c_p30/30
#round(ktri_precipCounts30,1)

## handle avg precip amount by rain event
ktri_amountByRainEvent10 <- round(ktri_precipCounts10,1)
ktri_amountByRainEvent20 <- round(ktri_precipCounts20,1)
ktri_amountByRainEvent30 <- round(ktri_precipCounts30,1)


#get avg array for years of data
ktri_avgArray10 <- data.frame(0,0,0,0,0,0,0,ktri_avgWDIR10,ktri_avgWSPD10,ktri_avgWGSP10,ktri_avgCCIG10,ktri_avgVIS10,ktri_avgTemp10,ktri_avgDPTemp10,ktri_amountByRainEvent10,ktri_avgCCOV10,ktri_avgALTS10,JD10)
ktri_avgArray20 <- data.frame(0,0,0,0,0,0,0,ktri_avgWDIR20,ktri_avgWSPD20,ktri_avgWGSP20,ktri_avgCCIG20,ktri_avgVIS20,ktri_avgTemp20,ktri_avgDPTemp20,ktri_amountByRainEvent20,ktri_avgCCOV20,ktri_avgALTS20,JD20)
ktri_avgArray30 <- data.frame(0,0,0,0,0,0,0,ktri_avgWDIR30,ktri_avgWSPD30,ktri_avgWGSP30,ktri_avgCCIG30,ktri_avgVIS30,ktri_avgTemp30,ktri_avgDPTemp30,ktri_amountByRainEvent30,ktri_avgCCOV30,ktri_avgALTS30,JD30)

#10 years
ktri_toKeep10 <- filter(ktri_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(ktri_toKeep10[,j]))) {
    JDrow <- ktri_toKeep10$JD[i]
    ktri_toKeep10[i,j] <- ktri_avgArray10[JDrow,j]
  }
}

#20 years
ktri_toKeep20 <- filter(ktri_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(ktri_toKeep20[,j]))) {
    JDrow <- ktri_toKeep20$JD[i]
    ktri_toKeep20[i,j] <- ktri_avgArray20[JDrow,j]
  }
}

#30 years
ktri_toKeep30 <- filter(ktri_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(ktri_toKeep30[,j]))) {
    JDrow <- ktri_toKeep$JD[i]
    ktri_toKeep30[i,j] <- ktri_avgArray30[JDrow,j]
  }
}

# write.csv(ktri_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri10.csv")
# write.csv(ktri_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri20.csv")
# write.csv(ktri_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri30.csv")
# write.csv(ktri_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri10_avg.csv")
# write.csv(ktri_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri20_avg.csv")
# write.csv(ktri_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri30_avg.csv")
# write.csv(ktri_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ktri_data.csv")


#### Juneau, AK {.tabset}
##### 10 Year Splines
#Temperature
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
pajn_toKeep <- pajn_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(pajn_toKeep[,i]==5)))&length(which(pajn_toKeep[,i]==5))>0) {
    rows <- which(pajn_toKeep[,i]==5)
    pajn_toKeep <- pajn_toKeep[-rows,]
  }
}

#remove qc columns
pajn_toKeep <- pajn_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
pajn_toKeep <- transform(pajn_toKeep,date = as.Date(pajn_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(pajn_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#pajn_toKeep = pajn_toKeep[order(pajn_toKeep$date),]

#set variables
pajn_avgTemp10 <- c()
pajn_avgDPTemp10 <- c()
pajn_avgWDIR10 <- c()
pajn_avgWSPD10 <- c()
pajn_avgWGSP10 <- c()
pajn_avgCCIG10 <- c()
pajn_avgVIS10 <- c()
pajn_avgALTS10 <- c()
pajn_avgCCOV10 <- c()
pajn_precipData10 <- c()
JD10 <- c()
pajn_avgTemp20 <- c()
pajn_avgDPTemp20 <- c()
pajn_avgWDIR20 <- c()
pajn_avgWSPD20 <- c()
pajn_avgWGSP20 <- c()
pajn_avgCCIG20 <- c()
pajn_avgVIS20 <- c()
pajn_avgALTS20 <- c()
pajn_avgCCOV20 <- c()
pajn_precipData20 <- c()
JD20 <- c()
pajn_avgTemp30 <- c()
pajn_avgDPTemp30 <- c()
pajn_avgWDIR30 <- c()
pajn_avgWSPD30 <- c()
pajn_avgWGSP30 <- c()
pajn_avgCCIG30 <- c()
pajn_avgVIS30 <- c()
pajn_avgALTS30 <- c()
pajn_avgCCOV30 <- c()
pajn_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(pajn_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(pajn_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      pajn_toKeep[rows[i],cols[i]] <- 0
    } else {
      pajn_toKeep[rows[i],cols[i]] <- pajn_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    pajn_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
pajn_precipAmounts <- aggregate(pajn_toKeep$PRECIPAMOUNT1, by=list(pajn_toKeep$date), sum)
colnames(pajn_precipAmounts) <- c("date","total")
pajn_precipAmounts <- transform(pajn_precipAmounts,JD = yday(as.Date(pajn_precipAmounts$date, "%m/%d/%Y")))
pajn_precipAmounts
pajn_precipCounts10 <-c()
pajn_precipCounts20 <-c()
pajn_precipCounts30 <-c()

### loop through and get averages by julian day

pajn_toKeep10 <- filter(pajn_toKeep, year(date) >= 2014)
pajn_precip10 <- filter(pajn_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  pajn_filtered <- filter(pajn_toKeep10, JD == i)
  pajn_precipFilt <- filter(pajn_precip10, JD == i)
  pajn_avgTemp10[i] <- mean(pajn_filtered$AIRTEMPERATURE, na.rm = TRUE)
  pajn_avgDPTemp10[i] <- mean(pajn_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  pajn_avgWDIR10[i] <- mean(pajn_filtered$WINDDIRECTION, na.rm = TRUE)
  pajn_avgWSPD10[i] <- mean(pajn_filtered$WINDSPEED, na.rm = TRUE)
  pajn_avgCCIG10[i] <- mean(pajn_filtered$CLOUDCEILING, na.rm = TRUE)
  pajn_avgVIS10[i] <- mean(pajn_filtered$VISIBILITY, na.rm = TRUE)
  pajn_avgWGSP10[i] <- mean(pajn_filtered$WINDGUSTSPEED, na.rm = TRUE)
  pajn_avgALTS10[i] <- mean(pajn_filtered$ALTIMETERSETTING, na.rm = TRUE)
  pajn_avgCCOV10[i] <- mean(pajn_filtered$CLOUDCOVER, na.rm = TRUE)
  #pajn_precipCounts10[i] <- sum(pajn_precipFilt$total)
  c_p10[i] <- sum(pajn_precipFilt$total)
  JD10[i] <- i
}

pajn_toKeep20 <- filter(pajn_toKeep,year(date) >= 2004)
pajn_precip20 <- filter(pajn_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  pajn_filtered <- filter(pajn_toKeep20, JD == i)
  pajn_precipFilt <- filter(pajn_precip20, JD == i)
  pajn_avgTemp20[i] <- mean(pajn_filtered$AIRTEMPERATURE, na.rm = TRUE)
  pajn_avgDPTemp20[i] <- mean(pajn_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  pajn_avgWDIR20[i] <- mean(pajn_filtered$WINDDIRECTION, na.rm = TRUE)
  pajn_avgWSPD20[i] <- mean(pajn_filtered$WINDSPEED, na.rm = TRUE)
  pajn_avgCCIG20[i] <- mean(pajn_filtered$CLOUDCEILING, na.rm = TRUE)
  pajn_avgVIS20[i] <- mean(pajn_filtered$VISIBILITY, na.rm = TRUE)
  pajn_avgWGSP20[i] <- mean(pajn_filtered$WINDGUSTSPEED, na.rm = TRUE)
  pajn_avgALTS20[i] <- mean(pajn_filtered$ALTIMETERSETTING, na.rm = TRUE)
  pajn_avgCCOV20[i] <- mean(pajn_filtered$CLOUDCOVER, na.rm = TRUE)
  #pajn_precipCounts20[i] <- sum(pajn_precipFilt$total)
  c_p20[i] <- sum(pajn_precipFilt$total)
  JD20[i] <- i
}

pajn_toKeep30 <- filter(pajn_toKeep,year(date) >= 1994)
pajn_precip30 <- filter(pajn_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  pajn_filtered <- filter(pajn_toKeep30, JD == i)
  pajn_precipFilt <- filter(pajn_precip30, JD == i)
  pajn_avgTemp30[i] <- mean(pajn_filtered$AIRTEMPERATURE, na.rm = TRUE)
  pajn_avgDPTemp30[i] <- mean(pajn_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  pajn_avgWDIR30[i] <- mean(pajn_filtered$WINDDIRECTION, na.rm = TRUE)
  pajn_avgWSPD30[i] <- mean(pajn_filtered$WINDSPEED, na.rm = TRUE)
  pajn_avgCCIG30[i] <- mean(pajn_filtered$CLOUDCEILING, na.rm = TRUE)
  pajn_avgVIS30[i] <- mean(pajn_filtered$VISIBILITY, na.rm = TRUE)
  pajn_avgWGSP30[i] <- mean(pajn_filtered$WINDGUSTSPEED, na.rm = TRUE)
  pajn_avgALTS30[i] <- mean(pajn_filtered$ALTIMETERSETTING, na.rm = TRUE)
  pajn_avgCCOV30[i] <- mean(pajn_filtered$CLOUDCOVER, na.rm = TRUE)
  #pajn_precipCounts30[i] <- sum(pajn_precipFilt$total)
  c_p30[i] <- sum(pajn_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
pajn_precipCounts10 <- c_p10/10
pajn_precipCounts20 <- c_p20/20
#round(pajn_precipCounts20,1)
pajn_precipCounts30 <- c_p30/30
#round(pajn_precipCounts30,1)

## handle avg precip amount by rain event
pajn_amountByRainEvent10 <- round(pajn_precipCounts10,1)
pajn_amountByRainEvent20 <- round(pajn_precipCounts20,1)
pajn_amountByRainEvent30 <- round(pajn_precipCounts30,1)


#get avg array for years of data
pajn_avgArray10 <- data.frame(0,0,0,0,0,0,0,pajn_avgWDIR10,pajn_avgWSPD10,pajn_avgWGSP10,pajn_avgCCIG10,pajn_avgVIS10,pajn_avgTemp10,pajn_avgDPTemp10,pajn_amountByRainEvent10,pajn_avgCCOV10,pajn_avgALTS10,JD10)
pajn_avgArray20 <- data.frame(0,0,0,0,0,0,0,pajn_avgWDIR20,pajn_avgWSPD20,pajn_avgWGSP20,pajn_avgCCIG20,pajn_avgVIS20,pajn_avgTemp20,pajn_avgDPTemp20,pajn_amountByRainEvent20,pajn_avgCCOV20,pajn_avgALTS20,JD20)
pajn_avgArray30 <- data.frame(0,0,0,0,0,0,0,pajn_avgWDIR30,pajn_avgWSPD30,pajn_avgWGSP30,pajn_avgCCIG30,pajn_avgVIS30,pajn_avgTemp30,pajn_avgDPTemp30,pajn_amountByRainEvent30,pajn_avgCCOV30,pajn_avgALTS30,JD30)

#10 years
pajn_toKeep10 <- filter(pajn_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(pajn_toKeep10[,j]))) {
    JDrow <- pajn_toKeep10$JD[i]
    pajn_toKeep10[i,j] <- pajn_avgArray10[JDrow,j]
  }
}

#20 years
pajn_toKeep20 <- filter(pajn_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(pajn_toKeep20[,j]))) {
    JDrow <- pajn_toKeep20$JD[i]
    pajn_toKeep20[i,j] <- pajn_avgArray20[JDrow,j]
  }
}

#30 years
pajn_toKeep30 <- filter(pajn_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(pajn_toKeep30[,j]))) {
    JDrow <- pajn_toKeep$JD[i]
    pajn_toKeep30[i,j] <- pajn_avgArray30[JDrow,j]
  }
}

# write.csv(pajn_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn10.csv")
# write.csv(pajn_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn20.csv")
# write.csv(pajn_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn30.csv")
# write.csv(pajn_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn10_avg.csv")
# write.csv(pajn_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn20_avg.csv")
# write.csv(pajn_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn30_avg.csv")
# write.csv(pajn_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/pajn_data.csv")

#### El Paso, TX {.tabset}
##### 10 Year Splines
#Temperature
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
kelp_toKeep <- kelp_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(kelp_toKeep[,i]==5)))&length(which(kelp_toKeep[,i]==5))>0) {
    rows <- which(kelp_toKeep[,i]==5)
    kelp_toKeep <- kelp_toKeep[-rows,]
  }
}

#remove qc columns
kelp_toKeep <- kelp_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
kelp_toKeep <- transform(kelp_toKeep,date = as.Date(kelp_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(kelp_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#kelp_toKeep = kelp_toKeep[order(kelp_toKeep$date),]

#set variables
kelp_avgTemp10 <- c()
kelp_avgDPTemp10 <- c()
kelp_avgWDIR10 <- c()
kelp_avgWSPD10 <- c()
kelp_avgWGSP10 <- c()
kelp_avgCCIG10 <- c()
kelp_avgVIS10 <- c()
kelp_avgALTS10 <- c()
kelp_avgCCOV10 <- c()
kelp_precipData10 <- c()
JD10 <- c()
kelp_avgTemp20 <- c()
kelp_avgDPTemp20 <- c()
kelp_avgWDIR20 <- c()
kelp_avgWSPD20 <- c()
kelp_avgWGSP20 <- c()
kelp_avgCCIG20 <- c()
kelp_avgVIS20 <- c()
kelp_avgALTS20 <- c()
kelp_avgCCOV20 <- c()
kelp_precipData20 <- c()
JD20 <- c()
kelp_avgTemp30 <- c()
kelp_avgDPTemp30 <- c()
kelp_avgWDIR30 <- c()
kelp_avgWSPD30 <- c()
kelp_avgWGSP30 <- c()
kelp_avgCCIG30 <- c()
kelp_avgVIS30 <- c()
kelp_avgALTS30 <- c()
kelp_avgCCOV30 <- c()
kelp_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(kelp_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(kelp_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      kelp_toKeep[rows[i],cols[i]] <- 0
    } else {
      kelp_toKeep[rows[i],cols[i]] <- kelp_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    kelp_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
kelp_precipAmounts <- aggregate(kelp_toKeep$PRECIPAMOUNT1, by=list(kelp_toKeep$date), sum)
colnames(kelp_precipAmounts) <- c("date","total")
kelp_precipAmounts <- transform(kelp_precipAmounts,JD = yday(as.Date(kelp_precipAmounts$date, "%m/%d/%Y")))
kelp_precipAmounts
kelp_precipCounts10 <-c()
kelp_precipCounts20 <-c()
kelp_precipCounts30 <-c()

### loop through and get averages by julian day

kelp_toKeep10 <- filter(kelp_toKeep, year(date) >= 2014)
kelp_precip10 <- filter(kelp_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  kelp_filtered <- filter(kelp_toKeep10, JD == i)
  kelp_precipFilt <- filter(kelp_precip10, JD == i)
  kelp_avgTemp10[i] <- mean(kelp_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kelp_avgDPTemp10[i] <- mean(kelp_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kelp_avgWDIR10[i] <- mean(kelp_filtered$WINDDIRECTION, na.rm = TRUE)
  kelp_avgWSPD10[i] <- mean(kelp_filtered$WINDSPEED, na.rm = TRUE)
  kelp_avgCCIG10[i] <- mean(kelp_filtered$CLOUDCEILING, na.rm = TRUE)
  kelp_avgVIS10[i] <- mean(kelp_filtered$VISIBILITY, na.rm = TRUE)
  kelp_avgWGSP10[i] <- mean(kelp_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kelp_avgALTS10[i] <- mean(kelp_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kelp_avgCCOV10[i] <- mean(kelp_filtered$CLOUDCOVER, na.rm = TRUE)
  #kelp_precipCounts10[i] <- sum(kelp_precipFilt$total)
  c_p10[i] <- sum(kelp_precipFilt$total)
  JD10[i] <- i
}

kelp_toKeep20 <- filter(kelp_toKeep,year(date) >= 2004)
kelp_precip20 <- filter(kelp_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  kelp_filtered <- filter(kelp_toKeep20, JD == i)
  kelp_precipFilt <- filter(kelp_precip20, JD == i)
  kelp_avgTemp20[i] <- mean(kelp_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kelp_avgDPTemp20[i] <- mean(kelp_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kelp_avgWDIR20[i] <- mean(kelp_filtered$WINDDIRECTION, na.rm = TRUE)
  kelp_avgWSPD20[i] <- mean(kelp_filtered$WINDSPEED, na.rm = TRUE)
  kelp_avgCCIG20[i] <- mean(kelp_filtered$CLOUDCEILING, na.rm = TRUE)
  kelp_avgVIS20[i] <- mean(kelp_filtered$VISIBILITY, na.rm = TRUE)
  kelp_avgWGSP20[i] <- mean(kelp_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kelp_avgALTS20[i] <- mean(kelp_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kelp_avgCCOV20[i] <- mean(kelp_filtered$CLOUDCOVER, na.rm = TRUE)
  #kelp_precipCounts20[i] <- sum(kelp_precipFilt$total)
  c_p20[i] <- sum(kelp_precipFilt$total)
  JD20[i] <- i
}

kelp_toKeep30 <- filter(kelp_toKeep,year(date) >= 1994)
kelp_precip30 <- filter(kelp_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  kelp_filtered <- filter(kelp_toKeep30, JD == i)
  kelp_precipFilt <- filter(kelp_precip30, JD == i)
  kelp_avgTemp30[i] <- mean(kelp_filtered$AIRTEMPERATURE, na.rm = TRUE)
  kelp_avgDPTemp30[i] <- mean(kelp_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kelp_avgWDIR30[i] <- mean(kelp_filtered$WINDDIRECTION, na.rm = TRUE)
  kelp_avgWSPD30[i] <- mean(kelp_filtered$WINDSPEED, na.rm = TRUE)
  kelp_avgCCIG30[i] <- mean(kelp_filtered$CLOUDCEILING, na.rm = TRUE)
  kelp_avgVIS30[i] <- mean(kelp_filtered$VISIBILITY, na.rm = TRUE)
  kelp_avgWGSP30[i] <- mean(kelp_filtered$WINDGUSTSPEED, na.rm = TRUE)
  kelp_avgALTS30[i] <- mean(kelp_filtered$ALTIMETERSETTING, na.rm = TRUE)
  kelp_avgCCOV30[i] <- mean(kelp_filtered$CLOUDCOVER, na.rm = TRUE)
  #kelp_precipCounts30[i] <- sum(kelp_precipFilt$total)
  c_p30[i] <- sum(kelp_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
kelp_precipCounts10 <- c_p10/10
kelp_precipCounts20 <- c_p20/20
#round(kelp_precipCounts20,1)
kelp_precipCounts30 <- c_p30/30
#round(kelp_precipCounts30,1)

## handle avg precip amount by rain event
kelp_amountByRainEvent10 <- round(kelp_precipCounts10,1)
kelp_amountByRainEvent20 <- round(kelp_precipCounts20,1)
kelp_amountByRainEvent30 <- round(kelp_precipCounts30,1)


#get avg array for years of data
kelp_avgArray10 <- data.frame(0,0,0,0,0,0,0,kelp_avgWDIR10,kelp_avgWSPD10,kelp_avgWGSP10,kelp_avgCCIG10,kelp_avgVIS10,kelp_avgTemp10,kelp_avgDPTemp10,kelp_amountByRainEvent10,kelp_avgCCOV10,kelp_avgALTS10,JD10)
kelp_avgArray20 <- data.frame(0,0,0,0,0,0,0,kelp_avgWDIR20,kelp_avgWSPD20,kelp_avgWGSP20,kelp_avgCCIG20,kelp_avgVIS20,kelp_avgTemp20,kelp_avgDPTemp20,kelp_amountByRainEvent20,kelp_avgCCOV20,kelp_avgALTS20,JD20)
kelp_avgArray30 <- data.frame(0,0,0,0,0,0,0,kelp_avgWDIR30,kelp_avgWSPD30,kelp_avgWGSP30,kelp_avgCCIG30,kelp_avgVIS30,kelp_avgTemp30,kelp_avgDPTemp30,kelp_amountByRainEvent30,kelp_avgCCOV30,kelp_avgALTS30,JD30)

#10 years
kelp_toKeep10 <- filter(kelp_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(kelp_toKeep10[,j]))) {
    JDrow <- kelp_toKeep10$JD[i]
    kelp_toKeep10[i,j] <- kelp_avgArray10[JDrow,j]
  }
}

#20 years
kelp_toKeep20 <- filter(kelp_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(kelp_toKeep20[,j]))) {
    JDrow <- kelp_toKeep20$JD[i]
    kelp_toKeep20[i,j] <- kelp_avgArray20[JDrow,j]
  }
}

#30 years
kelp_toKeep30 <- filter(kelp_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(kelp_toKeep30[,j]))) {
    JDrow <- kelp_toKeep$JD[i]
    kelp_toKeep30[i,j] <- kelp_avgArray30[JDrow,j]
  }
}

# write.csv(kelp_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp10.csv")
# write.csv(kelp_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp20.csv")
# write.csv(kelp_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp30.csv")
# write.csv(kelp_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp10_avg.csv")
# write.csv(kelp_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp20_avg.csv")
# write.csv(kelp_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp30_avg.csv")
# write.csv(kelp_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/kelp_data.csv")


#### St. George, UT {.tabset}
##### 10 Year Splines
#Temperature
removeCols <- c(7:13,16,17,20,21,24,27:30,33,34,39:42,45:144,147:164,167:190)
ksgu_toKeep <- ksgu_data[,-removeCols]
columnsToCycle <-c(8,10,12,14,16,18,20,22,24,26)
## this gets rid of QC Codes 5 - which mean the value was dropped and/or should not be included. These make 
## up a very small portion of the dataset, but should be dropped nevertheless

for(i in columnsToCycle) {
  print(i)
  if(!is.null(length(which(ksgu_toKeep[,i]==5)))&length(which(ksgu_toKeep[,i]==5))>0) {
    rows <- which(ksgu_toKeep[,i]==5)
    ksgu_toKeep <- ksgu_toKeep[-rows,]
  }
}

#remove qc columns
ksgu_toKeep <- ksgu_toKeep[,-columnsToCycle]

#change observationtime to date format and add julian date
ksgu_toKeep <- transform(ksgu_toKeep,date = as.Date(ksgu_toKeep$OBSERVATIONTIME, "%m/%d/%Y"), JD = yday(as.Date(ksgu_toKeep$OBSERVATIONTIME, "%m/%d/%Y")))
#ksgu_toKeep = ksgu_toKeep[order(ksgu_toKeep$date),]

#set variables
ksgu_avgTemp10 <- c()
ksgu_avgDPTemp10 <- c()
ksgu_avgWDIR10 <- c()
ksgu_avgWSPD10 <- c()
ksgu_avgWGSP10 <- c()
ksgu_avgCCIG10 <- c()
ksgu_avgVIS10 <- c()
ksgu_avgALTS10 <- c()
ksgu_avgCCOV10 <- c()
ksgu_precipData10 <- c()
JD10 <- c()
ksgu_avgTemp20 <- c()
ksgu_avgDPTemp20 <- c()
ksgu_avgWDIR20 <- c()
ksgu_avgWSPD20 <- c()
ksgu_avgWGSP20 <- c()
ksgu_avgCCIG20 <- c()
ksgu_avgVIS20 <- c()
ksgu_avgALTS20 <- c()
ksgu_avgCCOV20 <- c()
ksgu_precipData20 <- c()
JD20 <- c()
ksgu_avgTemp30 <- c()
ksgu_avgDPTemp30 <- c()
ksgu_avgWDIR30 <- c()
ksgu_avgWSPD30 <- c()
ksgu_avgWGSP30 <- c()
ksgu_avgCCIG30 <- c()
ksgu_avgVIS30 <- c()
ksgu_avgALTS30 <- c()
ksgu_avgCCOV30 <- c()
ksgu_precipData30 <- c()
c_p10 <- c()
c_p20 <- c()
c_p30 <- c()
JD30 <- c()

##handle NAs - take previous value for NA value... this takes a long while...
#weather data is continuous so it doesn't usually make sense to have an average 
#take the spot of the null in psudo-continuous data - not precip values though
rows <- which(is.na(ksgu_toKeep),arr.ind = TRUE)[,1]
cols <- which(is.na(ksgu_toKeep),arr.ind = TRUE)[,2]
for(i in 1:length(rows)) {
  if(cols[i] != 14) {
    if(rows[i] == 1) {
      ksgu_toKeep[rows[i],cols[i]] <- 0
    } else {
      ksgu_toKeep[rows[i],cols[i]] <- ksgu_toKeep[rows[i]-1,cols[i]]
    }
  }
  if(cols[i] == 14) {
    ksgu_toKeep[rows[i],cols[i]] <- 0
  }
}

### handle NULL precip amounts
ksgu_precipAmounts <- aggregate(ksgu_toKeep$PRECIPAMOUNT1, by=list(ksgu_toKeep$date), sum)
colnames(ksgu_precipAmounts) <- c("date","total")
ksgu_precipAmounts <- transform(ksgu_precipAmounts,JD = yday(as.Date(ksgu_precipAmounts$date, "%m/%d/%Y")))
ksgu_precipAmounts
ksgu_precipCounts10 <-c()
ksgu_precipCounts20 <-c()
ksgu_precipCounts30 <-c()

### loop through and get averages by julian day

ksgu_toKeep10 <- filter(ksgu_toKeep, year(date) >= 2014)
ksgu_precip10 <- filter(ksgu_precipAmounts, year(date) >= 2014)
for(i in 1:366) {
  ksgu_filtered <- filter(ksgu_toKeep10, JD == i)
  ksgu_precipFilt <- filter(ksgu_precip10, JD == i)
  ksgu_avgTemp10[i] <- mean(ksgu_filtered$AIRTEMPERATURE, na.rm = TRUE)
  ksgu_avgDPTemp10[i] <- mean(ksgu_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ksgu_avgWDIR10[i] <- mean(ksgu_filtered$WINDDIRECTION, na.rm = TRUE)
  ksgu_avgWSPD10[i] <- mean(ksgu_filtered$WINDSPEED, na.rm = TRUE)
  ksgu_avgCCIG10[i] <- mean(ksgu_filtered$CLOUDCEILING, na.rm = TRUE)
  ksgu_avgVIS10[i] <- mean(ksgu_filtered$VISIBILITY, na.rm = TRUE)
  ksgu_avgWGSP10[i] <- mean(ksgu_filtered$WINDGUSTSPEED, na.rm = TRUE)
  ksgu_avgALTS10[i] <- mean(ksgu_filtered$ALTIMETERSETTING, na.rm = TRUE)
  ksgu_avgCCOV10[i] <- mean(ksgu_filtered$CLOUDCOVER, na.rm = TRUE)
  #ksgu_precipCounts10[i] <- sum(ksgu_precipFilt$total)
  c_p10[i] <- sum(ksgu_precipFilt$total)
  JD10[i] <- i
}

ksgu_toKeep20 <- filter(ksgu_toKeep,year(date) >= 2004)
ksgu_precip20 <- filter(ksgu_precipAmounts, year(date) >= 2004)
for(i in 1:366) {
  ksgu_filtered <- filter(ksgu_toKeep20, JD == i)
  ksgu_precipFilt <- filter(ksgu_precip20, JD == i)
  ksgu_avgTemp20[i] <- mean(ksgu_filtered$AIRTEMPERATURE, na.rm = TRUE)
  ksgu_avgDPTemp20[i] <- mean(ksgu_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ksgu_avgWDIR20[i] <- mean(ksgu_filtered$WINDDIRECTION, na.rm = TRUE)
  ksgu_avgWSPD20[i] <- mean(ksgu_filtered$WINDSPEED, na.rm = TRUE)
  ksgu_avgCCIG20[i] <- mean(ksgu_filtered$CLOUDCEILING, na.rm = TRUE)
  ksgu_avgVIS20[i] <- mean(ksgu_filtered$VISIBILITY, na.rm = TRUE)
  ksgu_avgWGSP20[i] <- mean(ksgu_filtered$WINDGUSTSPEED, na.rm = TRUE)
  ksgu_avgALTS20[i] <- mean(ksgu_filtered$ALTIMETERSETTING, na.rm = TRUE)
  ksgu_avgCCOV20[i] <- mean(ksgu_filtered$CLOUDCOVER, na.rm = TRUE)
  #ksgu_precipCounts20[i] <- sum(ksgu_precipFilt$total)
  c_p20[i] <- sum(ksgu_precipFilt$total)
  JD20[i] <- i
}

ksgu_toKeep30 <- filter(ksgu_toKeep,year(date) >= 1994)
ksgu_precip30 <- filter(ksgu_precipAmounts, year(date) >= 1994)
for(i in 1:366) {
  ksgu_filtered <- filter(ksgu_toKeep30, JD == i)
  ksgu_precipFilt <- filter(ksgu_precip30, JD == i)
  ksgu_avgTemp30[i] <- mean(ksgu_filtered$AIRTEMPERATURE, na.rm = TRUE)
  ksgu_avgDPTemp30[i] <- mean(ksgu_filtered$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ksgu_avgWDIR30[i] <- mean(ksgu_filtered$WINDDIRECTION, na.rm = TRUE)
  ksgu_avgWSPD30[i] <- mean(ksgu_filtered$WINDSPEED, na.rm = TRUE)
  ksgu_avgCCIG30[i] <- mean(ksgu_filtered$CLOUDCEILING, na.rm = TRUE)
  ksgu_avgVIS30[i] <- mean(ksgu_filtered$VISIBILITY, na.rm = TRUE)
  ksgu_avgWGSP30[i] <- mean(ksgu_filtered$WINDGUSTSPEED, na.rm = TRUE)
  ksgu_avgALTS30[i] <- mean(ksgu_filtered$ALTIMETERSETTING, na.rm = TRUE)
  ksgu_avgCCOV30[i] <- mean(ksgu_filtered$CLOUDCOVER, na.rm = TRUE)
  #ksgu_precipCounts30[i] <- sum(ksgu_precipFilt$total)
  c_p30[i] <- sum(ksgu_precipFilt$total)
  JD30[i] <- i
}

#avg precip counts per julian day
ksgu_precipCounts10 <- c_p10/10
ksgu_precipCounts20 <- c_p20/20
#round(ksgu_precipCounts20,1)
ksgu_precipCounts30 <- c_p30/30
#round(ksgu_precipCounts30,1)

## handle avg precip amount by rain event
ksgu_amountByRainEvent10 <- round(ksgu_precipCounts10,1)
ksgu_amountByRainEvent20 <- round(ksgu_precipCounts20,1)
ksgu_amountByRainEvent30 <- round(ksgu_precipCounts30,1)


#get avg array for years of data
ksgu_avgArray10 <- data.frame(0,0,0,0,0,0,0,ksgu_avgWDIR10,ksgu_avgWSPD10,ksgu_avgWGSP10,ksgu_avgCCIG10,ksgu_avgVIS10,ksgu_avgTemp10,ksgu_avgDPTemp10,ksgu_amountByRainEvent10,ksgu_avgCCOV10,ksgu_avgALTS10,JD10)
ksgu_avgArray20 <- data.frame(0,0,0,0,0,0,0,ksgu_avgWDIR20,ksgu_avgWSPD20,ksgu_avgWGSP20,ksgu_avgCCIG20,ksgu_avgVIS20,ksgu_avgTemp20,ksgu_avgDPTemp20,ksgu_amountByRainEvent20,ksgu_avgCCOV20,ksgu_avgALTS20,JD20)
ksgu_avgArray30 <- data.frame(0,0,0,0,0,0,0,ksgu_avgWDIR30,ksgu_avgWSPD30,ksgu_avgWGSP30,ksgu_avgCCIG30,ksgu_avgVIS30,ksgu_avgTemp30,ksgu_avgDPTemp30,ksgu_amountByRainEvent30,ksgu_avgCCOV30,ksgu_avgALTS30,JD30)

#10 years
ksgu_toKeep10 <- filter(ksgu_toKeep,year(date) >= 2014)
for(j in 8:17){
  for(i in which(is.na(ksgu_toKeep10[,j]))) {
    JDrow <- ksgu_toKeep10$JD[i]
    ksgu_toKeep10[i,j] <- ksgu_avgArray10[JDrow,j]
  }
}

#20 years
ksgu_toKeep20 <- filter(ksgu_toKeep,year(date) >= 2004)
for(j in 8:17){
  for(i in which(is.na(ksgu_toKeep20[,j]))) {
    JDrow <- ksgu_toKeep20$JD[i]
    ksgu_toKeep20[i,j] <- ksgu_avgArray20[JDrow,j]
  }
}

#30 years
ksgu_toKeep30 <- filter(ksgu_toKeep,year(date) >= 1994)
for(j in 8:17){
  for(i in which(is.na(ksgu_toKeep30[,j]))) {
    JDrow <- ksgu_toKeep$JD[i]
    ksgu_toKeep30[i,j] <- ksgu_avgArray30[JDrow,j]
  }
}

# write.csv(ksgu_toKeep10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu10.csv")
# write.csv(ksgu_toKeep20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu20.csv")
# write.csv(ksgu_toKeep30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu30.csv")
# write.csv(ksgu_avgArray10,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu10_avg.csv")
# write.csv(ksgu_avgArray20,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu20_avg.csv")
# write.csv(ksgu_avgArray30,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu30_avg.csv")
# write.csv(ksgu_data,"C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/imputed/ksgu_data.csv")

### make all splines
## KRDM
##### 10 Year Splines
##Temperature
krdm_t10_spline_d <- lm(krdm_avgTemp10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_t10_ci_d <- predict(krdm_t10_spline_d, interval = "confidence", level = 0.95)
krdm_t10_spline_10d <- lm(krdm_avgTemp10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_t10_ci_10d <- predict(krdm_t10_spline_10d, interval = "confidence", level = 0.95)
krdm_t10_spline_m <- lm(krdm_avgTemp10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_t10_ci_m <- predict(krdm_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
krdm_dp10_spline_d <- lm(krdm_avgDPTemp10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_dp10_ci_d <- predict(krdm_dp10_spline_d, interval = "confidence", level = 0.95)
krdm_dp10_spline_10d <- lm(krdm_avgDPTemp10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_dp10_ci_10d <- predict(krdm_dp10_spline_10d, interval = "confidence", level = 0.95)
krdm_dp10_spline_m <- lm(krdm_avgDPTemp10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_dp10_ci_m <- predict(krdm_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
krdm_wd10_spline_d <- lm(krdm_avgWDIR10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_wd10_ci_d <- predict(krdm_wd10_spline_d, interval = "confidence", level = 0.95)
krdm_wd10_spline_10d <- lm(krdm_avgWDIR10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_wd10_ci_10d <- predict(krdm_wd10_spline_10d, interval = "confidence", level = 0.95)
krdm_wd10_spline_m <- lm(krdm_avgWDIR10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_wd10_ci_m <- predict(krdm_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
krdm_ws10_spline_d <- lm(krdm_avgWSPD10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_ws10_ci_d <- predict(krdm_ws10_spline_d, interval = "confidence", level = 0.95)
krdm_ws10_spline_10d <- lm(krdm_avgWSPD10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_ws10_ci_10d <- predict(krdm_ws10_spline_10d, interval = "confidence", level = 0.95)
krdm_ws10_spline_m <- lm(krdm_avgWSPD10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_ws10_ci_m <- predict(krdm_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
krdm_wg10_spline_d <- lm(krdm_avgWGSP10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_wg10_ci_d <- predict(krdm_wg10_spline_d, interval = "confidence", level = 0.95)
krdm_wg10_spline_10d <- lm(krdm_avgWGSP10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_wg10_ci_10d <- predict(krdm_wg10_spline_10d, interval = "confidence", level = 0.95)
krdm_wg10_spline_m <- lm(krdm_avgWGSP10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_wg10_ci_m <- predict(krdm_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
krdm_cig10_spline_d <- lm(krdm_avgCCIG10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_cig10_ci_d <- predict(krdm_cig10_spline_d, interval = "confidence", level = 0.95)
krdm_cig10_spline_10d <- lm(krdm_avgCCIG10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_cig10_ci_10d <- predict(krdm_cig10_spline_10d, interval = "confidence", level = 0.95)
krdm_cig10_spline_m <- lm(krdm_avgCCIG10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_cig10_ci_m <- predict(krdm_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
krdm_vis10_spline_d <- lm(krdm_avgVIS10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_vis10_ci_d <- predict(krdm_vis10_spline_d, interval = "confidence", level = 0.95)
krdm_vis10_spline_10d <- lm(krdm_avgVIS10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_vis10_ci_10d <- predict(krdm_vis10_spline_10d, interval = "confidence", level = 0.95)
krdm_vis10_spline_m <- lm(krdm_avgVIS10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_vis10_ci_m <- predict(krdm_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
krdm_cc10_spline_d <- lm(krdm_avgCCOV10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_cc10_ci_d <- predict(krdm_cc10_spline_d, interval = "confidence", level = 0.95)
krdm_cc10_spline_10d <- lm(krdm_avgCCOV10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_cc10_ci_10d <- predict(krdm_cc10_spline_10d, interval = "confidence", level = 0.95)
krdm_cc10_spline_m <- lm(krdm_avgCCOV10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_cc10_ci_m <- predict(krdm_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
krdm_alt10_spline_d <- lm(krdm_avgALTS10~ns(c(1:366), df=364), krdm_avgArray10)
krdm_alt10_ci_d <- predict(krdm_alt10_spline_d, interval = "confidence", level = 0.95)
krdm_alt10_spline_10d <- lm(krdm_avgALTS10~ns(c(1:366), df=35), krdm_avgArray10)
krdm_alt10_ci_10d <- predict(krdm_alt10_spline_10d, interval = "confidence", level = 0.95)
krdm_alt10_spline_m <- lm(krdm_avgALTS10~ns(c(1:366), df=11), krdm_avgArray10)
krdm_alt10_ci_m <- predict(krdm_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
krdm_t20_spline_d <- lm(krdm_avgTemp20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_t20_ci_d <- predict(krdm_t20_spline_d, interval = "confidence", level = 0.95)
krdm_t20_spline_10d <- lm(krdm_avgTemp20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_t20_ci_10d <- predict(krdm_t20_spline_10d, interval = "confidence", level = 0.95)
krdm_t20_spline_m <- lm(krdm_avgTemp20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_t20_ci_m <- predict(krdm_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
krdm_dp20_spline_d <- lm(krdm_avgDPTemp20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_dp20_ci_d <- predict(krdm_dp20_spline_d, interval = "confidence", level = 0.95)
krdm_dp20_spline_10d <- lm(krdm_avgDPTemp20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_dp20_ci_10d <- predict(krdm_dp20_spline_10d, interval = "confidence", level = 0.95)
krdm_dp20_spline_m <- lm(krdm_avgDPTemp20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_dp20_ci_m <- predict(krdm_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
krdm_wd20_spline_d <- lm(krdm_avgWDIR20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_wd20_ci_d <- predict(krdm_wd20_spline_d, interval = "confidence", level = 0.95)
krdm_wd20_spline_10d <- lm(krdm_avgWDIR20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_wd20_ci_10d <- predict(krdm_wd20_spline_10d, interval = "confidence", level = 0.95)
krdm_wd20_spline_m <- lm(krdm_avgWDIR20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_wd20_ci_m <- predict(krdm_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
krdm_ws20_spline_d <- lm(krdm_avgWSPD20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_ws20_ci_d <- predict(krdm_ws20_spline_d, interval = "confidence", level = 0.95)
krdm_ws20_spline_10d <- lm(krdm_avgWSPD20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_ws20_ci_10d <- predict(krdm_ws20_spline_10d, interval = "confidence", level = 0.95)
krdm_ws20_spline_m <- lm(krdm_avgWSPD20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_ws20_ci_m <- predict(krdm_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
krdm_wg20_spline_d <- lm(krdm_avgWGSP20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_wg20_ci_d <- predict(krdm_wg20_spline_d, interval = "confidence", level = 0.95)
krdm_wg20_spline_10d <- lm(krdm_avgWGSP20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_wg20_ci_10d <- predict(krdm_wg20_spline_10d, interval = "confidence", level = 0.95)
krdm_wg20_spline_m <- lm(krdm_avgWGSP20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_wg20_ci_m <- predict(krdm_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
krdm_cig20_spline_d <- lm(krdm_avgCCIG20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_cig20_ci_d <- predict(krdm_cig20_spline_d, interval = "confidence", level = 0.95)
krdm_cig20_spline_10d <- lm(krdm_avgCCIG20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_cig20_ci_10d <- predict(krdm_cig20_spline_10d, interval = "confidence", level = 0.95)
krdm_cig20_spline_m <- lm(krdm_avgCCIG20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_cig20_ci_m <- predict(krdm_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
krdm_vis20_spline_d <- lm(krdm_avgVIS20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_vis20_ci_d <- predict(krdm_vis20_spline_d, interval = "confidence", level = 0.95)
krdm_vis20_spline_10d <- lm(krdm_avgVIS20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_vis20_ci_10d <- predict(krdm_vis20_spline_10d, interval = "confidence", level = 0.95)
krdm_vis20_spline_m <- lm(krdm_avgVIS20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_vis20_ci_m <- predict(krdm_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
krdm_cc20_spline_d <- lm(krdm_avgCCOV20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_cc20_ci_d <- predict(krdm_cc20_spline_d, interval = "confidence", level = 0.95)
krdm_cc20_spline_10d <- lm(krdm_avgCCOV20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_cc20_ci_10d <- predict(krdm_cc20_spline_10d, interval = "confidence", level = 0.95)
krdm_cc20_spline_m <- lm(krdm_avgCCOV20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_cc20_ci_m <- predict(krdm_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
krdm_alt20_spline_d <- lm(krdm_avgALTS20~ns(c(1:366), df=364), krdm_avgArray20)
krdm_alt20_ci_d <- predict(krdm_alt20_spline_d, interval = "confidence", level = 0.95)
krdm_alt20_spline_10d <- lm(krdm_avgALTS20~ns(c(1:366), df=35), krdm_avgArray20)
krdm_alt20_ci_10d <- predict(krdm_alt20_spline_10d, interval = "confidence", level = 0.95)
krdm_alt20_spline_m <- lm(krdm_avgALTS20~ns(c(1:366), df=11), krdm_avgArray20)
krdm_alt20_ci_m <- predict(krdm_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
krdm_t30_spline_d <- lm(krdm_avgTemp30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_t30_ci_d <- predict(krdm_t30_spline_d, interval = "confidence", level = 0.95)
krdm_t30_spline_10d <- lm(krdm_avgTemp30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_t30_ci_10d <- predict(krdm_t30_spline_10d, interval = "confidence", level = 0.95)
krdm_t30_spline_m <- lm(krdm_avgTemp30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_t30_ci_m <- predict(krdm_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
krdm_dp30_spline_d <- lm(krdm_avgDPTemp30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_dp30_ci_d <- predict(krdm_dp30_spline_d, interval = "confidence", level = 0.95)
krdm_dp30_spline_10d <- lm(krdm_avgDPTemp30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_dp30_ci_10d <- predict(krdm_dp30_spline_10d, interval = "confidence", level = 0.95)
krdm_dp30_spline_m <- lm(krdm_avgDPTemp30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_dp30_ci_m <- predict(krdm_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
krdm_wd30_spline_d <- lm(krdm_avgWDIR30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_wd30_ci_d <- predict(krdm_wd30_spline_d, interval = "confidence", level = 0.95)
krdm_wd30_spline_10d <- lm(krdm_avgWDIR30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_wd30_ci_10d <- predict(krdm_wd30_spline_10d, interval = "confidence", level = 0.95)
krdm_wd30_spline_m <- lm(krdm_avgWDIR30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_wd30_ci_m <- predict(krdm_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
krdm_ws30_spline_d <- lm(krdm_avgWSPD30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_ws30_ci_d <- predict(krdm_ws30_spline_d, interval = "confidence", level = 0.95)
krdm_ws30_spline_10d <- lm(krdm_avgWSPD30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_ws30_ci_10d <- predict(krdm_ws30_spline_10d, interval = "confidence", level = 0.95)
krdm_ws30_spline_m <- lm(krdm_avgWSPD30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_ws30_ci_m <- predict(krdm_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
krdm_wg30_spline_d <- lm(krdm_avgWGSP30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_wg30_ci_d <- predict(krdm_wg30_spline_d, interval = "confidence", level = 0.95)
krdm_wg30_spline_10d <- lm(krdm_avgWGSP30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_wg30_ci_10d <- predict(krdm_wg30_spline_10d, interval = "confidence", level = 0.95)
krdm_wg30_spline_m <- lm(krdm_avgWGSP30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_wg30_ci_m <- predict(krdm_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
krdm_cig30_spline_d <- lm(krdm_avgCCIG30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_cig30_ci_d <- predict(krdm_cig30_spline_d, interval = "confidence", level = 0.95)
krdm_cig30_spline_10d <- lm(krdm_avgCCIG30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_cig30_ci_10d <- predict(krdm_cig30_spline_10d, interval = "confidence", level = 0.95)
krdm_cig30_spline_m <- lm(krdm_avgCCIG30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_cig30_ci_m <- predict(krdm_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
krdm_vis30_spline_d <- lm(krdm_avgVIS30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_vis30_ci_d <- predict(krdm_vis30_spline_d, interval = "confidence", level = 0.95)
krdm_vis30_spline_10d <- lm(krdm_avgVIS30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_vis30_ci_10d <- predict(krdm_vis30_spline_10d, interval = "confidence", level = 0.95)
krdm_vis30_spline_m <- lm(krdm_avgVIS30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_vis30_ci_m <- predict(krdm_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
krdm_cc30_spline_d <- lm(krdm_avgCCOV30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_cc30_ci_d <- predict(krdm_cc30_spline_d, interval = "confidence", level = 0.95)
krdm_cc30_spline_10d <- lm(krdm_avgCCOV30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_cc30_ci_10d <- predict(krdm_cc30_spline_10d, interval = "confidence", level = 0.95)
krdm_cc30_spline_m <- lm(krdm_avgCCOV30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_cc30_ci_m <- predict(krdm_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
krdm_alt30_spline_d <- lm(krdm_avgALTS30~ns(c(1:366), df=364), krdm_avgArray30)
krdm_alt30_ci_d <- predict(krdm_alt30_spline_d, interval = "confidence", level = 0.95)
krdm_alt30_spline_10d <- lm(krdm_avgALTS30~ns(c(1:366), df=35), krdm_avgArray30)
krdm_alt30_ci_10d <- predict(krdm_alt30_spline_10d, interval = "confidence", level = 0.95)
krdm_alt30_spline_m <- lm(krdm_avgALTS30~ns(c(1:366), df=11), krdm_avgArray30)
krdm_alt30_ci_m <- predict(krdm_alt30_spline_m, interval = "confidence", level = 0.95)



## KBUF
##### 10 Year Splines
##Temperature
kbuf_t10_spline_d <- lm(kbuf_avgTemp10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_t10_ci_d <- predict(kbuf_t10_spline_d, interval = "confidence", level = 0.95)
kbuf_t10_spline_10d <- lm(kbuf_avgTemp10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_t10_ci_10d <- predict(kbuf_t10_spline_10d, interval = "confidence", level = 0.95)
kbuf_t10_spline_m <- lm(kbuf_avgTemp10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_t10_ci_m <- predict(kbuf_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kbuf_dp10_spline_d <- lm(kbuf_avgDPTemp10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_dp10_ci_d <- predict(kbuf_dp10_spline_d, interval = "confidence", level = 0.95)
kbuf_dp10_spline_10d <- lm(kbuf_avgDPTemp10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_dp10_ci_10d <- predict(kbuf_dp10_spline_10d, interval = "confidence", level = 0.95)
kbuf_dp10_spline_m <- lm(kbuf_avgDPTemp10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_dp10_ci_m <- predict(kbuf_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kbuf_wd10_spline_d <- lm(kbuf_avgWDIR10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_wd10_ci_d <- predict(kbuf_wd10_spline_d, interval = "confidence", level = 0.95)
kbuf_wd10_spline_10d <- lm(kbuf_avgWDIR10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_wd10_ci_10d <- predict(kbuf_wd10_spline_10d, interval = "confidence", level = 0.95)
kbuf_wd10_spline_m <- lm(kbuf_avgWDIR10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_wd10_ci_m <- predict(kbuf_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kbuf_ws10_spline_d <- lm(kbuf_avgWSPD10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_ws10_ci_d <- predict(kbuf_ws10_spline_d, interval = "confidence", level = 0.95)
kbuf_ws10_spline_10d <- lm(kbuf_avgWSPD10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_ws10_ci_10d <- predict(kbuf_ws10_spline_10d, interval = "confidence", level = 0.95)
kbuf_ws10_spline_m <- lm(kbuf_avgWSPD10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_ws10_ci_m <- predict(kbuf_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kbuf_wg10_spline_d <- lm(kbuf_avgWGSP10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_wg10_ci_d <- predict(kbuf_wg10_spline_d, interval = "confidence", level = 0.95)
kbuf_wg10_spline_10d <- lm(kbuf_avgWGSP10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_wg10_ci_10d <- predict(kbuf_wg10_spline_10d, interval = "confidence", level = 0.95)
kbuf_wg10_spline_m <- lm(kbuf_avgWGSP10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_wg10_ci_m <- predict(kbuf_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kbuf_cig10_spline_d <- lm(kbuf_avgCCIG10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_cig10_ci_d <- predict(kbuf_cig10_spline_d, interval = "confidence", level = 0.95)
kbuf_cig10_spline_10d <- lm(kbuf_avgCCIG10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_cig10_ci_10d <- predict(kbuf_cig10_spline_10d, interval = "confidence", level = 0.95)
kbuf_cig10_spline_m <- lm(kbuf_avgCCIG10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_cig10_ci_m <- predict(kbuf_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kbuf_vis10_spline_d <- lm(kbuf_avgVIS10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_vis10_ci_d <- predict(kbuf_vis10_spline_d, interval = "confidence", level = 0.95)
kbuf_vis10_spline_10d <- lm(kbuf_avgVIS10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_vis10_ci_10d <- predict(kbuf_vis10_spline_10d, interval = "confidence", level = 0.95)
kbuf_vis10_spline_m <- lm(kbuf_avgVIS10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_vis10_ci_m <- predict(kbuf_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kbuf_cc10_spline_d <- lm(kbuf_avgCCOV10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_cc10_ci_d <- predict(kbuf_cc10_spline_d, interval = "confidence", level = 0.95)
kbuf_cc10_spline_10d <- lm(kbuf_avgCCOV10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_cc10_ci_10d <- predict(kbuf_cc10_spline_10d, interval = "confidence", level = 0.95)
kbuf_cc10_spline_m <- lm(kbuf_avgCCOV10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_cc10_ci_m <- predict(kbuf_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kbuf_alt10_spline_d <- lm(kbuf_avgALTS10~ns(c(1:366), df=364), kbuf_avgArray10)
kbuf_alt10_ci_d <- predict(kbuf_alt10_spline_d, interval = "confidence", level = 0.95)
kbuf_alt10_spline_10d <- lm(kbuf_avgALTS10~ns(c(1:366), df=35), kbuf_avgArray10)
kbuf_alt10_ci_10d <- predict(kbuf_alt10_spline_10d, interval = "confidence", level = 0.95)
kbuf_alt10_spline_m <- lm(kbuf_avgALTS10~ns(c(1:366), df=11), kbuf_avgArray10)
kbuf_alt10_ci_m <- predict(kbuf_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
kbuf_t20_spline_d <- lm(kbuf_avgTemp20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_t20_ci_d <- predict(kbuf_t20_spline_d, interval = "confidence", level = 0.95)
kbuf_t20_spline_10d <- lm(kbuf_avgTemp20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_t20_ci_10d <- predict(kbuf_t20_spline_10d, interval = "confidence", level = 0.95)
kbuf_t20_spline_m <- lm(kbuf_avgTemp20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_t20_ci_m <- predict(kbuf_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kbuf_dp20_spline_d <- lm(kbuf_avgDPTemp20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_dp20_ci_d <- predict(kbuf_dp20_spline_d, interval = "confidence", level = 0.95)
kbuf_dp20_spline_10d <- lm(kbuf_avgDPTemp20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_dp20_ci_10d <- predict(kbuf_dp20_spline_10d, interval = "confidence", level = 0.95)
kbuf_dp20_spline_m <- lm(kbuf_avgDPTemp20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_dp20_ci_m <- predict(kbuf_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kbuf_wd20_spline_d <- lm(kbuf_avgWDIR20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_wd20_ci_d <- predict(kbuf_wd20_spline_d, interval = "confidence", level = 0.95)
kbuf_wd20_spline_10d <- lm(kbuf_avgWDIR20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_wd20_ci_10d <- predict(kbuf_wd20_spline_10d, interval = "confidence", level = 0.95)
kbuf_wd20_spline_m <- lm(kbuf_avgWDIR20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_wd20_ci_m <- predict(kbuf_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kbuf_ws20_spline_d <- lm(kbuf_avgWSPD20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_ws20_ci_d <- predict(kbuf_ws20_spline_d, interval = "confidence", level = 0.95)
kbuf_ws20_spline_10d <- lm(kbuf_avgWSPD20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_ws20_ci_10d <- predict(kbuf_ws20_spline_10d, interval = "confidence", level = 0.95)
kbuf_ws20_spline_m <- lm(kbuf_avgWSPD20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_ws20_ci_m <- predict(kbuf_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kbuf_wg20_spline_d <- lm(kbuf_avgWGSP20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_wg20_ci_d <- predict(kbuf_wg20_spline_d, interval = "confidence", level = 0.95)
kbuf_wg20_spline_10d <- lm(kbuf_avgWGSP20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_wg20_ci_10d <- predict(kbuf_wg20_spline_10d, interval = "confidence", level = 0.95)
kbuf_wg20_spline_m <- lm(kbuf_avgWGSP20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_wg20_ci_m <- predict(kbuf_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kbuf_cig20_spline_d <- lm(kbuf_avgCCIG20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_cig20_ci_d <- predict(kbuf_cig20_spline_d, interval = "confidence", level = 0.95)
kbuf_cig20_spline_10d <- lm(kbuf_avgCCIG20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_cig20_ci_10d <- predict(kbuf_cig20_spline_10d, interval = "confidence", level = 0.95)
kbuf_cig20_spline_m <- lm(kbuf_avgCCIG20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_cig20_ci_m <- predict(kbuf_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kbuf_vis20_spline_d <- lm(kbuf_avgVIS20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_vis20_ci_d <- predict(kbuf_vis20_spline_d, interval = "confidence", level = 0.95)
kbuf_vis20_spline_10d <- lm(kbuf_avgVIS20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_vis20_ci_10d <- predict(kbuf_vis20_spline_10d, interval = "confidence", level = 0.95)
kbuf_vis20_spline_m <- lm(kbuf_avgVIS20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_vis20_ci_m <- predict(kbuf_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kbuf_cc20_spline_d <- lm(kbuf_avgCCOV20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_cc20_ci_d <- predict(kbuf_cc20_spline_d, interval = "confidence", level = 0.95)
kbuf_cc20_spline_10d <- lm(kbuf_avgCCOV20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_cc20_ci_10d <- predict(kbuf_cc20_spline_10d, interval = "confidence", level = 0.95)
kbuf_cc20_spline_m <- lm(kbuf_avgCCOV20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_cc20_ci_m <- predict(kbuf_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kbuf_alt20_spline_d <- lm(kbuf_avgALTS20~ns(c(1:366), df=364), kbuf_avgArray20)
kbuf_alt20_ci_d <- predict(kbuf_alt20_spline_d, interval = "confidence", level = 0.95)
kbuf_alt20_spline_10d <- lm(kbuf_avgALTS20~ns(c(1:366), df=35), kbuf_avgArray20)
kbuf_alt20_ci_10d <- predict(kbuf_alt20_spline_10d, interval = "confidence", level = 0.95)
kbuf_alt20_spline_m <- lm(kbuf_avgALTS20~ns(c(1:366), df=11), kbuf_avgArray20)
kbuf_alt20_ci_m <- predict(kbuf_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
kbuf_t30_spline_d <- lm(kbuf_avgTemp30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_t30_ci_d <- predict(kbuf_t30_spline_d, interval = "confidence", level = 0.95)
kbuf_t30_spline_10d <- lm(kbuf_avgTemp30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_t30_ci_10d <- predict(kbuf_t30_spline_10d, interval = "confidence", level = 0.95)
kbuf_t30_spline_m <- lm(kbuf_avgTemp30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_t30_ci_m <- predict(kbuf_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kbuf_dp30_spline_d <- lm(kbuf_avgDPTemp30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_dp30_ci_d <- predict(kbuf_dp30_spline_d, interval = "confidence", level = 0.95)
kbuf_dp30_spline_10d <- lm(kbuf_avgDPTemp30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_dp30_ci_10d <- predict(kbuf_dp30_spline_10d, interval = "confidence", level = 0.95)
kbuf_dp30_spline_m <- lm(kbuf_avgDPTemp30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_dp30_ci_m <- predict(kbuf_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kbuf_wd30_spline_d <- lm(kbuf_avgWDIR30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_wd30_ci_d <- predict(kbuf_wd30_spline_d, interval = "confidence", level = 0.95)
kbuf_wd30_spline_10d <- lm(kbuf_avgWDIR30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_wd30_ci_10d <- predict(kbuf_wd30_spline_10d, interval = "confidence", level = 0.95)
kbuf_wd30_spline_m <- lm(kbuf_avgWDIR30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_wd30_ci_m <- predict(kbuf_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kbuf_ws30_spline_d <- lm(kbuf_avgWSPD30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_ws30_ci_d <- predict(kbuf_ws30_spline_d, interval = "confidence", level = 0.95)
kbuf_ws30_spline_10d <- lm(kbuf_avgWSPD30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_ws30_ci_10d <- predict(kbuf_ws30_spline_10d, interval = "confidence", level = 0.95)
kbuf_ws30_spline_m <- lm(kbuf_avgWSPD30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_ws30_ci_m <- predict(kbuf_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kbuf_wg30_spline_d <- lm(kbuf_avgWGSP30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_wg30_ci_d <- predict(kbuf_wg30_spline_d, interval = "confidence", level = 0.95)
kbuf_wg30_spline_10d <- lm(kbuf_avgWGSP30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_wg30_ci_10d <- predict(kbuf_wg30_spline_10d, interval = "confidence", level = 0.95)
kbuf_wg30_spline_m <- lm(kbuf_avgWGSP30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_wg30_ci_m <- predict(kbuf_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kbuf_cig30_spline_d <- lm(kbuf_avgCCIG30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_cig30_ci_d <- predict(kbuf_cig30_spline_d, interval = "confidence", level = 0.95)
kbuf_cig30_spline_10d <- lm(kbuf_avgCCIG30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_cig30_ci_10d <- predict(kbuf_cig30_spline_10d, interval = "confidence", level = 0.95)
kbuf_cig30_spline_m <- lm(kbuf_avgCCIG30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_cig30_ci_m <- predict(kbuf_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kbuf_vis30_spline_d <- lm(kbuf_avgVIS30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_vis30_ci_d <- predict(kbuf_vis30_spline_d, interval = "confidence", level = 0.95)
kbuf_vis30_spline_10d <- lm(kbuf_avgVIS30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_vis30_ci_10d <- predict(kbuf_vis30_spline_10d, interval = "confidence", level = 0.95)
kbuf_vis30_spline_m <- lm(kbuf_avgVIS30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_vis30_ci_m <- predict(kbuf_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kbuf_cc30_spline_d <- lm(kbuf_avgCCOV30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_cc30_ci_d <- predict(kbuf_cc30_spline_d, interval = "confidence", level = 0.95)
kbuf_cc30_spline_10d <- lm(kbuf_avgCCOV30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_cc30_ci_10d <- predict(kbuf_cc30_spline_10d, interval = "confidence", level = 0.95)
kbuf_cc30_spline_m <- lm(kbuf_avgCCOV30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_cc30_ci_m <- predict(kbuf_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kbuf_alt30_spline_d <- lm(kbuf_avgALTS30~ns(c(1:366), df=364), kbuf_avgArray30)
kbuf_alt30_ci_d <- predict(kbuf_alt30_spline_d, interval = "confidence", level = 0.95)
kbuf_alt30_spline_10d <- lm(kbuf_avgALTS30~ns(c(1:366), df=35), kbuf_avgArray30)
kbuf_alt30_ci_10d <- predict(kbuf_alt30_spline_10d, interval = "confidence", level = 0.95)
kbuf_alt30_spline_m <- lm(kbuf_avgALTS30~ns(c(1:366), df=11), kbuf_avgArray30)
kbuf_alt30_ci_m <- predict(kbuf_alt30_spline_m, interval = "confidence", level = 0.95)



## KFOE
##### 10 Year Splines
##Temperature
kfoe_t10_spline_d <- lm(kfoe_avgTemp10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_t10_ci_d <- predict(kfoe_t10_spline_d, interval = "confidence", level = 0.95)
kfoe_t10_spline_10d <- lm(kfoe_avgTemp10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_t10_ci_10d <- predict(kfoe_t10_spline_10d, interval = "confidence", level = 0.95)
kfoe_t10_spline_m <- lm(kfoe_avgTemp10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_t10_ci_m <- predict(kfoe_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kfoe_dp10_spline_d <- lm(kfoe_avgDPTemp10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_dp10_ci_d <- predict(kfoe_dp10_spline_d, interval = "confidence", level = 0.95)
kfoe_dp10_spline_10d <- lm(kfoe_avgDPTemp10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_dp10_ci_10d <- predict(kfoe_dp10_spline_10d, interval = "confidence", level = 0.95)
kfoe_dp10_spline_m <- lm(kfoe_avgDPTemp10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_dp10_ci_m <- predict(kfoe_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kfoe_wd10_spline_d <- lm(kfoe_avgWDIR10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_wd10_ci_d <- predict(kfoe_wd10_spline_d, interval = "confidence", level = 0.95)
kfoe_wd10_spline_10d <- lm(kfoe_avgWDIR10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_wd10_ci_10d <- predict(kfoe_wd10_spline_10d, interval = "confidence", level = 0.95)
kfoe_wd10_spline_m <- lm(kfoe_avgWDIR10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_wd10_ci_m <- predict(kfoe_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kfoe_ws10_spline_d <- lm(kfoe_avgWSPD10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_ws10_ci_d <- predict(kfoe_ws10_spline_d, interval = "confidence", level = 0.95)
kfoe_ws10_spline_10d <- lm(kfoe_avgWSPD10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_ws10_ci_10d <- predict(kfoe_ws10_spline_10d, interval = "confidence", level = 0.95)
kfoe_ws10_spline_m <- lm(kfoe_avgWSPD10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_ws10_ci_m <- predict(kfoe_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kfoe_wg10_spline_d <- lm(kfoe_avgWGSP10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_wg10_ci_d <- predict(kfoe_wg10_spline_d, interval = "confidence", level = 0.95)
kfoe_wg10_spline_10d <- lm(kfoe_avgWGSP10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_wg10_ci_10d <- predict(kfoe_wg10_spline_10d, interval = "confidence", level = 0.95)
kfoe_wg10_spline_m <- lm(kfoe_avgWGSP10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_wg10_ci_m <- predict(kfoe_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kfoe_cig10_spline_d <- lm(kfoe_avgCCIG10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_cig10_ci_d <- predict(kfoe_cig10_spline_d, interval = "confidence", level = 0.95)
kfoe_cig10_spline_10d <- lm(kfoe_avgCCIG10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_cig10_ci_10d <- predict(kfoe_cig10_spline_10d, interval = "confidence", level = 0.95)
kfoe_cig10_spline_m <- lm(kfoe_avgCCIG10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_cig10_ci_m <- predict(kfoe_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kfoe_vis10_spline_d <- lm(kfoe_avgVIS10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_vis10_ci_d <- predict(kfoe_vis10_spline_d, interval = "confidence", level = 0.95)
kfoe_vis10_spline_10d <- lm(kfoe_avgVIS10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_vis10_ci_10d <- predict(kfoe_vis10_spline_10d, interval = "confidence", level = 0.95)
kfoe_vis10_spline_m <- lm(kfoe_avgVIS10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_vis10_ci_m <- predict(kfoe_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kfoe_cc10_spline_d <- lm(kfoe_avgCCOV10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_cc10_ci_d <- predict(kfoe_cc10_spline_d, interval = "confidence", level = 0.95)
kfoe_cc10_spline_10d <- lm(kfoe_avgCCOV10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_cc10_ci_10d <- predict(kfoe_cc10_spline_10d, interval = "confidence", level = 0.95)
kfoe_cc10_spline_m <- lm(kfoe_avgCCOV10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_cc10_ci_m <- predict(kfoe_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kfoe_alt10_spline_d <- lm(kfoe_avgALTS10~ns(c(1:366), df=364), kfoe_avgArray10)
kfoe_alt10_ci_d <- predict(kfoe_alt10_spline_d, interval = "confidence", level = 0.95)
kfoe_alt10_spline_10d <- lm(kfoe_avgALTS10~ns(c(1:366), df=35), kfoe_avgArray10)
kfoe_alt10_ci_10d <- predict(kfoe_alt10_spline_10d, interval = "confidence", level = 0.95)
kfoe_alt10_spline_m <- lm(kfoe_avgALTS10~ns(c(1:366), df=11), kfoe_avgArray10)
kfoe_alt10_ci_m <- predict(kfoe_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
kfoe_t20_spline_d <- lm(kfoe_avgTemp20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_t20_ci_d <- predict(kfoe_t20_spline_d, interval = "confidence", level = 0.95)
kfoe_t20_spline_10d <- lm(kfoe_avgTemp20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_t20_ci_10d <- predict(kfoe_t20_spline_10d, interval = "confidence", level = 0.95)
kfoe_t20_spline_m <- lm(kfoe_avgTemp20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_t20_ci_m <- predict(kfoe_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kfoe_dp20_spline_d <- lm(kfoe_avgDPTemp20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_dp20_ci_d <- predict(kfoe_dp20_spline_d, interval = "confidence", level = 0.95)
kfoe_dp20_spline_10d <- lm(kfoe_avgDPTemp20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_dp20_ci_10d <- predict(kfoe_dp20_spline_10d, interval = "confidence", level = 0.95)
kfoe_dp20_spline_m <- lm(kfoe_avgDPTemp20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_dp20_ci_m <- predict(kfoe_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kfoe_wd20_spline_d <- lm(kfoe_avgWDIR20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_wd20_ci_d <- predict(kfoe_wd20_spline_d, interval = "confidence", level = 0.95)
kfoe_wd20_spline_10d <- lm(kfoe_avgWDIR20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_wd20_ci_10d <- predict(kfoe_wd20_spline_10d, interval = "confidence", level = 0.95)
kfoe_wd20_spline_m <- lm(kfoe_avgWDIR20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_wd20_ci_m <- predict(kfoe_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kfoe_ws20_spline_d <- lm(kfoe_avgWSPD20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_ws20_ci_d <- predict(kfoe_ws20_spline_d, interval = "confidence", level = 0.95)
kfoe_ws20_spline_10d <- lm(kfoe_avgWSPD20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_ws20_ci_10d <- predict(kfoe_ws20_spline_10d, interval = "confidence", level = 0.95)
kfoe_ws20_spline_m <- lm(kfoe_avgWSPD20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_ws20_ci_m <- predict(kfoe_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kfoe_wg20_spline_d <- lm(kfoe_avgWGSP20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_wg20_ci_d <- predict(kfoe_wg20_spline_d, interval = "confidence", level = 0.95)
kfoe_wg20_spline_10d <- lm(kfoe_avgWGSP20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_wg20_ci_10d <- predict(kfoe_wg20_spline_10d, interval = "confidence", level = 0.95)
kfoe_wg20_spline_m <- lm(kfoe_avgWGSP20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_wg20_ci_m <- predict(kfoe_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kfoe_cig20_spline_d <- lm(kfoe_avgCCIG20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_cig20_ci_d <- predict(kfoe_cig20_spline_d, interval = "confidence", level = 0.95)
kfoe_cig20_spline_10d <- lm(kfoe_avgCCIG20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_cig20_ci_10d <- predict(kfoe_cig20_spline_10d, interval = "confidence", level = 0.95)
kfoe_cig20_spline_m <- lm(kfoe_avgCCIG20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_cig20_ci_m <- predict(kfoe_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kfoe_vis20_spline_d <- lm(kfoe_avgVIS20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_vis20_ci_d <- predict(kfoe_vis20_spline_d, interval = "confidence", level = 0.95)
kfoe_vis20_spline_10d <- lm(kfoe_avgVIS20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_vis20_ci_10d <- predict(kfoe_vis20_spline_10d, interval = "confidence", level = 0.95)
kfoe_vis20_spline_m <- lm(kfoe_avgVIS20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_vis20_ci_m <- predict(kfoe_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kfoe_cc20_spline_d <- lm(kfoe_avgCCOV20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_cc20_ci_d <- predict(kfoe_cc20_spline_d, interval = "confidence", level = 0.95)
kfoe_cc20_spline_10d <- lm(kfoe_avgCCOV20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_cc20_ci_10d <- predict(kfoe_cc20_spline_10d, interval = "confidence", level = 0.95)
kfoe_cc20_spline_m <- lm(kfoe_avgCCOV20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_cc20_ci_m <- predict(kfoe_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kfoe_alt20_spline_d <- lm(kfoe_avgALTS20~ns(c(1:366), df=364), kfoe_avgArray20)
kfoe_alt20_ci_d <- predict(kfoe_alt20_spline_d, interval = "confidence", level = 0.95)
kfoe_alt20_spline_10d <- lm(kfoe_avgALTS20~ns(c(1:366), df=35), kfoe_avgArray20)
kfoe_alt20_ci_10d <- predict(kfoe_alt20_spline_10d, interval = "confidence", level = 0.95)
kfoe_alt20_spline_m <- lm(kfoe_avgALTS20~ns(c(1:366), df=11), kfoe_avgArray20)
kfoe_alt20_ci_m <- predict(kfoe_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
kfoe_t30_spline_d <- lm(kfoe_avgTemp30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_t30_ci_d <- predict(kfoe_t30_spline_d, interval = "confidence", level = 0.95)
kfoe_t30_spline_10d <- lm(kfoe_avgTemp30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_t30_ci_10d <- predict(kfoe_t30_spline_10d, interval = "confidence", level = 0.95)
kfoe_t30_spline_m <- lm(kfoe_avgTemp30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_t30_ci_m <- predict(kfoe_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kfoe_dp30_spline_d <- lm(kfoe_avgDPTemp30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_dp30_ci_d <- predict(kfoe_dp30_spline_d, interval = "confidence", level = 0.95)
kfoe_dp30_spline_10d <- lm(kfoe_avgDPTemp30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_dp30_ci_10d <- predict(kfoe_dp30_spline_10d, interval = "confidence", level = 0.95)
kfoe_dp30_spline_m <- lm(kfoe_avgDPTemp30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_dp30_ci_m <- predict(kfoe_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kfoe_wd30_spline_d <- lm(kfoe_avgWDIR30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_wd30_ci_d <- predict(kfoe_wd30_spline_d, interval = "confidence", level = 0.95)
kfoe_wd30_spline_10d <- lm(kfoe_avgWDIR30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_wd30_ci_10d <- predict(kfoe_wd30_spline_10d, interval = "confidence", level = 0.95)
kfoe_wd30_spline_m <- lm(kfoe_avgWDIR30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_wd30_ci_m <- predict(kfoe_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kfoe_ws30_spline_d <- lm(kfoe_avgWSPD30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_ws30_ci_d <- predict(kfoe_ws30_spline_d, interval = "confidence", level = 0.95)
kfoe_ws30_spline_10d <- lm(kfoe_avgWSPD30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_ws30_ci_10d <- predict(kfoe_ws30_spline_10d, interval = "confidence", level = 0.95)
kfoe_ws30_spline_m <- lm(kfoe_avgWSPD30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_ws30_ci_m <- predict(kfoe_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kfoe_wg30_spline_d <- lm(kfoe_avgWGSP30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_wg30_ci_d <- predict(kfoe_wg30_spline_d, interval = "confidence", level = 0.95)
kfoe_wg30_spline_10d <- lm(kfoe_avgWGSP30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_wg30_ci_10d <- predict(kfoe_wg30_spline_10d, interval = "confidence", level = 0.95)
kfoe_wg30_spline_m <- lm(kfoe_avgWGSP30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_wg30_ci_m <- predict(kfoe_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kfoe_cig30_spline_d <- lm(kfoe_avgCCIG30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_cig30_ci_d <- predict(kfoe_cig30_spline_d, interval = "confidence", level = 0.95)
kfoe_cig30_spline_10d <- lm(kfoe_avgCCIG30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_cig30_ci_10d <- predict(kfoe_cig30_spline_10d, interval = "confidence", level = 0.95)
kfoe_cig30_spline_m <- lm(kfoe_avgCCIG30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_cig30_ci_m <- predict(kfoe_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kfoe_vis30_spline_d <- lm(kfoe_avgVIS30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_vis30_ci_d <- predict(kfoe_vis30_spline_d, interval = "confidence", level = 0.95)
kfoe_vis30_spline_10d <- lm(kfoe_avgVIS30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_vis30_ci_10d <- predict(kfoe_vis30_spline_10d, interval = "confidence", level = 0.95)
kfoe_vis30_spline_m <- lm(kfoe_avgVIS30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_vis30_ci_m <- predict(kfoe_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kfoe_cc30_spline_d <- lm(kfoe_avgCCOV30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_cc30_ci_d <- predict(kfoe_cc30_spline_d, interval = "confidence", level = 0.95)
kfoe_cc30_spline_10d <- lm(kfoe_avgCCOV30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_cc30_ci_10d <- predict(kfoe_cc30_spline_10d, interval = "confidence", level = 0.95)
kfoe_cc30_spline_m <- lm(kfoe_avgCCOV30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_cc30_ci_m <- predict(kfoe_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kfoe_alt30_spline_d <- lm(kfoe_avgALTS30~ns(c(1:366), df=364), kfoe_avgArray30)
kfoe_alt30_ci_d <- predict(kfoe_alt30_spline_d, interval = "confidence", level = 0.95)
kfoe_alt30_spline_10d <- lm(kfoe_avgALTS30~ns(c(1:366), df=35), kfoe_avgArray30)
kfoe_alt30_ci_10d <- predict(kfoe_alt30_spline_10d, interval = "confidence", level = 0.95)
kfoe_alt30_spline_m <- lm(kfoe_avgALTS30~ns(c(1:366), df=11), kfoe_avgArray30)
kfoe_alt30_ci_m <- predict(kfoe_alt30_spline_m, interval = "confidence", level = 0.95)


## KMSN
##### 10 Year Splines
##Temperature
kmsn_t10_spline_d <- lm(kmsn_avgTemp10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_t10_ci_d <- predict(kmsn_t10_spline_d, interval = "confidence", level = 0.95)
kmsn_t10_spline_10d <- lm(kmsn_avgTemp10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_t10_ci_10d <- predict(kmsn_t10_spline_10d, interval = "confidence", level = 0.95)
kmsn_t10_spline_m <- lm(kmsn_avgTemp10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_t10_ci_m <- predict(kmsn_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kmsn_dp10_spline_d <- lm(kmsn_avgDPTemp10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_dp10_ci_d <- predict(kmsn_dp10_spline_d, interval = "confidence", level = 0.95)
kmsn_dp10_spline_10d <- lm(kmsn_avgDPTemp10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_dp10_ci_10d <- predict(kmsn_dp10_spline_10d, interval = "confidence", level = 0.95)
kmsn_dp10_spline_m <- lm(kmsn_avgDPTemp10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_dp10_ci_m <- predict(kmsn_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kmsn_wd10_spline_d <- lm(kmsn_avgWDIR10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_wd10_ci_d <- predict(kmsn_wd10_spline_d, interval = "confidence", level = 0.95)
kmsn_wd10_spline_10d <- lm(kmsn_avgWDIR10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_wd10_ci_10d <- predict(kmsn_wd10_spline_10d, interval = "confidence", level = 0.95)
kmsn_wd10_spline_m <- lm(kmsn_avgWDIR10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_wd10_ci_m <- predict(kmsn_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kmsn_ws10_spline_d <- lm(kmsn_avgWSPD10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_ws10_ci_d <- predict(kmsn_ws10_spline_d, interval = "confidence", level = 0.95)
kmsn_ws10_spline_10d <- lm(kmsn_avgWSPD10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_ws10_ci_10d <- predict(kmsn_ws10_spline_10d, interval = "confidence", level = 0.95)
kmsn_ws10_spline_m <- lm(kmsn_avgWSPD10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_ws10_ci_m <- predict(kmsn_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kmsn_wg10_spline_d <- lm(kmsn_avgWGSP10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_wg10_ci_d <- predict(kmsn_wg10_spline_d, interval = "confidence", level = 0.95)
kmsn_wg10_spline_10d <- lm(kmsn_avgWGSP10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_wg10_ci_10d <- predict(kmsn_wg10_spline_10d, interval = "confidence", level = 0.95)
kmsn_wg10_spline_m <- lm(kmsn_avgWGSP10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_wg10_ci_m <- predict(kmsn_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kmsn_cig10_spline_d <- lm(kmsn_avgCCIG10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_cig10_ci_d <- predict(kmsn_cig10_spline_d, interval = "confidence", level = 0.95)
kmsn_cig10_spline_10d <- lm(kmsn_avgCCIG10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_cig10_ci_10d <- predict(kmsn_cig10_spline_10d, interval = "confidence", level = 0.95)
kmsn_cig10_spline_m <- lm(kmsn_avgCCIG10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_cig10_ci_m <- predict(kmsn_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kmsn_vis10_spline_d <- lm(kmsn_avgVIS10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_vis10_ci_d <- predict(kmsn_vis10_spline_d, interval = "confidence", level = 0.95)
kmsn_vis10_spline_10d <- lm(kmsn_avgVIS10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_vis10_ci_10d <- predict(kmsn_vis10_spline_10d, interval = "confidence", level = 0.95)
kmsn_vis10_spline_m <- lm(kmsn_avgVIS10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_vis10_ci_m <- predict(kmsn_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kmsn_cc10_spline_d <- lm(kmsn_avgCCOV10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_cc10_ci_d <- predict(kmsn_cc10_spline_d, interval = "confidence", level = 0.95)
kmsn_cc10_spline_10d <- lm(kmsn_avgCCOV10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_cc10_ci_10d <- predict(kmsn_cc10_spline_10d, interval = "confidence", level = 0.95)
kmsn_cc10_spline_m <- lm(kmsn_avgCCOV10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_cc10_ci_m <- predict(kmsn_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kmsn_alt10_spline_d <- lm(kmsn_avgALTS10~ns(c(1:366), df=364), kmsn_avgArray10)
kmsn_alt10_ci_d <- predict(kmsn_alt10_spline_d, interval = "confidence", level = 0.95)
kmsn_alt10_spline_10d <- lm(kmsn_avgALTS10~ns(c(1:366), df=35), kmsn_avgArray10)
kmsn_alt10_ci_10d <- predict(kmsn_alt10_spline_10d, interval = "confidence", level = 0.95)
kmsn_alt10_spline_m <- lm(kmsn_avgALTS10~ns(c(1:366), df=11), kmsn_avgArray10)
kmsn_alt10_ci_m <- predict(kmsn_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
kmsn_t20_spline_d <- lm(kmsn_avgTemp20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_t20_ci_d <- predict(kmsn_t20_spline_d, interval = "confidence", level = 0.95)
kmsn_t20_spline_10d <- lm(kmsn_avgTemp20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_t20_ci_10d <- predict(kmsn_t20_spline_10d, interval = "confidence", level = 0.95)
kmsn_t20_spline_m <- lm(kmsn_avgTemp20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_t20_ci_m <- predict(kmsn_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kmsn_dp20_spline_d <- lm(kmsn_avgDPTemp20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_dp20_ci_d <- predict(kmsn_dp20_spline_d, interval = "confidence", level = 0.95)
kmsn_dp20_spline_10d <- lm(kmsn_avgDPTemp20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_dp20_ci_10d <- predict(kmsn_dp20_spline_10d, interval = "confidence", level = 0.95)
kmsn_dp20_spline_m <- lm(kmsn_avgDPTemp20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_dp20_ci_m <- predict(kmsn_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kmsn_wd20_spline_d <- lm(kmsn_avgWDIR20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_wd20_ci_d <- predict(kmsn_wd20_spline_d, interval = "confidence", level = 0.95)
kmsn_wd20_spline_10d <- lm(kmsn_avgWDIR20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_wd20_ci_10d <- predict(kmsn_wd20_spline_10d, interval = "confidence", level = 0.95)
kmsn_wd20_spline_m <- lm(kmsn_avgWDIR20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_wd20_ci_m <- predict(kmsn_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kmsn_ws20_spline_d <- lm(kmsn_avgWSPD20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_ws20_ci_d <- predict(kmsn_ws20_spline_d, interval = "confidence", level = 0.95)
kmsn_ws20_spline_10d <- lm(kmsn_avgWSPD20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_ws20_ci_10d <- predict(kmsn_ws20_spline_10d, interval = "confidence", level = 0.95)
kmsn_ws20_spline_m <- lm(kmsn_avgWSPD20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_ws20_ci_m <- predict(kmsn_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kmsn_wg20_spline_d <- lm(kmsn_avgWGSP20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_wg20_ci_d <- predict(kmsn_wg20_spline_d, interval = "confidence", level = 0.95)
kmsn_wg20_spline_10d <- lm(kmsn_avgWGSP20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_wg20_ci_10d <- predict(kmsn_wg20_spline_10d, interval = "confidence", level = 0.95)
kmsn_wg20_spline_m <- lm(kmsn_avgWGSP20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_wg20_ci_m <- predict(kmsn_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kmsn_cig20_spline_d <- lm(kmsn_avgCCIG20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_cig20_ci_d <- predict(kmsn_cig20_spline_d, interval = "confidence", level = 0.95)
kmsn_cig20_spline_10d <- lm(kmsn_avgCCIG20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_cig20_ci_10d <- predict(kmsn_cig20_spline_10d, interval = "confidence", level = 0.95)
kmsn_cig20_spline_m <- lm(kmsn_avgCCIG20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_cig20_ci_m <- predict(kmsn_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kmsn_vis20_spline_d <- lm(kmsn_avgVIS20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_vis20_ci_d <- predict(kmsn_vis20_spline_d, interval = "confidence", level = 0.95)
kmsn_vis20_spline_10d <- lm(kmsn_avgVIS20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_vis20_ci_10d <- predict(kmsn_vis20_spline_10d, interval = "confidence", level = 0.95)
kmsn_vis20_spline_m <- lm(kmsn_avgVIS20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_vis20_ci_m <- predict(kmsn_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kmsn_cc20_spline_d <- lm(kmsn_avgCCOV20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_cc20_ci_d <- predict(kmsn_cc20_spline_d, interval = "confidence", level = 0.95)
kmsn_cc20_spline_10d <- lm(kmsn_avgCCOV20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_cc20_ci_10d <- predict(kmsn_cc20_spline_10d, interval = "confidence", level = 0.95)
kmsn_cc20_spline_m <- lm(kmsn_avgCCOV20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_cc20_ci_m <- predict(kmsn_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kmsn_alt20_spline_d <- lm(kmsn_avgALTS20~ns(c(1:366), df=364), kmsn_avgArray20)
kmsn_alt20_ci_d <- predict(kmsn_alt20_spline_d, interval = "confidence", level = 0.95)
kmsn_alt20_spline_10d <- lm(kmsn_avgALTS20~ns(c(1:366), df=35), kmsn_avgArray20)
kmsn_alt20_ci_10d <- predict(kmsn_alt20_spline_10d, interval = "confidence", level = 0.95)
kmsn_alt20_spline_m <- lm(kmsn_avgALTS20~ns(c(1:366), df=11), kmsn_avgArray20)
kmsn_alt20_ci_m <- predict(kmsn_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
kmsn_t30_spline_d <- lm(kmsn_avgTemp30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_t30_ci_d <- predict(kmsn_t30_spline_d, interval = "confidence", level = 0.95)
kmsn_t30_spline_10d <- lm(kmsn_avgTemp30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_t30_ci_10d <- predict(kmsn_t30_spline_10d, interval = "confidence", level = 0.95)
kmsn_t30_spline_m <- lm(kmsn_avgTemp30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_t30_ci_m <- predict(kmsn_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kmsn_dp30_spline_d <- lm(kmsn_avgDPTemp30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_dp30_ci_d <- predict(kmsn_dp30_spline_d, interval = "confidence", level = 0.95)
kmsn_dp30_spline_10d <- lm(kmsn_avgDPTemp30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_dp30_ci_10d <- predict(kmsn_dp30_spline_10d, interval = "confidence", level = 0.95)
kmsn_dp30_spline_m <- lm(kmsn_avgDPTemp30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_dp30_ci_m <- predict(kmsn_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kmsn_wd30_spline_d <- lm(kmsn_avgWDIR30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_wd30_ci_d <- predict(kmsn_wd30_spline_d, interval = "confidence", level = 0.95)
kmsn_wd30_spline_10d <- lm(kmsn_avgWDIR30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_wd30_ci_10d <- predict(kmsn_wd30_spline_10d, interval = "confidence", level = 0.95)
kmsn_wd30_spline_m <- lm(kmsn_avgWDIR30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_wd30_ci_m <- predict(kmsn_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kmsn_ws30_spline_d <- lm(kmsn_avgWSPD30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_ws30_ci_d <- predict(kmsn_ws30_spline_d, interval = "confidence", level = 0.95)
kmsn_ws30_spline_10d <- lm(kmsn_avgWSPD30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_ws30_ci_10d <- predict(kmsn_ws30_spline_10d, interval = "confidence", level = 0.95)
kmsn_ws30_spline_m <- lm(kmsn_avgWSPD30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_ws30_ci_m <- predict(kmsn_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kmsn_wg30_spline_d <- lm(kmsn_avgWGSP30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_wg30_ci_d <- predict(kmsn_wg30_spline_d, interval = "confidence", level = 0.95)
kmsn_wg30_spline_10d <- lm(kmsn_avgWGSP30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_wg30_ci_10d <- predict(kmsn_wg30_spline_10d, interval = "confidence", level = 0.95)
kmsn_wg30_spline_m <- lm(kmsn_avgWGSP30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_wg30_ci_m <- predict(kmsn_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kmsn_cig30_spline_d <- lm(kmsn_avgCCIG30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_cig30_ci_d <- predict(kmsn_cig30_spline_d, interval = "confidence", level = 0.95)
kmsn_cig30_spline_10d <- lm(kmsn_avgCCIG30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_cig30_ci_10d <- predict(kmsn_cig30_spline_10d, interval = "confidence", level = 0.95)
kmsn_cig30_spline_m <- lm(kmsn_avgCCIG30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_cig30_ci_m <- predict(kmsn_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kmsn_vis30_spline_d <- lm(kmsn_avgVIS30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_vis30_ci_d <- predict(kmsn_vis30_spline_d, interval = "confidence", level = 0.95)
kmsn_vis30_spline_10d <- lm(kmsn_avgVIS30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_vis30_ci_10d <- predict(kmsn_vis30_spline_10d, interval = "confidence", level = 0.95)
kmsn_vis30_spline_m <- lm(kmsn_avgVIS30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_vis30_ci_m <- predict(kmsn_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kmsn_cc30_spline_d <- lm(kmsn_avgCCOV30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_cc30_ci_d <- predict(kmsn_cc30_spline_d, interval = "confidence", level = 0.95)
kmsn_cc30_spline_10d <- lm(kmsn_avgCCOV30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_cc30_ci_10d <- predict(kmsn_cc30_spline_10d, interval = "confidence", level = 0.95)
kmsn_cc30_spline_m <- lm(kmsn_avgCCOV30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_cc30_ci_m <- predict(kmsn_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kmsn_alt30_spline_d <- lm(kmsn_avgALTS30~ns(c(1:366), df=364), kmsn_avgArray30)
kmsn_alt30_ci_d <- predict(kmsn_alt30_spline_d, interval = "confidence", level = 0.95)
kmsn_alt30_spline_10d <- lm(kmsn_avgALTS30~ns(c(1:366), df=35), kmsn_avgArray30)
kmsn_alt30_ci_10d <- predict(kmsn_alt30_spline_10d, interval = "confidence", level = 0.95)
kmsn_alt30_spline_m <- lm(kmsn_avgALTS30~ns(c(1:366), df=11), kmsn_avgArray30)
kmsn_alt30_ci_m <- predict(kmsn_alt30_spline_m, interval = "confidence", level = 0.95)




## KTRI
##### 10 Year Splines
##Temperature
ktri_t10_spline_d <- lm(ktri_avgTemp10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_t10_ci_d <- predict(ktri_t10_spline_d, interval = "confidence", level = 0.95)
ktri_t10_spline_10d <- lm(ktri_avgTemp10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_t10_ci_10d <- predict(ktri_t10_spline_10d, interval = "confidence", level = 0.95)
ktri_t10_spline_m <- lm(ktri_avgTemp10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_t10_ci_m <- predict(ktri_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
ktri_dp10_spline_d <- lm(ktri_avgDPTemp10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_dp10_ci_d <- predict(ktri_dp10_spline_d, interval = "confidence", level = 0.95)
ktri_dp10_spline_10d <- lm(ktri_avgDPTemp10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_dp10_ci_10d <- predict(ktri_dp10_spline_10d, interval = "confidence", level = 0.95)
ktri_dp10_spline_m <- lm(ktri_avgDPTemp10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_dp10_ci_m <- predict(ktri_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
ktri_wd10_spline_d <- lm(ktri_avgWDIR10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_wd10_ci_d <- predict(ktri_wd10_spline_d, interval = "confidence", level = 0.95)
ktri_wd10_spline_10d <- lm(ktri_avgWDIR10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_wd10_ci_10d <- predict(ktri_wd10_spline_10d, interval = "confidence", level = 0.95)
ktri_wd10_spline_m <- lm(ktri_avgWDIR10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_wd10_ci_m <- predict(ktri_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
ktri_ws10_spline_d <- lm(ktri_avgWSPD10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_ws10_ci_d <- predict(ktri_ws10_spline_d, interval = "confidence", level = 0.95)
ktri_ws10_spline_10d <- lm(ktri_avgWSPD10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_ws10_ci_10d <- predict(ktri_ws10_spline_10d, interval = "confidence", level = 0.95)
ktri_ws10_spline_m <- lm(ktri_avgWSPD10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_ws10_ci_m <- predict(ktri_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
ktri_wg10_spline_d <- lm(ktri_avgWGSP10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_wg10_ci_d <- predict(ktri_wg10_spline_d, interval = "confidence", level = 0.95)
ktri_wg10_spline_10d <- lm(ktri_avgWGSP10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_wg10_ci_10d <- predict(ktri_wg10_spline_10d, interval = "confidence", level = 0.95)
ktri_wg10_spline_m <- lm(ktri_avgWGSP10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_wg10_ci_m <- predict(ktri_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
ktri_cig10_spline_d <- lm(ktri_avgCCIG10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_cig10_ci_d <- predict(ktri_cig10_spline_d, interval = "confidence", level = 0.95)
ktri_cig10_spline_10d <- lm(ktri_avgCCIG10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_cig10_ci_10d <- predict(ktri_cig10_spline_10d, interval = "confidence", level = 0.95)
ktri_cig10_spline_m <- lm(ktri_avgCCIG10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_cig10_ci_m <- predict(ktri_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
ktri_vis10_spline_d <- lm(ktri_avgVIS10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_vis10_ci_d <- predict(ktri_vis10_spline_d, interval = "confidence", level = 0.95)
ktri_vis10_spline_10d <- lm(ktri_avgVIS10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_vis10_ci_10d <- predict(ktri_vis10_spline_10d, interval = "confidence", level = 0.95)
ktri_vis10_spline_m <- lm(ktri_avgVIS10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_vis10_ci_m <- predict(ktri_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
ktri_cc10_spline_d <- lm(ktri_avgCCOV10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_cc10_ci_d <- predict(ktri_cc10_spline_d, interval = "confidence", level = 0.95)
ktri_cc10_spline_10d <- lm(ktri_avgCCOV10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_cc10_ci_10d <- predict(ktri_cc10_spline_10d, interval = "confidence", level = 0.95)
ktri_cc10_spline_m <- lm(ktri_avgCCOV10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_cc10_ci_m <- predict(ktri_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
ktri_alt10_spline_d <- lm(ktri_avgALTS10~ns(c(1:366), df=364), ktri_avgArray10)
ktri_alt10_ci_d <- predict(ktri_alt10_spline_d, interval = "confidence", level = 0.95)
ktri_alt10_spline_10d <- lm(ktri_avgALTS10~ns(c(1:366), df=35), ktri_avgArray10)
ktri_alt10_ci_10d <- predict(ktri_alt10_spline_10d, interval = "confidence", level = 0.95)
ktri_alt10_spline_m <- lm(ktri_avgALTS10~ns(c(1:366), df=11), ktri_avgArray10)
ktri_alt10_ci_m <- predict(ktri_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
ktri_t20_spline_d <- lm(ktri_avgTemp20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_t20_ci_d <- predict(ktri_t20_spline_d, interval = "confidence", level = 0.95)
ktri_t20_spline_10d <- lm(ktri_avgTemp20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_t20_ci_10d <- predict(ktri_t20_spline_10d, interval = "confidence", level = 0.95)
ktri_t20_spline_m <- lm(ktri_avgTemp20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_t20_ci_m <- predict(ktri_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
ktri_dp20_spline_d <- lm(ktri_avgDPTemp20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_dp20_ci_d <- predict(ktri_dp20_spline_d, interval = "confidence", level = 0.95)
ktri_dp20_spline_10d <- lm(ktri_avgDPTemp20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_dp20_ci_10d <- predict(ktri_dp20_spline_10d, interval = "confidence", level = 0.95)
ktri_dp20_spline_m <- lm(ktri_avgDPTemp20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_dp20_ci_m <- predict(ktri_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
ktri_wd20_spline_d <- lm(ktri_avgWDIR20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_wd20_ci_d <- predict(ktri_wd20_spline_d, interval = "confidence", level = 0.95)
ktri_wd20_spline_10d <- lm(ktri_avgWDIR20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_wd20_ci_10d <- predict(ktri_wd20_spline_10d, interval = "confidence", level = 0.95)
ktri_wd20_spline_m <- lm(ktri_avgWDIR20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_wd20_ci_m <- predict(ktri_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
ktri_ws20_spline_d <- lm(ktri_avgWSPD20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_ws20_ci_d <- predict(ktri_ws20_spline_d, interval = "confidence", level = 0.95)
ktri_ws20_spline_10d <- lm(ktri_avgWSPD20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_ws20_ci_10d <- predict(ktri_ws20_spline_10d, interval = "confidence", level = 0.95)
ktri_ws20_spline_m <- lm(ktri_avgWSPD20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_ws20_ci_m <- predict(ktri_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
ktri_wg20_spline_d <- lm(ktri_avgWGSP20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_wg20_ci_d <- predict(ktri_wg20_spline_d, interval = "confidence", level = 0.95)
ktri_wg20_spline_10d <- lm(ktri_avgWGSP20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_wg20_ci_10d <- predict(ktri_wg20_spline_10d, interval = "confidence", level = 0.95)
ktri_wg20_spline_m <- lm(ktri_avgWGSP20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_wg20_ci_m <- predict(ktri_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
ktri_cig20_spline_d <- lm(ktri_avgCCIG20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_cig20_ci_d <- predict(ktri_cig20_spline_d, interval = "confidence", level = 0.95)
ktri_cig20_spline_10d <- lm(ktri_avgCCIG20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_cig20_ci_10d <- predict(ktri_cig20_spline_10d, interval = "confidence", level = 0.95)
ktri_cig20_spline_m <- lm(ktri_avgCCIG20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_cig20_ci_m <- predict(ktri_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
ktri_vis20_spline_d <- lm(ktri_avgVIS20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_vis20_ci_d <- predict(ktri_vis20_spline_d, interval = "confidence", level = 0.95)
ktri_vis20_spline_10d <- lm(ktri_avgVIS20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_vis20_ci_10d <- predict(ktri_vis20_spline_10d, interval = "confidence", level = 0.95)
ktri_vis20_spline_m <- lm(ktri_avgVIS20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_vis20_ci_m <- predict(ktri_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
ktri_cc20_spline_d <- lm(ktri_avgCCOV20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_cc20_ci_d <- predict(ktri_cc20_spline_d, interval = "confidence", level = 0.95)
ktri_cc20_spline_10d <- lm(ktri_avgCCOV20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_cc20_ci_10d <- predict(ktri_cc20_spline_10d, interval = "confidence", level = 0.95)
ktri_cc20_spline_m <- lm(ktri_avgCCOV20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_cc20_ci_m <- predict(ktri_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
ktri_alt20_spline_d <- lm(ktri_avgALTS20~ns(c(1:366), df=364), ktri_avgArray20)
ktri_alt20_ci_d <- predict(ktri_alt20_spline_d, interval = "confidence", level = 0.95)
ktri_alt20_spline_10d <- lm(ktri_avgALTS20~ns(c(1:366), df=35), ktri_avgArray20)
ktri_alt20_ci_10d <- predict(ktri_alt20_spline_10d, interval = "confidence", level = 0.95)
ktri_alt20_spline_m <- lm(ktri_avgALTS20~ns(c(1:366), df=11), ktri_avgArray20)
ktri_alt20_ci_m <- predict(ktri_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
ktri_t30_spline_d <- lm(ktri_avgTemp30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_t30_ci_d <- predict(ktri_t30_spline_d, interval = "confidence", level = 0.95)
ktri_t30_spline_10d <- lm(ktri_avgTemp30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_t30_ci_10d <- predict(ktri_t30_spline_10d, interval = "confidence", level = 0.95)
ktri_t30_spline_m <- lm(ktri_avgTemp30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_t30_ci_m <- predict(ktri_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
ktri_dp30_spline_d <- lm(ktri_avgDPTemp30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_dp30_ci_d <- predict(ktri_dp30_spline_d, interval = "confidence", level = 0.95)
ktri_dp30_spline_10d <- lm(ktri_avgDPTemp30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_dp30_ci_10d <- predict(ktri_dp30_spline_10d, interval = "confidence", level = 0.95)
ktri_dp30_spline_m <- lm(ktri_avgDPTemp30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_dp30_ci_m <- predict(ktri_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
ktri_wd30_spline_d <- lm(ktri_avgWDIR30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_wd30_ci_d <- predict(ktri_wd30_spline_d, interval = "confidence", level = 0.95)
ktri_wd30_spline_10d <- lm(ktri_avgWDIR30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_wd30_ci_10d <- predict(ktri_wd30_spline_10d, interval = "confidence", level = 0.95)
ktri_wd30_spline_m <- lm(ktri_avgWDIR30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_wd30_ci_m <- predict(ktri_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
ktri_ws30_spline_d <- lm(ktri_avgWSPD30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_ws30_ci_d <- predict(ktri_ws30_spline_d, interval = "confidence", level = 0.95)
ktri_ws30_spline_10d <- lm(ktri_avgWSPD30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_ws30_ci_10d <- predict(ktri_ws30_spline_10d, interval = "confidence", level = 0.95)
ktri_ws30_spline_m <- lm(ktri_avgWSPD30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_ws30_ci_m <- predict(ktri_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
ktri_wg30_spline_d <- lm(ktri_avgWGSP30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_wg30_ci_d <- predict(ktri_wg30_spline_d, interval = "confidence", level = 0.95)
ktri_wg30_spline_10d <- lm(ktri_avgWGSP30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_wg30_ci_10d <- predict(ktri_wg30_spline_10d, interval = "confidence", level = 0.95)
ktri_wg30_spline_m <- lm(ktri_avgWGSP30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_wg30_ci_m <- predict(ktri_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
ktri_cig30_spline_d <- lm(ktri_avgCCIG30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_cig30_ci_d <- predict(ktri_cig30_spline_d, interval = "confidence", level = 0.95)
ktri_cig30_spline_10d <- lm(ktri_avgCCIG30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_cig30_ci_10d <- predict(ktri_cig30_spline_10d, interval = "confidence", level = 0.95)
ktri_cig30_spline_m <- lm(ktri_avgCCIG30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_cig30_ci_m <- predict(ktri_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
ktri_vis30_spline_d <- lm(ktri_avgVIS30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_vis30_ci_d <- predict(ktri_vis30_spline_d, interval = "confidence", level = 0.95)
ktri_vis30_spline_10d <- lm(ktri_avgVIS30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_vis30_ci_10d <- predict(ktri_vis30_spline_10d, interval = "confidence", level = 0.95)
ktri_vis30_spline_m <- lm(ktri_avgVIS30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_vis30_ci_m <- predict(ktri_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
ktri_cc30_spline_d <- lm(ktri_avgCCOV30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_cc30_ci_d <- predict(ktri_cc30_spline_d, interval = "confidence", level = 0.95)
ktri_cc30_spline_10d <- lm(ktri_avgCCOV30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_cc30_ci_10d <- predict(ktri_cc30_spline_10d, interval = "confidence", level = 0.95)
ktri_cc30_spline_m <- lm(ktri_avgCCOV30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_cc30_ci_m <- predict(ktri_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
ktri_alt30_spline_d <- lm(ktri_avgALTS30~ns(c(1:366), df=364), ktri_avgArray30)
ktri_alt30_ci_d <- predict(ktri_alt30_spline_d, interval = "confidence", level = 0.95)
ktri_alt30_spline_10d <- lm(ktri_avgALTS30~ns(c(1:366), df=35), ktri_avgArray30)
ktri_alt30_ci_10d <- predict(ktri_alt30_spline_10d, interval = "confidence", level = 0.95)
ktri_alt30_spline_m <- lm(ktri_avgALTS30~ns(c(1:366), df=11), ktri_avgArray30)
ktri_alt30_ci_m <- predict(ktri_alt30_spline_m, interval = "confidence", level = 0.95)



## PAJN
##Temperature
pajn_t10_spline_d <- lm(pajn_avgTemp10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_t10_ci_d <- predict(pajn_t10_spline_d, interval = "confidence", level = 0.95)
pajn_t10_spline_10d <- lm(pajn_avgTemp10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_t10_ci_10d <- predict(pajn_t10_spline_10d, interval = "confidence", level = 0.95)
pajn_t10_spline_m <- lm(pajn_avgTemp10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_t10_ci_m <- predict(pajn_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
pajn_dp10_spline_d <- lm(pajn_avgDPTemp10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_dp10_ci_d <- predict(pajn_dp10_spline_d, interval = "confidence", level = 0.95)
pajn_dp10_spline_10d <- lm(pajn_avgDPTemp10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_dp10_ci_10d <- predict(pajn_dp10_spline_10d, interval = "confidence", level = 0.95)
pajn_dp10_spline_m <- lm(pajn_avgDPTemp10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_dp10_ci_m <- predict(pajn_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
pajn_wd10_spline_d <- lm(pajn_avgWDIR10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_wd10_ci_d <- predict(pajn_wd10_spline_d, interval = "confidence", level = 0.95)
pajn_wd10_spline_10d <- lm(pajn_avgWDIR10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_wd10_ci_10d <- predict(pajn_wd10_spline_10d, interval = "confidence", level = 0.95)
pajn_wd10_spline_m <- lm(pajn_avgWDIR10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_wd10_ci_m <- predict(pajn_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
pajn_ws10_spline_d <- lm(pajn_avgWSPD10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_ws10_ci_d <- predict(pajn_ws10_spline_d, interval = "confidence", level = 0.95)
pajn_ws10_spline_10d <- lm(pajn_avgWSPD10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_ws10_ci_10d <- predict(pajn_ws10_spline_10d, interval = "confidence", level = 0.95)
pajn_ws10_spline_m <- lm(pajn_avgWSPD10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_ws10_ci_m <- predict(pajn_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
pajn_wg10_spline_d <- lm(pajn_avgWGSP10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_wg10_ci_d <- predict(pajn_wg10_spline_d, interval = "confidence", level = 0.95)
pajn_wg10_spline_10d <- lm(pajn_avgWGSP10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_wg10_ci_10d <- predict(pajn_wg10_spline_10d, interval = "confidence", level = 0.95)
pajn_wg10_spline_m <- lm(pajn_avgWGSP10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_wg10_ci_m <- predict(pajn_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
pajn_cig10_spline_d <- lm(pajn_avgCCIG10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_cig10_ci_d <- predict(pajn_cig10_spline_d, interval = "confidence", level = 0.95)
pajn_cig10_spline_10d <- lm(pajn_avgCCIG10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_cig10_ci_10d <- predict(pajn_cig10_spline_10d, interval = "confidence", level = 0.95)
pajn_cig10_spline_m <- lm(pajn_avgCCIG10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_cig10_ci_m <- predict(pajn_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
pajn_vis10_spline_d <- lm(pajn_avgVIS10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_vis10_ci_d <- predict(pajn_vis10_spline_d, interval = "confidence", level = 0.95)
pajn_vis10_spline_10d <- lm(pajn_avgVIS10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_vis10_ci_10d <- predict(pajn_vis10_spline_10d, interval = "confidence", level = 0.95)
pajn_vis10_spline_m <- lm(pajn_avgVIS10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_vis10_ci_m <- predict(pajn_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
pajn_cc10_spline_d <- lm(pajn_avgCCOV10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_cc10_ci_d <- predict(pajn_cc10_spline_d, interval = "confidence", level = 0.95)
pajn_cc10_spline_10d <- lm(pajn_avgCCOV10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_cc10_ci_10d <- predict(pajn_cc10_spline_10d, interval = "confidence", level = 0.95)
pajn_cc10_spline_m <- lm(pajn_avgCCOV10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_cc10_ci_m <- predict(pajn_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
pajn_alt10_spline_d <- lm(pajn_avgALTS10~ns(c(1:366), df=364), pajn_avgArray10)
pajn_alt10_ci_d <- predict(pajn_alt10_spline_d, interval = "confidence", level = 0.95)
pajn_alt10_spline_10d <- lm(pajn_avgALTS10~ns(c(1:366), df=35), pajn_avgArray10)
pajn_alt10_ci_10d <- predict(pajn_alt10_spline_10d, interval = "confidence", level = 0.95)
pajn_alt10_spline_m <- lm(pajn_avgALTS10~ns(c(1:366), df=11), pajn_avgArray10)
pajn_alt10_ci_m <- predict(pajn_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
pajn_t20_spline_d <- lm(pajn_avgTemp20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_t20_ci_d <- predict(pajn_t20_spline_d, interval = "confidence", level = 0.95)
pajn_t20_spline_10d <- lm(pajn_avgTemp20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_t20_ci_10d <- predict(pajn_t20_spline_10d, interval = "confidence", level = 0.95)
pajn_t20_spline_m <- lm(pajn_avgTemp20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_t20_ci_m <- predict(pajn_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
pajn_dp20_spline_d <- lm(pajn_avgDPTemp20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_dp20_ci_d <- predict(pajn_dp20_spline_d, interval = "confidence", level = 0.95)
pajn_dp20_spline_10d <- lm(pajn_avgDPTemp20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_dp20_ci_10d <- predict(pajn_dp20_spline_10d, interval = "confidence", level = 0.95)
pajn_dp20_spline_m <- lm(pajn_avgDPTemp20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_dp20_ci_m <- predict(pajn_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
pajn_wd20_spline_d <- lm(pajn_avgWDIR20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_wd20_ci_d <- predict(pajn_wd20_spline_d, interval = "confidence", level = 0.95)
pajn_wd20_spline_10d <- lm(pajn_avgWDIR20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_wd20_ci_10d <- predict(pajn_wd20_spline_10d, interval = "confidence", level = 0.95)
pajn_wd20_spline_m <- lm(pajn_avgWDIR20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_wd20_ci_m <- predict(pajn_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
pajn_ws20_spline_d <- lm(pajn_avgWSPD20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_ws20_ci_d <- predict(pajn_ws20_spline_d, interval = "confidence", level = 0.95)
pajn_ws20_spline_10d <- lm(pajn_avgWSPD20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_ws20_ci_10d <- predict(pajn_ws20_spline_10d, interval = "confidence", level = 0.95)
pajn_ws20_spline_m <- lm(pajn_avgWSPD20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_ws20_ci_m <- predict(pajn_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
pajn_wg20_spline_d <- lm(pajn_avgWGSP20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_wg20_ci_d <- predict(pajn_wg20_spline_d, interval = "confidence", level = 0.95)
pajn_wg20_spline_10d <- lm(pajn_avgWGSP20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_wg20_ci_10d <- predict(pajn_wg20_spline_10d, interval = "confidence", level = 0.95)
pajn_wg20_spline_m <- lm(pajn_avgWGSP20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_wg20_ci_m <- predict(pajn_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
pajn_cig20_spline_d <- lm(pajn_avgCCIG20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_cig20_ci_d <- predict(pajn_cig20_spline_d, interval = "confidence", level = 0.95)
pajn_cig20_spline_10d <- lm(pajn_avgCCIG20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_cig20_ci_10d <- predict(pajn_cig20_spline_10d, interval = "confidence", level = 0.95)
pajn_cig20_spline_m <- lm(pajn_avgCCIG20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_cig20_ci_m <- predict(pajn_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
pajn_vis20_spline_d <- lm(pajn_avgVIS20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_vis20_ci_d <- predict(pajn_vis20_spline_d, interval = "confidence", level = 0.95)
pajn_vis20_spline_10d <- lm(pajn_avgVIS20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_vis20_ci_10d <- predict(pajn_vis20_spline_10d, interval = "confidence", level = 0.95)
pajn_vis20_spline_m <- lm(pajn_avgVIS20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_vis20_ci_m <- predict(pajn_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
pajn_cc20_spline_d <- lm(pajn_avgCCOV20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_cc20_ci_d <- predict(pajn_cc20_spline_d, interval = "confidence", level = 0.95)
pajn_cc20_spline_10d <- lm(pajn_avgCCOV20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_cc20_ci_10d <- predict(pajn_cc20_spline_10d, interval = "confidence", level = 0.95)
pajn_cc20_spline_m <- lm(pajn_avgCCOV20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_cc20_ci_m <- predict(pajn_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
pajn_alt20_spline_d <- lm(pajn_avgALTS20~ns(c(1:366), df=364), pajn_avgArray20)
pajn_alt20_ci_d <- predict(pajn_alt20_spline_d, interval = "confidence", level = 0.95)
pajn_alt20_spline_10d <- lm(pajn_avgALTS20~ns(c(1:366), df=35), pajn_avgArray20)
pajn_alt20_ci_10d <- predict(pajn_alt20_spline_10d, interval = "confidence", level = 0.95)
pajn_alt20_spline_m <- lm(pajn_avgALTS20~ns(c(1:366), df=11), pajn_avgArray20)
pajn_alt20_ci_m <- predict(pajn_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
pajn_t30_spline_d <- lm(pajn_avgTemp30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_t30_ci_d <- predict(pajn_t30_spline_d, interval = "confidence", level = 0.95)
pajn_t30_spline_10d <- lm(pajn_avgTemp30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_t30_ci_10d <- predict(pajn_t30_spline_10d, interval = "confidence", level = 0.95)
pajn_t30_spline_m <- lm(pajn_avgTemp30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_t30_ci_m <- predict(pajn_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
pajn_dp30_spline_d <- lm(pajn_avgDPTemp30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_dp30_ci_d <- predict(pajn_dp30_spline_d, interval = "confidence", level = 0.95)
pajn_dp30_spline_10d <- lm(pajn_avgDPTemp30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_dp30_ci_10d <- predict(pajn_dp30_spline_10d, interval = "confidence", level = 0.95)
pajn_dp30_spline_m <- lm(pajn_avgDPTemp30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_dp30_ci_m <- predict(pajn_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
pajn_wd30_spline_d <- lm(pajn_avgWDIR30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_wd30_ci_d <- predict(pajn_wd30_spline_d, interval = "confidence", level = 0.95)
pajn_wd30_spline_10d <- lm(pajn_avgWDIR30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_wd30_ci_10d <- predict(pajn_wd30_spline_10d, interval = "confidence", level = 0.95)
pajn_wd30_spline_m <- lm(pajn_avgWDIR30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_wd30_ci_m <- predict(pajn_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
pajn_ws30_spline_d <- lm(pajn_avgWSPD30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_ws30_ci_d <- predict(pajn_ws30_spline_d, interval = "confidence", level = 0.95)
pajn_ws30_spline_10d <- lm(pajn_avgWSPD30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_ws30_ci_10d <- predict(pajn_ws30_spline_10d, interval = "confidence", level = 0.95)
pajn_ws30_spline_m <- lm(pajn_avgWSPD30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_ws30_ci_m <- predict(pajn_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
pajn_wg30_spline_d <- lm(pajn_avgWGSP30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_wg30_ci_d <- predict(pajn_wg30_spline_d, interval = "confidence", level = 0.95)
pajn_wg30_spline_10d <- lm(pajn_avgWGSP30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_wg30_ci_10d <- predict(pajn_wg30_spline_10d, interval = "confidence", level = 0.95)
pajn_wg30_spline_m <- lm(pajn_avgWGSP30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_wg30_ci_m <- predict(pajn_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
pajn_cig30_spline_d <- lm(pajn_avgCCIG30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_cig30_ci_d <- predict(pajn_cig30_spline_d, interval = "confidence", level = 0.95)
pajn_cig30_spline_10d <- lm(pajn_avgCCIG30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_cig30_ci_10d <- predict(pajn_cig30_spline_10d, interval = "confidence", level = 0.95)
pajn_cig30_spline_m <- lm(pajn_avgCCIG30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_cig30_ci_m <- predict(pajn_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
pajn_vis30_spline_d <- lm(pajn_avgVIS30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_vis30_ci_d <- predict(pajn_vis30_spline_d, interval = "confidence", level = 0.95)
pajn_vis30_spline_10d <- lm(pajn_avgVIS30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_vis30_ci_10d <- predict(pajn_vis30_spline_10d, interval = "confidence", level = 0.95)
pajn_vis30_spline_m <- lm(pajn_avgVIS30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_vis30_ci_m <- predict(pajn_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
pajn_cc30_spline_d <- lm(pajn_avgCCOV30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_cc30_ci_d <- predict(pajn_cc30_spline_d, interval = "confidence", level = 0.95)
pajn_cc30_spline_10d <- lm(pajn_avgCCOV30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_cc30_ci_10d <- predict(pajn_cc30_spline_10d, interval = "confidence", level = 0.95)
pajn_cc30_spline_m <- lm(pajn_avgCCOV30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_cc30_ci_m <- predict(pajn_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
pajn_alt30_spline_d <- lm(pajn_avgALTS30~ns(c(1:366), df=364), pajn_avgArray30)
pajn_alt30_ci_d <- predict(pajn_alt30_spline_d, interval = "confidence", level = 0.95)
pajn_alt30_spline_10d <- lm(pajn_avgALTS30~ns(c(1:366), df=35), pajn_avgArray30)
pajn_alt30_ci_10d <- predict(pajn_alt30_spline_10d, interval = "confidence", level = 0.95)
pajn_alt30_spline_m <- lm(pajn_avgALTS30~ns(c(1:366), df=11), pajn_avgArray30)
pajn_alt30_ci_m <- predict(pajn_alt30_spline_m, interval = "confidence", level = 0.95)




### KELP
##### 10 Year Splines
##Temperature
kelp_t10_spline_d <- lm(kelp_avgTemp10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_t10_ci_d <- predict(kelp_t10_spline_d, interval = "confidence", level = 0.95)
kelp_t10_spline_10d <- lm(kelp_avgTemp10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_t10_ci_10d <- predict(kelp_t10_spline_10d, interval = "confidence", level = 0.95)
kelp_t10_spline_m <- lm(kelp_avgTemp10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_t10_ci_m <- predict(kelp_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kelp_dp10_spline_d <- lm(kelp_avgDPTemp10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_dp10_ci_d <- predict(kelp_dp10_spline_d, interval = "confidence", level = 0.95)
kelp_dp10_spline_10d <- lm(kelp_avgDPTemp10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_dp10_ci_10d <- predict(kelp_dp10_spline_10d, interval = "confidence", level = 0.95)
kelp_dp10_spline_m <- lm(kelp_avgDPTemp10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_dp10_ci_m <- predict(kelp_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kelp_wd10_spline_d <- lm(kelp_avgWDIR10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_wd10_ci_d <- predict(kelp_wd10_spline_d, interval = "confidence", level = 0.95)
kelp_wd10_spline_10d <- lm(kelp_avgWDIR10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_wd10_ci_10d <- predict(kelp_wd10_spline_10d, interval = "confidence", level = 0.95)
kelp_wd10_spline_m <- lm(kelp_avgWDIR10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_wd10_ci_m <- predict(kelp_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kelp_ws10_spline_d <- lm(kelp_avgWSPD10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_ws10_ci_d <- predict(kelp_ws10_spline_d, interval = "confidence", level = 0.95)
kelp_ws10_spline_10d <- lm(kelp_avgWSPD10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_ws10_ci_10d <- predict(kelp_ws10_spline_10d, interval = "confidence", level = 0.95)
kelp_ws10_spline_m <- lm(kelp_avgWSPD10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_ws10_ci_m <- predict(kelp_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kelp_wg10_spline_d <- lm(kelp_avgWGSP10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_wg10_ci_d <- predict(kelp_wg10_spline_d, interval = "confidence", level = 0.95)
kelp_wg10_spline_10d <- lm(kelp_avgWGSP10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_wg10_ci_10d <- predict(kelp_wg10_spline_10d, interval = "confidence", level = 0.95)
kelp_wg10_spline_m <- lm(kelp_avgWGSP10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_wg10_ci_m <- predict(kelp_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kelp_cig10_spline_d <- lm(kelp_avgCCIG10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_cig10_ci_d <- predict(kelp_cig10_spline_d, interval = "confidence", level = 0.95)
kelp_cig10_spline_10d <- lm(kelp_avgCCIG10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_cig10_ci_10d <- predict(kelp_cig10_spline_10d, interval = "confidence", level = 0.95)
kelp_cig10_spline_m <- lm(kelp_avgCCIG10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_cig10_ci_m <- predict(kelp_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kelp_vis10_spline_d <- lm(kelp_avgVIS10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_vis10_ci_d <- predict(kelp_vis10_spline_d, interval = "confidence", level = 0.95)
kelp_vis10_spline_10d <- lm(kelp_avgVIS10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_vis10_ci_10d <- predict(kelp_vis10_spline_10d, interval = "confidence", level = 0.95)
kelp_vis10_spline_m <- lm(kelp_avgVIS10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_vis10_ci_m <- predict(kelp_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kelp_cc10_spline_d <- lm(kelp_avgCCOV10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_cc10_ci_d <- predict(kelp_cc10_spline_d, interval = "confidence", level = 0.95)
kelp_cc10_spline_10d <- lm(kelp_avgCCOV10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_cc10_ci_10d <- predict(kelp_cc10_spline_10d, interval = "confidence", level = 0.95)
kelp_cc10_spline_m <- lm(kelp_avgCCOV10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_cc10_ci_m <- predict(kelp_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kelp_alt10_spline_d <- lm(kelp_avgALTS10~ns(c(1:366), df=364), kelp_avgArray10)
kelp_alt10_ci_d <- predict(kelp_alt10_spline_d, interval = "confidence", level = 0.95)
kelp_alt10_spline_10d <- lm(kelp_avgALTS10~ns(c(1:366), df=35), kelp_avgArray10)
kelp_alt10_ci_10d <- predict(kelp_alt10_spline_10d, interval = "confidence", level = 0.95)
kelp_alt10_spline_m <- lm(kelp_avgALTS10~ns(c(1:366), df=11), kelp_avgArray10)
kelp_alt10_ci_m <- predict(kelp_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
kelp_t20_spline_d <- lm(kelp_avgTemp20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_t20_ci_d <- predict(kelp_t20_spline_d, interval = "confidence", level = 0.95)
kelp_t20_spline_10d <- lm(kelp_avgTemp20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_t20_ci_10d <- predict(kelp_t20_spline_10d, interval = "confidence", level = 0.95)
kelp_t20_spline_m <- lm(kelp_avgTemp20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_t20_ci_m <- predict(kelp_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kelp_dp20_spline_d <- lm(kelp_avgDPTemp20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_dp20_ci_d <- predict(kelp_dp20_spline_d, interval = "confidence", level = 0.95)
kelp_dp20_spline_10d <- lm(kelp_avgDPTemp20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_dp20_ci_10d <- predict(kelp_dp20_spline_10d, interval = "confidence", level = 0.95)
kelp_dp20_spline_m <- lm(kelp_avgDPTemp20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_dp20_ci_m <- predict(kelp_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kelp_wd20_spline_d <- lm(kelp_avgWDIR20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_wd20_ci_d <- predict(kelp_wd20_spline_d, interval = "confidence", level = 0.95)
kelp_wd20_spline_10d <- lm(kelp_avgWDIR20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_wd20_ci_10d <- predict(kelp_wd20_spline_10d, interval = "confidence", level = 0.95)
kelp_wd20_spline_m <- lm(kelp_avgWDIR20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_wd20_ci_m <- predict(kelp_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kelp_ws20_spline_d <- lm(kelp_avgWSPD20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_ws20_ci_d <- predict(kelp_ws20_spline_d, interval = "confidence", level = 0.95)
kelp_ws20_spline_10d <- lm(kelp_avgWSPD20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_ws20_ci_10d <- predict(kelp_ws20_spline_10d, interval = "confidence", level = 0.95)
kelp_ws20_spline_m <- lm(kelp_avgWSPD20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_ws20_ci_m <- predict(kelp_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kelp_wg20_spline_d <- lm(kelp_avgWGSP20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_wg20_ci_d <- predict(kelp_wg20_spline_d, interval = "confidence", level = 0.95)
kelp_wg20_spline_10d <- lm(kelp_avgWGSP20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_wg20_ci_10d <- predict(kelp_wg20_spline_10d, interval = "confidence", level = 0.95)
kelp_wg20_spline_m <- lm(kelp_avgWGSP20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_wg20_ci_m <- predict(kelp_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kelp_cig20_spline_d <- lm(kelp_avgCCIG20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_cig20_ci_d <- predict(kelp_cig20_spline_d, interval = "confidence", level = 0.95)
kelp_cig20_spline_10d <- lm(kelp_avgCCIG20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_cig20_ci_10d <- predict(kelp_cig20_spline_10d, interval = "confidence", level = 0.95)
kelp_cig20_spline_m <- lm(kelp_avgCCIG20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_cig20_ci_m <- predict(kelp_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kelp_vis20_spline_d <- lm(kelp_avgVIS20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_vis20_ci_d <- predict(kelp_vis20_spline_d, interval = "confidence", level = 0.95)
kelp_vis20_spline_10d <- lm(kelp_avgVIS20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_vis20_ci_10d <- predict(kelp_vis20_spline_10d, interval = "confidence", level = 0.95)
kelp_vis20_spline_m <- lm(kelp_avgVIS20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_vis20_ci_m <- predict(kelp_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kelp_cc20_spline_d <- lm(kelp_avgCCOV20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_cc20_ci_d <- predict(kelp_cc20_spline_d, interval = "confidence", level = 0.95)
kelp_cc20_spline_10d <- lm(kelp_avgCCOV20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_cc20_ci_10d <- predict(kelp_cc20_spline_10d, interval = "confidence", level = 0.95)
kelp_cc20_spline_m <- lm(kelp_avgCCOV20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_cc20_ci_m <- predict(kelp_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kelp_alt20_spline_d <- lm(kelp_avgALTS20~ns(c(1:366), df=364), kelp_avgArray20)
kelp_alt20_ci_d <- predict(kelp_alt20_spline_d, interval = "confidence", level = 0.95)
kelp_alt20_spline_10d <- lm(kelp_avgALTS20~ns(c(1:366), df=35), kelp_avgArray20)
kelp_alt20_ci_10d <- predict(kelp_alt20_spline_10d, interval = "confidence", level = 0.95)
kelp_alt20_spline_m <- lm(kelp_avgALTS20~ns(c(1:366), df=11), kelp_avgArray20)
kelp_alt20_ci_m <- predict(kelp_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
kelp_t30_spline_d <- lm(kelp_avgTemp30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_t30_ci_d <- predict(kelp_t30_spline_d, interval = "confidence", level = 0.95)
kelp_t30_spline_10d <- lm(kelp_avgTemp30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_t30_ci_10d <- predict(kelp_t30_spline_10d, interval = "confidence", level = 0.95)
kelp_t30_spline_m <- lm(kelp_avgTemp30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_t30_ci_m <- predict(kelp_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
kelp_dp30_spline_d <- lm(kelp_avgDPTemp30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_dp30_ci_d <- predict(kelp_dp30_spline_d, interval = "confidence", level = 0.95)
kelp_dp30_spline_10d <- lm(kelp_avgDPTemp30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_dp30_ci_10d <- predict(kelp_dp30_spline_10d, interval = "confidence", level = 0.95)
kelp_dp30_spline_m <- lm(kelp_avgDPTemp30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_dp30_ci_m <- predict(kelp_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
kelp_wd30_spline_d <- lm(kelp_avgWDIR30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_wd30_ci_d <- predict(kelp_wd30_spline_d, interval = "confidence", level = 0.95)
kelp_wd30_spline_10d <- lm(kelp_avgWDIR30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_wd30_ci_10d <- predict(kelp_wd30_spline_10d, interval = "confidence", level = 0.95)
kelp_wd30_spline_m <- lm(kelp_avgWDIR30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_wd30_ci_m <- predict(kelp_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
kelp_ws30_spline_d <- lm(kelp_avgWSPD30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_ws30_ci_d <- predict(kelp_ws30_spline_d, interval = "confidence", level = 0.95)
kelp_ws30_spline_10d <- lm(kelp_avgWSPD30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_ws30_ci_10d <- predict(kelp_ws30_spline_10d, interval = "confidence", level = 0.95)
kelp_ws30_spline_m <- lm(kelp_avgWSPD30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_ws30_ci_m <- predict(kelp_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
kelp_wg30_spline_d <- lm(kelp_avgWGSP30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_wg30_ci_d <- predict(kelp_wg30_spline_d, interval = "confidence", level = 0.95)
kelp_wg30_spline_10d <- lm(kelp_avgWGSP30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_wg30_ci_10d <- predict(kelp_wg30_spline_10d, interval = "confidence", level = 0.95)
kelp_wg30_spline_m <- lm(kelp_avgWGSP30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_wg30_ci_m <- predict(kelp_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
kelp_cig30_spline_d <- lm(kelp_avgCCIG30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_cig30_ci_d <- predict(kelp_cig30_spline_d, interval = "confidence", level = 0.95)
kelp_cig30_spline_10d <- lm(kelp_avgCCIG30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_cig30_ci_10d <- predict(kelp_cig30_spline_10d, interval = "confidence", level = 0.95)
kelp_cig30_spline_m <- lm(kelp_avgCCIG30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_cig30_ci_m <- predict(kelp_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
kelp_vis30_spline_d <- lm(kelp_avgVIS30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_vis30_ci_d <- predict(kelp_vis30_spline_d, interval = "confidence", level = 0.95)
kelp_vis30_spline_10d <- lm(kelp_avgVIS30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_vis30_ci_10d <- predict(kelp_vis30_spline_10d, interval = "confidence", level = 0.95)
kelp_vis30_spline_m <- lm(kelp_avgVIS30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_vis30_ci_m <- predict(kelp_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
kelp_cc30_spline_d <- lm(kelp_avgCCOV30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_cc30_ci_d <- predict(kelp_cc30_spline_d, interval = "confidence", level = 0.95)
kelp_cc30_spline_10d <- lm(kelp_avgCCOV30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_cc30_ci_10d <- predict(kelp_cc30_spline_10d, interval = "confidence", level = 0.95)
kelp_cc30_spline_m <- lm(kelp_avgCCOV30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_cc30_ci_m <- predict(kelp_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
kelp_alt30_spline_d <- lm(kelp_avgALTS30~ns(c(1:366), df=364), kelp_avgArray30)
kelp_alt30_ci_d <- predict(kelp_alt30_spline_d, interval = "confidence", level = 0.95)
kelp_alt30_spline_10d <- lm(kelp_avgALTS30~ns(c(1:366), df=35), kelp_avgArray30)
kelp_alt30_ci_10d <- predict(kelp_alt30_spline_10d, interval = "confidence", level = 0.95)
kelp_alt30_spline_m <- lm(kelp_avgALTS30~ns(c(1:366), df=11), kelp_avgArray30)
kelp_alt30_ci_m <- predict(kelp_alt30_spline_m, interval = "confidence", level = 0.95)



#### KSGU
##### 10 Year Splines
##Temperature
ksgu_t10_spline_d <- lm(ksgu_avgTemp10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_t10_ci_d <- predict(ksgu_t10_spline_d, interval = "confidence", level = 0.95)
ksgu_t10_spline_10d <- lm(ksgu_avgTemp10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_t10_ci_10d <- predict(ksgu_t10_spline_10d, interval = "confidence", level = 0.95)
ksgu_t10_spline_m <- lm(ksgu_avgTemp10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_t10_ci_m <- predict(ksgu_t10_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
ksgu_dp10_spline_d <- lm(ksgu_avgDPTemp10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_dp10_ci_d <- predict(ksgu_dp10_spline_d, interval = "confidence", level = 0.95)
ksgu_dp10_spline_10d <- lm(ksgu_avgDPTemp10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_dp10_ci_10d <- predict(ksgu_dp10_spline_10d, interval = "confidence", level = 0.95)
ksgu_dp10_spline_m <- lm(ksgu_avgDPTemp10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_dp10_ci_m <- predict(ksgu_dp10_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
ksgu_wd10_spline_d <- lm(ksgu_avgWDIR10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_wd10_ci_d <- predict(ksgu_wd10_spline_d, interval = "confidence", level = 0.95)
ksgu_wd10_spline_10d <- lm(ksgu_avgWDIR10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_wd10_ci_10d <- predict(ksgu_wd10_spline_10d, interval = "confidence", level = 0.95)
ksgu_wd10_spline_m <- lm(ksgu_avgWDIR10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_wd10_ci_m <- predict(ksgu_wd10_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
ksgu_ws10_spline_d <- lm(ksgu_avgWSPD10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_ws10_ci_d <- predict(ksgu_ws10_spline_d, interval = "confidence", level = 0.95)
ksgu_ws10_spline_10d <- lm(ksgu_avgWSPD10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_ws10_ci_10d <- predict(ksgu_ws10_spline_10d, interval = "confidence", level = 0.95)
ksgu_ws10_spline_m <- lm(ksgu_avgWSPD10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_ws10_ci_m <- predict(ksgu_ws10_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
ksgu_wg10_spline_d <- lm(ksgu_avgWGSP10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_wg10_ci_d <- predict(ksgu_wg10_spline_d, interval = "confidence", level = 0.95)
ksgu_wg10_spline_10d <- lm(ksgu_avgWGSP10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_wg10_ci_10d <- predict(ksgu_wg10_spline_10d, interval = "confidence", level = 0.95)
ksgu_wg10_spline_m <- lm(ksgu_avgWGSP10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_wg10_ci_m <- predict(ksgu_wg10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
ksgu_cig10_spline_d <- lm(ksgu_avgCCIG10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_cig10_ci_d <- predict(ksgu_cig10_spline_d, interval = "confidence", level = 0.95)
ksgu_cig10_spline_10d <- lm(ksgu_avgCCIG10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_cig10_ci_10d <- predict(ksgu_cig10_spline_10d, interval = "confidence", level = 0.95)
ksgu_cig10_spline_m <- lm(ksgu_avgCCIG10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_cig10_ci_m <- predict(ksgu_cig10_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
ksgu_vis10_spline_d <- lm(ksgu_avgVIS10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_vis10_ci_d <- predict(ksgu_vis10_spline_d, interval = "confidence", level = 0.95)
ksgu_vis10_spline_10d <- lm(ksgu_avgVIS10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_vis10_ci_10d <- predict(ksgu_vis10_spline_10d, interval = "confidence", level = 0.95)
ksgu_vis10_spline_m <- lm(ksgu_avgVIS10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_vis10_ci_m <- predict(ksgu_vis10_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
ksgu_cc10_spline_d <- lm(ksgu_avgCCOV10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_cc10_ci_d <- predict(ksgu_cc10_spline_d, interval = "confidence", level = 0.95)
ksgu_cc10_spline_10d <- lm(ksgu_avgCCOV10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_cc10_ci_10d <- predict(ksgu_cc10_spline_10d, interval = "confidence", level = 0.95)
ksgu_cc10_spline_m <- lm(ksgu_avgCCOV10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_cc10_ci_m <- predict(ksgu_cc10_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
ksgu_alt10_spline_d <- lm(ksgu_avgALTS10~ns(c(1:366), df=364), ksgu_avgArray10)
ksgu_alt10_ci_d <- predict(ksgu_alt10_spline_d, interval = "confidence", level = 0.95)
ksgu_alt10_spline_10d <- lm(ksgu_avgALTS10~ns(c(1:366), df=35), ksgu_avgArray10)
ksgu_alt10_ci_10d <- predict(ksgu_alt10_spline_10d, interval = "confidence", level = 0.95)
ksgu_alt10_spline_m <- lm(ksgu_avgALTS10~ns(c(1:366), df=11), ksgu_avgArray10)
ksgu_alt10_ci_m <- predict(ksgu_alt10_spline_m, interval = "confidence", level = 0.95)

##### 20 Year Splines
##Temperature
ksgu_t20_spline_d <- lm(ksgu_avgTemp20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_t20_ci_d <- predict(ksgu_t20_spline_d, interval = "confidence", level = 0.95)
ksgu_t20_spline_10d <- lm(ksgu_avgTemp20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_t20_ci_10d <- predict(ksgu_t20_spline_10d, interval = "confidence", level = 0.95)
ksgu_t20_spline_m <- lm(ksgu_avgTemp20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_t20_ci_m <- predict(ksgu_t20_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
ksgu_dp20_spline_d <- lm(ksgu_avgDPTemp20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_dp20_ci_d <- predict(ksgu_dp20_spline_d, interval = "confidence", level = 0.95)
ksgu_dp20_spline_10d <- lm(ksgu_avgDPTemp20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_dp20_ci_10d <- predict(ksgu_dp20_spline_10d, interval = "confidence", level = 0.95)
ksgu_dp20_spline_m <- lm(ksgu_avgDPTemp20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_dp20_ci_m <- predict(ksgu_dp20_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
ksgu_wd20_spline_d <- lm(ksgu_avgWDIR20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_wd20_ci_d <- predict(ksgu_wd20_spline_d, interval = "confidence", level = 0.95)
ksgu_wd20_spline_10d <- lm(ksgu_avgWDIR20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_wd20_ci_10d <- predict(ksgu_wd20_spline_10d, interval = "confidence", level = 0.95)
ksgu_wd20_spline_m <- lm(ksgu_avgWDIR20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_wd20_ci_m <- predict(ksgu_wd20_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
ksgu_ws20_spline_d <- lm(ksgu_avgWSPD20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_ws20_ci_d <- predict(ksgu_ws20_spline_d, interval = "confidence", level = 0.95)
ksgu_ws20_spline_10d <- lm(ksgu_avgWSPD20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_ws20_ci_10d <- predict(ksgu_ws20_spline_10d, interval = "confidence", level = 0.95)
ksgu_ws20_spline_m <- lm(ksgu_avgWSPD20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_ws20_ci_m <- predict(ksgu_ws20_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
ksgu_wg20_spline_d <- lm(ksgu_avgWGSP20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_wg20_ci_d <- predict(ksgu_wg20_spline_d, interval = "confidence", level = 0.95)
ksgu_wg20_spline_10d <- lm(ksgu_avgWGSP20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_wg20_ci_10d <- predict(ksgu_wg20_spline_10d, interval = "confidence", level = 0.95)
ksgu_wg20_spline_m <- lm(ksgu_avgWGSP20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_wg20_ci_m <- predict(ksgu_wg20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
ksgu_cig20_spline_d <- lm(ksgu_avgCCIG20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_cig20_ci_d <- predict(ksgu_cig20_spline_d, interval = "confidence", level = 0.95)
ksgu_cig20_spline_10d <- lm(ksgu_avgCCIG20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_cig20_ci_10d <- predict(ksgu_cig20_spline_10d, interval = "confidence", level = 0.95)
ksgu_cig20_spline_m <- lm(ksgu_avgCCIG20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_cig20_ci_m <- predict(ksgu_cig20_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
ksgu_vis20_spline_d <- lm(ksgu_avgVIS20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_vis20_ci_d <- predict(ksgu_vis20_spline_d, interval = "confidence", level = 0.95)
ksgu_vis20_spline_10d <- lm(ksgu_avgVIS20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_vis20_ci_10d <- predict(ksgu_vis20_spline_10d, interval = "confidence", level = 0.95)
ksgu_vis20_spline_m <- lm(ksgu_avgVIS20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_vis20_ci_m <- predict(ksgu_vis20_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
ksgu_cc20_spline_d <- lm(ksgu_avgCCOV20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_cc20_ci_d <- predict(ksgu_cc20_spline_d, interval = "confidence", level = 0.95)
ksgu_cc20_spline_10d <- lm(ksgu_avgCCOV20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_cc20_ci_10d <- predict(ksgu_cc20_spline_10d, interval = "confidence", level = 0.95)
ksgu_cc20_spline_m <- lm(ksgu_avgCCOV20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_cc20_ci_m <- predict(ksgu_cc20_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
ksgu_alt20_spline_d <- lm(ksgu_avgALTS20~ns(c(1:366), df=364), ksgu_avgArray20)
ksgu_alt20_ci_d <- predict(ksgu_alt20_spline_d, interval = "confidence", level = 0.95)
ksgu_alt20_spline_10d <- lm(ksgu_avgALTS20~ns(c(1:366), df=35), ksgu_avgArray20)
ksgu_alt20_ci_10d <- predict(ksgu_alt20_spline_10d, interval = "confidence", level = 0.95)
ksgu_alt20_spline_m <- lm(ksgu_avgALTS20~ns(c(1:366), df=11), ksgu_avgArray20)
ksgu_alt20_ci_m <- predict(ksgu_alt20_spline_m, interval = "confidence", level = 0.95)

##### 30 Year Splines
##Temperature
ksgu_t30_spline_d <- lm(ksgu_avgTemp30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_t30_ci_d <- predict(ksgu_t30_spline_d, interval = "confidence", level = 0.95)
ksgu_t30_spline_10d <- lm(ksgu_avgTemp30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_t30_ci_10d <- predict(ksgu_t30_spline_10d, interval = "confidence", level = 0.95)
ksgu_t30_spline_m <- lm(ksgu_avgTemp30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_t30_ci_m <- predict(ksgu_t30_spline_m, interval = "confidence", level = 0.95)

#Dewpoint Temperature
ksgu_dp30_spline_d <- lm(ksgu_avgDPTemp30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_dp30_ci_d <- predict(ksgu_dp30_spline_d, interval = "confidence", level = 0.95)
ksgu_dp30_spline_10d <- lm(ksgu_avgDPTemp30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_dp30_ci_10d <- predict(ksgu_dp30_spline_10d, interval = "confidence", level = 0.95)
ksgu_dp30_spline_m <- lm(ksgu_avgDPTemp30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_dp30_ci_m <- predict(ksgu_dp30_spline_m, interval = "confidence", level = 0.95)

#Wind Direction
ksgu_wd30_spline_d <- lm(ksgu_avgWDIR30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_wd30_ci_d <- predict(ksgu_wd30_spline_d, interval = "confidence", level = 0.95)
ksgu_wd30_spline_10d <- lm(ksgu_avgWDIR30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_wd30_ci_10d <- predict(ksgu_wd30_spline_10d, interval = "confidence", level = 0.95)
ksgu_wd30_spline_m <- lm(ksgu_avgWDIR30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_wd30_ci_m <- predict(ksgu_wd30_spline_m, interval = "confidence", level = 0.95)

#Wind Speed
ksgu_ws30_spline_d <- lm(ksgu_avgWSPD30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_ws30_ci_d <- predict(ksgu_ws30_spline_d, interval = "confidence", level = 0.95)
ksgu_ws30_spline_10d <- lm(ksgu_avgWSPD30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_ws30_ci_10d <- predict(ksgu_ws30_spline_10d, interval = "confidence", level = 0.95)
ksgu_ws30_spline_m <- lm(ksgu_avgWSPD30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_ws30_ci_m <- predict(ksgu_ws30_spline_m, interval = "confidence", level = 0.95)

#Wind Gust Speed
ksgu_wg30_spline_d <- lm(ksgu_avgWGSP30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_wg30_ci_d <- predict(ksgu_wg30_spline_d, interval = "confidence", level = 0.95)
ksgu_wg30_spline_10d <- lm(ksgu_avgWGSP30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_wg30_ci_10d <- predict(ksgu_wg30_spline_10d, interval = "confidence", level = 0.95)
ksgu_wg30_spline_m <- lm(ksgu_avgWGSP30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_wg30_ci_m <- predict(ksgu_wg30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Ceiling Height
ksgu_cig30_spline_d <- lm(ksgu_avgCCIG30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_cig30_ci_d <- predict(ksgu_cig30_spline_d, interval = "confidence", level = 0.95)
ksgu_cig30_spline_10d <- lm(ksgu_avgCCIG30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_cig30_ci_10d <- predict(ksgu_cig30_spline_10d, interval = "confidence", level = 0.95)
ksgu_cig30_spline_m <- lm(ksgu_avgCCIG30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_cig30_ci_m <- predict(ksgu_cig30_spline_m, interval = "confidence", level = 0.95)

#Average Visibility
ksgu_vis30_spline_d <- lm(ksgu_avgVIS30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_vis30_ci_d <- predict(ksgu_vis30_spline_d, interval = "confidence", level = 0.95)
ksgu_vis30_spline_10d <- lm(ksgu_avgVIS30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_vis30_ci_10d <- predict(ksgu_vis30_spline_10d, interval = "confidence", level = 0.95)
ksgu_vis30_spline_m <- lm(ksgu_avgVIS30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_vis30_ci_m <- predict(ksgu_vis30_spline_m, interval = "confidence", level = 0.95)

#Average Cloud Cover
ksgu_cc30_spline_d <- lm(ksgu_avgCCOV30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_cc30_ci_d <- predict(ksgu_cc30_spline_d, interval = "confidence", level = 0.95)
ksgu_cc30_spline_10d <- lm(ksgu_avgCCOV30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_cc30_ci_10d <- predict(ksgu_cc30_spline_10d, interval = "confidence", level = 0.95)
ksgu_cc30_spline_m <- lm(ksgu_avgCCOV30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_cc30_ci_m <- predict(ksgu_cc30_spline_m, interval = "confidence", level = 0.95)

#Average Altimeter Setting
ksgu_alt30_spline_d <- lm(ksgu_avgALTS30~ns(c(1:366), df=364), ksgu_avgArray30)
ksgu_alt30_ci_d <- predict(ksgu_alt30_spline_d, interval = "confidence", level = 0.95)
ksgu_alt30_spline_10d <- lm(ksgu_avgALTS30~ns(c(1:366), df=35), ksgu_avgArray30)
ksgu_alt30_ci_10d <- predict(ksgu_alt30_spline_10d, interval = "confidence", level = 0.95)
ksgu_alt30_spline_m <- lm(ksgu_avgALTS30~ns(c(1:366), df=11), ksgu_avgArray30)
ksgu_alt30_ci_m <- predict(ksgu_alt30_spline_m, interval = "confidence", level = 0.95)

#### trends from 10-year to 10-year dataset
######################new section to include into rmd doc################################
###################this includes section 4 and on...#####################################
# need to find trend of data 30-20 20-10 and 10 yr data
# use the 10-day spline (df=35) to find how trends are changing for all variables at the locations


###### krdm

krdm_20test <- anti_join(krdm_toKeep20,krdm_toKeep10,by='OBSERVATIONTIME')
krdm_30test <- filter(krdm_toKeep30, date < "2004-01-01")

krdm_avgTemp202 <- c()
krdm_avgDPTemp202 <- c()
krdm_avgWDIR202 <- c()
krdm_avgWSPD202 <- c()
krdm_avgWGSP202 <- c()
krdm_avgCCIG202 <- c()
krdm_avgVIS202 <- c()
krdm_avgALTS202 <- c()
krdm_avgCCOV202 <- c()
krdm_precipData202 <- c()

krdm_avgTemp302 <- c()
krdm_avgDPTemp302 <- c()
krdm_avgWDIR302 <- c()
krdm_avgWSPD302 <- c()
krdm_avgWGSP302 <- c()
krdm_avgCCIG302 <- c()
krdm_avgVIS302 <- c()
krdm_avgALTS302 <- c()
krdm_avgCCOV302 <- c()
krdm_precipData302 <- c()

for(i in 1:366) {
  krdm_filtered2 <- filter(krdm_20test, JD == i)
  #krdm_precipFilt2 <- filter(krdm_20test$PRECIPAMOUNT1, JD == i)
  krdm_avgTemp202[i] <- mean(krdm_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  krdm_avgDPTemp202[i] <- mean(krdm_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  krdm_avgWDIR202[i] <- mean(krdm_filtered2$WINDDIRECTION, na.rm = TRUE)
  krdm_avgWSPD202[i] <- mean(krdm_filtered2$WINDSPEED, na.rm = TRUE)
  krdm_avgCCIG202[i] <- mean(krdm_filtered2$CLOUDCEILING, na.rm = TRUE)
  krdm_avgVIS202[i] <- mean(krdm_filtered2$VISIBILITY, na.rm = TRUE)
  krdm_avgWGSP202[i] <- mean(krdm_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  krdm_avgALTS202[i] <- mean(krdm_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  krdm_avgCCOV202[i] <- mean(krdm_filtered2$CLOUDCOVER, na.rm = TRUE)
  krdm_precipData202[i] <- sum(krdm_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  krdm_filtered3 <- filter(krdm_30test, JD == i)
  if(length(krdm_filtered3) == 0) {
    krdm_avgTemp302[i] <- 0
    krdm_avgDPTemp302[i] <- 0
    krdm_avgWDIR302[i] <- 0
    krdm_avgWSPD302[i] <- 0
    krdm_avgCCIG302[i] <- 0
    krdm_avgVIS302[i] <- 0
    krdm_avgWGSP302[i] <- 0
    krdm_avgALTS302[i] <- 0
    krdm_avgCCOV302[i] <- 0
    krdm_precipData302[i] <- 0
  } else {
    krdm_avgTemp302[i] <- mean(krdm_filtered3$AIRTEMPERATURE)
    krdm_avgDPTemp302[i] <- mean(krdm_filtered3$DEWPOINTTEMPERATURE)
    krdm_avgWDIR302[i] <- mean(krdm_filtered3$WINDDIRECTION)
    krdm_avgWSPD302[i] <- mean(krdm_filtered3$WINDSPEED)
    krdm_avgCCIG302[i] <- mean(krdm_filtered3$CLOUDCEILING)
    krdm_avgVIS302[i] <- mean(krdm_filtered3$VISIBILITY)
    krdm_avgWGSP302[i] <- mean(krdm_filtered3$WINDGUSTSPEED)
    krdm_avgALTS302[i] <- mean(krdm_filtered3$ALTIMETERSETTING)
    krdm_avgCCOV302[i] <- mean(krdm_filtered3$CLOUDCOVER)
    krdm_precipData302[i] <- sum(krdm_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
krdm_t202_spline_10d <- lm(krdm_avgTemp202~ns(c(1:366), df=35), krdm_20test)
krdm_t202_ci_10d <- predict(krdm_t202_spline_10d, interval = "confidence", level = 0.95)
krdm_t302_spline_10d <- lm(krdm_avgTemp302~ns(c(1:366), df=35), krdm_30test)
krdm_t302_ci_10d <- predict(krdm_t302_spline_10d, interval = "confidence", level = 0.95)

krdm_t_fv <- c()

krdm_t10 <- data.frame(transform(krdm_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_t10) <- c("FV","JD")
krdm_t20 <- data.frame(transform(krdm_t202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_t20) <- c("FV","JD")
krdm_t30 <- data.frame(transform(krdm_t302_spline_10d$fitted.values, JD = 1:length(krdm_t302_spline_10d$fitted.values)))
colnames(krdm_t30) <- c("FV","JD")

fv <- union(krdm_t10, krdm_t20)
fv_total <- union(fv, krdm_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
krdm_dp202_spline_10d <- lm(krdm_avgDPTemp20~ns(c(1:366), df=35), krdm_20test)
krdm_dp202_ci_10d <- predict(krdm_dp202_spline_10d, interval = "confidence", level = 0.95)
krdm_dp302_spline_10d <- lm(krdm_avgDPTemp30~ns(c(1:366), df=35), krdm_30test)
krdm_dp302_ci_10d <- predict(krdm_dp302_spline_10d, interval = "confidence", level = 0.95)

krdm_dp_fv <- c()

krdm_dp10 <- data.frame(transform(krdm_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_dp10) <- c("FV","JD")
krdm_dp20 <- data.frame(transform(krdm_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_dp20) <- c("FV","JD")
krdm_dp30 <- data.frame(transform(krdm_dp302_spline_10d$fitted.values, JD = 1:length(krdm_dp302_spline_10d$fitted.values)))
colnames(krdm_dp30) <- c("FV","JD")

fv <- union(krdm_dp10, krdm_dp20)
fv_total <- union(fv, krdm_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
krdm_wd302_spline_10d <- lm(krdm_avgWDIR30~ns(c(1:366), df=35), krdm_30test)
krdm_wd302_ci_10d <- predict(krdm_wd302_spline_10d, interval = "confidence", level = 0.95)
krdm_wd202_spline_10d <- lm(krdm_avgWDIR20~ns(c(1:366), df=35), krdm_20test)
krdm_wd202_ci_10d <- predict(krdm_wd202_spline_10d, interval = "confidence", level = 0.95)

krdm_wd_fv <- c()

krdm_wd10 <- data.frame(transform(krdm_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_wd10) <- c("FV","JD")
krdm_wd20 <- data.frame(transform(krdm_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_wd20) <- c("FV","JD")
krdm_wd30 <- data.frame(transform(krdm_wd302_spline_10d$fitted.values, JD = 1:length(krdm_wd302_spline_10d$fitted.values)))
colnames(krdm_wd30) <- c("FV","JD")

fv <- union(krdm_wd10, krdm_wd20)
fv_total <- union(fv, krdm_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
krdm_ws302_spline_10d <- lm(krdm_avgWSPD30~ns(c(1:366), df=35), krdm_30test)
krdm_ws302_ci_10d <- predict(krdm_ws302_spline_10d, interval = "confidence", level = 0.95)
krdm_ws202_spline_10d <- lm(krdm_avgWSPD20~ns(c(1:366), df=35), krdm_20test)
krdm_ws202_ci_10d <- predict(krdm_ws202_spline_10d, interval = "confidence", level = 0.95)

krdm_ws_fv <- c()

krdm_ws10 <- data.frame(transform(krdm_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_ws10) <- c("FV","JD")
krdm_ws20 <- data.frame(transform(krdm_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_ws20) <- c("FV","JD")
krdm_ws30 <- data.frame(transform(krdm_ws302_spline_10d$fitted.values, JD = 1:length(krdm_ws302_spline_10d$fitted.values)))
colnames(krdm_ws30) <- c("FV","JD")

fv <- union(krdm_ws10, krdm_ws20)
fv_total <- union(fv, krdm_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
krdm_wg302_spline_10d <- lm(krdm_avgWGSP30~ns(c(1:366), df=35), krdm_30test)
krdm_wg302_ci_10d <- predict(krdm_wg302_spline_10d, interval = "confidence", level = 0.95)
krdm_wg202_spline_10d <- lm(krdm_avgWGSP20~ns(c(1:366), df=35), krdm_20test)
krdm_wg202_ci_10d <- predict(krdm_wg202_spline_10d, interval = "confidence", level = 0.95)

krdm_wg_fv <- c()

krdm_wg10 <- data.frame(transform(krdm_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_wg10) <- c("FV","JD")
krdm_wg20 <- data.frame(transform(krdm_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_wg20) <- c("FV","JD")
krdm_wg30 <- data.frame(transform(krdm_wg302_spline_10d$fitted.values, JD = 1:length(krdm_wg302_spline_10d$fitted.values)))
colnames(krdm_wg30) <- c("FV","JD")

fv <- union(krdm_wg10, krdm_wg20)
fv_total <- union(fv, krdm_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
krdm_cig302_spline_10d <- lm(krdm_avgCCIG30~ns(c(1:366), df=35), krdm_30test)
krdm_cig302_ci_10d <- predict(krdm_cig302_spline_10d, interval = "confidence", level = 0.95)
krdm_cig202_spline_10d <- lm(krdm_avgCCIG20~ns(c(1:366), df=35), krdm_20test)
krdm_cig202_ci_10d <- predict(krdm_cig202_spline_10d, interval = "confidence", level = 0.95)

krdm_cig_fv <- c()

krdm_cig10 <- data.frame(transform(krdm_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_cig10) <- c("FV","JD")
krdm_cig20 <- data.frame(transform(krdm_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_cig20) <- c("FV","JD")
krdm_cig30 <- data.frame(transform(krdm_cig302_spline_10d$fitted.values, JD = 1:length(krdm_cig302_spline_10d$fitted.values)))
colnames(krdm_cig30) <- c("FV","JD")

fv <- union(krdm_cig10, krdm_cig20)
fv_total <- union(fv, krdm_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
krdm_vis302_spline_10d <- lm(krdm_avgVIS30~ns(c(1:366), df=35), krdm_30test)
krdm_vis302_ci_10d <- predict(krdm_vis302_spline_10d, interval = "confidence", level = 0.95)
krdm_vis202_spline_10d <- lm(krdm_avgVIS20~ns(c(1:366), df=35), krdm_20test)
krdm_vis202_ci_10d <- predict(krdm_vis202_spline_10d, interval = "confidence", level = 0.95)

krdm_vis_fv <- c()

krdm_vis10 <- data.frame(transform(krdm_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_vis10) <- c("FV","JD")
krdm_vis20 <- data.frame(transform(krdm_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_vis20) <- c("FV","JD")
krdm_vis30 <- data.frame(transform(krdm_vis302_spline_10d$fitted.values, JD = 1:length(krdm_vis302_spline_10d$fitted.values)))
colnames(krdm_vis30) <- c("FV","JD")

fv <- union(krdm_vis10, krdm_vis20)
fv_total <- union(fv, krdm_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
krdm_cc302_spline_10d <- lm(krdm_avgCCOV30~ns(c(1:366), df=35), krdm_30test)
krdm_cc302_ci_10d <- predict(krdm_cc302_spline_10d, interval = "confidence", level = 0.95)
krdm_cc202_spline_10d <- lm(krdm_avgCCOV20~ns(c(1:366), df=35), krdm_20test)
krdm_cc202_ci_10d <- predict(krdm_cc202_spline_10d, interval = "confidence", level = 0.95)

krdm_cc_fv <- c()

krdm_cc10 <- data.frame(transform(krdm_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_cc10) <- c("FV","JD")
krdm_cc20 <- data.frame(transform(krdm_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_cc20) <- c("FV","JD")
krdm_cc30 <- data.frame(transform(krdm_cc302_spline_10d$fitted.values, JD = 1:length(krdm_cc302_spline_10d$fitted.values)))
colnames(krdm_cc30) <- c("FV","JD")

fv <- union(krdm_cc10, krdm_cc20)
fv_total <- union(fv, krdm_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
krdm_alt302_spline_10d <- lm(krdm_avgALTS30~ns(c(1:366), df=35), krdm_30test)
krdm_alt302_ci_10d <- predict(krdm_alt302_spline_10d, interval = "confidence", level = 0.95)
krdm_alt202_spline_10d <- lm(krdm_avgALTS20~ns(c(1:366), df=35), krdm_20test)
krdm_alt202_ci_10d <- predict(krdm_alt202_spline_10d, interval = "confidence", level = 0.95)

krdm_alt_fv <- c()

krdm_alt10 <- data.frame(transform(krdm_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_alt10) <- c("FV","JD")
krdm_alt20 <- data.frame(transform(krdm_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(krdm_alt20) <- c("FV","JD")
krdm_alt30 <- data.frame(transform(krdm_alt302_spline_10d$fitted.values, JD = 1:length(krdm_alt302_spline_10d$fitted.values)))
colnames(krdm_alt30) <- c("FV","JD")

fv <- union(krdm_alt10, krdm_alt20)
fv_total <- union(fv, krdm_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average precipitation by rain event
krdm_p302_spline_10d <- lm(krdm_precipData302~ns(JD30, df=35), krdm_30test)
krdm_p302_ci_10d <- predict(krdm_p302_spline_10d, interval = "confidence", level = 0.95)
krdm_p202_spline_10d <- lm(krdm_precipData202~ns(JD20, df=35), krdm_20test)
krdm_p202_ci_10d <- predict(krdm_p202_spline_10d, interval = "confidence", level = 0.95)
krdm_p10_spline_10d <- lm(krdm_amountByRainEvent10~ns(JD10, df=35), krdm_avgArray10)
krdm_p10_ci_10d <- predict(krdm_p10_spline_10d, interval = "confidence", level = 0.95)

krdm_p_fv <- c()

krdm_p10 <- data.frame(transform(krdm_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(krdm_p10) <- c("FV","JD")
krdm_p20 <- data.frame(transform(krdm_p202_spline_10d$fitted.values, JD = c(1:366)))
colnames(krdm_p20) <- c("FV","JD")
krdm_p30 <- data.frame(transform(krdm_p302_spline_10d$fitted.values, JD = c(1:length(krdm_p302_spline_10d$fitted.values))))
colnames(krdm_p30) <- c("FV","JD")

fv <- union(krdm_p10, krdm_p20)
fv_total <- union(fv, krdm_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  krdm_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


###### kbuf

kbuf_20test <- anti_join(kbuf_toKeep20,kbuf_toKeep10,by='OBSERVATIONTIME')
kbuf_30test <- filter(kbuf_toKeep30, date < "2004-01-01")

kbuf_avgTemp202 <- c()
kbuf_avgDPTemp202 <- c()
kbuf_avgWDIR202 <- c()
kbuf_avgWSPD202 <- c()
kbuf_avgWGSP202 <- c()
kbuf_avgCCIG202 <- c()
kbuf_avgVIS202 <- c()
kbuf_avgALTS202 <- c()
kbuf_avgCCOV202 <- c()
kbuf_precipData202 <- c()

kbuf_avgTemp302 <- c()
kbuf_avgDPTemp302 <- c()
kbuf_avgWDIR302 <- c()
kbuf_avgWSPD302 <- c()
kbuf_avgWGSP302 <- c()
kbuf_avgCCIG302 <- c()
kbuf_avgVIS302 <- c()
kbuf_avgALTS302 <- c()
kbuf_avgCCOV302 <- c()
kbuf_precipData302 <- c()

for(i in 1:366) {
  kbuf_filtered2 <- filter(kbuf_20test, JD == i)
  #kbuf_precipFilt2 <- filter(kbuf_precip30, JD == i)
  kbuf_avgTemp202[i] <- mean(kbuf_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  kbuf_avgDPTemp202[i] <- mean(kbuf_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kbuf_avgWDIR202[i] <- mean(kbuf_filtered2$WINDDIRECTION, na.rm = TRUE)
  kbuf_avgWSPD202[i] <- mean(kbuf_filtered2$WINDSPEED, na.rm = TRUE)
  kbuf_avgCCIG202[i] <- mean(kbuf_filtered2$CLOUDCEILING, na.rm = TRUE)
  kbuf_avgVIS202[i] <- mean(kbuf_filtered2$VISIBILITY, na.rm = TRUE)
  kbuf_avgWGSP202[i] <- mean(kbuf_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  kbuf_avgALTS202[i] <- mean(kbuf_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  kbuf_avgCCOV202[i] <- mean(kbuf_filtered2$CLOUDCOVER, na.rm = TRUE)
  kbuf_precipData202[i] <- sum(kbuf_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  kbuf_filtered3 <- filter(kbuf_30test, JD == i)
  if(length(kbuf_filtered3) == 0) {
    kbuf_avgTemp302[i] <- 0
    kbuf_avgDPTemp302[i] <- 0
    kbuf_avgWDIR302[i] <- 0
    kbuf_avgWSPD302[i] <- 0
    kbuf_avgCCIG302[i] <- 0
    kbuf_avgVIS302[i] <- 0
    kbuf_avgWGSP302[i] <- 0
    kbuf_avgALTS302[i] <- 0
    kbuf_avgCCOV302[i] <- 0
    kbuf_precipData302[i] <- 0
  } else {
    kbuf_avgTemp302[i] <- mean(kbuf_filtered3$AIRTEMPERATURE)
    kbuf_avgDPTemp302[i] <- mean(kbuf_filtered3$DEWPOINTTEMPERATURE)
    kbuf_avgWDIR302[i] <- mean(kbuf_filtered3$WINDDIRECTION)
    kbuf_avgWSPD302[i] <- mean(kbuf_filtered3$WINDSPEED)
    kbuf_avgCCIG302[i] <- mean(kbuf_filtered3$CLOUDCEILING)
    kbuf_avgVIS302[i] <- mean(kbuf_filtered3$VISIBILITY)
    kbuf_avgWGSP302[i] <- mean(kbuf_filtered3$WINDGUSTSPEED)
    kbuf_avgALTS302[i] <- mean(kbuf_filtered3$ALTIMETERSETTING)
    kbuf_avgCCOV302[i] <- mean(kbuf_filtered3$CLOUDCOVER)
    kbuf_precipData302[i] <- sum(kbuf_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
kbuf_t202_spline_10d <- lm(kbuf_avgTemp202~ns(c(1:366), df=35), kbuf_20test)
kbuf_t202_ci_10d <- predict(kbuf_t202_spline_10d, interval = "confidence", level = 0.95)
kbuf_t302_spline_10d <- lm(kbuf_avgTemp302~ns(c(1:366), df=35), kbuf_30test)
kbuf_t302_ci_10d <- predict(kbuf_t302_spline_10d, interval = "confidence", level = 0.95)

kbuf_t_fv <- c()

kbuf_t10 <- data.frame(transform(kbuf_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_t10) <- c("FV","JD")
kbuf_t20 <- data.frame(transform(kbuf_t202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_t20) <- c("FV","JD")
kbuf_t30 <- data.frame(transform(kbuf_t302_spline_10d$fitted.values, JD = 1:length(kbuf_t302_spline_10d$fitted.values)))
colnames(kbuf_t30) <- c("FV","JD")

fv <- union(kbuf_t10, kbuf_t20)
fv_total <- union(fv, kbuf_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
kbuf_dp202_spline_10d <- lm(kbuf_avgDPTemp20~ns(c(1:366), df=35), kbuf_20test)
kbuf_dp202_ci_10d <- predict(kbuf_dp202_spline_10d, interval = "confidence", level = 0.95)
kbuf_dp302_spline_10d <- lm(kbuf_avgDPTemp30~ns(c(1:366), df=35), kbuf_30test)
kbuf_dp302_ci_10d <- predict(kbuf_dp302_spline_10d, interval = "confidence", level = 0.95)

kbuf_dp_fv <- c()

kbuf_dp10 <- data.frame(transform(kbuf_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_dp10) <- c("FV","JD")
kbuf_dp20 <- data.frame(transform(kbuf_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_dp20) <- c("FV","JD")
kbuf_dp30 <- data.frame(transform(kbuf_dp302_spline_10d$fitted.values, JD = 1:length(kbuf_dp302_spline_10d$fitted.values)))
colnames(kbuf_dp30) <- c("FV","JD")

fv <- union(kbuf_dp10, kbuf_dp20)
fv_total <- union(fv, kbuf_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
kbuf_wd302_spline_10d <- lm(kbuf_avgWDIR30~ns(c(1:366), df=35), kbuf_30test)
kbuf_wd302_ci_10d <- predict(kbuf_wd302_spline_10d, interval = "confidence", level = 0.95)
kbuf_wd202_spline_10d <- lm(kbuf_avgWDIR20~ns(c(1:366), df=35), kbuf_20test)
kbuf_wd202_ci_10d <- predict(kbuf_wd202_spline_10d, interval = "confidence", level = 0.95)

kbuf_wd_fv <- c()

kbuf_wd10 <- data.frame(transform(kbuf_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_wd10) <- c("FV","JD")
kbuf_wd20 <- data.frame(transform(kbuf_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_wd20) <- c("FV","JD")
kbuf_wd30 <- data.frame(transform(kbuf_wd302_spline_10d$fitted.values, JD = 1:length(kbuf_wd302_spline_10d$fitted.values)))
colnames(kbuf_wd30) <- c("FV","JD")

fv <- union(kbuf_wd10, kbuf_wd20)
fv_total <- union(fv, kbuf_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
kbuf_ws302_spline_10d <- lm(kbuf_avgWSPD30~ns(c(1:366), df=35), kbuf_30test)
kbuf_ws302_ci_10d <- predict(kbuf_ws302_spline_10d, interval = "confidence", level = 0.95)
kbuf_ws202_spline_10d <- lm(kbuf_avgWSPD20~ns(c(1:366), df=35), kbuf_20test)
kbuf_ws202_ci_10d <- predict(kbuf_ws202_spline_10d, interval = "confidence", level = 0.95)

kbuf_ws_fv <- c()

kbuf_ws10 <- data.frame(transform(kbuf_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_ws10) <- c("FV","JD")
kbuf_ws20 <- data.frame(transform(kbuf_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_ws20) <- c("FV","JD")
kbuf_ws30 <- data.frame(transform(kbuf_ws302_spline_10d$fitted.values, JD = 1:length(kbuf_ws302_spline_10d$fitted.values)))
colnames(kbuf_ws30) <- c("FV","JD")

fv <- union(kbuf_ws10, kbuf_ws20)
fv_total <- union(fv, kbuf_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
kbuf_wg302_spline_10d <- lm(kbuf_avgWGSP30~ns(c(1:366), df=35), kbuf_30test)
kbuf_wg302_ci_10d <- predict(kbuf_wg302_spline_10d, interval = "confidence", level = 0.95)
kbuf_wg202_spline_10d <- lm(kbuf_avgWGSP20~ns(c(1:366), df=35), kbuf_20test)
kbuf_wg202_ci_10d <- predict(kbuf_wg202_spline_10d, interval = "confidence", level = 0.95)

kbuf_wg_fv <- c()

kbuf_wg10 <- data.frame(transform(kbuf_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_wg10) <- c("FV","JD")
kbuf_wg20 <- data.frame(transform(kbuf_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_wg20) <- c("FV","JD")
kbuf_wg30 <- data.frame(transform(kbuf_wg302_spline_10d$fitted.values, JD = 1:length(kbuf_wg302_spline_10d$fitted.values)))
colnames(kbuf_wg30) <- c("FV","JD")

fv <- union(kbuf_wg10, kbuf_wg20)
fv_total <- union(fv, kbuf_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
kbuf_cig302_spline_10d <- lm(kbuf_avgCCIG30~ns(c(1:366), df=35), kbuf_30test)
kbuf_cig302_ci_10d <- predict(kbuf_cig302_spline_10d, interval = "confidence", level = 0.95)
kbuf_cig202_spline_10d <- lm(kbuf_avgCCIG20~ns(c(1:366), df=35), kbuf_20test)
kbuf_cig202_ci_10d <- predict(kbuf_cig202_spline_10d, interval = "confidence", level = 0.95)

kbuf_cig_fv <- c()

kbuf_cig10 <- data.frame(transform(kbuf_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_cig10) <- c("FV","JD")
kbuf_cig20 <- data.frame(transform(kbuf_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_cig20) <- c("FV","JD")
kbuf_cig30 <- data.frame(transform(kbuf_cig302_spline_10d$fitted.values, JD = 1:length(kbuf_cig302_spline_10d$fitted.values)))
colnames(kbuf_cig30) <- c("FV","JD")

fv <- union(kbuf_cig10, kbuf_cig20)
fv_total <- union(fv, kbuf_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
kbuf_vis302_spline_10d <- lm(kbuf_avgVIS30~ns(c(1:366), df=35), kbuf_30test)
kbuf_vis302_ci_10d <- predict(kbuf_vis302_spline_10d, interval = "confidence", level = 0.95)
kbuf_vis202_spline_10d <- lm(kbuf_avgVIS20~ns(c(1:366), df=35), kbuf_20test)
kbuf_vis202_ci_10d <- predict(kbuf_vis202_spline_10d, interval = "confidence", level = 0.95)

kbuf_vis_fv <- c()

kbuf_vis10 <- data.frame(transform(kbuf_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_vis10) <- c("FV","JD")
kbuf_vis20 <- data.frame(transform(kbuf_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_vis20) <- c("FV","JD")
kbuf_vis30 <- data.frame(transform(kbuf_vis302_spline_10d$fitted.values, JD = 1:length(kbuf_vis302_spline_10d$fitted.values)))
colnames(kbuf_vis30) <- c("FV","JD")

fv <- union(kbuf_vis10, kbuf_vis20)
fv_total <- union(fv, kbuf_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
kbuf_cc302_spline_10d <- lm(kbuf_avgCCOV30~ns(c(1:366), df=35), kbuf_30test)
kbuf_cc302_ci_10d <- predict(kbuf_cc302_spline_10d, interval = "confidence", level = 0.95)
kbuf_cc202_spline_10d <- lm(kbuf_avgCCOV20~ns(c(1:366), df=35), kbuf_20test)
kbuf_cc202_ci_10d <- predict(kbuf_cc202_spline_10d, interval = "confidence", level = 0.95)

kbuf_cc_fv <- c()

kbuf_cc10 <- data.frame(transform(kbuf_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_cc10) <- c("FV","JD")
kbuf_cc20 <- data.frame(transform(kbuf_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_cc20) <- c("FV","JD")
kbuf_cc30 <- data.frame(transform(kbuf_cc302_spline_10d$fitted.values, JD = 1:length(kbuf_cc302_spline_10d$fitted.values)))
colnames(kbuf_cc30) <- c("FV","JD")

fv <- union(kbuf_cc10, kbuf_cc20)
fv_total <- union(fv, kbuf_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
kbuf_alt302_spline_10d <- lm(kbuf_avgALTS30~ns(c(1:366), df=35), kbuf_30test)
kbuf_alt302_ci_10d <- predict(kbuf_alt302_spline_10d, interval = "confidence", level = 0.95)
kbuf_alt202_spline_10d <- lm(kbuf_avgALTS20~ns(c(1:366), df=35), kbuf_20test)
kbuf_alt202_ci_10d <- predict(kbuf_alt202_spline_10d, interval = "confidence", level = 0.95)

kbuf_alt_fv <- c()

kbuf_alt10 <- data.frame(transform(kbuf_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_alt10) <- c("FV","JD")
kbuf_alt20 <- data.frame(transform(kbuf_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_alt20) <- c("FV","JD")
kbuf_alt30 <- data.frame(transform(kbuf_alt302_spline_10d$fitted.values, JD = 1:length(kbuf_alt302_spline_10d$fitted.values)))
colnames(kbuf_alt30) <- c("FV","JD")

fv <- union(kbuf_alt10, kbuf_alt20)
fv_total <- union(fv, kbuf_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}


#Average precipitation
kbuf_p302_spline_10d <- lm(kbuf_precipData302~ns(JD30, df=35), kbuf_30test)
kbuf_p302_ci_10d <- predict(kbuf_p302_spline_10d, interval = "confidence", level = 0.95)
kbuf_p202_spline_10d <- lm(kbuf_precipData202~ns(JD20, df=35), kbuf_20test)
kbuf_p202_ci_10d <- predict(kbuf_p202_spline_10d, interval = "confidence", level = 0.95)
kbuf_p10_spline_10d <- lm(kbuf_amountByRainEvent10~ns(JD10, df=35), kbuf_avgArray10)
kbuf_p10_ci_10d <- predict(kbuf_p10_spline_10d, interval = "confidence", level = 0.95)

kbuf_p_fv <- c()

kbuf_p10 <- data.frame(transform(kbuf_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kbuf_p10) <- c("FV","JD")
kbuf_p20 <- data.frame(transform(kbuf_p202_spline_10d$fitted.values, JD = 1:366))
colnames(kbuf_p20) <- c("FV","JD")
kbuf_p30 <- data.frame(transform(kbuf_p302_spline_10d$fitted.values, JD = 1:length(kbuf_p302_spline_10d$fitted.values)))
colnames(kbuf_p30) <- c("FV","JD")

fv <- union(kbuf_p10, kbuf_p20)
fv_total <- union(fv, kbuf_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kbuf_p_fv[i] <- summary(slopes)$coefficients[2,1]
}

###### kfoe

kfoe_20test <- anti_join(kfoe_toKeep20,kfoe_toKeep10,by='OBSERVATIONTIME')
kfoe_30test <- filter(kfoe_toKeep30, date < "2004-01-01")

kfoe_avgTemp202 <- c()
kfoe_avgDPTemp202 <- c()
kfoe_avgWDIR202 <- c()
kfoe_avgWSPD202 <- c()
kfoe_avgWGSP202 <- c()
kfoe_avgCCIG202 <- c()
kfoe_avgVIS202 <- c()
kfoe_avgALTS202 <- c()
kfoe_avgCCOV202 <- c()
kfoe_precipData202 <- c()

kfoe_avgTemp302 <- c()
kfoe_avgDPTemp302 <- c()
kfoe_avgWDIR302 <- c()
kfoe_avgWSPD302 <- c()
kfoe_avgWGSP302 <- c()
kfoe_avgCCIG302 <- c()
kfoe_avgVIS302 <- c()
kfoe_avgALTS302 <- c()
kfoe_avgCCOV302 <- c()
kfoe_precipData302 <- c()

for(i in 1:366) {
  kfoe_filtered2 <- filter(kfoe_20test, JD == i)
  #kfoe_precipFilt2 <- filter(kfoe_precip30, JD == i)
  kfoe_avgTemp202[i] <- mean(kfoe_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  kfoe_avgDPTemp202[i] <- mean(kfoe_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kfoe_avgWDIR202[i] <- mean(kfoe_filtered2$WINDDIRECTION, na.rm = TRUE)
  kfoe_avgWSPD202[i] <- mean(kfoe_filtered2$WINDSPEED, na.rm = TRUE)
  kfoe_avgCCIG202[i] <- mean(kfoe_filtered2$CLOUDCEILING, na.rm = TRUE)
  kfoe_avgVIS202[i] <- mean(kfoe_filtered2$VISIBILITY, na.rm = TRUE)
  kfoe_avgWGSP202[i] <- mean(kfoe_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  kfoe_avgALTS202[i] <- mean(kfoe_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  kfoe_avgCCOV202[i] <- mean(kfoe_filtered2$CLOUDCOVER, na.rm = TRUE)
  kfoe_precipData202[i] <- sum(kfoe_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  kfoe_filtered3 <- filter(kfoe_30test, JD == i)
  if(length(kfoe_filtered3) == 0) {
    kfoe_avgTemp302[i] <- 0
    kfoe_avgDPTemp302[i] <- 0
    kfoe_avgWDIR302[i] <- 0
    kfoe_avgWSPD302[i] <- 0
    kfoe_avgCCIG302[i] <- 0
    kfoe_avgVIS302[i] <- 0
    kfoe_avgWGSP302[i] <- 0
    kfoe_avgALTS302[i] <- 0
    kfoe_avgCCOV302[i] <- 0
    kfoe_precipData302[i] <- 0
  } else {
    kfoe_avgTemp302[i] <- mean(kfoe_filtered3$AIRTEMPERATURE)
    kfoe_avgDPTemp302[i] <- mean(kfoe_filtered3$DEWPOINTTEMPERATURE)
    kfoe_avgWDIR302[i] <- mean(kfoe_filtered3$WINDDIRECTION)
    kfoe_avgWSPD302[i] <- mean(kfoe_filtered3$WINDSPEED)
    kfoe_avgCCIG302[i] <- mean(kfoe_filtered3$CLOUDCEILING)
    kfoe_avgVIS302[i] <- mean(kfoe_filtered3$VISIBILITY)
    kfoe_avgWGSP302[i] <- mean(kfoe_filtered3$WINDGUSTSPEED)
    kfoe_avgALTS302[i] <- mean(kfoe_filtered3$ALTIMETERSETTING)
    kfoe_avgCCOV302[i] <- mean(kfoe_filtered3$CLOUDCOVER)
    kfoe_precipData302[i] <- sum(kfoe_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
kfoe_t202_spline_10d <- lm(kfoe_avgTemp202~ns(c(1:366), df=35), kfoe_20test)
kfoe_t202_ci_10d <- predict(kfoe_t202_spline_10d, interval = "confidence", level = 0.95)
kfoe_t302_spline_10d <- lm(kfoe_avgTemp302~ns(c(1:366), df=35), kfoe_30test)
kfoe_t302_ci_10d <- predict(kfoe_t302_spline_10d, interval = "confidence", level = 0.95)

kfoe_t_fv <- c()

kfoe_t10 <- data.frame(transform(kfoe_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_t10) <- c("FV","JD")
kfoe_t20 <- data.frame(transform(kfoe_t202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_t20) <- c("FV","JD")
kfoe_t30 <- data.frame(transform(kfoe_t302_spline_10d$fitted.values, JD = 1:length(kfoe_t302_spline_10d$fitted.values)))
colnames(kfoe_t30) <- c("FV","JD")

fv <- union(kfoe_t10, kfoe_t20)
fv_total <- union(fv, kfoe_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
kfoe_dp202_spline_10d <- lm(kfoe_avgDPTemp20~ns(c(1:366), df=35), kfoe_20test)
kfoe_dp202_ci_10d <- predict(kfoe_dp202_spline_10d, interval = "confidence", level = 0.95)
kfoe_dp302_spline_10d <- lm(kfoe_avgDPTemp30~ns(c(1:366), df=35), kfoe_30test)
kfoe_dp302_ci_10d <- predict(kfoe_dp302_spline_10d, interval = "confidence", level = 0.95)

kfoe_dp_fv <- c()

kfoe_dp10 <- data.frame(transform(kfoe_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_dp10) <- c("FV","JD")
kfoe_dp20 <- data.frame(transform(kfoe_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_dp20) <- c("FV","JD")
kfoe_dp30 <- data.frame(transform(kfoe_dp302_spline_10d$fitted.values, JD = 1:length(kfoe_dp302_spline_10d$fitted.values)))
colnames(kfoe_dp30) <- c("FV","JD")

fv <- union(kfoe_dp10, kfoe_dp20)
fv_total <- union(fv, kfoe_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
kfoe_wd302_spline_10d <- lm(kfoe_avgWDIR30~ns(c(1:366), df=35), kfoe_30test)
kfoe_wd302_ci_10d <- predict(kfoe_wd302_spline_10d, interval = "confidence", level = 0.95)
kfoe_wd202_spline_10d <- lm(kfoe_avgWDIR20~ns(c(1:366), df=35), kfoe_20test)
kfoe_wd202_ci_10d <- predict(kfoe_wd202_spline_10d, interval = "confidence", level = 0.95)

kfoe_wd_fv <- c()

kfoe_wd10 <- data.frame(transform(kfoe_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_wd10) <- c("FV","JD")
kfoe_wd20 <- data.frame(transform(kfoe_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_wd20) <- c("FV","JD")
kfoe_wd30 <- data.frame(transform(kfoe_wd302_spline_10d$fitted.values, JD = 1:length(kfoe_wd302_spline_10d$fitted.values)))
colnames(kfoe_wd30) <- c("FV","JD")

fv <- union(kfoe_wd10, kfoe_wd20)
fv_total <- union(fv, kfoe_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
kfoe_ws302_spline_10d <- lm(kfoe_avgWSPD30~ns(c(1:366), df=35), kfoe_30test)
kfoe_ws302_ci_10d <- predict(kfoe_ws302_spline_10d, interval = "confidence", level = 0.95)
kfoe_ws202_spline_10d <- lm(kfoe_avgWSPD20~ns(c(1:366), df=35), kfoe_20test)
kfoe_ws202_ci_10d <- predict(kfoe_ws202_spline_10d, interval = "confidence", level = 0.95)

kfoe_ws_fv <- c()

kfoe_ws10 <- data.frame(transform(kfoe_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_ws10) <- c("FV","JD")
kfoe_ws20 <- data.frame(transform(kfoe_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_ws20) <- c("FV","JD")
kfoe_ws30 <- data.frame(transform(kfoe_ws302_spline_10d$fitted.values, JD = 1:length(kfoe_ws302_spline_10d$fitted.values)))
colnames(kfoe_ws30) <- c("FV","JD")

fv <- union(kfoe_ws10, kfoe_ws20)
fv_total <- union(fv, kfoe_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
kfoe_wg302_spline_10d <- lm(kfoe_avgWGSP30~ns(c(1:366), df=35), kfoe_30test)
kfoe_wg302_ci_10d <- predict(kfoe_wg302_spline_10d, interval = "confidence", level = 0.95)
kfoe_wg202_spline_10d <- lm(kfoe_avgWGSP20~ns(c(1:366), df=35), kfoe_20test)
kfoe_wg202_ci_10d <- predict(kfoe_wg202_spline_10d, interval = "confidence", level = 0.95)

kfoe_wg_fv <- c()

kfoe_wg10 <- data.frame(transform(kfoe_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_wg10) <- c("FV","JD")
kfoe_wg20 <- data.frame(transform(kfoe_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_wg20) <- c("FV","JD")
kfoe_wg30 <- data.frame(transform(kfoe_wg302_spline_10d$fitted.values, JD = 1:length(kfoe_wg302_spline_10d$fitted.values)))
colnames(kfoe_wg30) <- c("FV","JD")

fv <- union(kfoe_wg10, kfoe_wg20)
fv_total <- union(fv, kfoe_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
kfoe_cig302_spline_10d <- lm(kfoe_avgCCIG30~ns(c(1:366), df=35), kfoe_30test)
kfoe_cig302_ci_10d <- predict(kfoe_cig302_spline_10d, interval = "confidence", level = 0.95)
kfoe_cig202_spline_10d <- lm(kfoe_avgCCIG20~ns(c(1:366), df=35), kfoe_20test)
kfoe_cig202_ci_10d <- predict(kfoe_cig202_spline_10d, interval = "confidence", level = 0.95)

kfoe_cig_fv <- c()

kfoe_cig10 <- data.frame(transform(kfoe_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_cig10) <- c("FV","JD")
kfoe_cig20 <- data.frame(transform(kfoe_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_cig20) <- c("FV","JD")
kfoe_cig30 <- data.frame(transform(kfoe_cig302_spline_10d$fitted.values, JD = 1:length(kfoe_cig302_spline_10d$fitted.values)))
colnames(kfoe_cig30) <- c("FV","JD")

fv <- union(kfoe_cig10, kfoe_cig20)
fv_total <- union(fv, kfoe_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
kfoe_vis302_spline_10d <- lm(kfoe_avgVIS30~ns(c(1:366), df=35), kfoe_30test)
kfoe_vis302_ci_10d <- predict(kfoe_vis302_spline_10d, interval = "confidence", level = 0.95)
kfoe_vis202_spline_10d <- lm(kfoe_avgVIS20~ns(c(1:366), df=35), kfoe_20test)
kfoe_vis202_ci_10d <- predict(kfoe_vis202_spline_10d, interval = "confidence", level = 0.95)

kfoe_vis_fv <- c()

kfoe_vis10 <- data.frame(transform(kfoe_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_vis10) <- c("FV","JD")
kfoe_vis20 <- data.frame(transform(kfoe_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_vis20) <- c("FV","JD")
kfoe_vis30 <- data.frame(transform(kfoe_vis302_spline_10d$fitted.values, JD = 1:length(kfoe_vis302_spline_10d$fitted.values)))
colnames(kfoe_vis30) <- c("FV","JD")

fv <- union(kfoe_vis10, kfoe_vis20)
fv_total <- union(fv, kfoe_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
kfoe_cc302_spline_10d <- lm(kfoe_avgCCOV30~ns(c(1:366), df=35), kfoe_30test)
kfoe_cc302_ci_10d <- predict(kfoe_cc302_spline_10d, interval = "confidence", level = 0.95)
kfoe_cc202_spline_10d <- lm(kfoe_avgCCOV20~ns(c(1:366), df=35), kfoe_20test)
kfoe_cc202_ci_10d <- predict(kfoe_cc202_spline_10d, interval = "confidence", level = 0.95)

kfoe_cc_fv <- c()

kfoe_cc10 <- data.frame(transform(kfoe_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_cc10) <- c("FV","JD")
kfoe_cc20 <- data.frame(transform(kfoe_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_cc20) <- c("FV","JD")
kfoe_cc30 <- data.frame(transform(kfoe_cc302_spline_10d$fitted.values, JD = 1:length(kfoe_cc302_spline_10d$fitted.values)))
colnames(kfoe_cc30) <- c("FV","JD")

fv <- union(kfoe_cc10, kfoe_cc20)
fv_total <- union(fv, kfoe_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
kfoe_alt302_spline_10d <- lm(kfoe_avgALTS30~ns(c(1:366), df=35), kfoe_30test)
kfoe_alt302_ci_10d <- predict(kfoe_alt302_spline_10d, interval = "confidence", level = 0.95)
kfoe_alt202_spline_10d <- lm(kfoe_avgALTS20~ns(c(1:366), df=35), kfoe_20test)
kfoe_alt202_ci_10d <- predict(kfoe_alt202_spline_10d, interval = "confidence", level = 0.95)

kfoe_alt_fv <- c()

kfoe_alt10 <- data.frame(transform(kfoe_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_alt10) <- c("FV","JD")
kfoe_alt20 <- data.frame(transform(kfoe_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_alt20) <- c("FV","JD")
kfoe_alt30 <- data.frame(transform(kfoe_alt302_spline_10d$fitted.values, JD = 1:length(kfoe_alt302_spline_10d$fitted.values)))
colnames(kfoe_alt30) <- c("FV","JD")

fv <- union(kfoe_alt10, kfoe_alt20)
fv_total <- union(fv, kfoe_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average precipitation
kfoe_p302_spline_10d <- lm(kfoe_precipData302~ns(JD30, df=35), kfoe_30test)
kfoe_p302_ci_10d <- predict(kfoe_p302_spline_10d, interval = "confidence", level = 0.95)
kfoe_p202_spline_10d <- lm(kfoe_precipData202~ns(JD20, df=35), kfoe_20test)
kfoe_p202_ci_10d <- predict(kfoe_p202_spline_10d, interval = "confidence", level = 0.95)
kfoe_p10_spline_10d <- lm(kfoe_amountByRainEvent10~ns(JD10, df=35), kfoe_avgArray10)
kfoe_p10_ci_10d <- predict(kfoe_p10_spline_10d, interval = "confidence", level = 0.95)

kfoe_p_fv <- c()

kfoe_p10 <- data.frame(transform(kfoe_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kfoe_p10) <- c("FV","JD")
kfoe_p20 <- data.frame(transform(kfoe_p202_spline_10d$fitted.values, JD = 1:366))
colnames(kfoe_p20) <- c("FV","JD")
kfoe_p30 <- data.frame(transform(kfoe_p302_spline_10d$fitted.values, JD = 1:length(kfoe_p302_spline_10d$fitted.values)))
colnames(kfoe_p30) <- c("FV","JD")

fv <- union(kfoe_p10, kfoe_p20)
fv_total <- union(fv, kfoe_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kfoe_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


###### kmsn

kmsn_20test <- anti_join(kmsn_toKeep20,kmsn_toKeep10,by='OBSERVATIONTIME')
kmsn_30test <- filter(kmsn_toKeep30, date < "2004-01-01")

kmsn_avgTemp202 <- c()
kmsn_avgDPTemp202 <- c()
kmsn_avgWDIR202 <- c()
kmsn_avgWSPD202 <- c()
kmsn_avgWGSP202 <- c()
kmsn_avgCCIG202 <- c()
kmsn_avgVIS202 <- c()
kmsn_avgALTS202 <- c()
kmsn_avgCCOV202 <- c()
kmsn_precipData202 <- c()

kmsn_avgTemp302 <- c()
kmsn_avgDPTemp302 <- c()
kmsn_avgWDIR302 <- c()
kmsn_avgWSPD302 <- c()
kmsn_avgWGSP302 <- c()
kmsn_avgCCIG302 <- c()
kmsn_avgVIS302 <- c()
kmsn_avgALTS302 <- c()
kmsn_avgCCOV302 <- c()
kmsn_precipData302 <- c()

for(i in 1:366) {
  kmsn_filtered2 <- filter(kmsn_20test, JD == i)
  #kmsn_precipFilt2 <- filter(kmsn_precip30, JD == i)
  kmsn_avgTemp202[i] <- mean(kmsn_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  kmsn_avgDPTemp202[i] <- mean(kmsn_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kmsn_avgWDIR202[i] <- mean(kmsn_filtered2$WINDDIRECTION, na.rm = TRUE)
  kmsn_avgWSPD202[i] <- mean(kmsn_filtered2$WINDSPEED, na.rm = TRUE)
  kmsn_avgCCIG202[i] <- mean(kmsn_filtered2$CLOUDCEILING, na.rm = TRUE)
  kmsn_avgVIS202[i] <- mean(kmsn_filtered2$VISIBILITY, na.rm = TRUE)
  kmsn_avgWGSP202[i] <- mean(kmsn_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  kmsn_avgALTS202[i] <- mean(kmsn_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  kmsn_avgCCOV202[i] <- mean(kmsn_filtered2$CLOUDCOVER, na.rm = TRUE)
  kmsn_precipData202[i] <- sum(kmsn_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  kmsn_filtered3 <- filter(kmsn_30test, JD == i)
  if(length(kmsn_filtered3) == 0) {
    kmsn_avgTemp302[i] <- 0
    kmsn_avgDPTemp302[i] <- 0
    kmsn_avgWDIR302[i] <- 0
    kmsn_avgWSPD302[i] <- 0
    kmsn_avgCCIG302[i] <- 0
    kmsn_avgVIS302[i] <- 0
    kmsn_avgWGSP302[i] <- 0
    kmsn_avgALTS302[i] <- 0
    kmsn_avgCCOV302[i] <- 0
    kmsn_precipData302[i] <- 0
  } else {
    kmsn_avgTemp302[i] <- mean(kmsn_filtered3$AIRTEMPERATURE)
    kmsn_avgDPTemp302[i] <- mean(kmsn_filtered3$DEWPOINTTEMPERATURE)
    kmsn_avgWDIR302[i] <- mean(kmsn_filtered3$WINDDIRECTION)
    kmsn_avgWSPD302[i] <- mean(kmsn_filtered3$WINDSPEED)
    kmsn_avgCCIG302[i] <- mean(kmsn_filtered3$CLOUDCEILING)
    kmsn_avgVIS302[i] <- mean(kmsn_filtered3$VISIBILITY)
    kmsn_avgWGSP302[i] <- mean(kmsn_filtered3$WINDGUSTSPEED)
    kmsn_avgALTS302[i] <- mean(kmsn_filtered3$ALTIMETERSETTING)
    kmsn_avgCCOV302[i] <- mean(kmsn_filtered3$CLOUDCOVER)
    kmsn_precipData302[i] <- sum(kmsn_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
kmsn_t202_spline_10d <- lm(kmsn_avgTemp202~ns(c(1:366), df=35), kmsn_20test)
kmsn_t202_ci_10d <- predict(kmsn_t202_spline_10d, interval = "confidence", level = 0.95)
kmsn_t302_spline_10d <- lm(kmsn_avgTemp302~ns(c(1:366), df=35), kmsn_30test)
kmsn_t302_ci_10d <- predict(kmsn_t302_spline_10d, interval = "confidence", level = 0.95)

kmsn_t_fv <- c()

kmsn_t10 <- data.frame(transform(kmsn_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_t10) <- c("FV","JD")
kmsn_t20 <- data.frame(transform(kmsn_t202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_t20) <- c("FV","JD")
kmsn_t30 <- data.frame(transform(kmsn_t302_spline_10d$fitted.values, JD = 1:length(kmsn_t302_spline_10d$fitted.values)))
colnames(kmsn_t30) <- c("FV","JD")

fv <- union(kmsn_t10, kmsn_t20)
fv_total <- union(fv, kmsn_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
kmsn_dp202_spline_10d <- lm(kmsn_avgDPTemp20~ns(c(1:366), df=35), kmsn_20test)
kmsn_dp202_ci_10d <- predict(kmsn_dp202_spline_10d, interval = "confidence", level = 0.95)
kmsn_dp302_spline_10d <- lm(kmsn_avgDPTemp30~ns(c(1:366), df=35), kmsn_30test)
kmsn_dp302_ci_10d <- predict(kmsn_dp302_spline_10d, interval = "confidence", level = 0.95)

kmsn_dp_fv <- c()

kmsn_dp10 <- data.frame(transform(kmsn_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_dp10) <- c("FV","JD")
kmsn_dp20 <- data.frame(transform(kmsn_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_dp20) <- c("FV","JD")
kmsn_dp30 <- data.frame(transform(kmsn_dp302_spline_10d$fitted.values, JD = 1:length(kmsn_dp302_spline_10d$fitted.values)))
colnames(kmsn_dp30) <- c("FV","JD")

fv <- union(kmsn_dp10, kmsn_dp20)
fv_total <- union(fv, kmsn_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
kmsn_wd302_spline_10d <- lm(kmsn_avgWDIR30~ns(c(1:366), df=35), kmsn_30test)
kmsn_wd302_ci_10d <- predict(kmsn_wd302_spline_10d, interval = "confidence", level = 0.95)
kmsn_wd202_spline_10d <- lm(kmsn_avgWDIR20~ns(c(1:366), df=35), kmsn_20test)
kmsn_wd202_ci_10d <- predict(kmsn_wd202_spline_10d, interval = "confidence", level = 0.95)

kmsn_wd_fv <- c()

kmsn_wd10 <- data.frame(transform(kmsn_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_wd10) <- c("FV","JD")
kmsn_wd20 <- data.frame(transform(kmsn_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_wd20) <- c("FV","JD")
kmsn_wd30 <- data.frame(transform(kmsn_wd302_spline_10d$fitted.values, JD = 1:length(kmsn_wd302_spline_10d$fitted.values)))
colnames(kmsn_wd30) <- c("FV","JD")

fv <- union(kmsn_wd10, kmsn_wd20)
fv_total <- union(fv, kmsn_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
kmsn_ws302_spline_10d <- lm(kmsn_avgWSPD30~ns(c(1:366), df=35), kmsn_30test)
kmsn_ws302_ci_10d <- predict(kmsn_ws302_spline_10d, interval = "confidence", level = 0.95)
kmsn_ws202_spline_10d <- lm(kmsn_avgWSPD20~ns(c(1:366), df=35), kmsn_20test)
kmsn_ws202_ci_10d <- predict(kmsn_ws202_spline_10d, interval = "confidence", level = 0.95)

kmsn_ws_fv <- c()

kmsn_ws10 <- data.frame(transform(kmsn_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_ws10) <- c("FV","JD")
kmsn_ws20 <- data.frame(transform(kmsn_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_ws20) <- c("FV","JD")
kmsn_ws30 <- data.frame(transform(kmsn_ws302_spline_10d$fitted.values, JD = 1:length(kmsn_ws302_spline_10d$fitted.values)))
colnames(kmsn_ws30) <- c("FV","JD")

fv <- union(kmsn_ws10, kmsn_ws20)
fv_total <- union(fv, kmsn_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
kmsn_wg302_spline_10d <- lm(kmsn_avgWGSP30~ns(c(1:366), df=35), kmsn_30test)
kmsn_wg302_ci_10d <- predict(kmsn_wg302_spline_10d, interval = "confidence", level = 0.95)
kmsn_wg202_spline_10d <- lm(kmsn_avgWGSP20~ns(c(1:366), df=35), kmsn_20test)
kmsn_wg202_ci_10d <- predict(kmsn_wg202_spline_10d, interval = "confidence", level = 0.95)

kmsn_wg_fv <- c()

kmsn_wg10 <- data.frame(transform(kmsn_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_wg10) <- c("FV","JD")
kmsn_wg20 <- data.frame(transform(kmsn_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_wg20) <- c("FV","JD")
kmsn_wg30 <- data.frame(transform(kmsn_wg302_spline_10d$fitted.values, JD = 1:length(kmsn_wg302_spline_10d$fitted.values)))
colnames(kmsn_wg30) <- c("FV","JD")

fv <- union(kmsn_wg10, kmsn_wg20)
fv_total <- union(fv, kmsn_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
kmsn_cig302_spline_10d <- lm(kmsn_avgCCIG30~ns(c(1:366), df=35), kmsn_30test)
kmsn_cig302_ci_10d <- predict(kmsn_cig302_spline_10d, interval = "confidence", level = 0.95)
kmsn_cig202_spline_10d <- lm(kmsn_avgCCIG20~ns(c(1:366), df=35), kmsn_20test)
kmsn_cig202_ci_10d <- predict(kmsn_cig202_spline_10d, interval = "confidence", level = 0.95)

kmsn_cig_fv <- c()

kmsn_cig10 <- data.frame(transform(kmsn_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_cig10) <- c("FV","JD")
kmsn_cig20 <- data.frame(transform(kmsn_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_cig20) <- c("FV","JD")
kmsn_cig30 <- data.frame(transform(kmsn_cig302_spline_10d$fitted.values, JD = 1:length(kmsn_cig302_spline_10d$fitted.values)))
colnames(kmsn_cig30) <- c("FV","JD")

fv <- union(kmsn_cig10, kmsn_cig20)
fv_total <- union(fv, kmsn_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
kmsn_vis302_spline_10d <- lm(kmsn_avgVIS30~ns(c(1:366), df=35), kmsn_30test)
kmsn_vis302_ci_10d <- predict(kmsn_vis302_spline_10d, interval = "confidence", level = 0.95)
kmsn_vis202_spline_10d <- lm(kmsn_avgVIS20~ns(c(1:366), df=35), kmsn_20test)
kmsn_vis202_ci_10d <- predict(kmsn_vis202_spline_10d, interval = "confidence", level = 0.95)

kmsn_vis_fv <- c()

kmsn_vis10 <- data.frame(transform(kmsn_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_vis10) <- c("FV","JD")
kmsn_vis20 <- data.frame(transform(kmsn_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_vis20) <- c("FV","JD")
kmsn_vis30 <- data.frame(transform(kmsn_vis302_spline_10d$fitted.values, JD = 1:length(kmsn_vis302_spline_10d$fitted.values)))
colnames(kmsn_vis30) <- c("FV","JD")

fv <- union(kmsn_vis10, kmsn_vis20)
fv_total <- union(fv, kmsn_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
kmsn_cc302_spline_10d <- lm(kmsn_avgCCOV30~ns(c(1:366), df=35), kmsn_30test)
kmsn_cc302_ci_10d <- predict(kmsn_cc302_spline_10d, interval = "confidence", level = 0.95)
kmsn_cc202_spline_10d <- lm(kmsn_avgCCOV20~ns(c(1:366), df=35), kmsn_20test)
kmsn_cc202_ci_10d <- predict(kmsn_cc202_spline_10d, interval = "confidence", level = 0.95)

kmsn_cc_fv <- c()

kmsn_cc10 <- data.frame(transform(kmsn_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_cc10) <- c("FV","JD")
kmsn_cc20 <- data.frame(transform(kmsn_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_cc20) <- c("FV","JD")
kmsn_cc30 <- data.frame(transform(kmsn_cc302_spline_10d$fitted.values, JD = 1:length(kmsn_cc302_spline_10d$fitted.values)))
colnames(kmsn_cc30) <- c("FV","JD")

fv <- union(kmsn_cc10, kmsn_cc20)
fv_total <- union(fv, kmsn_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
kmsn_alt302_spline_10d <- lm(kmsn_avgALTS30~ns(c(1:366), df=35), kmsn_30test)
kmsn_alt302_ci_10d <- predict(kmsn_alt302_spline_10d, interval = "confidence", level = 0.95)
kmsn_alt202_spline_10d <- lm(kmsn_avgALTS20~ns(c(1:366), df=35), kmsn_20test)
kmsn_alt202_ci_10d <- predict(kmsn_alt202_spline_10d, interval = "confidence", level = 0.95)

kmsn_alt_fv <- c()

kmsn_alt10 <- data.frame(transform(kmsn_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_alt10) <- c("FV","JD")
kmsn_alt20 <- data.frame(transform(kmsn_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_alt20) <- c("FV","JD")
kmsn_alt30 <- data.frame(transform(kmsn_alt302_spline_10d$fitted.values, JD = 1:length(kmsn_alt302_spline_10d$fitted.values)))
colnames(kmsn_alt30) <- c("FV","JD")

fv <- union(kmsn_alt10, kmsn_alt20)
fv_total <- union(fv, kmsn_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}


#Average precipitation
kmsn_p302_spline_10d <- lm(kmsn_precipData302~ns(JD30, df=35), kmsn_30test)
kmsn_p302_ci_10d <- predict(kmsn_p302_spline_10d, interval = "confidence", level = 0.95)
kmsn_p202_spline_10d <- lm(kmsn_precipData202~ns(JD20, df=35), kmsn_20test)
kmsn_p202_ci_10d <- predict(kmsn_p202_spline_10d, interval = "confidence", level = 0.95)
kmsn_p10_spline_10d <- lm(kmsn_amountByRainEvent10~ns(JD10, df=35), kmsn_avgArray10)
kmsn_p10_ci_10d <- predict(kmsn_p10_spline_10d, interval = "confidence", level = 0.95)

kmsn_p_fv <- c()

kmsn_p10 <- data.frame(transform(kmsn_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kmsn_p10) <- c("FV","JD")
kmsn_p20 <- data.frame(transform(kmsn_p202_spline_10d$fitted.values, JD = 1:366))
colnames(kmsn_p20) <- c("FV","JD")
kmsn_p30 <- data.frame(transform(kmsn_p302_spline_10d$fitted.values, JD = 1:length(kmsn_p302_spline_10d$fitted.values)))
colnames(kmsn_p30) <- c("FV","JD")

fv <- union(kmsn_p10, kmsn_p20)
fv_total <- union(fv, kmsn_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kmsn_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


###### ktri

ktri_20test <- anti_join(ktri_toKeep20,ktri_toKeep10,by='OBSERVATIONTIME')
ktri_30test <- filter(ktri_toKeep30, date < "2004-01-01")

ktri_avgTemp202 <- c()
ktri_avgDPTemp202 <- c()
ktri_avgWDIR202 <- c()
ktri_avgWSPD202 <- c()
ktri_avgWGSP202 <- c()
ktri_avgCCIG202 <- c()
ktri_avgVIS202 <- c()
ktri_avgALTS202 <- c()
ktri_avgCCOV202 <- c()
ktri_precipData202 <- c()

ktri_avgTemp302 <- c()
ktri_avgDPTemp302 <- c()
ktri_avgWDIR302 <- c()
ktri_avgWSPD302 <- c()
ktri_avgWGSP302 <- c()
ktri_avgCCIG302 <- c()
ktri_avgVIS302 <- c()
ktri_avgALTS302 <- c()
ktri_avgCCOV302 <- c()
ktri_precipData302 <- c()

for(i in 1:366) {
  ktri_filtered2 <- filter(ktri_20test, JD == i)
  #ktri_precipFilt2 <- filter(ktri_precip30, JD == i)
  ktri_avgTemp202[i] <- mean(ktri_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  ktri_avgDPTemp202[i] <- mean(ktri_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ktri_avgWDIR202[i] <- mean(ktri_filtered2$WINDDIRECTION, na.rm = TRUE)
  ktri_avgWSPD202[i] <- mean(ktri_filtered2$WINDSPEED, na.rm = TRUE)
  ktri_avgCCIG202[i] <- mean(ktri_filtered2$CLOUDCEILING, na.rm = TRUE)
  ktri_avgVIS202[i] <- mean(ktri_filtered2$VISIBILITY, na.rm = TRUE)
  ktri_avgWGSP202[i] <- mean(ktri_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  ktri_avgALTS202[i] <- mean(ktri_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  ktri_avgCCOV202[i] <- mean(ktri_filtered2$CLOUDCOVER, na.rm = TRUE)
  ktri_precipData202[i] <- sum(ktri_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  ktri_filtered3 <- filter(ktri_30test, JD == i)
  if(length(ktri_filtered3) == 0) {
    ktri_avgTemp302[i] <- 0
    ktri_avgDPTemp302[i] <- 0
    ktri_avgWDIR302[i] <- 0
    ktri_avgWSPD302[i] <- 0
    ktri_avgCCIG302[i] <- 0
    ktri_avgVIS302[i] <- 0
    ktri_avgWGSP302[i] <- 0
    ktri_avgALTS302[i] <- 0
    ktri_avgCCOV302[i] <- 0
    ktri_precipData302[i] <- 0
  } else {
    ktri_avgTemp302[i] <- mean(ktri_filtered3$AIRTEMPERATURE)
    ktri_avgDPTemp302[i] <- mean(ktri_filtered3$DEWPOINTTEMPERATURE)
    ktri_avgWDIR302[i] <- mean(ktri_filtered3$WINDDIRECTION)
    ktri_avgWSPD302[i] <- mean(ktri_filtered3$WINDSPEED)
    ktri_avgCCIG302[i] <- mean(ktri_filtered3$CLOUDCEILING)
    ktri_avgVIS302[i] <- mean(ktri_filtered3$VISIBILITY)
    ktri_avgWGSP302[i] <- mean(ktri_filtered3$WINDGUSTSPEED)
    ktri_avgALTS302[i] <- mean(ktri_filtered3$ALTIMETERSETTING)
    ktri_avgCCOV302[i] <- mean(ktri_filtered3$CLOUDCOVER)
    ktri_precipData302[i] <- sum(ktri_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
ktri_t202_spline_10d <- lm(ktri_avgTemp202~ns(c(1:366), df=35), ktri_20test)
ktri_t202_ci_10d <- predict(ktri_t202_spline_10d, interval = "confidence", level = 0.95)
ktri_t302_spline_10d <- lm(ktri_avgTemp302~ns(c(1:366), df=35), ktri_30test)
ktri_t302_ci_10d <- predict(ktri_t302_spline_10d, interval = "confidence", level = 0.95)

ktri_t_fv <- c()

ktri_t10 <- data.frame(transform(ktri_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_t10) <- c("FV","JD")
ktri_t20 <- data.frame(transform(ktri_t202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_t20) <- c("FV","JD")
ktri_t30 <- data.frame(transform(ktri_t302_spline_10d$fitted.values, JD = 1:length(ktri_t302_spline_10d$fitted.values)))
colnames(ktri_t30) <- c("FV","JD")

fv <- union(ktri_t10, ktri_t20)
fv_total <- union(fv, ktri_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
ktri_dp202_spline_10d <- lm(ktri_avgDPTemp20~ns(c(1:366), df=35), ktri_20test)
ktri_dp202_ci_10d <- predict(ktri_dp202_spline_10d, interval = "confidence", level = 0.95)
ktri_dp302_spline_10d <- lm(ktri_avgDPTemp30~ns(c(1:366), df=35), ktri_30test)
ktri_dp302_ci_10d <- predict(ktri_dp302_spline_10d, interval = "confidence", level = 0.95)

ktri_dp_fv <- c()

ktri_dp10 <- data.frame(transform(ktri_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_dp10) <- c("FV","JD")
ktri_dp20 <- data.frame(transform(ktri_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_dp20) <- c("FV","JD")
ktri_dp30 <- data.frame(transform(ktri_dp302_spline_10d$fitted.values, JD = 1:length(ktri_dp302_spline_10d$fitted.values)))
colnames(ktri_dp30) <- c("FV","JD")

fv <- union(ktri_dp10, ktri_dp20)
fv_total <- union(fv, ktri_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
ktri_wd302_spline_10d <- lm(ktri_avgWDIR30~ns(c(1:366), df=35), ktri_30test)
ktri_wd302_ci_10d <- predict(ktri_wd302_spline_10d, interval = "confidence", level = 0.95)
ktri_wd202_spline_10d <- lm(ktri_avgWDIR20~ns(c(1:366), df=35), ktri_20test)
ktri_wd202_ci_10d <- predict(ktri_wd202_spline_10d, interval = "confidence", level = 0.95)

ktri_wd_fv <- c()

ktri_wd10 <- data.frame(transform(ktri_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_wd10) <- c("FV","JD")
ktri_wd20 <- data.frame(transform(ktri_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_wd20) <- c("FV","JD")
ktri_wd30 <- data.frame(transform(ktri_wd302_spline_10d$fitted.values, JD = 1:length(ktri_wd302_spline_10d$fitted.values)))
colnames(ktri_wd30) <- c("FV","JD")

fv <- union(ktri_wd10, ktri_wd20)
fv_total <- union(fv, ktri_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
ktri_ws302_spline_10d <- lm(ktri_avgWSPD30~ns(c(1:366), df=35), ktri_30test)
ktri_ws302_ci_10d <- predict(ktri_ws302_spline_10d, interval = "confidence", level = 0.95)
ktri_ws202_spline_10d <- lm(ktri_avgWSPD20~ns(c(1:366), df=35), ktri_20test)
ktri_ws202_ci_10d <- predict(ktri_ws202_spline_10d, interval = "confidence", level = 0.95)

ktri_ws_fv <- c()

ktri_ws10 <- data.frame(transform(ktri_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_ws10) <- c("FV","JD")
ktri_ws20 <- data.frame(transform(ktri_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_ws20) <- c("FV","JD")
ktri_ws30 <- data.frame(transform(ktri_ws302_spline_10d$fitted.values, JD = 1:length(ktri_ws302_spline_10d$fitted.values)))
colnames(ktri_ws30) <- c("FV","JD")

fv <- union(ktri_ws10, ktri_ws20)
fv_total <- union(fv, ktri_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
ktri_wg302_spline_10d <- lm(ktri_avgWGSP30~ns(c(1:366), df=35), ktri_30test)
ktri_wg302_ci_10d <- predict(ktri_wg302_spline_10d, interval = "confidence", level = 0.95)
ktri_wg202_spline_10d <- lm(ktri_avgWGSP20~ns(c(1:366), df=35), ktri_20test)
ktri_wg202_ci_10d <- predict(ktri_wg202_spline_10d, interval = "confidence", level = 0.95)

ktri_wg_fv <- c()

ktri_wg10 <- data.frame(transform(ktri_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_wg10) <- c("FV","JD")
ktri_wg20 <- data.frame(transform(ktri_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_wg20) <- c("FV","JD")
ktri_wg30 <- data.frame(transform(ktri_wg302_spline_10d$fitted.values, JD = 1:length(ktri_wg302_spline_10d$fitted.values)))
colnames(ktri_wg30) <- c("FV","JD")

fv <- union(ktri_wg10, ktri_wg20)
fv_total <- union(fv, ktri_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
ktri_cig302_spline_10d <- lm(ktri_avgCCIG30~ns(c(1:366), df=35), ktri_30test)
ktri_cig302_ci_10d <- predict(ktri_cig302_spline_10d, interval = "confidence", level = 0.95)
ktri_cig202_spline_10d <- lm(ktri_avgCCIG20~ns(c(1:366), df=35), ktri_20test)
ktri_cig202_ci_10d <- predict(ktri_cig202_spline_10d, interval = "confidence", level = 0.95)

ktri_cig_fv <- c()

ktri_cig10 <- data.frame(transform(ktri_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_cig10) <- c("FV","JD")
ktri_cig20 <- data.frame(transform(ktri_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_cig20) <- c("FV","JD")
ktri_cig30 <- data.frame(transform(ktri_cig302_spline_10d$fitted.values, JD = 1:length(ktri_cig302_spline_10d$fitted.values)))
colnames(ktri_cig30) <- c("FV","JD")

fv <- union(ktri_cig10, ktri_cig20)
fv_total <- union(fv, ktri_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
ktri_vis302_spline_10d <- lm(ktri_avgVIS30~ns(c(1:366), df=35), ktri_30test)
ktri_vis302_ci_10d <- predict(ktri_vis302_spline_10d, interval = "confidence", level = 0.95)
ktri_vis202_spline_10d <- lm(ktri_avgVIS20~ns(c(1:366), df=35), ktri_20test)
ktri_vis202_ci_10d <- predict(ktri_vis202_spline_10d, interval = "confidence", level = 0.95)

ktri_vis_fv <- c()

ktri_vis10 <- data.frame(transform(ktri_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_vis10) <- c("FV","JD")
ktri_vis20 <- data.frame(transform(ktri_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_vis20) <- c("FV","JD")
ktri_vis30 <- data.frame(transform(ktri_vis302_spline_10d$fitted.values, JD = 1:length(ktri_vis302_spline_10d$fitted.values)))
colnames(ktri_vis30) <- c("FV","JD")

fv <- union(ktri_vis10, ktri_vis20)
fv_total <- union(fv, ktri_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
ktri_cc302_spline_10d <- lm(ktri_avgCCOV30~ns(c(1:366), df=35), ktri_30test)
ktri_cc302_ci_10d <- predict(ktri_cc302_spline_10d, interval = "confidence", level = 0.95)
ktri_cc202_spline_10d <- lm(ktri_avgCCOV20~ns(c(1:366), df=35), ktri_20test)
ktri_cc202_ci_10d <- predict(ktri_cc202_spline_10d, interval = "confidence", level = 0.95)

ktri_cc_fv <- c()

ktri_cc10 <- data.frame(transform(ktri_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_cc10) <- c("FV","JD")
ktri_cc20 <- data.frame(transform(ktri_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_cc20) <- c("FV","JD")
ktri_cc30 <- data.frame(transform(ktri_cc302_spline_10d$fitted.values, JD = 1:length(ktri_cc302_spline_10d$fitted.values)))
colnames(ktri_cc30) <- c("FV","JD")

fv <- union(ktri_cc10, ktri_cc20)
fv_total <- union(fv, ktri_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
ktri_alt302_spline_10d <- lm(ktri_avgALTS30~ns(c(1:366), df=35), ktri_30test)
ktri_alt302_ci_10d <- predict(ktri_alt302_spline_10d, interval = "confidence", level = 0.95)
ktri_alt202_spline_10d <- lm(ktri_avgALTS20~ns(c(1:366), df=35), ktri_20test)
ktri_alt202_ci_10d <- predict(ktri_alt202_spline_10d, interval = "confidence", level = 0.95)

ktri_alt_fv <- c()

ktri_alt10 <- data.frame(transform(ktri_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_alt10) <- c("FV","JD")
ktri_alt20 <- data.frame(transform(ktri_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_alt20) <- c("FV","JD")
ktri_alt30 <- data.frame(transform(ktri_alt302_spline_10d$fitted.values, JD = 1:length(ktri_alt302_spline_10d$fitted.values)))
colnames(ktri_alt30) <- c("FV","JD")

fv <- union(ktri_alt10, ktri_alt20)
fv_total <- union(fv, ktri_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average precipitation
ktri_p302_spline_10d <- lm(ktri_precipData302~ns(JD30, df=35), ktri_30test)
ktri_p302_ci_10d <- predict(ktri_p302_spline_10d, interval = "confidence", level = 0.95)
ktri_p202_spline_10d <- lm(ktri_precipData202~ns(JD20, df=35), ktri_20test)
ktri_p202_ci_10d <- predict(ktri_p202_spline_10d, interval = "confidence", level = 0.95)
ktri_p10_spline_10d <- lm(ktri_amountByRainEvent10~ns(JD10, df=35), ktri_avgArray10)
ktri_p10_ci_10d <- predict(ktri_p10_spline_10d, interval = "confidence", level = 0.95)

ktri_p_fv <- c()

ktri_p10 <- data.frame(transform(ktri_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ktri_p10) <- c("FV","JD")
ktri_p20 <- data.frame(transform(ktri_p202_spline_10d$fitted.values, JD = 1:366))
colnames(ktri_p20) <- c("FV","JD")
ktri_p30 <- data.frame(transform(ktri_p302_spline_10d$fitted.values, JD = 1:length(ktri_p302_spline_10d$fitted.values)))
colnames(ktri_p30) <- c("FV","JD")

fv <- union(ktri_p10, ktri_p20)
fv_total <- union(fv, ktri_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ktri_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


###### pajn

pajn_20test <- anti_join(pajn_toKeep20,pajn_toKeep10,by='OBSERVATIONTIME')
pajn_30test <- filter(pajn_toKeep30, date < "2004-01-01")

pajn_avgTemp202 <- c()
pajn_avgDPTemp202 <- c()
pajn_avgWDIR202 <- c()
pajn_avgWSPD202 <- c()
pajn_avgWGSP202 <- c()
pajn_avgCCIG202 <- c()
pajn_avgVIS202 <- c()
pajn_avgALTS202 <- c()
pajn_avgCCOV202 <- c()
pajn_precipData202 <- c()

pajn_avgTemp302 <- c()
pajn_avgDPTemp302 <- c()
pajn_avgWDIR302 <- c()
pajn_avgWSPD302 <- c()
pajn_avgWGSP302 <- c()
pajn_avgCCIG302 <- c()
pajn_avgVIS302 <- c()
pajn_avgALTS302 <- c()
pajn_avgCCOV302 <- c()
pajn_precipData302 <- c()

for(i in 1:366) {
  pajn_filtered2 <- filter(pajn_20test, JD == i)
  #pajn_precipFilt2 <- filter(pajn_precip30, JD == i)
  pajn_avgTemp202[i] <- mean(pajn_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  pajn_avgDPTemp202[i] <- mean(pajn_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  pajn_avgWDIR202[i] <- mean(pajn_filtered2$WINDDIRECTION, na.rm = TRUE)
  pajn_avgWSPD202[i] <- mean(pajn_filtered2$WINDSPEED, na.rm = TRUE)
  pajn_avgCCIG202[i] <- mean(pajn_filtered2$CLOUDCEILING, na.rm = TRUE)
  pajn_avgVIS202[i] <- mean(pajn_filtered2$VISIBILITY, na.rm = TRUE)
  pajn_avgWGSP202[i] <- mean(pajn_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  pajn_avgALTS202[i] <- mean(pajn_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  pajn_avgCCOV202[i] <- mean(pajn_filtered2$CLOUDCOVER, na.rm = TRUE)
  pajn_precipData202[i] <- sum(pajn_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  pajn_filtered3 <- filter(pajn_30test, JD == i)
  if(length(pajn_filtered3) == 0) {
    pajn_avgTemp302[i] <- 0
    pajn_avgDPTemp302[i] <- 0
    pajn_avgWDIR302[i] <- 0
    pajn_avgWSPD302[i] <- 0
    pajn_avgCCIG302[i] <- 0
    pajn_avgVIS302[i] <- 0
    pajn_avgWGSP302[i] <- 0
    pajn_avgALTS302[i] <- 0
    pajn_avgCCOV302[i] <- 0
    pajn_precipData302[i] <- 0
  } else {
    pajn_avgTemp302[i] <- mean(pajn_filtered3$AIRTEMPERATURE)
    pajn_avgDPTemp302[i] <- mean(pajn_filtered3$DEWPOINTTEMPERATURE)
    pajn_avgWDIR302[i] <- mean(pajn_filtered3$WINDDIRECTION)
    pajn_avgWSPD302[i] <- mean(pajn_filtered3$WINDSPEED)
    pajn_avgCCIG302[i] <- mean(pajn_filtered3$CLOUDCEILING)
    pajn_avgVIS302[i] <- mean(pajn_filtered3$VISIBILITY)
    pajn_avgWGSP302[i] <- mean(pajn_filtered3$WINDGUSTSPEED)
    pajn_avgALTS302[i] <- mean(pajn_filtered3$ALTIMETERSETTING)
    pajn_avgCCOV302[i] <- mean(pajn_filtered3$CLOUDCOVER)
    pajn_precipData302[i] <- sum(pajn_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
pajn_t202_spline_10d <- lm(pajn_avgTemp202~ns(c(1:366), df=35), pajn_20test)
pajn_t202_ci_10d <- predict(pajn_t202_spline_10d, interval = "confidence", level = 0.95)
pajn_t302_spline_10d <- lm(pajn_avgTemp302~ns(c(1:366), df=35), pajn_30test)
pajn_t302_ci_10d <- predict(pajn_t302_spline_10d, interval = "confidence", level = 0.95)

pajn_t_fv <- c()

pajn_t10 <- data.frame(transform(pajn_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_t10) <- c("FV","JD")
pajn_t20 <- data.frame(transform(pajn_t202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_t20) <- c("FV","JD")
pajn_t30 <- data.frame(transform(pajn_t302_spline_10d$fitted.values, JD = 1:length(pajn_t302_spline_10d$fitted.values)))
colnames(pajn_t30) <- c("FV","JD")

fv <- union(pajn_t10, pajn_t20)
fv_total <- union(fv, pajn_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
pajn_dp202_spline_10d <- lm(pajn_avgDPTemp20~ns(c(1:366), df=35), pajn_20test)
pajn_dp202_ci_10d <- predict(pajn_dp202_spline_10d, interval = "confidence", level = 0.95)
pajn_dp302_spline_10d <- lm(pajn_avgDPTemp30~ns(c(1:366), df=35), pajn_30test)
pajn_dp302_ci_10d <- predict(pajn_dp302_spline_10d, interval = "confidence", level = 0.95)

pajn_dp_fv <- c()

pajn_dp10 <- data.frame(transform(pajn_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_dp10) <- c("FV","JD")
pajn_dp20 <- data.frame(transform(pajn_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_dp20) <- c("FV","JD")
pajn_dp30 <- data.frame(transform(pajn_dp302_spline_10d$fitted.values, JD = 1:length(pajn_dp302_spline_10d$fitted.values)))
colnames(pajn_dp30) <- c("FV","JD")

fv <- union(pajn_dp10, pajn_dp20)
fv_total <- union(fv, pajn_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
pajn_wd302_spline_10d <- lm(pajn_avgWDIR30~ns(c(1:366), df=35), pajn_30test)
pajn_wd302_ci_10d <- predict(pajn_wd302_spline_10d, interval = "confidence", level = 0.95)
pajn_wd202_spline_10d <- lm(pajn_avgWDIR20~ns(c(1:366), df=35), pajn_20test)
pajn_wd202_ci_10d <- predict(pajn_wd202_spline_10d, interval = "confidence", level = 0.95)

pajn_wd_fv <- c()

pajn_wd10 <- data.frame(transform(pajn_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_wd10) <- c("FV","JD")
pajn_wd20 <- data.frame(transform(pajn_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_wd20) <- c("FV","JD")
pajn_wd30 <- data.frame(transform(pajn_wd302_spline_10d$fitted.values, JD = 1:length(pajn_wd302_spline_10d$fitted.values)))
colnames(pajn_wd30) <- c("FV","JD")

fv <- union(pajn_wd10, pajn_wd20)
fv_total <- union(fv, pajn_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
pajn_ws302_spline_10d <- lm(pajn_avgWSPD30~ns(c(1:366), df=35), pajn_30test)
pajn_ws302_ci_10d <- predict(pajn_ws302_spline_10d, interval = "confidence", level = 0.95)
pajn_ws202_spline_10d <- lm(pajn_avgWSPD20~ns(c(1:366), df=35), pajn_20test)
pajn_ws202_ci_10d <- predict(pajn_ws202_spline_10d, interval = "confidence", level = 0.95)

pajn_ws_fv <- c()

pajn_ws10 <- data.frame(transform(pajn_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_ws10) <- c("FV","JD")
pajn_ws20 <- data.frame(transform(pajn_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_ws20) <- c("FV","JD")
pajn_ws30 <- data.frame(transform(pajn_ws302_spline_10d$fitted.values, JD = 1:length(pajn_ws302_spline_10d$fitted.values)))
colnames(pajn_ws30) <- c("FV","JD")

fv <- union(pajn_ws10, pajn_ws20)
fv_total <- union(fv, pajn_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
pajn_wg302_spline_10d <- lm(pajn_avgWGSP30~ns(c(1:366), df=35), pajn_30test)
pajn_wg302_ci_10d <- predict(pajn_wg302_spline_10d, interval = "confidence", level = 0.95)
pajn_wg202_spline_10d <- lm(pajn_avgWGSP20~ns(c(1:366), df=35), pajn_20test)
pajn_wg202_ci_10d <- predict(pajn_wg202_spline_10d, interval = "confidence", level = 0.95)

pajn_wg_fv <- c()

pajn_wg10 <- data.frame(transform(pajn_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_wg10) <- c("FV","JD")
pajn_wg20 <- data.frame(transform(pajn_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_wg20) <- c("FV","JD")
pajn_wg30 <- data.frame(transform(pajn_wg302_spline_10d$fitted.values, JD = 1:length(pajn_wg302_spline_10d$fitted.values)))
colnames(pajn_wg30) <- c("FV","JD")

fv <- union(pajn_wg10, pajn_wg20)
fv_total <- union(fv, pajn_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
pajn_cig302_spline_10d <- lm(pajn_avgCCIG30~ns(c(1:366), df=35), pajn_30test)
pajn_cig302_ci_10d <- predict(pajn_cig302_spline_10d, interval = "confidence", level = 0.95)
pajn_cig202_spline_10d <- lm(pajn_avgCCIG20~ns(c(1:366), df=35), pajn_20test)
pajn_cig202_ci_10d <- predict(pajn_cig202_spline_10d, interval = "confidence", level = 0.95)

pajn_cig_fv <- c()

pajn_cig10 <- data.frame(transform(pajn_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_cig10) <- c("FV","JD")
pajn_cig20 <- data.frame(transform(pajn_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_cig20) <- c("FV","JD")
pajn_cig30 <- data.frame(transform(pajn_cig302_spline_10d$fitted.values, JD = 1:length(pajn_cig302_spline_10d$fitted.values)))
colnames(pajn_cig30) <- c("FV","JD")

fv <- union(pajn_cig10, pajn_cig20)
fv_total <- union(fv, pajn_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
pajn_vis302_spline_10d <- lm(pajn_avgVIS30~ns(c(1:366), df=35), pajn_30test)
pajn_vis302_ci_10d <- predict(pajn_vis302_spline_10d, interval = "confidence", level = 0.95)
pajn_vis202_spline_10d <- lm(pajn_avgVIS20~ns(c(1:366), df=35), pajn_20test)
pajn_vis202_ci_10d <- predict(pajn_vis202_spline_10d, interval = "confidence", level = 0.95)

pajn_vis_fv <- c()

pajn_vis10 <- data.frame(transform(pajn_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_vis10) <- c("FV","JD")
pajn_vis20 <- data.frame(transform(pajn_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_vis20) <- c("FV","JD")
pajn_vis30 <- data.frame(transform(pajn_vis302_spline_10d$fitted.values, JD = 1:length(pajn_vis302_spline_10d$fitted.values)))
colnames(pajn_vis30) <- c("FV","JD")

fv <- union(pajn_vis10, pajn_vis20)
fv_total <- union(fv, pajn_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
pajn_cc302_spline_10d <- lm(pajn_avgCCOV30~ns(c(1:366), df=35), pajn_30test)
pajn_cc302_ci_10d <- predict(pajn_cc302_spline_10d, interval = "confidence", level = 0.95)
pajn_cc202_spline_10d <- lm(pajn_avgCCOV20~ns(c(1:366), df=35), pajn_20test)
pajn_cc202_ci_10d <- predict(pajn_cc202_spline_10d, interval = "confidence", level = 0.95)

pajn_cc_fv <- c()

pajn_cc10 <- data.frame(transform(pajn_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_cc10) <- c("FV","JD")
pajn_cc20 <- data.frame(transform(pajn_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_cc20) <- c("FV","JD")
pajn_cc30 <- data.frame(transform(pajn_cc302_spline_10d$fitted.values, JD = 1:length(pajn_cc302_spline_10d$fitted.values)))
colnames(pajn_cc30) <- c("FV","JD")

fv <- union(pajn_cc10, pajn_cc20)
fv_total <- union(fv, pajn_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
pajn_alt302_spline_10d <- lm(pajn_avgALTS30~ns(c(1:366), df=35), pajn_30test)
pajn_alt302_ci_10d <- predict(pajn_alt302_spline_10d, interval = "confidence", level = 0.95)
pajn_alt202_spline_10d <- lm(pajn_avgALTS20~ns(c(1:366), df=35), pajn_20test)
pajn_alt202_ci_10d <- predict(pajn_alt202_spline_10d, interval = "confidence", level = 0.95)

pajn_alt_fv <- c()

pajn_alt10 <- data.frame(transform(pajn_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_alt10) <- c("FV","JD")
pajn_alt20 <- data.frame(transform(pajn_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_alt20) <- c("FV","JD")
pajn_alt30 <- data.frame(transform(pajn_alt302_spline_10d$fitted.values, JD = 1:length(pajn_alt302_spline_10d$fitted.values)))
colnames(pajn_alt30) <- c("FV","JD")

fv <- union(pajn_alt10, pajn_alt20)
fv_total <- union(fv, pajn_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average precipitation
pajn_p302_spline_10d <- lm(pajn_precipData302~ns(JD30, df=35), pajn_30test)
pajn_p302_ci_10d <- predict(pajn_p302_spline_10d, interval = "confidence", level = 0.95)
pajn_p202_spline_10d <- lm(pajn_precipData202~ns(JD20, df=35), pajn_20test)
pajn_p202_ci_10d <- predict(pajn_p202_spline_10d, interval = "confidence", level = 0.95)
pajn_p10_spline_10d <- lm(pajn_amountByRainEvent10~ns(JD10, df=35), pajn_avgArray10)
pajn_p10_ci_10d <- predict(pajn_p10_spline_10d, interval = "confidence", level = 0.95)

pajn_p_fv <- c()

pajn_p10 <- data.frame(transform(pajn_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(pajn_p10) <- c("FV","JD")
pajn_p20 <- data.frame(transform(pajn_p202_spline_10d$fitted.values, JD = 1:366))
colnames(pajn_p20) <- c("FV","JD")
pajn_p30 <- data.frame(transform(pajn_p302_spline_10d$fitted.values, JD = 1:length(pajn_p302_spline_10d$fitted.values)))
colnames(pajn_p30) <- c("FV","JD")

fv <- union(pajn_p10, pajn_p20)
fv_total <- union(fv, pajn_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  pajn_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


###### kelp

kelp_20test <- anti_join(kelp_toKeep20,kelp_toKeep10,by='OBSERVATIONTIME')
kelp_30test <- filter(kelp_toKeep30, date < "2004-01-01")

kelp_avgTemp202 <- c()
kelp_avgDPTemp202 <- c()
kelp_avgWDIR202 <- c()
kelp_avgWSPD202 <- c()
kelp_avgWGSP202 <- c()
kelp_avgCCIG202 <- c()
kelp_avgVIS202 <- c()
kelp_avgALTS202 <- c()
kelp_avgCCOV202 <- c()
kelp_precipData202 <- c()

kelp_avgTemp302 <- c()
kelp_avgDPTemp302 <- c()
kelp_avgWDIR302 <- c()
kelp_avgWSPD302 <- c()
kelp_avgWGSP302 <- c()
kelp_avgCCIG302 <- c()
kelp_avgVIS302 <- c()
kelp_avgALTS302 <- c()
kelp_avgCCOV302 <- c()
kelp_precipData302 <- c()

for(i in 1:366) {
  kelp_filtered2 <- filter(kelp_20test, JD == i)
  #kelp_precipFilt2 <- filter(kelp_precip30, JD == i)
  kelp_avgTemp202[i] <- mean(kelp_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  kelp_avgDPTemp202[i] <- mean(kelp_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  kelp_avgWDIR202[i] <- mean(kelp_filtered2$WINDDIRECTION, na.rm = TRUE)
  kelp_avgWSPD202[i] <- mean(kelp_filtered2$WINDSPEED, na.rm = TRUE)
  kelp_avgCCIG202[i] <- mean(kelp_filtered2$CLOUDCEILING, na.rm = TRUE)
  kelp_avgVIS202[i] <- mean(kelp_filtered2$VISIBILITY, na.rm = TRUE)
  kelp_avgWGSP202[i] <- mean(kelp_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  kelp_avgALTS202[i] <- mean(kelp_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  kelp_avgCCOV202[i] <- mean(kelp_filtered2$CLOUDCOVER, na.rm = TRUE)
  kelp_precipData202[i] <- sum(kelp_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  kelp_filtered3 <- filter(kelp_30test, JD == i)
  if(length(kelp_filtered3) == 0) {
    kelp_avgTemp302[i] <- 0
    kelp_avgDPTemp302[i] <- 0
    kelp_avgWDIR302[i] <- 0
    kelp_avgWSPD302[i] <- 0
    kelp_avgCCIG302[i] <- 0
    kelp_avgVIS302[i] <- 0
    kelp_avgWGSP302[i] <- 0
    kelp_avgALTS302[i] <- 0
    kelp_avgCCOV302[i] <- 0
    kelp_precipData302[i] <- 0
  } else {
    kelp_avgTemp302[i] <- mean(kelp_filtered3$AIRTEMPERATURE)
    kelp_avgDPTemp302[i] <- mean(kelp_filtered3$DEWPOINTTEMPERATURE)
    kelp_avgWDIR302[i] <- mean(kelp_filtered3$WINDDIRECTION)
    kelp_avgWSPD302[i] <- mean(kelp_filtered3$WINDSPEED)
    kelp_avgCCIG302[i] <- mean(kelp_filtered3$CLOUDCEILING)
    kelp_avgVIS302[i] <- mean(kelp_filtered3$VISIBILITY)
    kelp_avgWGSP302[i] <- mean(kelp_filtered3$WINDGUSTSPEED)
    kelp_avgALTS302[i] <- mean(kelp_filtered3$ALTIMETERSETTING)
    kelp_avgCCOV302[i] <- mean(kelp_filtered3$CLOUDCOVER)
    kelp_precipData302[i] <- sum(kelp_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
kelp_t202_spline_10d <- lm(kelp_avgTemp202~ns(c(1:366), df=35), kelp_20test)
kelp_t202_ci_10d <- predict(kelp_t202_spline_10d, interval = "confidence", level = 0.95)
kelp_t302_spline_10d <- lm(kelp_avgTemp302~ns(c(1:366), df=35), kelp_30test)
kelp_t302_ci_10d <- predict(kelp_t302_spline_10d, interval = "confidence", level = 0.95)

kelp_t_fv <- c()

kelp_t10 <- data.frame(transform(kelp_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_t10) <- c("FV","JD")
kelp_t20 <- data.frame(transform(kelp_t202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_t20) <- c("FV","JD")
kelp_t30 <- data.frame(transform(kelp_t302_spline_10d$fitted.values, JD = 1:length(kelp_t302_spline_10d$fitted.values)))
colnames(kelp_t30) <- c("FV","JD")

fv <- union(kelp_t10, kelp_t20)
fv_total <- union(fv, kelp_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
kelp_dp202_spline_10d <- lm(kelp_avgDPTemp20~ns(c(1:366), df=35), kelp_20test)
kelp_dp202_ci_10d <- predict(kelp_dp202_spline_10d, interval = "confidence", level = 0.95)
kelp_dp302_spline_10d <- lm(kelp_avgDPTemp30~ns(c(1:366), df=35), kelp_30test)
kelp_dp302_ci_10d <- predict(kelp_dp302_spline_10d, interval = "confidence", level = 0.95)

kelp_dp_fv <- c()

kelp_dp10 <- data.frame(transform(kelp_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_dp10) <- c("FV","JD")
kelp_dp20 <- data.frame(transform(kelp_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_dp20) <- c("FV","JD")
kelp_dp30 <- data.frame(transform(kelp_dp302_spline_10d$fitted.values, JD = 1:length(kelp_dp302_spline_10d$fitted.values)))
colnames(kelp_dp30) <- c("FV","JD")

fv <- union(kelp_dp10, kelp_dp20)
fv_total <- union(fv, kelp_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
kelp_wd302_spline_10d <- lm(kelp_avgWDIR30~ns(c(1:366), df=35), kelp_30test)
kelp_wd302_ci_10d <- predict(kelp_wd302_spline_10d, interval = "confidence", level = 0.95)
kelp_wd202_spline_10d <- lm(kelp_avgWDIR20~ns(c(1:366), df=35), kelp_20test)
kelp_wd202_ci_10d <- predict(kelp_wd202_spline_10d, interval = "confidence", level = 0.95)

kelp_wd_fv <- c()

kelp_wd10 <- data.frame(transform(kelp_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_wd10) <- c("FV","JD")
kelp_wd20 <- data.frame(transform(kelp_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_wd20) <- c("FV","JD")
kelp_wd30 <- data.frame(transform(kelp_wd302_spline_10d$fitted.values, JD = 1:length(kelp_wd302_spline_10d$fitted.values)))
colnames(kelp_wd30) <- c("FV","JD")

fv <- union(kelp_wd10, kelp_wd20)
fv_total <- union(fv, kelp_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
kelp_ws302_spline_10d <- lm(kelp_avgWSPD30~ns(c(1:366), df=35), kelp_30test)
kelp_ws302_ci_10d <- predict(kelp_ws302_spline_10d, interval = "confidence", level = 0.95)
kelp_ws202_spline_10d <- lm(kelp_avgWSPD20~ns(c(1:366), df=35), kelp_20test)
kelp_ws202_ci_10d <- predict(kelp_ws202_spline_10d, interval = "confidence", level = 0.95)

kelp_ws_fv <- c()

kelp_ws10 <- data.frame(transform(kelp_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_ws10) <- c("FV","JD")
kelp_ws20 <- data.frame(transform(kelp_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_ws20) <- c("FV","JD")
kelp_ws30 <- data.frame(transform(kelp_ws302_spline_10d$fitted.values, JD = 1:length(kelp_ws302_spline_10d$fitted.values)))
colnames(kelp_ws30) <- c("FV","JD")

fv <- union(kelp_ws10, kelp_ws20)
fv_total <- union(fv, kelp_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
kelp_wg302_spline_10d <- lm(kelp_avgWGSP30~ns(c(1:366), df=35), kelp_30test)
kelp_wg302_ci_10d <- predict(kelp_wg302_spline_10d, interval = "confidence", level = 0.95)
kelp_wg202_spline_10d <- lm(kelp_avgWGSP20~ns(c(1:366), df=35), kelp_20test)
kelp_wg202_ci_10d <- predict(kelp_wg202_spline_10d, interval = "confidence", level = 0.95)

kelp_wg_fv <- c()

kelp_wg10 <- data.frame(transform(kelp_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_wg10) <- c("FV","JD")
kelp_wg20 <- data.frame(transform(kelp_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_wg20) <- c("FV","JD")
kelp_wg30 <- data.frame(transform(kelp_wg302_spline_10d$fitted.values, JD = 1:length(kelp_wg302_spline_10d$fitted.values)))
colnames(kelp_wg30) <- c("FV","JD")

fv <- union(kelp_wg10, kelp_wg20)
fv_total <- union(fv, kelp_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
kelp_cig302_spline_10d <- lm(kelp_avgCCIG30~ns(c(1:366), df=35), kelp_30test)
kelp_cig302_ci_10d <- predict(kelp_cig302_spline_10d, interval = "confidence", level = 0.95)
kelp_cig202_spline_10d <- lm(kelp_avgCCIG20~ns(c(1:366), df=35), kelp_20test)
kelp_cig202_ci_10d <- predict(kelp_cig202_spline_10d, interval = "confidence", level = 0.95)

kelp_cig_fv <- c()

kelp_cig10 <- data.frame(transform(kelp_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_cig10) <- c("FV","JD")
kelp_cig20 <- data.frame(transform(kelp_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_cig20) <- c("FV","JD")
kelp_cig30 <- data.frame(transform(kelp_cig302_spline_10d$fitted.values, JD = 1:length(kelp_cig302_spline_10d$fitted.values)))
colnames(kelp_cig30) <- c("FV","JD")

fv <- union(kelp_cig10, kelp_cig20)
fv_total <- union(fv, kelp_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
kelp_vis302_spline_10d <- lm(kelp_avgVIS30~ns(c(1:366), df=35), kelp_30test)
kelp_vis302_ci_10d <- predict(kelp_vis302_spline_10d, interval = "confidence", level = 0.95)
kelp_vis202_spline_10d <- lm(kelp_avgVIS20~ns(c(1:366), df=35), kelp_20test)
kelp_vis202_ci_10d <- predict(kelp_vis202_spline_10d, interval = "confidence", level = 0.95)

kelp_vis_fv <- c()

kelp_vis10 <- data.frame(transform(kelp_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_vis10) <- c("FV","JD")
kelp_vis20 <- data.frame(transform(kelp_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_vis20) <- c("FV","JD")
kelp_vis30 <- data.frame(transform(kelp_vis302_spline_10d$fitted.values, JD = 1:length(kelp_vis302_spline_10d$fitted.values)))
colnames(kelp_vis30) <- c("FV","JD")

fv <- union(kelp_vis10, kelp_vis20)
fv_total <- union(fv, kelp_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
kelp_cc302_spline_10d <- lm(kelp_avgCCOV30~ns(c(1:366), df=35), kelp_30test)
kelp_cc302_ci_10d <- predict(kelp_cc302_spline_10d, interval = "confidence", level = 0.95)
kelp_cc202_spline_10d <- lm(kelp_avgCCOV20~ns(c(1:366), df=35), kelp_20test)
kelp_cc202_ci_10d <- predict(kelp_cc202_spline_10d, interval = "confidence", level = 0.95)

kelp_cc_fv <- c()

kelp_cc10 <- data.frame(transform(kelp_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_cc10) <- c("FV","JD")
kelp_cc20 <- data.frame(transform(kelp_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_cc20) <- c("FV","JD")
kelp_cc30 <- data.frame(transform(kelp_cc302_spline_10d$fitted.values, JD = 1:length(kelp_cc302_spline_10d$fitted.values)))
colnames(kelp_cc30) <- c("FV","JD")

fv <- union(kelp_cc10, kelp_cc20)
fv_total <- union(fv, kelp_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
kelp_alt302_spline_10d <- lm(kelp_avgALTS30~ns(c(1:366), df=35), kelp_30test)
kelp_alt302_ci_10d <- predict(kelp_alt302_spline_10d, interval = "confidence", level = 0.95)
kelp_alt202_spline_10d <- lm(kelp_avgALTS20~ns(c(1:366), df=35), kelp_20test)
kelp_alt202_ci_10d <- predict(kelp_alt202_spline_10d, interval = "confidence", level = 0.95)

kelp_alt_fv <- c()

kelp_alt10 <- data.frame(transform(kelp_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_alt10) <- c("FV","JD")
kelp_alt20 <- data.frame(transform(kelp_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_alt20) <- c("FV","JD")
kelp_alt30 <- data.frame(transform(kelp_alt302_spline_10d$fitted.values, JD = 1:length(kelp_alt302_spline_10d$fitted.values)))
colnames(kelp_alt30) <- c("FV","JD")

fv <- union(kelp_alt10, kelp_alt20)
fv_total <- union(fv, kelp_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}


#Average precipitation
kelp_p302_spline_10d <- lm(kelp_precipData302~ns(JD30, df=35), kelp_30test)
kelp_p302_ci_10d <- predict(kelp_p302_spline_10d, interval = "confidence", level = 0.95)
kelp_p202_spline_10d <- lm(kelp_precipData202~ns(JD20, df=35), kelp_20test)
kelp_p202_ci_10d <- predict(kelp_p202_spline_10d, interval = "confidence", level = 0.95)
kelp_p10_spline_10d <- lm(kelp_amountByRainEvent10~ns(JD10, df=35), kelp_avgArray10)
kelp_p10_ci_10d <- predict(kelp_p10_spline_10d, interval = "confidence", level = 0.95)

kelp_p_fv <- c()

kelp_p10 <- data.frame(transform(kelp_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(kelp_p10) <- c("FV","JD")
kelp_p20 <- data.frame(transform(kelp_p202_spline_10d$fitted.values, JD = 1:366))
colnames(kelp_p20) <- c("FV","JD")
kelp_p30 <- data.frame(transform(kelp_p302_spline_10d$fitted.values, JD = 1:length(kelp_p302_spline_10d$fitted.values)))
colnames(kelp_p30) <- c("FV","JD")

fv <- union(kelp_p10, kelp_p20)
fv_total <- union(fv, kelp_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  kelp_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


###### ksgu
ksgu_20test <- anti_join(ksgu_toKeep20,ksgu_toKeep10,by='OBSERVATIONTIME')
ksgu_30test <- filter(ksgu_toKeep30, date < "2004-01-01")

ksgu_avgTemp202 <- c()
ksgu_avgDPTemp202 <- c()
ksgu_avgWDIR202 <- c()
ksgu_avgWSPD202 <- c()
ksgu_avgWGSP202 <- c()
ksgu_avgCCIG202 <- c()
ksgu_avgVIS202 <- c()
ksgu_avgALTS202 <- c()
ksgu_avgCCOV202 <- c()
ksgu_precipData202 <- c()

ksgu_avgTemp302 <- c()
ksgu_avgDPTemp302 <- c()
ksgu_avgWDIR302 <- c()
ksgu_avgWSPD302 <- c()
ksgu_avgWGSP302 <- c()
ksgu_avgCCIG302 <- c()
ksgu_avgVIS302 <- c()
ksgu_avgALTS302 <- c()
ksgu_avgCCOV302 <- c()
ksgu_precipData302 <- c()

for(i in 1:366) {
  ksgu_filtered2 <- filter(ksgu_20test, JD == i)
  #ksgu_precipFilt2 <- filter(ksgu_precip30, JD == i)
  ksgu_avgTemp202[i] <- mean(ksgu_filtered2$AIRTEMPERATURE, na.rm = TRUE)
  ksgu_avgDPTemp202[i] <- mean(ksgu_filtered2$DEWPOINTTEMPERATURE, na.rm = TRUE)
  ksgu_avgWDIR202[i] <- mean(ksgu_filtered2$WINDDIRECTION, na.rm = TRUE)
  ksgu_avgWSPD202[i] <- mean(ksgu_filtered2$WINDSPEED, na.rm = TRUE)
  ksgu_avgCCIG202[i] <- mean(ksgu_filtered2$CLOUDCEILING, na.rm = TRUE)
  ksgu_avgVIS202[i] <- mean(ksgu_filtered2$VISIBILITY, na.rm = TRUE)
  ksgu_avgWGSP202[i] <- mean(ksgu_filtered2$WINDGUSTSPEED, na.rm = TRUE)
  ksgu_avgALTS202[i] <- mean(ksgu_filtered2$ALTIMETERSETTING, na.rm = TRUE)
  ksgu_avgCCOV202[i] <- mean(ksgu_filtered2$CLOUDCOVER, na.rm = TRUE)
  ksgu_precipData202[i] <- sum(ksgu_filtered2$PRECIPAMOUNT1)/10
}

for(i in 1:366) {
  ksgu_filtered3 <- filter(ksgu_30test, JD == i)
  if(length(ksgu_filtered3) == 0) {
    ksgu_avgTemp302[i] <- 0
    ksgu_avgDPTemp302[i] <- 0
    ksgu_avgWDIR302[i] <- 0
    ksgu_avgWSPD302[i] <- 0
    ksgu_avgCCIG302[i] <- 0
    ksgu_avgVIS302[i] <- 0
    ksgu_avgWGSP302[i] <- 0
    ksgu_avgALTS302[i] <- 0
    ksgu_avgCCOV302[i] <- 0
    ksgu_precipData302[i] <- 0
  } else {
    ksgu_avgTemp302[i] <- mean(ksgu_filtered3$AIRTEMPERATURE)
    ksgu_avgDPTemp302[i] <- mean(ksgu_filtered3$DEWPOINTTEMPERATURE)
    ksgu_avgWDIR302[i] <- mean(ksgu_filtered3$WINDDIRECTION)
    ksgu_avgWSPD302[i] <- mean(ksgu_filtered3$WINDSPEED)
    ksgu_avgCCIG302[i] <- mean(ksgu_filtered3$CLOUDCEILING)
    ksgu_avgVIS302[i] <- mean(ksgu_filtered3$VISIBILITY)
    ksgu_avgWGSP302[i] <- mean(ksgu_filtered3$WINDGUSTSPEED)
    ksgu_avgALTS302[i] <- mean(ksgu_filtered3$ALTIMETERSETTING)
    ksgu_avgCCOV302[i] <- mean(ksgu_filtered3$CLOUDCOVER)
    ksgu_precipData302[i] <- sum(ksgu_filtered3$PRECIPAMOUNT1)/10
  }
}

##Temperature
ksgu_t202_spline_10d <- lm(ksgu_avgTemp202~ns(c(1:366), df=35), ksgu_20test)
ksgu_t202_ci_10d <- predict(ksgu_t202_spline_10d, interval = "confidence", level = 0.95)
ksgu_t302_spline_10d <- lm(ksgu_avgTemp302~ns(c(1:366), df=35), ksgu_30test)
ksgu_t302_ci_10d <- predict(ksgu_t302_spline_10d, interval = "confidence", level = 0.95)

ksgu_t_fv <- c()

ksgu_t10 <- data.frame(transform(ksgu_t10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_t10) <- c("FV","JD")
ksgu_t20 <- data.frame(transform(ksgu_t202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_t20) <- c("FV","JD")
ksgu_t30 <- data.frame(transform(ksgu_t302_spline_10d$fitted.values, JD = 1:length(ksgu_t302_spline_10d$fitted.values)))
colnames(ksgu_t30) <- c("FV","JD")

fv <- union(ksgu_t10, ksgu_t20)
fv_total <- union(fv, ksgu_t30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_t_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Dewpoint Temperature
ksgu_dp202_spline_10d <- lm(ksgu_avgDPTemp20~ns(c(1:366), df=35), ksgu_20test)
ksgu_dp202_ci_10d <- predict(ksgu_dp202_spline_10d, interval = "confidence", level = 0.95)
ksgu_dp302_spline_10d <- lm(ksgu_avgDPTemp30~ns(c(1:366), df=35), ksgu_30test)
ksgu_dp302_ci_10d <- predict(ksgu_dp302_spline_10d, interval = "confidence", level = 0.95)

ksgu_dp_fv <- c()

ksgu_dp10 <- data.frame(transform(ksgu_dp10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_dp10) <- c("FV","JD")
ksgu_dp20 <- data.frame(transform(ksgu_dp202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_dp20) <- c("FV","JD")
ksgu_dp30 <- data.frame(transform(ksgu_dp302_spline_10d$fitted.values, JD = 1:length(ksgu_dp302_spline_10d$fitted.values)))
colnames(ksgu_dp30) <- c("FV","JD")

fv <- union(ksgu_dp10, ksgu_dp20)
fv_total <- union(fv, ksgu_dp30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_dp_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Direction
ksgu_wd302_spline_10d <- lm(ksgu_avgWDIR30~ns(c(1:366), df=35), ksgu_30test)
ksgu_wd302_ci_10d <- predict(ksgu_wd302_spline_10d, interval = "confidence", level = 0.95)
ksgu_wd202_spline_10d <- lm(ksgu_avgWDIR20~ns(c(1:366), df=35), ksgu_20test)
ksgu_wd202_ci_10d <- predict(ksgu_wd202_spline_10d, interval = "confidence", level = 0.95)

ksgu_wd_fv <- c()

ksgu_wd10 <- data.frame(transform(ksgu_wd10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_wd10) <- c("FV","JD")
ksgu_wd20 <- data.frame(transform(ksgu_wd202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_wd20) <- c("FV","JD")
ksgu_wd30 <- data.frame(transform(ksgu_wd302_spline_10d$fitted.values, JD = 1:length(ksgu_wd302_spline_10d$fitted.values)))
colnames(ksgu_wd30) <- c("FV","JD")

fv <- union(ksgu_wd10, ksgu_wd20)
fv_total <- union(fv, ksgu_wd30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_wd_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Speed
ksgu_ws302_spline_10d <- lm(ksgu_avgWSPD30~ns(c(1:366), df=35), ksgu_30test)
ksgu_ws302_ci_10d <- predict(ksgu_ws302_spline_10d, interval = "confidence", level = 0.95)
ksgu_ws202_spline_10d <- lm(ksgu_avgWSPD20~ns(c(1:366), df=35), ksgu_20test)
ksgu_ws202_ci_10d <- predict(ksgu_ws202_spline_10d, interval = "confidence", level = 0.95)

ksgu_ws_fv <- c()

ksgu_ws10 <- data.frame(transform(ksgu_ws10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_ws10) <- c("FV","JD")
ksgu_ws20 <- data.frame(transform(ksgu_ws202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_ws20) <- c("FV","JD")
ksgu_ws30 <- data.frame(transform(ksgu_ws302_spline_10d$fitted.values, JD = 1:length(ksgu_ws302_spline_10d$fitted.values)))
colnames(ksgu_ws30) <- c("FV","JD")

fv <- union(ksgu_ws10, ksgu_ws20)
fv_total <- union(fv, ksgu_ws30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_ws_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Wind Gust Speed
ksgu_wg302_spline_10d <- lm(ksgu_avgWGSP30~ns(c(1:366), df=35), ksgu_30test)
ksgu_wg302_ci_10d <- predict(ksgu_wg302_spline_10d, interval = "confidence", level = 0.95)
ksgu_wg202_spline_10d <- lm(ksgu_avgWGSP20~ns(c(1:366), df=35), ksgu_20test)
ksgu_wg202_ci_10d <- predict(ksgu_wg202_spline_10d, interval = "confidence", level = 0.95)

ksgu_wg_fv <- c()

ksgu_wg10 <- data.frame(transform(ksgu_wg10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_wg10) <- c("FV","JD")
ksgu_wg20 <- data.frame(transform(ksgu_wg202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_wg20) <- c("FV","JD")
ksgu_wg30 <- data.frame(transform(ksgu_wg302_spline_10d$fitted.values, JD = 1:length(ksgu_wg302_spline_10d$fitted.values)))
colnames(ksgu_wg30) <- c("FV","JD")

fv <- union(ksgu_wg10, ksgu_wg20)
fv_total <- union(fv, ksgu_wg30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_wg_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Ceiling Height
ksgu_cig302_spline_10d <- lm(ksgu_avgCCIG30~ns(c(1:366), df=35), ksgu_30test)
ksgu_cig302_ci_10d <- predict(ksgu_cig302_spline_10d, interval = "confidence", level = 0.95)
ksgu_cig202_spline_10d <- lm(ksgu_avgCCIG20~ns(c(1:366), df=35), ksgu_20test)
ksgu_cig202_ci_10d <- predict(ksgu_cig202_spline_10d, interval = "confidence", level = 0.95)

ksgu_cig_fv <- c()

ksgu_cig10 <- data.frame(transform(ksgu_cig10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_cig10) <- c("FV","JD")
ksgu_cig20 <- data.frame(transform(ksgu_cig202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_cig20) <- c("FV","JD")
ksgu_cig30 <- data.frame(transform(ksgu_cig302_spline_10d$fitted.values, JD = 1:length(ksgu_cig302_spline_10d$fitted.values)))
colnames(ksgu_cig30) <- c("FV","JD")

fv <- union(ksgu_cig10, ksgu_cig20)
fv_total <- union(fv, ksgu_cig30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_cig_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Visibility
ksgu_vis302_spline_10d <- lm(ksgu_avgVIS30~ns(c(1:366), df=35), ksgu_30test)
ksgu_vis302_ci_10d <- predict(ksgu_vis302_spline_10d, interval = "confidence", level = 0.95)
ksgu_vis202_spline_10d <- lm(ksgu_avgVIS20~ns(c(1:366), df=35), ksgu_20test)
ksgu_vis202_ci_10d <- predict(ksgu_vis202_spline_10d, interval = "confidence", level = 0.95)
ksgu_vis_fv <- c()

ksgu_vis10 <- data.frame(transform(ksgu_vis10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_vis10) <- c("FV","JD")
ksgu_vis20 <- data.frame(transform(ksgu_vis202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_vis20) <- c("FV","JD")
ksgu_vis30 <- data.frame(transform(ksgu_vis302_spline_10d$fitted.values, JD = 1:length(ksgu_vis302_spline_10d$fitted.values)))
colnames(ksgu_vis30) <- c("FV","JD")

fv <- union(ksgu_vis10, ksgu_vis20)
fv_total <- union(fv, ksgu_vis30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_vis_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Cloud Cover
ksgu_cc302_spline_10d <- lm(ksgu_avgCCOV30~ns(c(1:366), df=35), ksgu_30test)
ksgu_cc302_ci_10d <- predict(ksgu_cc302_spline_10d, interval = "confidence", level = 0.95)
ksgu_cc202_spline_10d <- lm(ksgu_avgCCOV20~ns(c(1:366), df=35), ksgu_20test)
ksgu_cc202_ci_10d <- predict(ksgu_cc202_spline_10d, interval = "confidence", level = 0.95)

ksgu_cc_fv <- c()

ksgu_cc10 <- data.frame(transform(ksgu_cc10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_cc10) <- c("FV","JD")
ksgu_cc20 <- data.frame(transform(ksgu_cc202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_cc20) <- c("FV","JD")
ksgu_cc30 <- data.frame(transform(ksgu_cc302_spline_10d$fitted.values, JD = 1:length(ksgu_cc302_spline_10d$fitted.values)))
colnames(ksgu_cc30) <- c("FV","JD")

fv <- union(ksgu_cc10, ksgu_cc20)
fv_total <- union(fv, ksgu_cc30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_cc_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average Altimeter Setting
ksgu_alt302_spline_10d <- lm(ksgu_avgALTS30~ns(c(1:366), df=35), ksgu_30test)
ksgu_alt302_ci_10d <- predict(ksgu_alt302_spline_10d, interval = "confidence", level = 0.95)
ksgu_alt202_spline_10d <- lm(ksgu_avgALTS20~ns(c(1:366), df=35), ksgu_20test)
ksgu_alt202_ci_10d <- predict(ksgu_alt202_spline_10d, interval = "confidence", level = 0.95)

ksgu_alt_fv <- c()

ksgu_alt10 <- data.frame(transform(ksgu_alt10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_alt10) <- c("FV","JD")
ksgu_alt20 <- data.frame(transform(ksgu_alt202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_alt20) <- c("FV","JD")
ksgu_alt30 <- data.frame(transform(ksgu_alt302_spline_10d$fitted.values, JD = 1:length(ksgu_alt302_spline_10d$fitted.values)))
colnames(ksgu_alt30) <- c("FV","JD")

fv <- union(ksgu_alt10, ksgu_alt20)
fv_total <- union(fv, ksgu_alt30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_alt_fv[i] <- summary(slopes)$coefficients[2,1]
  
}

#Average precipitation
ksgu_p302_spline_10d <- lm(ksgu_precipData302~ns(JD30, df=35), ksgu_30test)
ksgu_p302_ci_10d <- predict(ksgu_p302_spline_10d, interval = "confidence", level = 0.95)
ksgu_p202_spline_10d <- lm(ksgu_precipData202~ns(JD20, df=35), ksgu_20test)
ksgu_p202_ci_10d <- predict(ksgu_p202_spline_10d, interval = "confidence", level = 0.95)
ksgu_p10_spline_10d <- lm(ksgu_amountByRainEvent10~ns(JD10, df=35), ksgu_avgArray10)
ksgu_p10_ci_10d <- predict(ksgu_p10_spline_10d, interval = "confidence", level = 0.95)

ksgu_p_fv <- c()

ksgu_p10 <- data.frame(transform(ksgu_p10_spline_10d$fitted.values,JD = c(1:366)))
colnames(ksgu_p10) <- c("FV","JD")
ksgu_p20 <- data.frame(transform(ksgu_p202_spline_10d$fitted.values, JD = 1:366))
colnames(ksgu_p20) <- c("FV","JD")
ksgu_p30 <- data.frame(transform(ksgu_p302_spline_10d$fitted.values, JD = 1:length(ksgu_p302_spline_10d$fitted.values)))
colnames(ksgu_p30) <- c("FV","JD")

fv <- union(ksgu_p10, ksgu_p20)
fv_total <- union(fv, ksgu_p30)

##trend by julian date
for(i in 1:366) {
  fv_filt <- filter(fv_total,JD == i)
  slopes<- lm(FV ~ c(count(fv_filt)$n:1), fv_filt)
  print(slopes)
  print(i)
  ksgu_p_fv[i] <- summary(slopes)$coefficients[2,1]
}


################r-squared matrix - doesn't include precipitation

## r_squared matrix
r_squared.df <- data.frame(
  station = c('KRDM','KRDM','KRDM','KRDM','KRDM','KRDM','KRDM','KRDM','KRDM',
              'KBUF','KBUF','KBUF','KBUF','KBUF','KBUF','KBUF','KBUF','KBUF',
              'KFOE','KFOE','KFOE','KFOE','KFOE','KFOE','KFOE','KFOE','KFOE',
              'KMSN','KMSN','KMSN','KMSN','KMSN','KMSN','KMSN','KMSN','KMSN',
              'KTRI','KTRI','KTRI','KTRI','KTRI','KTRI','KTRI','KTRI','KTRI',
              'PAJN','PAJN','PAJN','PAJN','PAJN','PAJN','PAJN','PAJN','PAJN',
              'KELP','KELP','KELP','KELP','KELP','KELP','KELP','KELP','KELP',
              'KSGU','KSGU','KSGU','KSGU','KSGU','KSGU','KSGU','KSGU','KSGU'),
  ten_yr_monthly = c(round(summary(krdm_t10_spline_m)$adj.r.squared,4),round(summary(krdm_dp10_spline_m)$adj.r.squared,4),
                     round(summary(krdm_wd10_spline_m)$adj.r.squared,4),round(summary(krdm_ws10_spline_m)$adj.r.squared,4),
                     round(summary(krdm_wg10_spline_m)$adj.r.squared,4),round(summary(krdm_cig10_spline_m)$adj.r.squared,4),round(summary(krdm_vis10_spline_m)$adj.r.squared,4),
                     round(summary(krdm_cc10_spline_m)$adj.r.squared,4),round(summary(krdm_alt10_spline_m)$adj.r.squared,4),
                     round(summary(kbuf_t10_spline_m)$adj.r.squared,4),round(summary(kbuf_dp10_spline_m)$adj.r.squared,4),
                     round(summary(kbuf_wd10_spline_m)$adj.r.squared,4),round(summary(kbuf_ws10_spline_m)$adj.r.squared,4),
                     round(summary(kbuf_wg10_spline_m)$adj.r.squared,4),round(summary(kbuf_cig10_spline_m)$adj.r.squared,4),round(summary(kbuf_vis10_spline_m)$adj.r.squared,4),
                     round(summary(kbuf_cc10_spline_m)$adj.r.squared,4),round(summary(kbuf_alt10_spline_m)$adj.r.squared,4),
                     round(summary(kfoe_t10_spline_m)$adj.r.squared,4),round(summary(kfoe_dp10_spline_m)$adj.r.squared,4),
                     round(summary(kfoe_wd10_spline_m)$adj.r.squared,4),round(summary(kfoe_ws10_spline_m)$adj.r.squared,4),
                     round(summary(kfoe_wg10_spline_m)$adj.r.squared,4),round(summary(kfoe_cig10_spline_m)$adj.r.squared,4),round(summary(kfoe_vis10_spline_m)$adj.r.squared,4),
                     round(summary(kfoe_cc10_spline_m)$adj.r.squared,4),round(summary(kfoe_alt10_spline_m)$adj.r.squared,4),
                     round(summary(kmsn_t10_spline_m)$adj.r.squared,4),round(summary(kmsn_dp10_spline_m)$adj.r.squared,4),
                     round(summary(kmsn_wd10_spline_m)$adj.r.squared,4),round(summary(kmsn_ws10_spline_m)$adj.r.squared,4),
                     round(summary(kmsn_wg10_spline_m)$adj.r.squared,4),round(summary(kmsn_cig10_spline_m)$adj.r.squared,4),round(summary(kmsn_vis10_spline_m)$adj.r.squared,4),
                     round(summary(kmsn_cc10_spline_m)$adj.r.squared,4),round(summary(kmsn_alt10_spline_m)$adj.r.squared,4),
                     round(summary(ktri_t10_spline_m)$adj.r.squared,4),round(summary(ktri_dp10_spline_m)$adj.r.squared,4),
                     round(summary(ktri_wd10_spline_m)$adj.r.squared,4),round(summary(ktri_ws10_spline_m)$adj.r.squared,4),
                     round(summary(ktri_wg10_spline_m)$adj.r.squared,4),round(summary(ktri_cig10_spline_m)$adj.r.squared,4),round(summary(ktri_vis10_spline_m)$adj.r.squared,4),
                     round(summary(ktri_cc10_spline_m)$adj.r.squared,4),round(summary(ktri_alt10_spline_m)$adj.r.squared,4),
                     round(summary(pajn_t10_spline_m)$adj.r.squared,4),round(summary(pajn_dp10_spline_m)$adj.r.squared,4),
                     round(summary(pajn_wd10_spline_m)$adj.r.squared,4),round(summary(pajn_ws10_spline_m)$adj.r.squared,4),
                     round(summary(pajn_wg10_spline_m)$adj.r.squared,4),round(summary(pajn_cig10_spline_m)$adj.r.squared,4),round(summary(pajn_vis10_spline_m)$adj.r.squared,4),
                     round(summary(pajn_cc10_spline_m)$adj.r.squared,4),round(summary(pajn_alt10_spline_m)$adj.r.squared,4),
                     round(summary(kelp_t10_spline_m)$adj.r.squared,4),round(summary(kelp_dp10_spline_m)$adj.r.squared,4),
                     round(summary(kelp_wd10_spline_m)$adj.r.squared,4),round(summary(kelp_ws10_spline_m)$adj.r.squared,4),
                     round(summary(kelp_wg10_spline_m)$adj.r.squared,4),round(summary(kelp_cig10_spline_m)$adj.r.squared,4),round(summary(kelp_vis10_spline_m)$adj.r.squared,4),
                     round(summary(kelp_cc10_spline_m)$adj.r.squared,4),round(summary(kelp_alt10_spline_m)$adj.r.squared,4),
                     round(summary(ksgu_t10_spline_m)$adj.r.squared,4),round(summary(ksgu_dp10_spline_m)$adj.r.squared,4),
                     round(summary(ksgu_wd10_spline_m)$adj.r.squared,4),round(summary(ksgu_ws10_spline_m)$adj.r.squared,4),
                     round(summary(ksgu_wg10_spline_m)$adj.r.squared,4),round(summary(ksgu_cig10_spline_m)$adj.r.squared,4),round(summary(ksgu_vis10_spline_m)$adj.r.squared,4),
                     round(summary(ksgu_cc10_spline_m)$adj.r.squared,4),round(summary(ksgu_alt10_spline_m)$adj.r.squared,4)),
  twenty_yr_monthly = c(round(summary(krdm_t20_spline_m)$adj.r.squared,4),round(summary(krdm_dp20_spline_m)$adj.r.squared,4),
                        round(summary(krdm_wd20_spline_m)$adj.r.squared,4),round(summary(krdm_ws20_spline_m)$adj.r.squared,4),
                        round(summary(krdm_wg20_spline_m)$adj.r.squared,4),round(summary(krdm_cig20_spline_m)$adj.r.squared,4),round(summary(krdm_vis20_spline_m)$adj.r.squared,4),
                        round(summary(krdm_cc20_spline_m)$adj.r.squared,4),round(summary(krdm_alt20_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_t20_spline_m)$adj.r.squared,4),round(summary(kbuf_dp20_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_wd20_spline_m)$adj.r.squared,4),round(summary(kbuf_ws20_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_wg20_spline_m)$adj.r.squared,4),round(summary(kbuf_cig20_spline_m)$adj.r.squared,4),round(summary(kbuf_vis20_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_cc20_spline_m)$adj.r.squared,4),round(summary(kbuf_alt20_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_t20_spline_m)$adj.r.squared,4),round(summary(kfoe_dp20_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_wd20_spline_m)$adj.r.squared,4),round(summary(kfoe_ws20_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_wg20_spline_m)$adj.r.squared,4),round(summary(kfoe_cig20_spline_m)$adj.r.squared,4),round(summary(kfoe_vis20_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_cc20_spline_m)$adj.r.squared,4),round(summary(kfoe_alt20_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_t20_spline_m)$adj.r.squared,4),round(summary(kmsn_dp20_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_wd20_spline_m)$adj.r.squared,4),round(summary(kmsn_ws20_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_wg20_spline_m)$adj.r.squared,4),round(summary(kmsn_cig20_spline_m)$adj.r.squared,4),round(summary(kmsn_vis20_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_cc20_spline_m)$adj.r.squared,4),round(summary(kmsn_alt20_spline_m)$adj.r.squared,4),
                        round(summary(ktri_t20_spline_m)$adj.r.squared,4),round(summary(ktri_dp20_spline_m)$adj.r.squared,4),
                        round(summary(ktri_wd20_spline_m)$adj.r.squared,4),round(summary(ktri_ws20_spline_m)$adj.r.squared,4),
                        round(summary(ktri_wg20_spline_m)$adj.r.squared,4),round(summary(ktri_cig20_spline_m)$adj.r.squared,4),round(summary(ktri_vis20_spline_m)$adj.r.squared,4),
                        round(summary(ktri_cc20_spline_m)$adj.r.squared,4),round(summary(ktri_alt20_spline_m)$adj.r.squared,4),
                        round(summary(pajn_t20_spline_m)$adj.r.squared,4),round(summary(pajn_dp20_spline_m)$adj.r.squared,4),
                        round(summary(pajn_wd20_spline_m)$adj.r.squared,4),round(summary(pajn_ws20_spline_m)$adj.r.squared,4),
                        round(summary(pajn_wg20_spline_m)$adj.r.squared,4),round(summary(pajn_cig20_spline_m)$adj.r.squared,4),round(summary(pajn_vis20_spline_m)$adj.r.squared,4),
                        round(summary(pajn_cc20_spline_m)$adj.r.squared,4),round(summary(pajn_alt20_spline_m)$adj.r.squared,4),
                        round(summary(kelp_t20_spline_m)$adj.r.squared,4),round(summary(kelp_dp20_spline_m)$adj.r.squared,4),
                        round(summary(kelp_wd20_spline_m)$adj.r.squared,4),round(summary(kelp_ws20_spline_m)$adj.r.squared,4),
                        round(summary(kelp_wg20_spline_m)$adj.r.squared,4),round(summary(kelp_cig20_spline_m)$adj.r.squared,4),round(summary(kelp_vis20_spline_m)$adj.r.squared,4),
                        round(summary(kelp_cc20_spline_m)$adj.r.squared,4),round(summary(kelp_alt20_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_t20_spline_m)$adj.r.squared,4),round(summary(ksgu_dp20_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_wd20_spline_m)$adj.r.squared,4),round(summary(ksgu_ws20_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_wg20_spline_m)$adj.r.squared,4),round(summary(ksgu_cig20_spline_m)$adj.r.squared,4),round(summary(ksgu_vis20_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_cc20_spline_m)$adj.r.squared,4),round(summary(ksgu_alt20_spline_m)$adj.r.squared,4)),
  thirty_yr_monthly = c(round(summary(krdm_t30_spline_m)$adj.r.squared,4),round(summary(krdm_dp30_spline_m)$adj.r.squared,4),
                        round(summary(krdm_wd30_spline_m)$adj.r.squared,4),round(summary(krdm_ws30_spline_m)$adj.r.squared,4),
                        round(summary(krdm_wg30_spline_m)$adj.r.squared,4),round(summary(krdm_cig30_spline_m)$adj.r.squared,4),round(summary(krdm_vis30_spline_m)$adj.r.squared,4),
                        round(summary(krdm_cc30_spline_m)$adj.r.squared,4),round(summary(krdm_alt30_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_t30_spline_m)$adj.r.squared,4),round(summary(kbuf_dp30_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_wd30_spline_m)$adj.r.squared,4),round(summary(kbuf_ws30_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_wg30_spline_m)$adj.r.squared,4),round(summary(kbuf_cig30_spline_m)$adj.r.squared,4),round(summary(kbuf_vis30_spline_m)$adj.r.squared,4),
                        round(summary(kbuf_cc30_spline_m)$adj.r.squared,4),round(summary(kbuf_alt30_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_t30_spline_m)$adj.r.squared,4),round(summary(kfoe_dp30_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_wd30_spline_m)$adj.r.squared,4),round(summary(kfoe_ws30_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_wg30_spline_m)$adj.r.squared,4),round(summary(kfoe_cig30_spline_m)$adj.r.squared,4),round(summary(kfoe_vis30_spline_m)$adj.r.squared,4),
                        round(summary(kfoe_cc30_spline_m)$adj.r.squared,4),round(summary(kfoe_alt30_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_t30_spline_m)$adj.r.squared,4),round(summary(kmsn_dp30_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_wd30_spline_m)$adj.r.squared,4),round(summary(kmsn_ws30_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_wg30_spline_m)$adj.r.squared,4),round(summary(kmsn_cig30_spline_m)$adj.r.squared,4),round(summary(kmsn_vis30_spline_m)$adj.r.squared,4),
                        round(summary(kmsn_cc30_spline_m)$adj.r.squared,4),round(summary(kmsn_alt30_spline_m)$adj.r.squared,4),
                        round(summary(ktri_t30_spline_m)$adj.r.squared,4),round(summary(ktri_dp30_spline_m)$adj.r.squared,4),
                        round(summary(ktri_wd30_spline_m)$adj.r.squared,4),round(summary(ktri_ws30_spline_m)$adj.r.squared,4),
                        round(summary(ktri_wg30_spline_m)$adj.r.squared,4),round(summary(ktri_cig30_spline_m)$adj.r.squared,4),round(summary(ktri_vis30_spline_m)$adj.r.squared,4),
                        round(summary(ktri_cc30_spline_m)$adj.r.squared,4),round(summary(ktri_alt30_spline_m)$adj.r.squared,4),
                        round(summary(pajn_t30_spline_m)$adj.r.squared,4),round(summary(pajn_dp30_spline_m)$adj.r.squared,4),
                        round(summary(pajn_wd30_spline_m)$adj.r.squared,4),round(summary(pajn_ws30_spline_m)$adj.r.squared,4),
                        round(summary(pajn_wg30_spline_m)$adj.r.squared,4),round(summary(pajn_cig30_spline_m)$adj.r.squared,4),round(summary(pajn_vis30_spline_m)$adj.r.squared,4),
                        round(summary(pajn_cc30_spline_m)$adj.r.squared,4),round(summary(pajn_alt30_spline_m)$adj.r.squared,4),
                        round(summary(kelp_t30_spline_m)$adj.r.squared,4),round(summary(kelp_dp30_spline_m)$adj.r.squared,4),
                        round(summary(kelp_wd30_spline_m)$adj.r.squared,4),round(summary(kelp_ws30_spline_m)$adj.r.squared,4),
                        round(summary(kelp_wg30_spline_m)$adj.r.squared,4),round(summary(kelp_cig30_spline_m)$adj.r.squared,4),round(summary(kelp_vis30_spline_m)$adj.r.squared,4),
                        round(summary(kelp_cc30_spline_m)$adj.r.squared,4),round(summary(kelp_alt30_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_t30_spline_m)$adj.r.squared,4),round(summary(ksgu_dp30_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_wd30_spline_m)$adj.r.squared,4),round(summary(ksgu_ws30_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_wg30_spline_m)$adj.r.squared,4),round(summary(ksgu_cig30_spline_m)$adj.r.squared,4),round(summary(ksgu_vis30_spline_m)$adj.r.squared,4),
                        round(summary(ksgu_cc30_spline_m)$adj.r.squared,4),round(summary(ksgu_alt30_spline_m)$adj.r.squared,4)),
  ten_yr_daily = c(round(summary(krdm_t10_spline_d)$adj.r.squared,4),round(summary(krdm_dp10_spline_d)$adj.r.squared,4),
                   round(summary(krdm_wd10_spline_d)$adj.r.squared,4),round(summary(krdm_ws10_spline_d)$adj.r.squared,4),
                   round(summary(krdm_wg10_spline_d)$adj.r.squared,4),round(summary(krdm_cig10_spline_d)$adj.r.squared,4),round(summary(krdm_vis10_spline_d)$adj.r.squared,4),
                   round(summary(krdm_cc10_spline_d)$adj.r.squared,4),round(summary(krdm_alt10_spline_d)$adj.r.squared,4),
                   round(summary(kbuf_t10_spline_d)$adj.r.squared,4),round(summary(kbuf_dp10_spline_d)$adj.r.squared,4),
                   round(summary(kbuf_wd10_spline_d)$adj.r.squared,4),round(summary(kbuf_ws10_spline_d)$adj.r.squared,4),
                   round(summary(kbuf_wg10_spline_d)$adj.r.squared,4),round(summary(kbuf_cig10_spline_d)$adj.r.squared,4),round(summary(kbuf_vis10_spline_d)$adj.r.squared,4),
                   round(summary(kbuf_cc10_spline_d)$adj.r.squared,4),round(summary(kbuf_alt10_spline_d)$adj.r.squared,4),
                   round(summary(kfoe_t10_spline_d)$adj.r.squared,4),round(summary(kfoe_dp10_spline_d)$adj.r.squared,4),
                   round(summary(kfoe_wd10_spline_d)$adj.r.squared,4),round(summary(kfoe_ws10_spline_d)$adj.r.squared,4),
                   round(summary(kfoe_wg10_spline_d)$adj.r.squared,4),round(summary(kfoe_cig10_spline_d)$adj.r.squared,4),round(summary(kfoe_vis10_spline_d)$adj.r.squared,4),
                   round(summary(kfoe_cc10_spline_d)$adj.r.squared,4),round(summary(kfoe_alt10_spline_d)$adj.r.squared,4),
                   round(summary(kmsn_t10_spline_d)$adj.r.squared,4),round(summary(kmsn_dp10_spline_d)$adj.r.squared,4),
                   round(summary(kmsn_wd10_spline_d)$adj.r.squared,4),round(summary(kmsn_ws10_spline_d)$adj.r.squared,4),
                   round(summary(kmsn_wg10_spline_d)$adj.r.squared,4),round(summary(kmsn_cig10_spline_d)$adj.r.squared,4),round(summary(kmsn_vis10_spline_d)$adj.r.squared,4),
                   round(summary(kmsn_cc10_spline_d)$adj.r.squared,4),round(summary(kmsn_alt10_spline_d)$adj.r.squared,4),
                   round(summary(ktri_t10_spline_d)$adj.r.squared,4),round(summary(ktri_dp10_spline_d)$adj.r.squared,4),
                   round(summary(ktri_wd10_spline_d)$adj.r.squared,4),round(summary(ktri_ws10_spline_d)$adj.r.squared,4),
                   round(summary(ktri_wg10_spline_d)$adj.r.squared,4),round(summary(ktri_cig10_spline_d)$adj.r.squared,4),round(summary(ktri_vis10_spline_d)$adj.r.squared,4),
                   round(summary(ktri_cc10_spline_d)$adj.r.squared,4),round(summary(ktri_alt10_spline_d)$adj.r.squared,4),
                   round(summary(pajn_t10_spline_d)$adj.r.squared,4),round(summary(pajn_dp10_spline_d)$adj.r.squared,4),
                   round(summary(pajn_wd10_spline_d)$adj.r.squared,4),round(summary(pajn_ws10_spline_d)$adj.r.squared,4),
                   round(summary(pajn_wg10_spline_d)$adj.r.squared,4),round(summary(pajn_cig10_spline_d)$adj.r.squared,4),round(summary(pajn_vis10_spline_d)$adj.r.squared,4),
                   round(summary(pajn_cc10_spline_d)$adj.r.squared,4),round(summary(pajn_alt10_spline_d)$adj.r.squared,4),
                   round(summary(kelp_t10_spline_d)$adj.r.squared,4),round(summary(kelp_dp10_spline_d)$adj.r.squared,4),
                   round(summary(kelp_wd10_spline_d)$adj.r.squared,4),round(summary(kelp_ws10_spline_d)$adj.r.squared,4),
                   round(summary(kelp_wg10_spline_d)$adj.r.squared,4),round(summary(kelp_cig10_spline_d)$adj.r.squared,4),round(summary(kelp_vis10_spline_d)$adj.r.squared,4),
                   round(summary(kelp_cc10_spline_d)$adj.r.squared,4),round(summary(kelp_alt10_spline_d)$adj.r.squared,4),
                   round(summary(ksgu_t10_spline_d)$adj.r.squared,4),round(summary(ksgu_dp10_spline_d)$adj.r.squared,4),
                   round(summary(ksgu_wd10_spline_d)$adj.r.squared,4),round(summary(ksgu_ws10_spline_d)$adj.r.squared,4),
                   round(summary(ksgu_wg10_spline_d)$adj.r.squared,4),round(summary(ksgu_cig10_spline_d)$adj.r.squared,4),round(summary(ksgu_vis10_spline_d)$adj.r.squared,4),
                   round(summary(ksgu_cc10_spline_d)$adj.r.squared,4),round(summary(ksgu_alt10_spline_d)$adj.r.squared,4)),
  twenty_yr_daily = c(round(summary(krdm_t20_spline_d)$adj.r.squared,4),round(summary(krdm_dp20_spline_d)$adj.r.squared,4),
                      round(summary(krdm_wd20_spline_d)$adj.r.squared,4),round(summary(krdm_ws20_spline_d)$adj.r.squared,4),
                      round(summary(krdm_wg20_spline_d)$adj.r.squared,4),round(summary(krdm_cig20_spline_d)$adj.r.squared,4),round(summary(krdm_vis20_spline_d)$adj.r.squared,4),
                      round(summary(krdm_cc20_spline_d)$adj.r.squared,4),round(summary(krdm_alt20_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_t20_spline_d)$adj.r.squared,4),round(summary(kbuf_dp20_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_wd20_spline_d)$adj.r.squared,4),round(summary(kbuf_ws20_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_wg20_spline_d)$adj.r.squared,4),round(summary(kbuf_cig20_spline_d)$adj.r.squared,4),round(summary(kbuf_vis20_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_cc20_spline_d)$adj.r.squared,4),round(summary(kbuf_alt20_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_t20_spline_d)$adj.r.squared,4),round(summary(kfoe_dp20_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_wd20_spline_d)$adj.r.squared,4),round(summary(kfoe_ws20_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_wg20_spline_d)$adj.r.squared,4),round(summary(kfoe_cig20_spline_d)$adj.r.squared,4),round(summary(kfoe_vis20_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_cc20_spline_d)$adj.r.squared,4),round(summary(kfoe_alt20_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_t20_spline_d)$adj.r.squared,4),round(summary(kmsn_dp20_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_wd20_spline_d)$adj.r.squared,4),round(summary(kmsn_ws20_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_wg20_spline_d)$adj.r.squared,4),round(summary(kmsn_cig20_spline_d)$adj.r.squared,4),round(summary(kmsn_vis20_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_cc20_spline_d)$adj.r.squared,4),round(summary(kmsn_alt20_spline_d)$adj.r.squared,4),
                      round(summary(ktri_t20_spline_d)$adj.r.squared,4),round(summary(ktri_dp20_spline_d)$adj.r.squared,4),
                      round(summary(ktri_wd20_spline_d)$adj.r.squared,4),round(summary(ktri_ws20_spline_d)$adj.r.squared,4),
                      round(summary(ktri_wg20_spline_d)$adj.r.squared,4),round(summary(ktri_cig20_spline_d)$adj.r.squared,4),round(summary(ktri_vis20_spline_d)$adj.r.squared,4),
                      round(summary(ktri_cc20_spline_d)$adj.r.squared,4),round(summary(ktri_alt20_spline_d)$adj.r.squared,4),
                      round(summary(pajn_t20_spline_d)$adj.r.squared,4),round(summary(pajn_dp20_spline_d)$adj.r.squared,4),
                      round(summary(pajn_wd20_spline_d)$adj.r.squared,4),round(summary(pajn_ws20_spline_d)$adj.r.squared,4),
                      round(summary(pajn_wg20_spline_d)$adj.r.squared,4),round(summary(pajn_cig20_spline_d)$adj.r.squared,4),round(summary(pajn_vis20_spline_d)$adj.r.squared,4),
                      round(summary(pajn_cc20_spline_d)$adj.r.squared,4),round(summary(pajn_alt20_spline_d)$adj.r.squared,4),
                      round(summary(kelp_t20_spline_d)$adj.r.squared,4),round(summary(kelp_dp20_spline_d)$adj.r.squared,4),
                      round(summary(kelp_wd20_spline_d)$adj.r.squared,4),round(summary(kelp_ws20_spline_d)$adj.r.squared,4),
                      round(summary(kelp_wg20_spline_d)$adj.r.squared,4),round(summary(kelp_cig20_spline_d)$adj.r.squared,4),round(summary(kelp_vis20_spline_d)$adj.r.squared,4),
                      round(summary(kelp_cc20_spline_d)$adj.r.squared,4),round(summary(kelp_alt20_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_t20_spline_d)$adj.r.squared,4),round(summary(ksgu_dp20_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_wd20_spline_d)$adj.r.squared,4),round(summary(ksgu_ws20_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_wg20_spline_d)$adj.r.squared,4),round(summary(ksgu_cig20_spline_d)$adj.r.squared,4),round(summary(ksgu_vis20_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_cc20_spline_d)$adj.r.squared,4),round(summary(ksgu_alt20_spline_d)$adj.r.squared,4)),
  thirty_yr_daily = c(round(summary(krdm_t30_spline_d)$adj.r.squared,4),round(summary(krdm_dp30_spline_d)$adj.r.squared,4),
                      round(summary(krdm_wd30_spline_d)$adj.r.squared,4),round(summary(krdm_ws30_spline_d)$adj.r.squared,4),
                      round(summary(krdm_wg30_spline_d)$adj.r.squared,4),round(summary(krdm_cig30_spline_d)$adj.r.squared,4),round(summary(krdm_vis30_spline_d)$adj.r.squared,4),
                      round(summary(krdm_cc30_spline_d)$adj.r.squared,4),round(summary(krdm_alt30_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_t30_spline_d)$adj.r.squared,4),round(summary(kbuf_dp30_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_wd30_spline_d)$adj.r.squared,4),round(summary(kbuf_ws30_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_wg30_spline_d)$adj.r.squared,4),round(summary(kbuf_cig30_spline_d)$adj.r.squared,4),round(summary(kbuf_vis30_spline_d)$adj.r.squared,4),
                      round(summary(kbuf_cc30_spline_d)$adj.r.squared,4),round(summary(kbuf_alt30_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_t30_spline_d)$adj.r.squared,4),round(summary(kfoe_dp30_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_wd30_spline_d)$adj.r.squared,4),round(summary(kfoe_ws30_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_wg30_spline_d)$adj.r.squared,4),round(summary(kfoe_cig30_spline_d)$adj.r.squared,4),round(summary(kfoe_vis30_spline_d)$adj.r.squared,4),
                      round(summary(kfoe_cc30_spline_d)$adj.r.squared,4),round(summary(kfoe_alt30_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_t30_spline_d)$adj.r.squared,4),round(summary(kmsn_dp30_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_wd30_spline_d)$adj.r.squared,4),round(summary(kmsn_ws30_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_wg30_spline_d)$adj.r.squared,4),round(summary(kmsn_cig30_spline_d)$adj.r.squared,4),round(summary(kmsn_vis30_spline_d)$adj.r.squared,4),
                      round(summary(kmsn_cc30_spline_d)$adj.r.squared,4),round(summary(kmsn_alt30_spline_d)$adj.r.squared,4),
                      round(summary(ktri_t30_spline_d)$adj.r.squared,4),round(summary(ktri_dp30_spline_d)$adj.r.squared,4),
                      round(summary(ktri_wd30_spline_d)$adj.r.squared,4),round(summary(ktri_ws30_spline_d)$adj.r.squared,4),
                      round(summary(ktri_wg30_spline_d)$adj.r.squared,4),round(summary(ktri_cig30_spline_d)$adj.r.squared,4),round(summary(ktri_vis30_spline_d)$adj.r.squared,4),
                      round(summary(ktri_cc30_spline_d)$adj.r.squared,4),round(summary(ktri_alt30_spline_d)$adj.r.squared,4),
                      round(summary(pajn_t30_spline_d)$adj.r.squared,4),round(summary(pajn_dp30_spline_d)$adj.r.squared,4),
                      round(summary(pajn_wd30_spline_d)$adj.r.squared,4),round(summary(pajn_ws30_spline_d)$adj.r.squared,4),
                      round(summary(pajn_wg30_spline_d)$adj.r.squared,4),round(summary(pajn_cig30_spline_d)$adj.r.squared,4),round(summary(pajn_vis30_spline_d)$adj.r.squared,4),
                      round(summary(pajn_cc30_spline_d)$adj.r.squared,4),round(summary(pajn_alt30_spline_d)$adj.r.squared,4),
                      round(summary(kelp_t30_spline_d)$adj.r.squared,4),round(summary(kelp_dp30_spline_d)$adj.r.squared,4),
                      round(summary(kelp_wd30_spline_d)$adj.r.squared,4),round(summary(kelp_ws30_spline_d)$adj.r.squared,4),
                      round(summary(kelp_wg30_spline_d)$adj.r.squared,4),round(summary(kelp_cig30_spline_d)$adj.r.squared,4),round(summary(kelp_vis30_spline_d)$adj.r.squared,4),
                      round(summary(kelp_cc30_spline_d)$adj.r.squared,4),round(summary(kelp_alt30_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_t30_spline_d)$adj.r.squared,4),round(summary(ksgu_dp30_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_wd30_spline_d)$adj.r.squared,4),round(summary(ksgu_ws30_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_wg30_spline_d)$adj.r.squared,4),round(summary(ksgu_cig30_spline_d)$adj.r.squared,4),round(summary(ksgu_vis30_spline_d)$adj.r.squared,4),
                      round(summary(ksgu_cc30_spline_d)$adj.r.squared,4),round(summary(ksgu_alt30_spline_m)$adj.r.squared,4)),
  ten_yr_10day = c(round(summary(krdm_t10_spline_10d)$adj.r.squared,4),round(summary(krdm_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(krdm_wd10_spline_10d)$adj.r.squared,4),round(summary(krdm_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(krdm_wg10_spline_10d)$adj.r.squared,4),round(summary(krdm_cig10_spline_10d)$adj.r.squared,4),round(summary(krdm_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(krdm_cc10_spline_10d)$adj.r.squared,4),round(summary(krdm_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_t10_spline_10d)$adj.r.squared,4),round(summary(kbuf_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_wd10_spline_10d)$adj.r.squared,4),round(summary(kbuf_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_wg10_spline_10d)$adj.r.squared,4),round(summary(kbuf_cig10_spline_10d)$adj.r.squared,4),round(summary(kbuf_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_cc10_spline_10d)$adj.r.squared,4),round(summary(kbuf_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_t10_spline_10d)$adj.r.squared,4),round(summary(kfoe_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_wd10_spline_10d)$adj.r.squared,4),round(summary(kfoe_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_wg10_spline_10d)$adj.r.squared,4),round(summary(kfoe_cig10_spline_10d)$adj.r.squared,4),round(summary(kfoe_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_cc10_spline_10d)$adj.r.squared,4),round(summary(kfoe_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_t10_spline_10d)$adj.r.squared,4),round(summary(kmsn_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_wd10_spline_10d)$adj.r.squared,4),round(summary(kmsn_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_wg10_spline_10d)$adj.r.squared,4),round(summary(kmsn_cig10_spline_10d)$adj.r.squared,4),round(summary(kmsn_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_cc10_spline_10d)$adj.r.squared,4),round(summary(kmsn_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_t10_spline_10d)$adj.r.squared,4),round(summary(ktri_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_wd10_spline_10d)$adj.r.squared,4),round(summary(ktri_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_wg10_spline_10d)$adj.r.squared,4),round(summary(ktri_cig10_spline_10d)$adj.r.squared,4),round(summary(ktri_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_cc10_spline_10d)$adj.r.squared,4),round(summary(ktri_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_t10_spline_10d)$adj.r.squared,4),round(summary(pajn_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_wd10_spline_10d)$adj.r.squared,4),round(summary(pajn_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_wg10_spline_10d)$adj.r.squared,4),round(summary(pajn_cig10_spline_10d)$adj.r.squared,4),round(summary(pajn_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_cc10_spline_10d)$adj.r.squared,4),round(summary(pajn_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_t10_spline_10d)$adj.r.squared,4),round(summary(kelp_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_wd10_spline_10d)$adj.r.squared,4),round(summary(kelp_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_wg10_spline_10d)$adj.r.squared,4),round(summary(kelp_cig10_spline_10d)$adj.r.squared,4),round(summary(kelp_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_cc10_spline_10d)$adj.r.squared,4),round(summary(kelp_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_t10_spline_10d)$adj.r.squared,4),round(summary(ksgu_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_wd10_spline_10d)$adj.r.squared,4),round(summary(ksgu_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_wg10_spline_10d)$adj.r.squared,4),round(summary(ksgu_cig10_spline_10d)$adj.r.squared,4),round(summary(ksgu_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_cc10_spline_10d)$adj.r.squared,4),round(summary(ksgu_alt10_spline_10d)$adj.r.squared,4)),
  twenty_yr_10day = c(round(summary(krdm_t20_spline_10d)$adj.r.squared,4),round(summary(krdm_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(krdm_wd20_spline_10d)$adj.r.squared,4),round(summary(krdm_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(krdm_wg20_spline_10d)$adj.r.squared,4),round(summary(krdm_cig20_spline_10d)$adj.r.squared,4),round(summary(krdm_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(krdm_cc20_spline_10d)$adj.r.squared,4),round(summary(krdm_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_t20_spline_10d)$adj.r.squared,4),round(summary(kbuf_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_wd20_spline_10d)$adj.r.squared,4),round(summary(kbuf_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_wg20_spline_10d)$adj.r.squared,4),round(summary(kbuf_cig20_spline_10d)$adj.r.squared,4),round(summary(kbuf_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_cc20_spline_10d)$adj.r.squared,4),round(summary(kbuf_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_t20_spline_10d)$adj.r.squared,4),round(summary(kfoe_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_wd20_spline_10d)$adj.r.squared,4),round(summary(kfoe_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_wg20_spline_10d)$adj.r.squared,4),round(summary(kfoe_cig20_spline_10d)$adj.r.squared,4),round(summary(kfoe_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_cc20_spline_10d)$adj.r.squared,4),round(summary(kfoe_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_t20_spline_10d)$adj.r.squared,4),round(summary(kmsn_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_wd20_spline_10d)$adj.r.squared,4),round(summary(kmsn_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_wg20_spline_10d)$adj.r.squared,4),round(summary(kmsn_cig20_spline_10d)$adj.r.squared,4),round(summary(kmsn_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_cc20_spline_10d)$adj.r.squared,4),round(summary(kmsn_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_t20_spline_10d)$adj.r.squared,4),round(summary(ktri_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_wd20_spline_10d)$adj.r.squared,4),round(summary(ktri_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_wg20_spline_10d)$adj.r.squared,4),round(summary(ktri_cig20_spline_10d)$adj.r.squared,4),round(summary(ktri_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_cc20_spline_10d)$adj.r.squared,4),round(summary(ktri_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_t20_spline_10d)$adj.r.squared,4),round(summary(pajn_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_wd20_spline_10d)$adj.r.squared,4),round(summary(pajn_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_wg20_spline_10d)$adj.r.squared,4),round(summary(pajn_cig20_spline_10d)$adj.r.squared,4),round(summary(pajn_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_cc20_spline_10d)$adj.r.squared,4),round(summary(pajn_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_t20_spline_10d)$adj.r.squared,4),round(summary(kelp_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_wd20_spline_10d)$adj.r.squared,4),round(summary(kelp_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_wg20_spline_10d)$adj.r.squared,4),round(summary(kelp_cig20_spline_10d)$adj.r.squared,4),round(summary(kelp_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_cc20_spline_10d)$adj.r.squared,4),round(summary(kelp_alt20_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_t20_spline_10d)$adj.r.squared,4),round(summary(ksgu_dp20_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_wd20_spline_10d)$adj.r.squared,4),round(summary(ksgu_ws20_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_wg20_spline_10d)$adj.r.squared,4),round(summary(ksgu_cig20_spline_10d)$adj.r.squared,4),round(summary(ksgu_vis20_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_cc20_spline_10d)$adj.r.squared,4),round(summary(ksgu_alt20_spline_10d)$adj.r.squared,4)),
  thirty_yr_10day = c(round(summary(krdm_t30_spline_10d)$adj.r.squared,4),round(summary(krdm_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(krdm_wd30_spline_10d)$adj.r.squared,4),round(summary(krdm_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(krdm_wg30_spline_10d)$adj.r.squared,4),round(summary(krdm_cig30_spline_10d)$adj.r.squared,4),round(summary(krdm_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(krdm_cc30_spline_10d)$adj.r.squared,4),round(summary(krdm_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_t30_spline_10d)$adj.r.squared,4),round(summary(kbuf_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_wd30_spline_10d)$adj.r.squared,4),round(summary(kbuf_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_wg30_spline_10d)$adj.r.squared,4),round(summary(kbuf_cig30_spline_10d)$adj.r.squared,4),round(summary(kbuf_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(kbuf_cc30_spline_10d)$adj.r.squared,4),round(summary(kbuf_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_t30_spline_10d)$adj.r.squared,4),round(summary(kfoe_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_wd30_spline_10d)$adj.r.squared,4),round(summary(kfoe_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_wg30_spline_10d)$adj.r.squared,4),round(summary(kfoe_cig30_spline_10d)$adj.r.squared,4),round(summary(kfoe_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(kfoe_cc30_spline_10d)$adj.r.squared,4),round(summary(kfoe_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_t30_spline_10d)$adj.r.squared,4),round(summary(kmsn_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_wd30_spline_10d)$adj.r.squared,4),round(summary(kmsn_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_wg30_spline_10d)$adj.r.squared,4),round(summary(kmsn_cig30_spline_10d)$adj.r.squared,4),round(summary(kmsn_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(kmsn_cc30_spline_10d)$adj.r.squared,4),round(summary(kmsn_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_t30_spline_10d)$adj.r.squared,4),round(summary(ktri_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_wd30_spline_10d)$adj.r.squared,4),round(summary(ktri_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_wg30_spline_10d)$adj.r.squared,4),round(summary(ktri_cig30_spline_10d)$adj.r.squared,4),round(summary(ktri_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(ktri_cc30_spline_10d)$adj.r.squared,4),round(summary(ktri_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_t30_spline_10d)$adj.r.squared,4),round(summary(pajn_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_wd30_spline_10d)$adj.r.squared,4),round(summary(pajn_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_wg30_spline_10d)$adj.r.squared,4),round(summary(pajn_cig30_spline_10d)$adj.r.squared,4),round(summary(pajn_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(pajn_cc30_spline_10d)$adj.r.squared,4),round(summary(pajn_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_t30_spline_10d)$adj.r.squared,4),round(summary(kelp_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_wd30_spline_10d)$adj.r.squared,4),round(summary(kelp_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_wg30_spline_10d)$adj.r.squared,4),round(summary(kelp_cig30_spline_10d)$adj.r.squared,4),round(summary(kelp_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(kelp_cc30_spline_10d)$adj.r.squared,4),round(summary(kelp_alt30_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_t30_spline_10d)$adj.r.squared,4),round(summary(ksgu_dp30_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_wd30_spline_10d)$adj.r.squared,4),round(summary(ksgu_ws30_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_wg30_spline_10d)$adj.r.squared,4),round(summary(ksgu_cig30_spline_10d)$adj.r.squared,4),round(summary(ksgu_vis30_spline_10d)$adj.r.squared,4),
                      round(summary(ksgu_cc30_spline_10d)$adj.r.squared,4),round(summary(ksgu_alt30_spline_10d)$adj.r.squared,4)),
  vars = c('Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter')
)


### adjusted r-squared values for the revised 10,20,30 year data for trends

r_squared_revised.df <- data.frame(
  station = c('KRDM','KRDM','KRDM','KRDM','KRDM','KRDM','KRDM','KRDM','KRDM',
              'KBUF','KBUF','KBUF','KBUF','KBUF','KBUF','KBUF','KBUF','KBUF',
              'KFOE','KFOE','KFOE','KFOE','KFOE','KFOE','KFOE','KFOE','KFOE',
              'KMSN','KMSN','KMSN','KMSN','KMSN','KMSN','KMSN','KMSN','KMSN',
              'KTRI','KTRI','KTRI','KTRI','KTRI','KTRI','KTRI','KTRI','KTRI',
              'PAJN','PAJN','PAJN','PAJN','PAJN','PAJN','PAJN','PAJN','PAJN',
              'KELP','KELP','KELP','KELP','KELP','KELP','KELP','KELP','KELP',
              'KSGU','KSGU','KSGU','KSGU','KSGU','KSGU','KSGU','KSGU','KSGU'),
  ten_yr_10day = c(round(summary(krdm_t10_spline_10d)$adj.r.squared,4),round(summary(krdm_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(krdm_wd10_spline_10d)$adj.r.squared,4),round(summary(krdm_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(krdm_wg10_spline_10d)$adj.r.squared,4),round(summary(krdm_cig10_spline_10d)$adj.r.squared,4),round(summary(krdm_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(krdm_cc10_spline_10d)$adj.r.squared,4),round(summary(krdm_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_t10_spline_10d)$adj.r.squared,4),round(summary(kbuf_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_wd10_spline_10d)$adj.r.squared,4),round(summary(kbuf_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_wg10_spline_10d)$adj.r.squared,4),round(summary(kbuf_cig10_spline_10d)$adj.r.squared,4),round(summary(kbuf_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kbuf_cc10_spline_10d)$adj.r.squared,4),round(summary(kbuf_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_t10_spline_10d)$adj.r.squared,4),round(summary(kfoe_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_wd10_spline_10d)$adj.r.squared,4),round(summary(kfoe_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_wg10_spline_10d)$adj.r.squared,4),round(summary(kfoe_cig10_spline_10d)$adj.r.squared,4),round(summary(kfoe_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kfoe_cc10_spline_10d)$adj.r.squared,4),round(summary(kfoe_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_t10_spline_10d)$adj.r.squared,4),round(summary(kmsn_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_wd10_spline_10d)$adj.r.squared,4),round(summary(kmsn_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_wg10_spline_10d)$adj.r.squared,4),round(summary(kmsn_cig10_spline_10d)$adj.r.squared,4),round(summary(kmsn_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kmsn_cc10_spline_10d)$adj.r.squared,4),round(summary(kmsn_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_t10_spline_10d)$adj.r.squared,4),round(summary(ktri_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_wd10_spline_10d)$adj.r.squared,4),round(summary(ktri_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_wg10_spline_10d)$adj.r.squared,4),round(summary(ktri_cig10_spline_10d)$adj.r.squared,4),round(summary(ktri_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(ktri_cc10_spline_10d)$adj.r.squared,4),round(summary(ktri_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_t10_spline_10d)$adj.r.squared,4),round(summary(pajn_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_wd10_spline_10d)$adj.r.squared,4),round(summary(pajn_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_wg10_spline_10d)$adj.r.squared,4),round(summary(pajn_cig10_spline_10d)$adj.r.squared,4),round(summary(pajn_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(pajn_cc10_spline_10d)$adj.r.squared,4),round(summary(pajn_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_t10_spline_10d)$adj.r.squared,4),round(summary(kelp_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_wd10_spline_10d)$adj.r.squared,4),round(summary(kelp_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_wg10_spline_10d)$adj.r.squared,4),round(summary(kelp_cig10_spline_10d)$adj.r.squared,4),round(summary(kelp_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(kelp_cc10_spline_10d)$adj.r.squared,4),round(summary(kelp_alt10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_t10_spline_10d)$adj.r.squared,4),round(summary(ksgu_dp10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_wd10_spline_10d)$adj.r.squared,4),round(summary(ksgu_ws10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_wg10_spline_10d)$adj.r.squared,4),round(summary(ksgu_cig10_spline_10d)$adj.r.squared,4),round(summary(ksgu_vis10_spline_10d)$adj.r.squared,4),
                   round(summary(ksgu_cc10_spline_10d)$adj.r.squared,4),round(summary(ksgu_alt10_spline_10d)$adj.r.squared,4)),
  twenty_yr_rvsd_10day = c(round(summary(krdm_t202_spline_10d)$adj.r.squared,4),round(summary(krdm_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(krdm_wd202_spline_10d)$adj.r.squared,4),round(summary(krdm_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(krdm_wg202_spline_10d)$adj.r.squared,4),round(summary(krdm_cig202_spline_10d)$adj.r.squared,4),round(summary(krdm_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(krdm_cc202_spline_10d)$adj.r.squared,4),round(summary(krdm_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_t202_spline_10d)$adj.r.squared,4),round(summary(kbuf_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_wd202_spline_10d)$adj.r.squared,4),round(summary(kbuf_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_wg202_spline_10d)$adj.r.squared,4),round(summary(kbuf_cig202_spline_10d)$adj.r.squared,4),round(summary(kbuf_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_cc202_spline_10d)$adj.r.squared,4),round(summary(kbuf_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_t202_spline_10d)$adj.r.squared,4),round(summary(kfoe_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_wd202_spline_10d)$adj.r.squared,4),round(summary(kfoe_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_wg202_spline_10d)$adj.r.squared,4),round(summary(kfoe_cig202_spline_10d)$adj.r.squared,4),round(summary(kfoe_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_cc202_spline_10d)$adj.r.squared,4),round(summary(kfoe_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_t202_spline_10d)$adj.r.squared,4),round(summary(kmsn_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_wd202_spline_10d)$adj.r.squared,4),round(summary(kmsn_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_wg202_spline_10d)$adj.r.squared,4),round(summary(kmsn_cig202_spline_10d)$adj.r.squared,4),round(summary(kmsn_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_cc202_spline_10d)$adj.r.squared,4),round(summary(kmsn_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_t202_spline_10d)$adj.r.squared,4),round(summary(ktri_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_wd202_spline_10d)$adj.r.squared,4),round(summary(ktri_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_wg202_spline_10d)$adj.r.squared,4),round(summary(ktri_cig202_spline_10d)$adj.r.squared,4),round(summary(ktri_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_cc202_spline_10d)$adj.r.squared,4),round(summary(ktri_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_t202_spline_10d)$adj.r.squared,4),round(summary(pajn_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_wd202_spline_10d)$adj.r.squared,4),round(summary(pajn_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_wg202_spline_10d)$adj.r.squared,4),round(summary(pajn_cig202_spline_10d)$adj.r.squared,4),round(summary(pajn_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_cc202_spline_10d)$adj.r.squared,4),round(summary(pajn_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_t202_spline_10d)$adj.r.squared,4),round(summary(kelp_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_wd202_spline_10d)$adj.r.squared,4),round(summary(kelp_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_wg202_spline_10d)$adj.r.squared,4),round(summary(kelp_cig202_spline_10d)$adj.r.squared,4),round(summary(kelp_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_cc202_spline_10d)$adj.r.squared,4),round(summary(kelp_alt202_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_t202_spline_10d)$adj.r.squared,4),round(summary(ksgu_dp202_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_wd202_spline_10d)$adj.r.squared,4),round(summary(ksgu_ws202_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_wg202_spline_10d)$adj.r.squared,4),round(summary(ksgu_cig202_spline_10d)$adj.r.squared,4),round(summary(ksgu_vis202_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_cc202_spline_10d)$adj.r.squared,4),round(summary(ksgu_alt202_spline_10d)$adj.r.squared,4)),
  thirty_yr_rvsd_10day = c(round(summary(krdm_t302_spline_10d)$adj.r.squared,4),round(summary(krdm_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(krdm_wd302_spline_10d)$adj.r.squared,4),round(summary(krdm_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(krdm_wg302_spline_10d)$adj.r.squared,4),round(summary(krdm_cig302_spline_10d)$adj.r.squared,4),round(summary(krdm_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(krdm_cc302_spline_10d)$adj.r.squared,4),round(summary(krdm_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_t302_spline_10d)$adj.r.squared,4),round(summary(kbuf_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_wd302_spline_10d)$adj.r.squared,4),round(summary(kbuf_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_wg302_spline_10d)$adj.r.squared,4),round(summary(kbuf_cig302_spline_10d)$adj.r.squared,4),round(summary(kbuf_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(kbuf_cc302_spline_10d)$adj.r.squared,4),round(summary(kbuf_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_t302_spline_10d)$adj.r.squared,4),round(summary(kfoe_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_wd302_spline_10d)$adj.r.squared,4),round(summary(kfoe_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_wg302_spline_10d)$adj.r.squared,4),round(summary(kfoe_cig302_spline_10d)$adj.r.squared,4),round(summary(kfoe_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(kfoe_cc302_spline_10d)$adj.r.squared,4),round(summary(kfoe_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_t302_spline_10d)$adj.r.squared,4),round(summary(kmsn_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_wd302_spline_10d)$adj.r.squared,4),round(summary(kmsn_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_wg302_spline_10d)$adj.r.squared,4),round(summary(kmsn_cig302_spline_10d)$adj.r.squared,4),round(summary(kmsn_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(kmsn_cc302_spline_10d)$adj.r.squared,4),round(summary(kmsn_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_t302_spline_10d)$adj.r.squared,4),round(summary(ktri_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_wd302_spline_10d)$adj.r.squared,4),round(summary(ktri_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_wg302_spline_10d)$adj.r.squared,4),round(summary(ktri_cig302_spline_10d)$adj.r.squared,4),round(summary(ktri_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(ktri_cc302_spline_10d)$adj.r.squared,4),round(summary(ktri_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_t302_spline_10d)$adj.r.squared,4),round(summary(pajn_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_wd302_spline_10d)$adj.r.squared,4),round(summary(pajn_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_wg302_spline_10d)$adj.r.squared,4),round(summary(pajn_cig302_spline_10d)$adj.r.squared,4),round(summary(pajn_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(pajn_cc302_spline_10d)$adj.r.squared,4),round(summary(pajn_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_t302_spline_10d)$adj.r.squared,4),round(summary(kelp_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_wd302_spline_10d)$adj.r.squared,4),round(summary(kelp_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_wg302_spline_10d)$adj.r.squared,4),round(summary(kelp_cig302_spline_10d)$adj.r.squared,4),round(summary(kelp_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(kelp_cc302_spline_10d)$adj.r.squared,4),round(summary(kelp_alt302_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_t302_spline_10d)$adj.r.squared,4),round(summary(ksgu_dp302_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_wd302_spline_10d)$adj.r.squared,4),round(summary(ksgu_ws302_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_wg302_spline_10d)$adj.r.squared,4),round(summary(ksgu_cig302_spline_10d)$adj.r.squared,4),round(summary(ksgu_vis302_spline_10d)$adj.r.squared,4),
                           round(summary(ksgu_cc302_spline_10d)$adj.r.squared,4),round(summary(ksgu_alt302_spline_10d)$adj.r.squared,4)),
  vars = c('Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter',
           'Temperature','Dewpoint Temperature','Wind Direction','Wind Speed','Wind Gust Speed','Cloud Ceiling',
           'Visibility','Cloud Cover','Altimeter')
)

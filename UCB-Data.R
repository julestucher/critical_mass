## File for importing and formatting UC Berkeley data ##

library(plyr)
library(car)
library(EnvStats)

app_data = function() {
  berkeley.data = read.csv("./UCB Data/Demo Data/Freshmen_Eth_data.csv")
  berkeley.app = cbind(dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "All")["Academic.Yr"], dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "All")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "African American")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "American Indian")[["FilteredCountFR"]],
                 dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "Chicano/Latino")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "Asian")[["FilteredCountFR"]],
                 dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "White")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "Unknown")[["FilteredCountFR"]],
                 dplyr::filter(berkeley.data, Category == "Applicants" & Ethnicity == "International")[["FilteredCountFR"]])
  colnames(berkeley.app)=c("Year", "App.Total", "App.AfAm", "App.NaAm", "App.HiLa", "App.AsAm", "App.White", "App.Unknown", "App.Int")
  
  berkeley.ad = cbind(dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "All")["Academic.Yr"], dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "All")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "African American")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "American Indian")[["FilteredCountFR"]],
                 dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "Chicano/Latino")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "Asian")[["FilteredCountFR"]],
                 dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "White")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "Unknown")[["FilteredCountFR"]],
                 dplyr::filter(berkeley.data, Category == "Admits" & Ethnicity == "International")[["FilteredCountFR"]])
  colnames(berkeley.ad)=c("Year", "Ad.Total", "Ad.AfAm", "Ad.NaAm", "Ad.HiLa", "Ad.AsAm", "Ad.White", "Ad.Unknown", "Ad.Int")
  
  berkeley.en = cbind(dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "All")["Academic.Yr"], dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "All")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "African American")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "American Indian")[["FilteredCountFR"]],
                dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "Chicano/Latino")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "Asian")[["FilteredCountFR"]],
                dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "White")[["FilteredCountFR"]], dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "Unknown")[["FilteredCountFR"]],
                dplyr::filter(berkeley.data, Category == "Enrollees" & Ethnicity == "International")[["FilteredCountFR"]])
  colnames(berkeley.en)=c("Year", "En.Total", "En.AfAm", "En.NaAm", "En.HiLa", "En.AsAm", "En.White", "En.Unknown", "En.Int")
  
  uc.berkeley.all=cbind(berkeley.app, berkeley.ad[,c(2:9)], berkeley.en[,c(2:9)])
  uc.berkeley=cbind(berkeley.app[,c(1:3,5:7)], berkeley.ad[,c(2:3, 5:7)], berkeley.en[,c(2:3, 5:7)])
  uc.berkeley.temp=uc.berkeley[,c(1, 2, 7, 12, 3, 8, 13, 4, 9, 14, 5, 10, 15, 6, 11, 16)]
  uc.berkeley.ordered=uc.berkeley.temp[c(26:1),]
  uc.berkeley.data=uc.berkeley.ordered[,c(1:4)]
  
  # account for change in AffAct
  return(uc.berkeley.ordered[5:length(uc.berkeley.ordered[,1]),])
}

grad_rates = function() {
  years = 1999:2018
  
  black.data <- read.csv("./UCB Data/RR and Grad Data/Black_FR_rates.csv")
  black.data <- apply(black.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  black.data <- cbind(years,"Black.RR"=black.data[,2],"Black.4.year"=black.data[,4],"Black.5.year"=black.data[,5],"Black.6.year"=black.data[,6])
  
  hila.data = read.csv("./UCB Data/RR and Grad Data/HiLa_FR_rates.csv")
  hila.data <- apply(hila.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  hila.data <- cbind("HiLa.RR"=hila.data[,2],"HiLa.4.year"=hila.data[,4],"HiLa.5.year"=hila.data[,5],"HiLa.6.year"=hila.data[,6])
  
  asian.data = read.csv("./UCB Data/RR and Grad Data/Asian_FR_rates.csv")
  asian.data <- apply(asian.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  asian.data <- cbind("Asian.RR"=asian.data[,2],"Asian.4.year"=asian.data[,4],"Asian.5.year"=asian.data[,5],"Asian.6.year"=asian.data[,6])
  
  white.data = read.csv("./UCB Data/RR and Grad Data/White_FR_rates.csv")
  white.data <- apply(white.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  white.data <- cbind("White.RR"=white.data[,2],"White.4.year"=white.data[,4],"White.5.year"=white.data[,5],"White.6.year"=white.data[,6])
  
  rates = cbind(black.data, hila.data, asian.data, white.data)
  return(rates)
}

### Old Data (1994-2017) ###

# uc.berkeley.app=read.delim("./Berkeley Applicants 94-15.txt")
# uc.berkeley.app=rename(uc.berkeley.app, c('Total'='App.Total', 'AfAm'='App.AfAm', 'NaAm'='App.NaAm', 'HiLa'='App.HiLa', 'AsAm'='App.AsAm', 'White'='App.White', 'Unknown'='App.Unknown', 'Int'='App.Int'))
# uc.berkeley.ad=read.delim("./Berkeley Admits 94-15.txt")
# uc.berkeley.ad=rename(uc.berkeley.ad, c('Total'='Ad.Total', 'AfAm'='Ad.AfAm', 'NaAm'='Ad.NaAm', 'HiLa'='Ad.HiLa', 'AsAm'='Ad.AsAm', 'White'='Ad.White', 'Unknown'='Ad.Unknown', 'Int'='Ad.Int'))
# uc.berkeley.en=read.delim("./Berkeley Enrolees 94-15.txt")
# uc.berkeley.en=rename(uc.berkeley.en, c('Total'='En.Total', 'AfAm'='En.AfAm', 'NaAm'='En.NaAm', 'HiLa'='En.HiLa', 'AsAm'='En.AsAm', 'White'='En.White', 'Unknown'='En.Unknown', 'Int'='En.Int'))
# uc.berkeley.all=cbind(uc.berkeley.app, uc.berkeley.ad[,c(2:9)], uc.berkeley.en[,c(2:9)])
# uc.berkeley=cbind(uc.berkeley.app[,c(1:3,5:7)], uc.berkeley.ad[,c(2:3, 5:7)], uc.berkeley.en[,c(2:3, 5:7)])
# uc.berkeley.ordered=uc.berkeley[,c(1, 2, 7, 12, 3, 8, 13, 4, 9, 14, 5, 10, 15, 6, 11, 16)]
# uc.berkeley.data=uc.berkeley.ordered[,c(1:4)]
# 
# rm(uc.berkeley, uc.berkeley.ad, uc.berkeley.all, uc.berkeley.app, uc.berkeley.data, uc.berkeley.en)



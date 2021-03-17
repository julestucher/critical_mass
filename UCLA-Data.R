
## File for importing and formatting UC data ##

library(plyr)
library(car)
library(EnvStats)

app_data = function() {
  la.data = read.csv("./UCLA Data/Demo Data/Freshmen_Eth_data.csv")
  la.app = cbind(dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "All")["Academic.Yr"], dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "All")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "African American")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "American Indian")[["FilteredCountFR"]],
                 dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "Chicano/Latino")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "Asian")[["FilteredCountFR"]],
                 dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "White")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "Unknown")[["FilteredCountFR"]],
                 dplyr::filter(la.data, Category == "Applicants" & Ethnicity == "International")[["FilteredCountFR"]])
  colnames(la.app)=c("Year", "App.Total", "App.AfAm", "App.NaAm", "App.HiLa", "App.AsAm", "App.White", "App.Unknown", "App.Int")
  
  la.ad = cbind(dplyr::filter(la.data, Category == "Admits" & Ethnicity == "All")["Academic.Yr"], dplyr::filter(la.data, Category == "Admits" & Ethnicity == "All")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Admits" & Ethnicity == "African American")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Admits" & Ethnicity == "American Indian")[["FilteredCountFR"]],
                 dplyr::filter(la.data, Category == "Admits" & Ethnicity == "Chicano/Latino")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Admits" & Ethnicity == "Asian")[["FilteredCountFR"]],
                 dplyr::filter(la.data, Category == "Admits" & Ethnicity == "White")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Admits" & Ethnicity == "Unknown")[["FilteredCountFR"]],
                 dplyr::filter(la.data, Category == "Admits" & Ethnicity == "International")[["FilteredCountFR"]])
  colnames(la.ad)=c("Year", "Ad.Total", "Ad.AfAm", "Ad.NaAm", "Ad.HiLa", "Ad.AsAm", "Ad.White", "Ad.Unknown", "Ad.Int")
  
  la.en = cbind(dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "All")["Academic.Yr"], dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "All")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "African American")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "American Indian")[["FilteredCountFR"]],
                dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "Chicano/Latino")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "Asian")[["FilteredCountFR"]],
                dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "White")[["FilteredCountFR"]], dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "Unknown")[["FilteredCountFR"]],
                dplyr::filter(la.data, Category == "Enrollees" & Ethnicity == "International")[["FilteredCountFR"]])
  colnames(la.en)=c("Year", "En.Total", "En.AfAm", "En.NaAm", "En.HiLa", "En.AsAm", "En.White", "En.Unknown", "En.Int")
  
  uc.la.all=cbind(la.app, la.ad[,c(2:9)], la.en[,c(2:9)])
  uc.la=cbind(la.app[,c(1:3,5:7)], la.ad[,c(2:3, 5:7)], la.en[,c(2:3, 5:7)])
  uc.la.temp=uc.la[,c(1, 2, 7, 12, 3, 8, 13, 4, 9, 14, 5, 10, 15, 6, 11, 16)]
  uc.la.ordered=uc.la.temp[c(26:1),]
  uc.la.data=uc.la.ordered[,c(1:4)]
  
  # account for change in AffAct
  
  return(uc.la.ordered[5:length(uc.la.ordered[,1]),])
}

grad_rates = function() {
  years = 1999:2018
  
  black.data <- read.csv("./UCLA Data/RR and Grad Data/Black_FR_rates.csv")
  black.data <- apply(black.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  black.data <- cbind(years,"Black.RR"=black.data[,2],"Black.4.year"=black.data[,4],"Black.5.year"=black.data[,5],"Black.6.year"=black.data[,6])
  
  hila.data = read.csv("./UCLA Data/RR and Grad Data/HiLa_FR_rates.csv")
  hila.data <- apply(hila.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  hila.data <- cbind("HiLa.RR"=hila.data[,2],"HiLa.4.year"=hila.data[,4],"HiLa.5.year"=hila.data[,5],"HiLa.6.year"=hila.data[,6])
  
  asian.data = read.csv("./UCLA Data/RR and Grad Data/Asian_FR_rates.csv")
  asian.data <- apply(asian.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  asian.data <- cbind("Asian.RR"=asian.data[,2],"Asian.4.year"=asian.data[,4],"Asian.5.year"=asian.data[,5],"Asian.6.year"=asian.data[,6])
  
  white.data = read.csv("./UCLA Data/RR and Grad Data/White_FR_rates.csv")
  white.data <- apply(white.data,2, function(x){as.numeric(sub("%", "", x, fixed=TRUE))/100})
  white.data <- cbind("White.RR"=white.data[,2],"White.4.year"=white.data[,4],"White.5.year"=white.data[,5],"White.6.year"=white.data[,6])
  
  rates = cbind(black.data, hila.data, asian.data, white.data)
  
  return(rates)
}



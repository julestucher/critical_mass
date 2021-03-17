### For testing the accuracy of the model ###

# Import the model to test #
source("MC-Pred-Functions.R")
library(lhs)
library(magrittr)
library(tidyverse)

# Comparing model results with historical enrollment proportions #
test_model = function() {
  ## Set global variables ##
  pred_years <<- 2010:2019
  
  csv <- read.csv("UCB Data/Demo Data/Historical Enrollment/Historical Enrollment.csv", colClasses=c('character', 'character', rep('numeric', times=21)))
  records <- as.data.frame(csv[,14:23], row.names=paste(csv[,2], csv[,1])) ## update 14:23 wrt 'years'
  hist_enr <- cbind(sapply(1:10, function(i) c((records[1,i] + records[2,i])/sum(records[,i]), (records[3,i] + records[4,i])/sum(records[,i]),
                                               (records[5,i] + records[6,i])/sum(records[,i]), (records[7,i] + records[8,i])/sum(records[,i]))))
  
  temp <- pred.MC(pred_years = pred_years)[['Overall Dem']]
  model_results <- rbind(temp[1,], temp[3,], temp[2,], temp[4,])
  
  error <- (model_results - hist_enr) * 100
  rownames(error) = c("African-American", "Asian-American", "Hispanic/Latino", "White/Caucasian"); rownames(hist_enr) = rownames(error)
  colnames(error) = c("year.2010", "year.2011", "year.2012", "year.2013", "year.2014", "year.2015", "year.2016", "year.2017", "year.2018", "year.2019")
  
  ### MAPE (maybe not useful) ###
  # error <- hist_enr - model_results
  # mape <- matrix(NA, 4, 1)
  # for(i in 1:4) {
  #   mape[i,] = sum(abs(error[i,])/abs(hist_enr[i,])) * 100 / length(years)
  # }
  # mape_overall = sum(abs(error)/abs(hist_enr)) * 100 / (length(years) * 4)
  # mape_out = cbind(mape, mape_overall); rownames(mape_out) = rownames(error); colnames(mape_out) = c("MAPE (per grp)", "MAPE (overall)")
  
  
  ### LHS Results ###
  LHS <- generate_LHS()
  LHS_res <- conf_model_multiple(LHS)
  
  # prepare results for plotting
  observed <- data.frame(Year=pred_years, Black=hist_enr[1,], Asian=hist_enr[2,], HiLa=hist_enr[3,], White=hist_enr[4,])
  
  predframe <- with(observed, data.frame(Year, Black=model_results[1,], Asian=model_results[2,], HiLa=model_results[3,], White=model_results[4,],
                                         BlackLower=mapply(function(x) min(LHS_res[4*x + 1,], model_results[1,x+1], na.rm=TRUE), 0:9), 
                                         BlackUpper=mapply(function(x) max(LHS_res[4*x + 1,], model_results[1,x+1], na.rm=TRUE), 0:9),
                                         AsianLower=mapply(function(x) min(LHS_res[4*x + 3,], model_results[2,x+1], na.rm=TRUE), 0:9),
                                         AsianUpper=mapply(function(x) max(LHS_res[4*x + 3,], model_results[2,x+1], na.rm=TRUE), 0:9),
                                         HiLaLower=mapply(function(x) min(LHS_res[4*x + 2,], model_results[3,x+1], na.rm=TRUE), 0:9),
                                         HiLaUpper=mapply(function(x) max(LHS_res[4*x + 2,],  model_results[3,x+1], na.rm=TRUE), 0:9),
                                         WhiteLower=mapply(function(x) min(LHS_res[4*x + 4,],  model_results[4,x+1], na.rm=TRUE), 0:9),
                                         WhiteUpper=mapply(function(x) max(LHS_res[4*x + 4,],  model_results[4,x+1], na.rm=TRUE), 0:9)))
  
  plot1 <- ggplot(observed, aes(x=Year)) + geom_point(aes(y=Black)) + geom_line(data=predframe,aes(y=Black)) + 
    geom_ribbon(data=predframe, aes(ymin=BlackLower,ymax=BlackUpper),alpha=0.3) +
    ylab("Demographic Proportion") + labs(color="Race/Ethnicity") + 
    scale_x_discrete(name="Year", breaks=c(2010, 2012, 2014, 2016, 2018), labels=c("2010", "2012", "2014", "2016", "2018"), limits=c(2010:2020))
  
  
  plot2 <- ggplot(observed, aes(x=Year)) + geom_point(aes(y=Black), size=0.75) + geom_point(data=predframe,aes(y=Black, color="African-American"), size=0.75) + geom_line(data=predframe,aes(y=Black, color="African-American"), size=0.5) + 
         geom_ribbon(data=predframe, aes(ymin=BlackLower,ymax=BlackUpper),alpha=0.3) +
         geom_point(aes(y=Asian), size=0.75) + geom_point(data=predframe,aes(y=Asian, color="Asian-American"), size=0.75) + geom_line(data=predframe,aes(y=Asian, color="Asian-American"), size=0.5) + 
         geom_ribbon(data=predframe, aes(ymin=AsianLower,ymax=AsianUpper),alpha=0.3) +
         geom_point(aes(y=HiLa), size=0.75) + geom_point(data=predframe,aes(y=HiLa, color="Hispanic/Latinx"), size=0.75) + geom_line(data=predframe,aes(y=HiLa, color="Hispanic/Latinx"), size=0.5) + 
         geom_ribbon(data=predframe, aes(ymin=HiLaLower,ymax=HiLaUpper),alpha=0.3) +
         geom_point(aes(y=White), size=0.75) + geom_point(data=predframe,aes(y=White, color="White"), size=0.5) + geom_line(data=predframe,aes(y=White, color="White"), size=0.5) + 
         geom_ribbon(data=predframe, aes(ymin=WhiteLower,ymax=WhiteUpper),alpha=0.3) +
         ylab("Demographic Proportion") + labs(color="Race/Ethnicity") + 
         scale_x_discrete(name="Year", breaks=c(2010, 2012, 2014, 2016, 2018), labels=c("2010", "2012", "2014", "2016", "2018"), limits=c(2010:2020))
  
  #Print plots to file
  # ggsave(filename="LHS-hist-black.png", plot=plot1, device="png", path="/Users/juliatucher/Documents/19-20/Chad RA/Figures", width=6, height=4, units="in", dpi="retina")
  ggsave(filename="fig6.pdf", plot=plot2, device="pdf", path="/Users/juliatucher/Documents/20-21/Affirmative Action/Figures", width=5.1, height=6, units="in", dpi="retina")

  
  out = list(round(hist_enr, digits=2), round(model_results, digits=2), error); names(out) = c("Historical Enrollment", "Model Results", "Percent Error")
  return(LHS_res)
}

# Predicting over new years with confidence interval
pred_model = function(pred_years = 2020:2029) {
  pred_years <<- pred_years

  
  model_results <- pred.MC(pred_years = pred_years)[['Overall Dem']]
  
  ### LHS Results ###
  LHS <- generate_LHS()
  LHS_res <- conf_model_multiple(LHS)
  
  # prepare results for plotting
  predframe <- data.frame(Year=as.numeric(as.character(pred_years)), Black=model_results[1,], Asian=model_results[3,], HiLa=model_results[2,], White=model_results[4,],
                                         BlackLower=mapply(function(x) min(LHS_res[4*x + 1,], na.rm=TRUE), 0:9), 
                                         BlackUpper=mapply(function(x) max(LHS_res[4*x + 1,], na.rm=TRUE), 0:9),
                                         HiLaLower=mapply(function(x) min(LHS_res[4*x + 2,], na.rm=TRUE), 0:9),
                                         HiLaUpper=mapply(function(x) max(LHS_res[4*x + 2,], na.rm=TRUE), 0:9),
                                         AsianLower=mapply(function(x) min(LHS_res[4*x + 3,], na.rm=TRUE), 0:9),
                                         AsianUpper=mapply(function(x) max(LHS_res[4*x + 3,], na.rm=TRUE), 0:9),
                                         WhiteLower=mapply(function(x) min(LHS_res[4*x + 4,], na.rm=TRUE), 0:9),
                                         WhiteUpper=mapply(function(x) max(LHS_res[4*x + 4,], na.rm=TRUE), 0:9))
  
  plot1 <- ggplot(predframe, aes(x=Year)) + geom_point(aes(y=Black)) + geom_line(aes(y=Black)) + 
    geom_ribbon(aes(ymin=BlackLower,ymax=BlackUpper),alpha=0.3)
  
  plot3 <- ggplot(predframe, aes(x=Year)) + geom_point(aes(y=Black, color="African-American")) + geom_line(data=predframe,aes(y=Black, color="African-American")) + 
    geom_ribbon(data=predframe, aes(ymin=BlackLower,ymax=BlackUpper),alpha=0.3) +
    geom_point(aes(y=Asian, color="Asian-American")) + geom_line(data=predframe,aes(y=Asian, color="Asian-American")) + 
    geom_ribbon(data=predframe, aes(ymin=AsianLower,ymax=AsianUpper),alpha=0.3) +
    geom_point(aes(y=HiLa, color="Hispanic/Latinx")) + geom_line(data=predframe,aes(y=HiLa, color="Hispanic/Latinx")) + 
    geom_ribbon(data=predframe, aes(ymin=HiLaLower,ymax=HiLaUpper),alpha=0.3) +
    geom_point(aes(y=White, color="White")) + geom_line(data=predframe,aes(y=White, color="White")) + 
    geom_ribbon(data=predframe, aes(ymin=WhiteLower,ymax=WhiteUpper),alpha=0.3) +
    ylab("Demographic Proportion") + labs(color="Race/Ethnicity") + 
    scale_x_discrete(name="Year", breaks=c(2020, 2022, 2024, 2026, 2028), labels=c("2020", "2022", "2024", "2026", "2028"), limits=c(2020:2030))

  
  #Print plots to file
  #ggsave(filename="LHS-proj-black.png", plot=plot1, device="png", path="/Users/juliatucher/Documents/19-20/Chad RA/Figures", width=6, height=4, units="in", dpi="retina")
  ggsave(filename="fig7.pdf", plot=plot3, device="pdf", path="/Users/juliatucher/Documents/20-21/Affirmative Action/Figures", width=5.1, height=6, units="in", dpi="retina")
  
  return(predframe)
  
}
## Helper method for running the model with 280 params (variable names)
# parameters will be named by combining group+data type+year and passed in a dataframe of 280
conf_model = function(params) {
  
  ## construct 'preds' and 'rates' arguments for MC_matrices()
  preds = application_preds(pred_years) # init with these methods to get observed values
  rates = rate_preds(pred_years)
  
  for(i in 1:10) {
    for(j in 1:4) {
      index = ((i-1)*28) + (j-1)*7
      year = i + 5
      preds$Applicants[year, j] = params[index+1]
      preds$Acceptances[year, j] = params[index+2]
      preds$Enrollment[year,j] = params[index+3]
      
      rates$RR[year, j] = params[index+4]
      rates$G4[year, j] = params[index+5]
      rates$G5[year, j] = params[index+6]
      rates$G6[year, j] = params[index+7]
    }
  }
  
  ## call MC_matrices and pass result to pred.MC()
  out = pred.MC(P = MC_matrices(preds=preds, rates=rates))
  
  return(array(out$`Overall Dem`))
}

conf_model_params = function(AppBlack2020,AccBlack2020,EnrBlack2020,RRBlack2020,G4Black2020,G5Black2020,G6Black2020,AppHiLa2020,
                             AccHiLa2020,EnrHiLa2020,RRHiLa2020,G4HiLa2020,G5HiLa2020,G6HiLa2020,AppAsian2020,AccAsian2020,
                             EnrAsian2020,RRAsian2020,G4Asian2020,G5Asian2020,G6Asian2020,AppWhite2020,AccWhite2020,EnrWhite2020,
                             RRWhite2020,G4White2020,G5White2020,G6White2020,AppBlack2021,AccBlack2021,EnrBlack2021,RRBlack2021,
                             G4Black2021,G5Black2021,G6Black2021,AppHiLa2021,AccHiLa2021,EnrHiLa2021,RRHiLa2021,G4HiLa2021,
                             G5HiLa2021,G6HiLa2021,AppAsian2021,AccAsian2021,EnrAsian2021,RRAsian2021,G4Asian2021,G5Asian2021,
                             G6Asian2021,AppWhite2021,AccWhite2021,EnrWhite2021,RRWhite2021,G4White2021,G5White2021,G6White2021,
                             AppBlack2022,AccBlack2022,EnrBlack2022,RRBlack2022,G4Black2022,G5Black2022,G6Black2022,AppHiLa2022,
                             AccHiLa2022,EnrHiLa2022,RRHiLa2022,G4HiLa2022,G5HiLa2022,G6HiLa2022,AppAsian2022,AccAsian2022,
                             EnrAsian2022,RRAsian2022,G4Asian2022,G5Asian2022,G6Asian2022,AppWhite2022,AccWhite2022,EnrWhite2022,
                             RRWhite2022,G4White2022,G5White2022,G6White2022,AppBlack2023,AccBlack2023,EnrBlack2023,RRBlack2023,
                             G4Black2023,G5Black2023,G6Black2023,AppHiLa2023,AccHiLa2023,EnrHiLa2023,RRHiLa2023,G4HiLa2023,
                             G5HiLa2023,G6HiLa2023,AppAsian2023,AccAsian2023,EnrAsian2023,RRAsian2023,G4Asian2023,G5Asian2023,
                             G6Asian2023,AppWhite2023,AccWhite2023,EnrWhite2023,RRWhite2023,G4White2023,G5White2023,G6White2023,
                             AppBlack2024,AccBlack2024,EnrBlack2024,RRBlack2024,G4Black2024,G5Black2024,G6Black2024,AppHiLa2024,
                             AccHiLa2024,EnrHiLa2024,RRHiLa2024,G4HiLa2024,G5HiLa2024,G6HiLa2024,AppAsian2024,AccAsian2024,
                             EnrAsian2024,RRAsian2024,G4Asian2024,G5Asian2024,G6Asian2024,AppWhite2024,AccWhite2024,EnrWhite2024,
                             RRWhite2024,G4White2024,G5White2024,G6White2024,AppBlack2025,AccBlack2025,EnrBlack2025,RRBlack2025,
                             G4Black2025,G5Black2025,G6Black2025,AppHiLa2025,AccHiLa2025,EnrHiLa2025,RRHiLa2025,G4HiLa2025,
                             G5HiLa2025,G6HiLa2025,AppAsian2025,AccAsian2025,EnrAsian2025,RRAsian2025,G4Asian2025,G5Asian2025,
                             G6Asian2025,AppWhite2025,AccWhite2025,EnrWhite2025,RRWhite2025,G4White2025,G5White2025,G6White2025,
                             AppBlack2026,AccBlack2026,EnrBlack2026,RRBlack2026,G4Black2026,G5Black2026,G6Black2026,AppHiLa2026,
                             AccHiLa2026,EnrHiLa2026,RRHiLa2026,G4HiLa2026,G5HiLa2026,G6HiLa2026,AppAsian2026,AccAsian2026,
                             EnrAsian2026,RRAsian2026,G4Asian2026,G5Asian2026,G6Asian2026,AppWhite2026,AccWhite2026,EnrWhite2026,
                             RRWhite2026,G4White2026,G5White2026,G6White2026,AppBlack2027,AccBlack2027,EnrBlack2027,RRBlack2027,
                             G4Black2027,G5Black2027,G6Black2027,AppHiLa2027,AccHiLa2027,EnrHiLa2027,RRHiLa2027,G4HiLa2027,
                             G5HiLa2027,G6HiLa2027,AppAsian2027,AccAsian2027,EnrAsian2027,RRAsian2027,G4Asian2027,G5Asian2027,
                             G6Asian2027,AppWhite2027,AccWhite2027,EnrWhite2027,RRWhite2027,G4White2027,G5White2027,G6White2027,
                             AppBlack2028,AccBlack2028,EnrBlack2028,RRBlack2028,G4Black2028,G5Black2028,G6Black2028,AppHiLa2028,
                             AccHiLa2028,EnrHiLa2028,RRHiLa2028,G4HiLa2028,G5HiLa2028,G6HiLa2028,AppAsian2028,AccAsian2028,
                             EnrAsian2028,RRAsian2028,G4Asian2028,G5Asian2028,G6Asian2028,AppWhite2028,AccWhite2028,EnrWhite2028,
                             RRWhite2028,G4White2028,G5White2028,G6White2028,AppBlack2029,AccBlack2029,EnrBlack2029,RRBlack2029,
                             G4Black2029,G5Black2029,G6Black2029,AppHiLa2029,AccHiLa2029,EnrHiLa2029,RRHiLa2029,G4HiLa2029,
                             G5HiLa2029,G6HiLa2029,AppAsian2029,AccAsian2029,EnrAsian2029,RRAsian2029,G4Asian2029,G5Asian2029,
                             G6Asian2029,AppWhite2029,AccWhite2029,EnrWhite2029,RRWhite2029,G4White2029,G5White2029,G6White2029) {
  
  params = c(AppBlack2020,AccBlack2020,EnrBlack2020,RRBlack2020,G4Black2020,G5Black2020,G6Black2020,AppHiLa2020,
             AccHiLa2020,EnrHiLa2020,RRHiLa2020,G4HiLa2020,G5HiLa2020,G6HiLa2020,AppAsian2020,AccAsian2020,
             EnrAsian2020,RRAsian2020,G4Asian2020,G5Asian2020,G6Asian2020,AppWhite2020,AccWhite2020,EnrWhite2020,
             RRWhite2020,G4White2020,G5White2020,G6White2020,AppBlack2021,AccBlack2021,EnrBlack2021,RRBlack2021,
             G4Black2021,G5Black2021,G6Black2021,AppHiLa2021,AccHiLa2021,EnrHiLa2021,RRHiLa2021,G4HiLa2021,
             G5HiLa2021,G6HiLa2021,AppAsian2021,AccAsian2021,EnrAsian2021,RRAsian2021,G4Asian2021,G5Asian2021,
             G6Asian2021,AppWhite2021,AccWhite2021,EnrWhite2021,RRWhite2021,G4White2021,G5White2021,G6White2021,
             AppBlack2022,AccBlack2022,EnrBlack2022,RRBlack2022,G4Black2022,G5Black2022,G6Black2022,AppHiLa2022,
             AccHiLa2022,EnrHiLa2022,RRHiLa2022,G4HiLa2022,G5HiLa2022,G6HiLa2022,AppAsian2022,AccAsian2022,
             EnrAsian2022,RRAsian2022,G4Asian2022,G5Asian2022,G6Asian2022,AppWhite2022,AccWhite2022,EnrWhite2022,
             RRWhite2022,G4White2022,G5White2022,G6White2022,AppBlack2023,AccBlack2023,EnrBlack2023,RRBlack2023,
             G4Black2023,G5Black2023,G6Black2023,AppHiLa2023,AccHiLa2023,EnrHiLa2023,RRHiLa2023,G4HiLa2023,
             G5HiLa2023,G6HiLa2023,AppAsian2023,AccAsian2023,EnrAsian2023,RRAsian2023,G4Asian2023,G5Asian2023,
             G6Asian2023,AppWhite2023,AccWhite2023,EnrWhite2023,RRWhite2023,G4White2023,G5White2023,G6White2023,
             AppBlack2024,AccBlack2024,EnrBlack2024,RRBlack2024,G4Black2024,G5Black2024,G6Black2024,AppHiLa2024,
             AccHiLa2024,EnrHiLa2024,RRHiLa2024,G4HiLa2024,G5HiLa2024,G6HiLa2024,AppAsian2024,AccAsian2024,
             EnrAsian2024,RRAsian2024,G4Asian2024,G5Asian2024,G6Asian2024,AppWhite2024,AccWhite2024,EnrWhite2024,
             RRWhite2024,G4White2024,G5White2024,G6White2024,AppBlack2025,AccBlack2025,EnrBlack2025,RRBlack2025,
             G4Black2025,G5Black2025,G6Black2025,AppHiLa2025,AccHiLa2025,EnrHiLa2025,RRHiLa2025,G4HiLa2025,
             G5HiLa2025,G6HiLa2025,AppAsian2025,AccAsian2025,EnrAsian2025,RRAsian2025,G4Asian2025,G5Asian2025,
             G6Asian2025,AppWhite2025,AccWhite2025,EnrWhite2025,RRWhite2025,G4White2025,G5White2025,G6White2025,
             AppBlack2026,AccBlack2026,EnrBlack2026,RRBlack2026,G4Black2026,G5Black2026,G6Black2026,AppHiLa2026,
             AccHiLa2026,EnrHiLa2026,RRHiLa2026,G4HiLa2026,G5HiLa2026,G6HiLa2026,AppAsian2026,AccAsian2026,
             EnrAsian2026,RRAsian2026,G4Asian2026,G5Asian2026,G6Asian2026,AppWhite2026,AccWhite2026,EnrWhite2026,
             RRWhite2026,G4White2026,G5White2026,G6White2026,AppBlack2027,AccBlack2027,EnrBlack2027,RRBlack2027,
             G4Black2027,G5Black2027,G6Black2027,AppHiLa2027,AccHiLa2027,EnrHiLa2027,RRHiLa2027,G4HiLa2027,
             G5HiLa2027,G6HiLa2027,AppAsian2027,AccAsian2027,EnrAsian2027,RRAsian2027,G4Asian2027,G5Asian2027,
             G6Asian2027,AppWhite2027,AccWhite2027,EnrWhite2027,RRWhite2027,G4White2027,G5White2027,G6White2027,
             AppBlack2028,AccBlack2028,EnrBlack2028,RRBlack2028,G4Black2028,G5Black2028,G6Black2028,AppHiLa2028,
             AccHiLa2028,EnrHiLa2028,RRHiLa2028,G4HiLa2028,G5HiLa2028,G6HiLa2028,AppAsian2028,AccAsian2028,
             EnrAsian2028,RRAsian2028,G4Asian2028,G5Asian2028,G6Asian2028,AppWhite2028,AccWhite2028,EnrWhite2028,
             RRWhite2028,G4White2028,G5White2028,G6White2028,AppBlack2029,AccBlack2029,EnrBlack2029,RRBlack2029,
             G4Black2029,G5Black2029,G6Black2029,AppHiLa2029,AccHiLa2029,EnrHiLa2029,RRHiLa2029,G4HiLa2029,
             G5HiLa2029,G6HiLa2029,AppAsian2029,AccAsian2029,EnrAsian2029,RRAsian2029,G4Asian2029,G5Asian2029,
             G6Asian2029,AppWhite2029,AccWhite2029,EnrWhite2029,RRWhite2029,G4White2029,G5White2029,G6White2029)
  
  return(conf_model(params))
}


conf_model_multiple = function(my.data){
  return(mapply(conf_model_params, my.data[,1],  my.data[,2],  my.data[,3],  my.data[,4],  my.data[,5],  my.data[,6],
                my.data[,7],  my.data[,8],  my.data[,9],  my.data[,10], my.data[,11], my.data[,12],
                my.data[,13], my.data[,14], my.data[,15], my.data[,16], my.data[,17], my.data[,18],
                my.data[,19], my.data[,20], my.data[,21], my.data[,22], my.data[,23], my.data[,24],
                my.data[,25], my.data[,26], my.data[,27], my.data[,28], my.data[,29], my.data[,30],
                my.data[,31], my.data[,32], my.data[,33], my.data[,34], my.data[,35], my.data[,36],
                my.data[,37], my.data[,38], my.data[,39], my.data[,40], my.data[,41], my.data[,42],
                my.data[,43], my.data[,44], my.data[,45], my.data[,46], my.data[,47], my.data[,48],
                my.data[,49], my.data[,50], my.data[,51], my.data[,52], my.data[,53], my.data[,54],
                my.data[,55], my.data[,56], my.data[,57], my.data[,58], my.data[,59], my.data[,60],
                my.data[,61], my.data[,62], my.data[,63], my.data[,64], my.data[,65], my.data[,66],
                my.data[,67], my.data[,68], my.data[,69], my.data[,70], my.data[,71], my.data[,72],
                my.data[,73], my.data[,74], my.data[,75], my.data[,76], my.data[,77], my.data[,78],
                my.data[,79], my.data[,80], my.data[,81], my.data[,82], my.data[,83], my.data[,84],
                my.data[,85], my.data[,86], my.data[,87], my.data[,88], my.data[,89], my.data[,90],
                my.data[,91], my.data[,92], my.data[,93], my.data[,94], my.data[,95], my.data[,96],
                my.data[,97], my.data[,98], my.data[,99], my.data[,100],my.data[,101],my.data[,102],
                my.data[,103],my.data[,104],my.data[,105],my.data[,106],my.data[,107],my.data[,108],
                my.data[,109],my.data[,110],my.data[,111],my.data[,112],my.data[,113],my.data[,114],
                my.data[,115],my.data[,116],my.data[,117],my.data[,118],my.data[,119],my.data[,120],
                my.data[,121],my.data[,122],my.data[,123],my.data[,124],my.data[,125],my.data[,126],
                my.data[,127],my.data[,128],my.data[,129],my.data[,130],my.data[,131],my.data[,132],
                my.data[,133],my.data[,134],my.data[,135],my.data[,136],my.data[,137],my.data[,138],
                my.data[,139],my.data[,140],my.data[,141],my.data[,142],my.data[,143],my.data[,144],
                my.data[,145],my.data[,146],my.data[,147],my.data[,148],my.data[,149],my.data[,150],
                my.data[,151],my.data[,152],my.data[,153],my.data[,154],my.data[,155],my.data[,156],
                my.data[,157],my.data[,158],my.data[,159],my.data[,160],my.data[,161],my.data[,162],
                my.data[,163],my.data[,164],my.data[,165],my.data[,166],my.data[,167],my.data[,168],
                my.data[,169],my.data[,170],my.data[,171],my.data[,172],my.data[,173],my.data[,174],
                my.data[,175],my.data[,176],my.data[,177],my.data[,178],my.data[,179],my.data[,180],
                my.data[,181],my.data[,182],my.data[,183],my.data[,184],my.data[,185],my.data[,186],
                my.data[,187],my.data[,188],my.data[,189],my.data[,190],my.data[,191],my.data[,192],
                my.data[,193],my.data[,194],my.data[,195],my.data[,196],my.data[,197],my.data[,198],
                my.data[,199],my.data[,200],my.data[,201],my.data[,202],my.data[,203],my.data[,204],
                my.data[,205],my.data[,206],my.data[,207],my.data[,208],my.data[,209],my.data[,210],
                my.data[,211],my.data[,212],my.data[,213],my.data[,214],my.data[,215],my.data[,216],
                my.data[,217],my.data[,218],my.data[,219],my.data[,220],my.data[,221],my.data[,222],
                my.data[,223],my.data[,224],my.data[,225],my.data[,226],my.data[,227],my.data[,228],
                my.data[,229],my.data[,230],my.data[,231],my.data[,232],my.data[,233],my.data[,234],
                my.data[,235],my.data[,236],my.data[,237],my.data[,238],my.data[,239],my.data[,240],
                my.data[,241],my.data[,242],my.data[,243],my.data[,244],my.data[,245],my.data[,246],
                my.data[,247],my.data[,248],my.data[,249],my.data[,250],my.data[,251],my.data[,252],
                my.data[,253],my.data[,254],my.data[,255],my.data[,256],my.data[,257],my.data[,258],
                my.data[,259],my.data[,260],my.data[,261],my.data[,262],my.data[,263],my.data[,264],
                my.data[,265],my.data[,266],my.data[,267],my.data[,268],my.data[,269],my.data[,270],
                my.data[,271],my.data[,272],my.data[,273],my.data[,274],my.data[,275],my.data[,276],
                my.data[,277],my.data[,278],my.data[,279],my.data[,280]))
}

## Generate LHS (w/o running the model)
# Note that 2020 is the latest start year for pred_years given constraint of observed data
generate_LHS = function() {
  
  group_names=c('Black','HiLa','Asian','White')
  
  ## Predicting data and parameters
  
  # load in uc berkeley app and rate data
  uc.berkeley = app_data()
  rates = grad_rates()
  
  # use helper function to predict applicant, acceptance, and matriculate counts  
  apps = as.matrix(uc.berkeley[,c(5,8,11,14)])
  app_types = c('log', 'log', 'log', 'log')
  app_preds = application_preds_ci(source_data=apps, types = app_types)
  
  accs = as.matrix(uc.berkeley[,c(6,9,12,15)])
  acc_types = c('box', 'log', 'log', 'box')
  acc_preds = application_preds_ci(source_data=accs, types=acc_types)
  
  enrs = as.matrix(uc.berkeley[,c(7,10,13,16)])
  enr_types = c('box', 'log', 'log', 'log')
  enr_preds = application_preds_ci(source_data=enrs, types=enr_types)
  
  # use helper function to predict RR, 4-yr, 5-yr, 6-yr grad rates
  rr = as.matrix(rates[,c(2,6,10,14)][c(1:20),])
  rr_types = c('lm', 'lm', 'lm', 'lm')
  if (min(pred_years) > 2018) { span=2019:max(pred_years) } else { span=pred_years } # unsure how we're dealing with "observed" data
  rr_preds = rate_preds_ci(source_data=rr, types=rr_types)
  
  g4 = as.matrix(rates[,c(3,7,11,15)][c(1:17),])
  g4_types = c('box', 'box', 'box', 'box')
  if (min(pred_years) > 2015) { span=2016:max(pred_years) } else { span=pred_years }
  g4_preds = rate_preds_ci(source_data=g4, types=g4_types)
  
  g5 = as.matrix(rates[,c(4,8,12,16)][c(1:16),])
  g5_types = c('box', 'log', 'box', 'box')
  if (min(pred_years) > 2014) { span=2015:max(pred_years) } else { span=pred_years }
  g5_preds = rate_preds_ci(source_data=g5, types=g5_types)
  
  g6 = as.matrix(rates[,c(5,9,13,17)][c(1:15),])
  g6_types = c('log', 'log', 'box', 'box')
  g6_preds = rate_preds_ci(source_data=g6, types=g6_types)
  
  # build empty tibble
  param_bounds = tibble(.rows=2)
  
  # total all preds to make one LHS entry
  preds = tibble(.rows=1)
  
  # add data in order of racial group
  for(j in 1:10) {
    
    # add data for 10 years of predicted data
    for(i in 1:4) {
      year = pred_years[j]
      
      param_names = c(paste("App", group_names[i], year, sep=""),
                      paste("Acc", group_names[i], year, sep=""),
                      paste("Enr", group_names[i], year, sep=""),
                      paste("RR", group_names[i], year, sep=""),
                      paste("G4", group_names[i], year, sep=""),
                      paste("G5", group_names[i], year, sep=""),
                      paste("G6", group_names[i], year, sep=""))

      
      temp.tb <- tibble("App"=c(app_preds[[i]][j,1], app_preds[[i]][j,2]),
                        "Acc"=c(acc_preds[[i]][j,1], acc_preds[[i]][j,2]),
                        "Enr"=c(enr_preds[[i]][j,1], enr_preds[[i]][j,2]),
                        "RR"=c(rr_preds[[i]][j,1], rr_preds[[i]][j,2]),
                        "G4"=c(g4_preds[[i]][j,1], g4_preds[[i]][j,2]),
                        "G5"=c(g5_preds[[i]][j,1], g5_preds[[i]][j,2]),
                        "G6"=c(g6_preds[[i]][j,1], g6_preds[[i]][j,2]))
      
      names(temp.tb) = param_names
      
      param_bounds = cbind(param_bounds, temp.tb)
      
      ## Compound known predictions ##
      preds.tb <- tibble("App"=app_preds[[i]][j,1], "Acc"=acc_preds[[i]][j,1],"Enr"=enr_preds[[i]][j,1],
                         "RR"=rr_preds[[i]][j,1],"G4"=g4_preds[[i]][j,1], "G5"=g5_preds[[i]][j,1], "G6"=g6_preds[[i]][j,1])
      names(preds.tb) = param_names
      preds <- cbind(preds, preds.tb)
    }
  }
  
  ### Prepare for LHS - one for each of the 10 years ###
  
  ## generate and run LH using 'lhs'
  n_sample <- 500
  n_par <- ncol(param_bounds)
  
  par_iter1 <- improvedLHS(n=n_sample,k=n_par) %>%
    as.data.frame(., .name_repair = "minimal")
  
  colnames(par_iter1) <- colnames(param_bounds)
  final <- map2_df(par_iter1, as.data.frame(param_bounds), ~ (.x * (.y[2] - .y[1]) + .y[1])) 
  return(as.data.frame(final))
  
}

## Helper methods for generating predictions with a confidence interval (see 'calculate_predictions' for non-conf interval version)
# params: source_data is submatrix derived from app_data(), types is 4 element list with types of predictions, pred_years is 10 year prediction span
application_preds_ci = function(source_data, types) {
  
  #### Setup ####
  group_names=c('Black','HiLa','Asian','White')
  
  #### Put together matrix of group predictions for applications ####
  data_years = 1998:(1998 + length(source_data[,1]) - 1)
  colnames(source_data) = group_names; rownames(source_data) = data_years
  
  # 10 years by 2 variables (fit, error = fit - lwr) for each of 4 groups
  temp_lower = matrix(NA, 10, 4)
  temp_upper = matrix(NA, 10, 4)
  
  for (i in 1:4) { 
    temp = pred_with_conf(data = cbind(data_years, source_data[,i]), pred_years=pred_years, best_reg_type=types[i])
    temp_lower[,i] = temp[,2]
    temp_upper[,i] = temp[,3]
  }
  
  ## Calculate and predict total counts using log-linear regression
  temp.df = data.frame('year' = data_years, 'admission' = rowSums(source_data))
  temp.lm = lm(log(admission)~year, data = temp.df)
  totals = exp(predict(temp.lm, newdata = data.frame(year = pred_years)))
  
  ## Scale predictions based on total
  admit_preds = list(matrix(NA, 10, 2), matrix(NA, 10, 2), matrix(NA, 10, 2), matrix(NA, 10, 2))
  scalars = cbind(abs(totals/rowSums(temp_lower)), abs(totals/rowSums(temp_upper)))
  for (j in 1:4){
    for (i in 1:10) {
      #admit_preds[[j]][i,] = c(scalars[i, 1]*temp_lower[i,j], scalars[i, 2]*temp_upper[i,j])
      admit_preds[[j]][i,] = c(temp_lower[i,j], temp_upper[i,j])
    }
    rownames(admit_preds[[j]]) = pred_years; colnames(admit_preds[[j]]) = c(paste(group_names[j], "Lower"), paste(group_names[j], "Upper"))
  }
  
  return(admit_preds)  
  
}

rate_preds_ci = function(source_data, types) {
  
  ## Setup ###
  group_names=c('Black','HiLa','Asian','White')
  
  #### Put together matrix of group predictions for applications ####
  data_years = 1999:(1999 + length(source_data[,1]) - 1)
  colnames(source_data) = group_names; rownames(source_data) = data_years
  
  # 10 years by 2 variables (fit, error = fit - lwr) for each of 4 groups
  admit_preds = list(matrix(NA, length(pred_years), 2), matrix(NA, length(pred_years), 2), matrix(NA, length(pred_years), 2), matrix(NA, length(pred_years), 2))
  
  for (i in 1:4) { 
    temp = pred_with_conf(data = cbind(data_years, source_data[,i]), pred_years=pred_years, best_reg_type=types[i])
    admit_preds[[i]] = cbind(temp[,2], temp[,3])
    rownames(admit_preds[[i]]) = pred_years; colnames(admit_preds[[i]]) = c(paste(group_names[i], "Lower"), paste(group_names[i], "Upper"))
    
  }
  
  return(admit_preds)  
  
}

## generate prediction with confidence interval -- this may feel redundant, but for now we'll keep this separate from Predicting-Rates.R code
pred_with_conf = function(data, pred_years, best_reg_type) {
  
  temp.df <<- data.frame('year' = data[,1], 'rate' = data[,2])
  
  if(best_reg_type == 'log') {
    log.lm = lm(log(rate)~year, data = temp.df)
    log.preds = exp(predict(log.lm, newdata = data.frame(year = pred_years), interval="confidence"))
    return(log.preds)
  } else if(best_reg_type == 'box') {
    b <- EnvStats::boxcox(lm(rate~year, data = temp.df), optimize=TRUE)
    lambda = b$lambda
    new = ((temp.df[,2])^lambda - 1) / lambda
    trans.data = data.frame('year' = temp.df['year'], 'count' = new)
    bc.lm = lm(count~year, data = trans.data)
    pre = predict(bc.lm, newdata=data.frame(year = pred_years), interval="confidence")
    bc.preds = (pre*lambda+1)^(1/lambda)
    return(bc.preds)
  } else {
    reg.lm = lm(rate~year, data = temp.df)
    lm.preds = predict(reg.lm, newdata = data.frame(year = pred_years), interval="confidence")
    return(lm.preds)
  }
}


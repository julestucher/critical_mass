### Predicting admissions counts and graduation rates ###

source('UCB-Data.R') ## Change this source file to change which school
school <<- "ucb"

#### Setup ####
group_names=c('African-American','Hispanic/Latinx','Asian-American','White')

## pred_years must be a 10 year time range with at least the five previous years in the dataset (i.e. 2015:2029 or 2000:2014)
# use conf_int as a bool to turn on/off whether confidence interval is included in predictions
application_preds = function(pred_years = 2020:2029) {
  
  #### Put together matrix of group predictions for applications ####
  college_data = app_data();
  apps = as.matrix(college_data[,c(5,8,11,14)]);
  data_years = 1998:(1998 + length(apps[,1]) - 1)
  span = (pred_years[1] - 5):pred_years[length(pred_years)]
  
  colnames(apps) = group_names; rownames(apps) = data_years
  
  temp.preds = matrix(NA, 10, 5)
  
  # use this to indicate which type of pred model to use (index corresponds to racial group and total)
  types = switch(school, "ucb" = c('log', 'log', 'log', 'log'), "ucla" = c('log', 'log', 'box', 'log'))
  
  for (i in 1:4) { 
    temp = calculate_predictions(data = cbind(data_years, apps[,i]), pred_years=span, data_years=data_years, best_reg_type=types[i], data_type="app", group_name=group_names[i])
    temp.preds[,i] = temp[(length(temp)-9):length(temp)]
  }

  ## Calculate and predict total using log-linear regression
  temp.df = data.frame('year' = data_years, 'app' = rowSums(apps))
  temp.lm = lm(log(app)~year, data = temp.df)
  temp.preds[,5] = exp(predict(temp.lm, newdata = data.frame(year = pred_years)))
  
  temp.preds = cbind(temp.preds, rowSums(temp.preds) - temp.preds[,5])
  
  ## Scale predictions based on total
  applicants_preds = matrix(NA, 10, 4); scalars = temp.preds[,5]/temp.preds[,6]
  for (i in 1:10) {applicants_preds[i,] = scalars[i]*temp.preds[i,1:4]}
  applicants_preds = rbind(apps[match(c(span[1]), rownames(apps)):match(c(span[5]), rownames(apps)),1:4], applicants_preds)
  rownames(applicants_preds) = span; colnames(applicants_preds) = group_names
  
  #### Put together matrix of group predictions for acceptances ####
  accs = as.matrix(college_data[,c(6,9,12,15)]); data_years = 1998:(1998 + length(accs[,1]) - 1);
  accs = cbind(accs,rowSums(accs)); colnames(accs) = c(group_names,'Total'); rownames(accs) = data_years
  
  types = switch(school, "ucb" = c('box', 'log', 'log', 'box'), "ucla" = c('lm', 'box', 'log', 'box'))
  
  temp.preds = matrix(NA, 10, 5)
  
  for (i in 1:4) { 
    temp = calculate_predictions(data = cbind(data_years, accs[,i]), pred_years=span, data_years=data_years, best_reg_type=types[i], data_type="acc", group_name=group_names[i])
    temp.preds[,i] = temp[(length(temp)-9):length(temp)]
  }
  
  ## Calculate and predict total
  temp.df = data.frame('year' = data_years, 'acc' = accs[,5])
  temp.lm = lm(log(acc)~year, data = temp.df)
  temp.preds[,5] = exp(predict(temp.lm, newdata = data.frame(year = pred_years)))
  
  temp.preds = cbind(temp.preds, rowSums(temp.preds) - temp.preds[,5])
  
  ## Scale predictions based on total  
  acceptances_preds = matrix(NA, 10, 4); c = temp.preds[,5]/temp.preds[,6]
  for (i in 1:10) {acceptances_preds[i,] = c[i]*temp.preds[i,1:4]}
  acceptances_preds = rbind(accs[match(c(span[1]), rownames(accs)):match(c(span[5]), rownames(accs)),1:4], acceptances_preds)
  rownames(acceptances_preds) = span; colnames(acceptances_preds) = group_names
  
  #### Put together matrix of group predictions for enrollment ####
  ens = as.matrix(college_data[,c(7,10,13,16)]); data_years = 1998:(1998 + length(ens[,1]) - 1)
  ens = cbind(ens,rowSums(ens)); colnames(ens) = c(group_names,'Total'); rownames(ens) = data_years
  
  types = switch(school, "ucb" = c('box', 'log', 'log', 'log'), "ucla" = c('box', 'box', 'lm', 'box'))
  
  temp.preds = matrix(NA, 10, 5)
  
  for (i in 1:4) { 
    temp = calculate_predictions(data = cbind(data_years, ens[,i]), pred_years=span, data_years=data_years, best_reg_type=types[i], data_type="enr", group_name=group_names[i])
    temp.preds[,i] = temp[(length(temp)-9):length(temp)]
  }
  
  ## Calculate and predict total
  temp.df = data.frame('year' = data_years, 'en' = ens[,5])
  temp.lm = lm(log(en)~year, data = temp.df)
  temp.preds[,5] = exp(predict(temp.lm, newdata = data.frame(year = pred_years)))
  
  temp.preds = cbind(temp.preds, rowSums(temp.preds) - temp.preds[,5])
  
  ## Scale predictions based on total  
  enrollment_preds = matrix(NA, 10, 4); c = temp.preds[,5]/temp.preds[,6]
  for (i in 1:10) {enrollment_preds[i,] = c[i]*temp.preds[i,1:4]}
  enrollment_preds = rbind(ens[match(c(span[1]), rownames(ens)):match(c(span[5]), rownames(ens)),1:4], enrollment_preds)
  rownames(enrollment_preds) = span; colnames(enrollment_preds) = group_names

  
  #### Output ####
  preds = list(round(applicants_preds), round(acceptances_preds), round(enrollment_preds))
  names(preds) = c('Applicants','Acceptances','Enrollment')
  return(preds)
  
}

## pred_years must be a 10 year time range
rate_preds = function(pred_years = 2020:2029) {
  
  ### Setup data from UC Data ###
  rates = grad_rates()
  pred_years = (pred_years[1] - 5):pred_years[length(pred_years)]
  
  #### First-year Retention Rate ####
  rr = as.matrix(rates[,c(1,2,6,10,14)][c(1:20),])
  data_years = rr[,'years']
  
  types = switch(school, "ucb" = c('lm', 'lm', 'lm', 'lm'), "ucla" = c('box', 'box', 'box', 'box'))
  
  rr_preds = matrix(NA, 15, 4)
  rownames(rr_preds)=pred_years; colnames(rr_preds)=group_names
  
  for (i in 2:5) {
    temp = calculate_predictions(data = cbind(rr[,1], rr[,i]), pred_years=pred_years, data_years=data_years, best_reg_type=types[i-1], data_type="rr", group_name=group_names[i-1])
    rr_preds[,i-1] = temp[(length(temp)-14):length(temp)]
  }
  
  #### Four-Year Graudation Rate ####
  g4 = as.matrix(rates[,c(1,3,7,11,15)][c(1:17),])
  data_years = g4[,'years']
  
  types = switch(school, "ucb" = c('box', 'box', 'box', 'box'), "ucla" = c('lm', 'lm', 'lm', 'box'))
  
  g4_preds = matrix(NA, 15, 4)
  rownames(g4_preds)=pred_years; colnames(g4_preds)=group_names
  
  for (i in 2:5) { 
    temp = calculate_predictions(data = cbind(g4[,1], g4[,i]), pred_years=pred_years, data_years=data_years, best_reg_type=types[i-1], data_type="g4", group_name=group_names[i-1])
    g4_preds[,i-1] = temp[(length(temp)-14):length(temp)]
  }
  
  #### Five-Year Graduation Rate ####
  g5 = as.matrix(rates[,c(1,4,8,12,16)][c(1:16),])
  data_years = g5[,'years']
  
  types = switch(school, "ucb" = c('box', 'log', 'box', 'box'), "ucla" = c('box', 'log', 'log', 'box'))
  
  g5_preds = matrix(NA, 15, 4)
  rownames(g5_preds)=pred_years; colnames(g5_preds)=group_names
  
  for (i in 2:5) {
    temp = calculate_predictions(data = cbind(g5[,1], g5[,i]), pred_years=pred_years, data_years=data_years, best_reg_type=types[i-1], data_type="g5", group_name=group_names[i-1])
    g5_preds[,i-1] = temp[(length(temp)-14):length(temp)]
  }
  
  
  #### Six-Year Graduation Rate ####
  g6 = as.matrix(rates[,c(1,5,9,13,17)][c(1:15),])
  data_years = g6[,'years']
  
  types = switch(school, "ucb" = c('log', 'log', 'box', 'box'), "ucla" = c('box', 'lm', 'lm', 'box'))
  
  g6_preds = matrix(NA, 15, 4)
  rownames(g6_preds)=pred_years; colnames(g6_preds)=group_names
  
  for (i in 2:5) { 
    temp = calculate_predictions(data = cbind(g6[,1], g6[,i]), pred_years=pred_years, data_years=data_years, best_reg_type=types[i-1], data_type="g6", group_name=group_names[i-1])
    g6_preds[,i-1] = temp[(length(temp)-14):length(temp)]
  }
  
  
  #### Ensuring RR > G6 > G5 > G4 and Output ####
  rate_preds = list(round(rr_preds,3), round(g4_preds,3), round(g5_preds,3), round(g6_preds,3))
  names(rate_preds) = c('RR','G4','G5','G6')
  
  for(j in 1:4){
    
    for (i in 1:15) {
      if (rate_preds[['G4']][i,group_names[j]] > rate_preds[['G5']][i,group_names[j]]) {rate_preds[['G5']][i,group_names[j]] = rate_preds[['G4']][i,group_names[j]]}
      if (rate_preds[['G5']][i,group_names[j]] > rate_preds[['G6']][i,group_names[j]]) {rate_preds[['G6']][i,group_names[j]] = rate_preds[['G5']][i,group_names[j]]}
      if (rate_preds[['G6']][i,group_names[j]] > rate_preds[['RR']][i,group_names[j]]) {rate_preds[['G6']][i,group_names[j]] = rate_preds[['RR']][i,group_names[j]]}
    }
  }
  
  return(rate_preds)
  
}


## Helper function for calculating and plotting predictions based on the best type ##
calculate_predictions = function(data, pred_years, data_years, best_reg_type, data_type, group_name, print=FALSE) {
  
  total_years = min(data_years):max(pred_years)
  
  temp.df <<- data.frame('year' = data[,1], 'rate' = data[,2])
  
  reg.lm = lm(rate~year, data = temp.df)
  lm.preds = predict(reg.lm, newdata = data.frame(year = total_years))
  
  log.lm = lm(log(rate)~year, data = temp.df)
  log.preds = exp(predict(log.lm, newdata = data.frame(year = total_years)))
  
  b <- EnvStats::boxcox(lm(rate~year, data = temp.df), optimize=TRUE)
  lambda = b$lambda
  new = ((temp.df[,2])^lambda - 1) / lambda
  trans.data = data.frame('year' = data_years, 'count' = new)
  bc.lm = lm(count~year, data = trans.data)
  pre = predict(bc.lm, newdata=data.frame(year = total_years))
  bc.preds = (pre*lambda+1)^(1/lambda)
  
  # Note that print only works for projecting data (aka pred_years = 2020:2029)
  if (print){
    count = switch(best_reg_type, "lm"=lm.preds[(length(data[,1])+1):length(total_years)], "log"=log.preds[(length(data[,1])+1):length(total_years)], "box"=bc.preds[(length(data[,1])+1):length(total_years)])
    plot_df = data.frame(Year=data_years, Count=data[,2], Lm=lm.preds[1:length(data[,1])], Log=log.preds[1:length(data[,1])], Bc=bc.preds[1:length(data[,1])], Race=group_name, Type="Observed")
    plot_df = rbind(plot_df, data.frame(Year=(max(data_years) + 1):max(pred_years), Count=as.numeric(count), Lm=0, Log=0, Bc=0, Race=group_name, Type="Predicted"))
    
    return(plot_df)
  }

  
  if(best_reg_type == 'log') {
    return(log.preds)
  } else if(best_reg_type == 'box') {
    return(bc.preds)
  } else {
    return(lm.preds)
  }
  
}



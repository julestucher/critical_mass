#### Necessary Materials ####

source('MC-Matrices.R')
library(matpow)

#### Deriving Critical Mass From Class-Structured MC Model (null model preds from full MC model, assuming equal app/acc/en rates b/w groups) ####

# argument MC_matrices_arg is some result of MC_matrices() and pred_years is a 10 year span to be predicted
pred.MC = function(P = NULL, pred_years = NULL, d = c(1,1,1,1), a = c(1,1,1,1), e = c(1,1,1,1)) {
  
  #### Setup ####
  
  u = c(1,rep(0,23));
  I = diag(24)
  colnames(I)=c('0A','0B','0C','1A','1B','1C','2A','2B','2C','3A','3B','3C','4A','4B','4C','5A','5B','5C','6A','6B','4G','5G','6G','DNF')
  
  HS = HS.setup();
  
  if(is.null(pred_years)) {
    if(is.null(P)){
      P = MC_matrices(a=a, d=d, e=e)
    }
    time.span=as.numeric(substr(names(P[6:length(P)]), 6, 9))
  } else {
    time.span = pred_years
    P = MC_matrices(preds = application_preds(pred_years), rates = rate_preds(pred_years), a=a, d=d, e=e)
  }
  time.names =paste('year', time.span, sep='.')
  group.names=c('Black','Hispanic.Latino','Asian','White')
  
  #### Running the MCs ####
  group.results = list();
  
  # pre.P are P values from years before 2018, pre is a MC per ethnicity group for existing years 
  pre.P = list(); pre = list();
  
  #MC.P are P matrix values from years after 2018, MC is per ethnicity group
  MC.P = list(); MC = list();
  
  # post.P and post 
  post.P = list(); post = list()
  for(j in 1:length(group.names)) {
    pre.start=time.span[1] - 5; pre.end=time.span[1]; pre.run=c(pre.start:pre.end); MC.run=c(time.span[1]:time.span[length(time.span)])
    P.span=c((time.span[1]-5):(time.span[length(time.span)])); P.names = paste('year', P.span, sep='.')
    
    # init pre.P and MC.P as the calculated proportions from P matrix (aka existing data) and predicted values from P matrix (predicted data)
    for (i in 1:6) {pre.P[[i]]=P[[i]][[j]]}; names(pre.P) = c(P.names[1:6])
    for (i in 1:9) {MC.P[[i]]=P[[i+6]][[j]]}; names(MC.P) = c(P.names[7:15])
    
    # init Markov Chains as having HS graduate numbers in them
    for (i in 1:6) {pre[[i]]=round(HS[i,j]*u%*%matpow(pre.P[[i]],3*(7-i))$prod1,0)};
    names(pre) = time.span[1:6]-5
    for (i in 1:9) {MC[[i]]=round(HS[i+6,j]*u,0)}
    
    # total enrollment vector
    enroll.b = NULL; 
    
    # total dropout vector
    dropout = NULL;
    
    # y.MC is a counter for years into the progression (increased every 3 cycles)
    y.MC = 0; y.post = 0;
    
    # declare add function used to calculate aggregate MC by summing row vectors of HS class year (i.e. where are they now)
    add = function(x) {Reduce("+",x)}
    
    # create an empty matrix -- 3*time.span by 24 "step" where each row vector is a quarter of a year
    step = matrix(rep(0, 3*length(time.span)*24), 3*length(time.span), 24, byrow=TRUE)
    colnames(step) = colnames(I);
    num.steps = 3*length(time.span)
    
    # first step of num.steps uses data from pre and MC[1]
    step[1,] = round(add(append(pre, MC[1])),0)
    enroll.b[1] = round(sum(step[1,4:20]),0)
    dropout[1] = 0
    for (z in 1:length(pre)) {
      temp = pre[[z]]
      pre[[z]] = pre[[z]]%*%pre.P[[z]]
      dropout[1] = round(dropout[1] + pre[[z]][24] - temp[24], 0) # calculate dropout rate as the number of UCB students that don't advance
    };
    MC[[1]]=MC[[1]]%*%MC.P[[1]]
    
    for (i in 2:num.steps) {
      
      #if (1 < i && i < (length(time.span)*3+1)) {
      
      step[i,] = round(add(append(pre,MC[1:min(y.MC+1,length(MC))])),0)
      enroll.b[i] = round(sum(step[i,4:20]),0)
      dropout[i] = 0
      for (z in 1:length(pre)) {
        temp = pre[[z]]
        pre[[z]] = pre[[z]]%*%pre.P[[z]]
        dropout[i] = round(dropout[i] + pre[[z]][24] - temp[24], 0)
      };
      for (z in 1:min(y.MC+1, length(MC))) {
        temp = MC[[z]]
        MC[[z]]= MC[[z]]%*%MC.P[[z]]
        if(z < min(y.MC+1, length(MC))){
          dropout[i] = round(dropout[i] + MC[[z]][24] - temp[24], 0)
        }
      }
      
      
      # if step is a year step 
      if (i%%3 == 0) { y.MC = y.MC + 1}
    }
    
    #### Formatting Results ####
    
    # enrollment results
    enroll.B = matrix(enroll.b, length(time.span), 3, byrow=TRUE)
    colnames(enroll.B) = c('A','B','C'); rownames(enroll.B) = time.names; #enroll.B=enroll.B[-1,]
    
    # dropout results
    dropout.F = matrix(dropout, length(time.span), 3, byrow=TRUE)
    colnames(dropout.F) = c('A', 'B', 'C'); rownames(dropout.F) = time.names;
    
    A = rep(time.names,each=3); B = rep(c('A','B','C'),length(time.span))
    step.names = paste(A,B,sep="."); rownames(step) = step.names
    
    # group all results
    inter.results = list(enroll.B, dropout.F, step); names(inter.results) = c('Enrollment', 'Dropout', 'Steps')
    group.results[[j]] = inter.results
  }
  
  names(group.results) = group.names
  
  #### Output ####
  ### Overall Makeup of UCB ###
  overall = cbind(group.results$Black$Enrollment[,1], group.results$Hispanic.Latino$Enrollment[,1],
                  group.results$Asian$Enrollment[,1], group.results$White$Enrollment[,1])
  overall = round(apply(overall, 1, function(x) {x/sum(x)}),5)
  overall = rbind(overall[1,], overall[2,], overall[3,], overall[4,])
  rownames(overall) = c('African-American','Hispanic/Latino','Asian-American','White/Caucasian')
  colnames(overall) = paste('year', time.span, sep = '.')
  
  ### Dropout Rates of UCB ###
  drop = cbind(group.results$Black$Dropout[,1], group.results$Hispanic.Latino$Dropout[,1], 
               group.results$Asian$Dropout[,1], group.results$White$Dropout[,1])
  drop = round(apply(drop, 1, function(x) {x/sum(x)}), 5)
  drop = rbind(drop[1,], drop[2,], drop[3,], drop[4])
  rownames(drop) = rownames(overall)
  colnames(drop) = paste('year', time.span, sep = '.')
  
  ### Fall Makeup at UCB ###
  
  # init empty matrix to store phase 1A counts (aka freshman fall)
  fall = matrix(NA, 10, 4); 
  
  # indexes of years within results (i.e. steps[1] is index of year 1 in groups.results)
  steps = 3*(1:10) - 2
  for (i in 1:10) {
    for (j in 1:4) {
      fall[i,j] = group.results[[j]][[3]][steps[i],4]
    }
  }
  
  # normalize fall matrix to represent proportions
  fall = round(apply(fall, 1, function(x) {x/sum(x)}),5)
  fall = rbind(fall[1,], fall[2,], fall[3,], fall[4,])
  
  colnames(fall) = colnames(overall); rownames(fall) = rownames(overall)
  
  ### 4-Year Grad Makeup at UCB ###
  
  # init empty matrix to store phase 4G counts (aka 4-year grad/senior spring)
  grad = matrix(NA, 10, 4)
  
  # use same indexes as 'fall': iterate through years, racial groups
  for(i in 1:10) {
    for (j in 1:4) {
      grad[i,j] = group.results[[j]][[3]][steps[i],21]
    }
  }
  
  # normalize fall matrix to represent proportions
  grad = round(apply(grad, 1, function(x) {x/sum(x)}),5)
  grad = rbind(grad[1,], grad[2,], grad[3,], grad[4,])
  
  colnames(grad) = colnames(overall); rownames(grad) = rownames(overall)
  
  ### Actual Output ###
  out = list(overall, fall); names(out) = c('Overall Dem', 'Fall Dem')
  
  return(out)
}


#### Deriving CM From Enrollment Data (null models preds from enrollment preds alone, similar to 'Fall.Dem' from pred.MC) ####

## These are not critical masses, they are predicted enrollment percentages from UC data log-linear predictions
enroll.CM = function() {
  
  #### Generating Demographic Data from the Respective Data Sources ####
  time.span=c(2018:2027); time.names = paste('year',time.span,sep = '.')
  group_names=c('Black','Hispanic.Latino','Asian','White')
  
  UC_data = application_preds();
  en = UC_data$Enrollment[6:15,]
  percent = round(apply(en, 1, function(x) x/sum(x)),5);
  colnames(percent) = time.names
  percent = rbind(percent[1,], percent[3,], percent[2,], percent[4,])
  rownames(percent) = c('African-American','Asian-American','Hispanic/Latino','White/Caucasian')
  
  #### Output ####
  return(percent)
  
}


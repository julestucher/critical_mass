#### Necessary Materials ####

## v2: model many samples ##

source("Accuracy-Testing.R")
UCB.IS.OOS = read.csv( "./UCB Data/Demo Data/UCB In State OOS.csv" )
CA.proj = read.csv( "./UCB Data/Demo Data/CA Formatted.csv" ) 
US.proj = read.csv( "./UCB Data/Demo Data/US Projections, No Nativity.csv" )

#### Critical Mass from Projection Data ####

project.CM = function(t = 0) { 
  
  #### Setup ####
  group.names=c('Black','Hispanic.Latino','Asian','White')
  time.span=2020:2029; time.names = paste('year', time.span, sep='.')
  
  #### UC-Berkeley In-State and OOS Demographics #### 
  
  UCB.IS.OOS = UCB.IS.OOS[c(2,4:6,9,11:13),]; years = 1999:2019; year.names = paste('year', years, sep='.')
  
  # iterate over each year to compile IS/OOS historic percentages
  year.list = list(); total = NULL; percent.UCB.in = list(); percent.UCB.out = list()
  for (i in 1:length(years)) {
    year.list[[i]] = UCB.IS.OOS[,c(1,2,i+2)]
    total[i] = sum(UCB.IS.OOS[,(i+2)])
    percent.UCB.in[[i]] = data.frame( Race.Ethicity = group.names,
                                      Percent = round(c(year.list[[i]][1,3]/total[i], year.list[[i]][2,3]/total[i],
                                                        year.list[[i]][3,3]/total[i], year.list[[i]][4,3]/total[i]),5))
    percent.UCB.out[[i]] = data.frame( Race.Ethicity = group.names,
                                       Percent = round(c(year.list[[i]][5,3]/total[i], year.list[[i]][6,3]/total[i],
                                                         year.list[[i]][7,3]/total[i], year.list[[i]][8,3]/total[i]),5))
  }
  names(year.list) = year.names; names(percent.UCB.in) = year.names; names(percent.UCB.out) = year.names
  
  #### Inputing UC-Berkeley In-State and OOS Demographics ####
  
  # calculate weights used in kernel density function (deault is 1/x and this is either ~e^x or ~1/e^2)
  weighting = function(data, t) {
    if (t > 0) {
      a = 1/length(data)
      b=NULL;for (i in 1:length(data)) {b[i] = exp(a*t*i) } 
      c = (b - 1)/sum(b-1)
    } else if (t < 0) {
      a = 1/length(data)
      b=NULL; for (i in 1:length(data)) {b[i] = exp(-a*t*(length(data)-i+1)) } 
      c = (b - 1)/sum(b-1)
    }else {
      c = rep(1/length(data), length(data))
    }
    return(c)
  }
  
  # samps[i] is one ethnic group (4 IS, 4 OOS)
  samps = list();
  for (i in 1:8) {
    samps[[i]] = as.numeric(UCB.IS.OOS[i,(3:length(UCB.IS.OOS[i,]))])
  }
  weights.years = 1999:2019; c = weighting(weights.years, t)
  
  inputes = matrix(NA, 8, 10)
  for (m in 1:10) {
    
    test.samps = rep(list(NULL),8)
    for (i in 1:8) {
      for (j in 1:length(c)) {
        test.samps[[i]] = c(test.samps[[i]], rep(samps[[i]][j], round(c[j]*10000)))
      }
    } 
    
    #test density = kernel density estimates for samps using weights calculated above
    test.dens = list();
    for (i in 1:8) {
      test.dens[[i]] = density(samps[[i]], weights = c)
    }
    
    test.preds = NULL
    for (i in 1:8) {
      test.preds[i] = round(sample(test.samps[[i]], 1, replace = TRUE) + rnorm(1, 0, test.dens[[i]]$bw))
      if (test.preds[i] < 0) {
        test.preds[i] = 0
      }
      samps[[i]] = c(samps[[i]], test.preds[i])
    }
    
    weights.years = c(weights.years, 2019 + m); c = weighting(weights.years, t)
    
    for(i in 1:8) {inputes[i,m] = samps[[i]][length(year.list)+m]}
  
  }
  
  rownames(inputes) = paste(c('AfAm','HiLa','AsAm','White'),rep(c('CA','US'),each=4),sep = '.')
  colnames(inputes) = paste('year', c(2020:2029), sep = '.')
  
  #CA vs US stands in for IS and OOS
  CA.mid = inputes[1:4,]; CA.mid = colSums(CA.mid)
  US.mid = inputes[5:8,]; US.mid = colSums(US.mid)
  
  percent.UCB = rbind(CA.mid, US.mid)
  percent.UCB = round(apply(percent.UCB, 2, function(x) {x/sum(x)}),5)
  
  #### CA Projected % Demographics 2010-2060 ####
  
  CA.proj = CA.proj[c(7,8,10,13,14,16),]
  
  year.list=list(); total=NULL; percent.CA=list()
  for(i in 1:length(time.span)) {
    year.list[[i]] = CA.proj[,c(1,i+1)]
    year.list[[i]] = data.frame( Race.Ethnicity = c("White","Black","Asian","Hispanic"),
                                 Population = c(year.list[[i]][1:3,2], sum(year.list[[i]][4:6,2])))
    total[i] = sum(year.list[[i]][,2])
    percent.CA[[i]] = data.frame(Race.Ethnicity = c("Black","Hispanic.Latino","Asian","White"),
                                 Percent = round(c(year.list[[i]][2,2]/total[i], year.list[[i]][4,2]/total[i], year.list[[i]][3,2]/total[i], year.list[[i]][1,2]/total[i]),5))
  }
  names(year.list)=time.names; names(percent.CA)=time.names
  
  #### U.S. Projected % Demographics from 2016-2060 ####
  
  US.proj = US.proj[,c(1,2,3,4,5)]
  
  year.list=list(); total=NULL; percent.US=list()
  for (i in 1:length(time.span)) {
    year.list[[i]] = subset(US.proj, YEAR==time.span[i] & SEX==0 & (ORIGIN==1 | ORIGIN==2) & (RACE==1 | RACE==2 | RACE==4))[,-1]
    year.list[[i]] = rbind(year.list[[i]][1:3,], c(2,12,years[i],year.list[[i]][4,4]+year.list[[i]][5,4]+year.list[[i]][6,4]))
    total[i] = sum(year.list[[i]][,4])
    percent.US[[i]] = data.frame(Race.Ethnicity = c("Black","Hispanic.Latino","Asian","White"),
                                 Percent = round(c(year.list[[i]][2,4]/total[i], year.list[[i]][4,4]/total[i], year.list[[i]][3,4]/total[i], year.list[[i]][1,4]/total[i]),5))
  }
  names(year.list)=time.names; names(percent.US)=time.names
  
  #### Projected CM by Year ####
  
  CM.proj=matrix(NA,4,10)
  for (i in 1:length(time.span)) {
    for (j in 1:4) {
      CM.proj[j,i] = round(percent.CA[[i]][j,2]*percent.UCB[1,i] +  percent.US[[i]][j,2]*percent.UCB[2,i], 5)
    }
  }
  rownames(CM.proj)=group.names; colnames(CM.proj)=time.names
  
  #### Output ####
  return(CM.proj)
  
}

project.CM()
#### Reiterating CM Projections ####

### Need to do this since we are using a stochastic process for IS/OOS acceptances here ###

reiterate = function(n, t = 0) {
  
  #### Creating and Storing the Iteration ####
  init = Sys.time()
  
  reits = list(); saved.values = rep(list(matrix(NA, 4, n)), 10)
  for (i in 1:n) {
    
    # call CM projection each time (this function could be optimized to minimize repeat code)
    reits[[i]] = project.CM(t)
    for (j in 1:10) {
      
      saved.values[[j]][,i] = reits[[i]][,j]
    }
    
  }
  names(reits) = paste('iter',1:n, sep = '.'); names(saved.values) = paste('year', 2020:2029, sep = '.')
  
  for (i in 1:10) {
    
    rownames(saved.values[[i]]) = rownames(reits[[1]])
    colnames(saved.values[[i]]) = paste('iter', 1:n, sep = '.')
    
  }
  
  #### Taking the Mean, Standard Deviation, and Standard Error of the Iterations ####
  
  stats = rep(list(matrix(NA, 4, 10)),3); names(stats) = c('Mean', 'Lower 95% Bound', 'Upper 95% Bound')
  for (i in 1:10) {
    for (j in 1:4) {
      
      stats[[1]][j,i] = round(mean(saved.values[[i]][j,]),5)
      quantile = quantile(saved.values[[i]][j,],probs=c(.025,.975))
      stats[[2]][j,i] = round(quantile[[1]],5)
      stats[[3]][j,i] = round(quantile[[2]],5)
      
    }
  }
  
  for (i in 1:3) {
    
    stats[[i]] = rbind(stats[[i]][1,], stats[[i]][2,], stats[[i]][3,], stats[[i]][4,])
    rownames(stats[[i]]) = c('African-American','Hispanic/Latino', 'Asian-American', 'White/Caucasian')
    colnames(stats[[i]]) = paste('year', 2020:2029, sep = '.')
    
  }
  
  print(Sys.time() - init)
  
  #### Output ####
  
  return(stats)
  
}

### t = 0   (NO recency-bias for weighting) ###
#reit.t0.n500     = reiterate(500,0)      ### elapsed time: ~   55.83
#reit.t0.n1000    = reiterate(1000,0)    ### elapsed time: ~  2:15.87
#reit.t0.n10000   = reiterate(10000,0)   ### elapsed time: ~ 21:56.29

### t = 5   (LOW recency-bias for weigting) ###
#reit.t5.n500     = reiterate(500,5)      ### elapsed time: ~   44.37
#reit.t5.n1000    = reiterate(1000,5)    ### elapsed time: ~  1:44.70
#reit.t5.n10000   = reiterate(10000,5)   ### elapsed time: ~ 19:15.09

### t = 10  (MEDIUM recency-bias for weighting) ###
#reit.t10.n500    = reiterate(500,10)     ### elapsed time: ~   41.66
#reit.t10.n1000   = reiterate(1000,10)   ### elapsed time: ~  1:39.90
#reit.t10.n10000  = reiterate(10000,10)  ### elapsed time: ~ 17:12.80 

### t = 100 (HIGH recency-bias for weigthing) ###
#reit.t100.n500   = reiterate(500,100)    ### elapsed time: ~   39.85
#reit.t100.n1000  = reiterate(1000,100)  ### elapsed time: ~  1:27.30
#reit.t100.n10000 = reiterate(10000,100) ### elapsed time: ~ 22:16.52

#### Generating Results ####

# d is scalar for each group applying, a " " accepted, e " " enrolling #
results.MC = function(pred_years, d = c(1,1,1,1), a = c(1,1,1,1), e = c(1,1,1,1), n = 50, t = 0) {
  
  #### Setup ####
  MC.pred = pred_model(pred_years=pred_years)
  dem.pred = reiterate(n, t)
  
  #### Creating Results ####
  res = list()
  res[[1]] = round(as.matrix(MC.pred[[1]])*100/as.matrix(dem.pred[[1]]))
  res[[2]] = round(as.matrix(MC.pred[[2]])*100/as.matrix(dem.pred[[1]]))
  names(res) = c('Overall Dem', 'Fall Dem')
  
  #### Output ####
  return(res)
  
}

## Confidence Interval Results
conf_int_CM = function(pred_years, d = c(1,1,1,1), a = c(1,1,1,1), e = c(1,1,1,1), n = 50, t = 0) {
  
  ## Setup ##
  MC.pred = pred_model()
  dem.pred = reiterate(n, t)
  
  ### Criterion Results ###
  res = data.frame(Year = pred_years,
                   Black = MC.pred$Black * 100 / as.matrix(dem.pred$Mean)[1,],
                   BlackLower = MC.pred$BlackLower * 100 / dem.pred$`Upper 95% Bound`[1,],
                   BlackUpper = MC.pred$BlackUpper * 100 / dem.pred$`Lower 95% Bound`[1,])
  res = cbind(res, data.frame(HiLa = MC.pred$HiLa * 100 / as.matrix(dem.pred$Mean)[2,],
              HiLaLower = MC.pred$HiLaLower * 100 / dem.pred$`Upper 95% Bound`[2,],
              HiLaUpper = MC.pred$HiLaUpper * 100 / dem.pred$`Lower 95% Bound`[2,]))
  res = cbind(res, data.frame(Asian = MC.pred$Asian * 100 / as.matrix(dem.pred$Mean)[3,],
                              AsianLower = MC.pred$AsianLower * 100 / dem.pred$`Upper 95% Bound`[3,],
                              AsianUpper = MC.pred$AsianUpper * 100 / dem.pred$`Lower 95% Bound`[3,]))
  res = cbind(res, data.frame(White = MC.pred$White * 100 / as.matrix(dem.pred$Mean)[4,],
                        WhiteLower = MC.pred$WhiteLower * 100 / dem.pred$`Upper 95% Bound`[4,],
                        WhiteUpper = MC.pred$WhiteUpper * 100 / dem.pred$`Lower 95% Bound`[4,]))
  
  cm_plot <- ggplot(data=res, aes(x=Year)) + geom_line(aes(y=Black, color="African-American")) + 
    geom_ribbon(aes(ymin=BlackLower, ymax=BlackUpper), alpha=0.2) + 
    geom_line(aes(y=HiLa, color="Hispanic/Latinx")) + geom_ribbon(aes(ymin=HiLaLower, ymax=HiLaUpper), alpha=0.2) + 
    geom_abline(intercept=100, slope=0, linetype="dashed") + xlab('Year') + ylab('Number of Students') + labs(color="Race/Ethnicity") + ylim(0,100) +
    scale_x_discrete(name="Year", breaks=seq(2020, 2028, 2), labels=paste(seq(2020, 2028, 2)), limits=c(2020:2030))
  
  ggsave(filename="fig9.pdf", plot=cm_plot, device="pdf", path="/Users/juliatucher/Documents/20-21/Affirmative Action/Figures", width=5.1, height=6, units="in", dpi="retina")
  
  # res = data.frame(Year = pred_years,
  #                  CM = MC.pred$Black * 100 / as.matrix(dem.pred$Mean)[1,],
  #                  Lower = MC.pred$BlackLower * 100 / dem.pred$`Upper 95% Bound`[1,],
  #                  Upper = MC.pred$BlackUpper * 100 / dem.pred$`Lower 95% Bound`[1,],
  #                  Race="African-American")
  # res = rbind(res, data.frame(Year = pred_years,
  #                             CM = MC.pred$HiLa * 100 / as.matrix(dem.pred$Mean)[2,],
  #                             Lower = MC.pred$HiLaLower * 100 / dem.pred$`Upper 95% Bound`[2,],
  #                             Upper = MC.pred$HiLaUpper * 100 / dem.pred$`Lower 95% Bound`[2,],
  #                             Race="Hispanic/Latinx"))
  # res = rbind(res, data.frame(Year = pred_years,
  #                             CM = MC.pred$Asian * 100 / as.matrix(dem.pred$Mean)[3,],
  #                             Lower = MC.pred$AsianLower * 100 / dem.pred$`Upper 95% Bound`[3,],
  #                             Upper = MC.pred$AsianUpper * 100 / dem.pred$`Lower 95% Bound`[3,],
  #                             Race="Asian-American"))
  # res = rbind(res, data.frame(Year = pred_years,
  #                             CM = MC.pred$White * 100 / as.matrix(dem.pred$Mean)[4,],
  #                             Lower = MC.pred$WhiteLower * 100 / dem.pred$`Upper 95% Bound`[4,],
  #                             Upper = MC.pred$WhiteUpper * 100 / dem.pred$`Lower 95% Bound`[4,],
  #                             Race="White"))
  # 
  # cm_plot_v2 <- ggplot(data=res, mapping=aes(x=Year)) + facet_grid(cols=vars(Race)) +
  #   geom_line(aes(y=CM)) + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.2) + 
  #   scale_x_discrete(name="Year", breaks=c(2020, 2024, 2028), labels=c("2020", "2024", "2028"), limits=c(2020:2029)) +
  #   ylab("Number of Students")
  # 
  # ggsave(filename="CM_v2.png", plot=cm_plot_v2, device="png", path="/Users/juliatucher/Documents/19-20/Chad RA/Figures", width=10, height=6, units="in", dpi="retina")
  # 
}

#### Plotting Results from results.MC() ####

plot.res = function(d = c(1,1,1,1), a = c(1,1,1,1), e = c(1,1,1,1), time = 'o' ) {
  pred_years=2020:2029
  
  #### Setup ####
  save = results.MC(pred_years, d, a, e)
  
  #### Creating the Plot ####
  if (time == 'o') { n = 1 } else if ( time == 'f' ) { n = 2 } else {stop("Please choose a valid  for time. Overall demographics are 'o' and fall demographics are 'f'. ")}
  
  cm.frame <- data.frame(Year=pred_years, Black=save[[1]][1,], Asian=save[[1]][3,], HiLa=save[[1]][2,], 
                         White=save[[1]][4,])
  cm_plot <- ggplot(data=cm.frame, aes(x=Year)) + geom_line(aes(y=Black, color="African-American")) + 
    geom_line(aes(y=Asian, color="Asian-American")) + geom_line(aes(y=HiLa, color="Hispanic/Latinx")) +
    geom_line(aes(y=White, color="White")) + geom_abline(intercept=100, slope=0, linetype="dashed") +
    xlab('Year') + ylab('Number of Students') + labs(color="Race/Ethnicity") + ylim(0,400) +
    scale_x_discrete(name="Year", breaks=c(2020, 2022, 2024, 2026, 2028), labels=c("2020", "2022", "2024", "2026", "2028"), limits=c(2020:2030))
    
  ggsave(filename = "CM-proj.png",plot = cm_plot,device = "png", path = "/Users/juliatucher/Documents/19-20/Chad RA/Figures",width = 6, height=4,units = "in",dpi = "retina")
    
  # cm_zoom_plot <- ggplot(data=cm.frame, aes(x=Year)) + geom_line(aes(y=Black, color="African-American")) + 
  #   geom_line(aes(y=HiLa, color="Hispanic/Latinx")) + geom_line(aes(y=White, color="White")) + xlab('Year') +
  #   geom_abline(intercept=100, slope=0, linetype="dashed") + ylab('Number of Students') + ylim(0,100) +
  #   scale_x_discrete(name="Year", breaks=c(2020, 2022, 2024, 2026, 2028), labels=c("2020", "2022", "2024", "2026", "2028"), limits=c(2020:2030))
  # 
}



# ### Results given no intervention ###
# plot.res(d = c(1, 1, 1, 1), e = c(1, 1, 1, 1), time = 'o')
# legend('topright', c('African-American', 'Hispanic/Latino', 'Asian-American', 'White/Caucasian'), 
#        lwd = c(2,2,2,2), col = c('red','blue','green','orange'))
# 
# ### Results given moderate-high intervention in application and enrollment rates for under-represented groups ###
# plot.res(d = c(3, 3, 1, 3), e = c(3, 3, 1, 2), time = 'o')
# legend('topright', c('African-American: 3x App, 3x En','Hispanic/Latino:    3x App, 3x En','Asian-American:   1x App, 1x En','White/Caucasian: 3x App, 2x En'), 
#        lwd = c(2,2,2,2), col = c('red','blue','green','orange'))

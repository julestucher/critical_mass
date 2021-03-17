
source('Predicting-Rates.R')
library(readxl)
library(ggplot2)
library(nleqslv)

#### High School Graduates ####

### Setting up the number of graduates per year in each dem group ###

HS.setup = function(print=FALSE) {

  #### Setup ####
  
  ## Old Methods ##
  # pred_years = c(2013:2027); group_names=c('Black','HiLa','Asian','White')
  # 
  # #### Putting HS Grad Data Together ####
  # HS_grads_future = c(3478027, 3479920, 3498350, 3538820, 3568400,
  #                     3612500, 3606230, 3566910, 3589040, 3604150,
  #                     3601740, 3664390, 3721920, 3701840, 3597370)
  # #### Data from here: https://nces.ed.gov/programs/digest/d18/tables/dt18_219.10.asp?current=yes ####
  # 
  # HS_rate_future = matrix(c(.146,.202,.057,.565,   .143,.208,.058,.559,   .144,.214,.058,.550,   .150,.214,.058,.547,   .157,.211,.057,.544,
  #                           .165,.208,.059,.537,   .160,.219,.059,.530,   .162,.218,.061,.526,   .161,.220,.063,.523,   .161,.225,.064,.517,
  #                           .165,.229,.064,.509,   .171,.233,.063,.500,   .174,.235,.063,.494,   .177,.234,.064,.490,   .165,.249,.064,.486), 15, 4, byrow=TRUE)
  # #### Data from here: https://nces.ed.gov/programs/digest/d18/tables/dt18_219.30.asp?current=yes ####
  # HS_preds = matrix(NA, 15, 4)
  # for (i in 1:15) {HS_preds[i,] = round(HS_grads_future[i]*HS_rate_future[i,])}
  # rownames(HS_preds) = pred_years; colnames(HS_preds) = group_names
  
  overall_years = c(1999:2029); df_names=c('Year', 'Count', 'Race', 'Predicted') 
  pred_years = c(2015:2029); group_names=c('Black','HiLa','Asian','White')
  
  #### Data from here: https://nces.ed.gov/programs/digest/d18/tables/dt18_219.30.asp?current=yes ####
  HS_data = read_excel("High-School-Grads.xls")
  
  black <- as.numeric(HS_data$...4[4:34])
  hila <- as.numeric(HS_data$...5[4:34])
  asian <- as.numeric(HS_data$...6[4:34])
  white <- as.numeric(HS_data$...3[4:34])
  
  HS_preds <- cbind(black, hila, asian, white)
  HS_preds <- HS_preds[17:31,]
  rownames(HS_preds) = pred_years; colnames(HS_preds) = group_names
  
  black_data <-data.frame(overall_years, black, "African-American", overall_years > 2012)
  hila_data <- data.frame(overall_years, hila, "Hispanic/Latinx", overall_years > 2012)
  asian_data <- data.frame(overall_years, asian, "Asian-American", overall_years > 2012)
  white_data <- data.frame(overall_years, white, "White", overall_years > 2012)
  names(black_data)=df_names; names(hila_data)=df_names; names(asian_data)=df_names; names(white_data)=df_names
  
  ## Plot HS Data ##
  hs.df <- rbind(black_data, hila_data, asian_data, white_data)
  
  hs.df$Count <- as.numeric(as.character(hs.df$Count))
  
  p <- ggplot(hs.df) + geom_point(mapping = aes(x=Year, y=Count, shape=Predicted, color=Race)) + 
    labs(x="Year",y="U.S. High School Graduates")
  
  if(print) ggsave(filename="HighSchoolGrads.png", plot=p, device=png(), path="/Users/juliatucher/Documents/19-20/Chad RA/Figures", width=6, height=4, units="in", dpi="retina")
  
  #### Output ####
  return(HS_preds)
  
}




#### Creating Transition Matrices (2015 - 2029) ####

### Creates our MC matrices in canonical form (i.e. seperating transient and absorbing states) ###

# parameter preds has default value from prediction function, but can be run with other starting matrices when doing sensitivity analysis
MC_matrices = function(preds = NULL, rates = NULL, d = c(1,1,1,1), a = c(1,1,1,1), e = c(1,1,1,1)) {
  
  #### Setup ####
  group_names=c('Black','HiLa','Asian','White')
  
  #### Putting Matrices Together ####
  P = list(); HS_preds = HS.setup(); 
  
  if(!is.null(preds)) {
    college_data = preds
  } else {
    college_data = application_preds()
  }
  
  if(!is.null(rates)) {
    rate_data = rates
  } else {
    rate_data = rate_preds()
  }
  
  # iterate over all 15 years
  for (i in 1:15) {
    
    overall = rbind(HS_preds[i,], college_data$Applicants[i,], college_data$Acceptances[i,], college_data$Enrollment[i,], 
                    rate_data[['RR']][i,1:4], rate_data[['G4']][i,1:4], rate_data[['G5']][i,1:4], rate_data[['G6']][i,1:4])
    rownames(overall) = c('HS','app','acc','en','rr','g4','g5','g6')
    
    # HS rates -- apply, accept, enroll
    delta=overall['app',]/overall['HS',];
    alpha=overall['acc',]/overall['app',];
    epsilon=overall['en',]/overall['acc',]
    
    
    if (i > 5) {
      for (j in 1:4) {
        if (d[j] == 1) {d_seq = rep(1,10)} else {d_seq = seq(1, d[j], .1*(d[j] - 1))[2:11]}
        if (a[j] == 1) {a_seq = rep(1,10)} else {a_seq = seq(1, a[j], .1*(a[j] - 1))[2:11]}
        if (e[j] == 1) {e_seq = rep(1,10)} else {e_seq = seq(1, e[j], .1*(e[j] - 1))[2:11]}
        
        
        delta[j]   = delta[j]*d_seq[i-5];   if (delta[j] > 1)   {delta[j] = 1}
        alpha[j]   = alpha[j]*a_seq[i-5];   if (alpha[j] > 1)   {alpha[j] = 1} 
        epsilon[j] = epsilon[j]*e_seq[i-5]; if (epsilon[j] > 1) {epsilon[j] = 1}
      }
    }
    
    # retention rate
    rho=mapply(function(x) min(1, x)^(1/3), as.numeric(overall['rr',]))
    
    # graduation rates
    g6=mapply(function(x) min(1, as.numeric(overall['g6',x]), rho[x]^3), 1:4)
    g5=mapply(function(x) min(1, as.numeric(overall['g5',x]), g6[x]),1:4)
    g4=mapply(function(x) min(1, as.numeric(overall['g4',x]), g5[x]), 1:4)
    
    
    
    # tau, lambda 4C, 5C, 4G, 5G, 6G for each of the ethnic groups
    # tau1=NULL; l4C1=NULL; l5C1=NULL; l4G1=NULL; l5G1=NULL; l6G1=NULL;
    # for(j in 1:length(group_names)){
    #   
    #   # x is a six-element vector (tau, l4c, l4g, l5c, l5g, l6g)
    #   fn_system <- function(x, v, g4, g5, g6){
    #     y = numeric(6)
    #     y[1] = v*x[1]^7*x[3] - g4
    #     y[2] = x[2]/(1-x[3]) - x[1]
    #     y[3] = g4 + v*x[1]^9*x[2]*x[5] - g5
    #     y[4] = x[4]/(1-x[5]) - x[1]
    #     y[5] = g5 + v*x[1]^11*x[2]*x[4]*x[6] - g6
    #     y[6] = (1 - x[1]^7*x[3] - x[1]^9*x[2]*x[5] - x[1]^11*x[2]*x[4]*x[6])/v - v + g6
    #     
    #     return(y)
    #   }
    #   solns = nleqslv(c(0.98,0.12,0.70,0.14,0.85,0.83), fn_system, v=rho[j]^3, g4=g4[j], g5=g5[j], g6=g6[j], control=list(allowSingular=TRUE))
    #   tau1[j] = solns$x[1]; l4C1[j]=solns$x[2]; l4G1[j]=solns$x[3]; l5C1[j]=solns$x[4]; l5G1[j]=solns$x[5]; l6G1[j]=solns$x[6];
    # }
    
    # tau, lambda 4C, 5C, 4G, 5G, 6G for each of the ethnic groups
    # tau1=NULL; l4C1=NULL; l5C1=NULL; l4G1=NULL; l5G1=NULL; l6G1=NULL;
    # for (j in 1:length(group_names)) {
    #   v=rho[j]^3
    #   l4G=function(x) g4[j] / (v * x^7)
    #   l4C=function(x) x - x*l4G(x)
    #   l5G=function(x) (g5[j]-g4[j]) / (v*x^9*l4C(x))
    #   l5C=function(x) x - x*l5G(x)
    #   l6G=function(x) (g6[j] - g5[j]) / (v*x^11*l4C(x)*l5C(x))
    #   root=function(x) (1 - x^7*l4G(x) - x^9*l4C(x)*l5G(x) - x^11*l4C(x)*l5C(x)*l6G(x))/v - (v - g6[j])
    #   tau1[j]=tryCatch({
    #     uniroot(root,c(0.01,1))$root
    #   }, finally = { 1 }
    #   )
    #   l4C1[j]=l4C(tau1[j]); l5C1[j]=l5C(tau1[j]); l4G1=l4G(tau1[j]); l5G1=l5G(tau1[j]); l6G1=l6G(tau1[j]);
    # }
    
    # tau, lambda 4C, 5C, 4G, 5G, 6G for each of the ethnic groups
    tau=NULL; l4C=NULL; l5C=NULL; l4G=NULL; l5G=NULL; l6G=NULL;
    for (j in 1:length(group_names)) {
      v=rho[j]^3
      l4Gf=function(x) g4[j] / (v * x^7)
      l4Cf=function(x) x - l4Gf(x)
      l5Gf=function(x) (g5[j]-g4[j]) / (v*x^9*l4Cf(x))
      l5Cf=function(x) x - l5Gf(x)
      root=function(x) g5[j] + v*x^12*l4Cf(x)*l5Cf(x) - g6[j]
      tau[j]=tryCatch({
        uniroot(root,c(0.0001,1))$root
      }, error = function(e){
        #print(paste("v =", v))
        #print(paste("g6 =", g6[j]))
        #print(e)
        1
      },
      finally = { 1 }
      )
      l4C[j]=max(l4Cf(tau[j]), 0, na.rm=TRUE)
      l5C[j]=max(l5Cf(tau[j]), 0, na.rm=TRUE)
      l4G[j]=min(l4Gf(tau[j]), 1, na.rm=TRUE)
      l5G[j]=min(l5Gf(tau[j]), 1, na.rm=TRUE)
      l6G[j]=tau[j];
    }
    
    # tau=NULL
    # for (j in 1:length(group_names)) {
    #   root=function(x) rho[j]^3*x^13-g4[j]*(x^6-x^3)-g5[j]*(x^3-1)-((rho[j]^3)/14)-(g6[j]*13/14);
    #   #root=function(x) rho[j]^3*x^13-g4[j]*(x^6-x^3)-g5[j]*(x^3-1)-g6[j];
    #   tau[j]=tryCatch({
    #     uniroot(root,c(0.1,1))$root
    #   }, error = function(err) {
    #     print(err)
    #     print(rho[j]^3)
    #     print(g6[j])
    #   }, finally = { 1 }
    #   )
    # }
    # 
    # l4C=NULL; l5C=NULL; l4G=NULL; l5G=NULL; l6G=NULL;
    # for (j in 1:length(group_names)) {
    #   l4C[j]=max((rho[j]^3*tau[j]^7-g4[j])/(rho[j]^3*tau[j]^6), 0, na.rm=TRUE)
    #   l5C[j]=max((rho[j]^3*tau[j]^10-g4[j]*(tau[j]^3-1)-g5[j])/(rho[j]^3*tau[j]^9-g4[j]*tau[j]^2), 0, na.rm=TRUE)
    #   l4G[j]=min((g4[j])/(rho[j]^3*tau[j]^7), 1, na.rm=TRUE)
    #   l5G[j]=min((g5[j]-g4[j])/(rho[j]^3*tau[j]^10-g4[j]*tau[j]^3), 1, na.rm=TRUE)
    #   l6G[j]=min((g6[j]-g5[j])/(rho[j]^3*tau[j]^13-g4[j]*(tau[j]^6-tau[j]^3)-g5[j]*tau[j]^3), 1, na.rm=TRUE)
    # }
    
    formatted=rbind(delta,alpha,epsilon,rho,tau,l4C,l5C,l4G,l5G,l6G); rownames(formatted)=c('delta','alpha','epsilon','rho','tau','4C','5C','4G','5G','6G')
    
    Q=list(); R=list(); P.group=list()
    
    # for each 
    for (j in 1:length(group_names)) {
      Q[[j]]=round(matrix(c(rep(0,1),formatted['delta',j],rep(0,18),
                            rep(0,2),formatted['alpha',j],rep(0,17),
                            rep(0,3),formatted['epsilon',j],rep(0,16),
                            rep(0,4),formatted['rho',j],rep(0,15),
                            rep(0,5),formatted['rho',j],rep(0,14),
                            rep(0,6),formatted['rho',j],rep(0,13),
                            rep(0,7),formatted['tau',j],rep(0,12),
                            rep(0,8),formatted['tau',j],rep(0,11),
                            rep(0,9),formatted['tau',j],rep(0,10),
                            rep(0,10),formatted['tau',j],rep(0,9),
                            rep(0,11),formatted['tau',j],rep(0,8),
                            rep(0,12),formatted['tau',j],rep(0,7),
                            rep(0,13),formatted['tau',j],rep(0,6),
                            rep(0,14),formatted['4C',j],rep(0,5),
                            rep(0,15),formatted['tau',j],rep(0,4),
                            rep(0,16),formatted['tau',j],rep(0,3),
                            rep(0,17),formatted['5C',j],rep(0,2),
                            rep(0,18),formatted['tau',j],rep(0,1),
                            rep(0,19),formatted['tau',j],
                            rep(0,20)),20,20,byrow=TRUE),5)
      colnames(Q[[j]])=c('0A','0B','0C','1A','1B','1C','2A','2B','2C','3A','3B','3C','4A','4B','4C','5A','5B','5C','6A','6B')
      rownames(Q[[j]])=c('0A','0B','0C','1A','1B','1C','2A','2B','2C','3A','3B','3C','4A','4B','4C','5A','5B','5C','6A','6B')
      
      R[[j]]=matrix(round(c(rep(0,3),1-formatted['delta',j],
                            rep(0,3),1-formatted['alpha',j],
                            rep(0,3),1-formatted['epsilon',j],
                            rep(0,3),1-formatted['rho',j],
                            rep(0,3),1-formatted['rho',j],
                            rep(0,3),1-formatted['rho',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            formatted['4G',j],rep(0,2),1-formatted['4C',j]-formatted['4G',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,1),formatted['5G',j],rep(0,1),1-formatted['5C',j]-formatted['5G',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,3),1-formatted['tau',j],
                            rep(0,2),formatted['6G',j],1-formatted['6G',j]),5),20,4,byrow=TRUE)
      colnames(R[[j]])=c("4G","5G","6G","DNF")
      rownames(R[[j]])=c('0A','0B','0C','1A','1B','1C','2A','2B','2C','3A','3B','3C','4A','4B','4C','5A','5B','5C','6A','6B')
      
      zeros=matrix(0, 4, 20, byrow=TRUE); ident=diag(4)
      bottom=cbind(zeros,ident); rownames(bottom)=c('4G','5G','6G','DNF')
      temp=cbind(Q[[j]], R[[j]])
      P.group[[j]]=rbind(temp, bottom)
    }
    
    names(P.group)=group_names
    
    P[[i]]=P.group
  }
  names(P) = paste('year',rownames(college_data$Applicants),sep='.')
  
  #### Output ####
  return(P)
}

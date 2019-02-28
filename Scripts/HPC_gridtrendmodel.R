# script to run Chun's grid analysis on HPC
require(tidyverse)
require(lubridate)
require(raster)
require(lme4)
## ----rcurl----------------------------------------------------------
require(maptools)
require(mgcv)

setwd("/project/RDS-FAE-HPWC-RW")

load("Data/Chundata.Rdata")

## perform step test on the data using ns = 4
## function to calculate the step trend statistic (Hirsch, 1985)
stepTrend2 <- function(x,y,i, endyear=2015){
  #browser()
  n1 <- 2003-1997
  
  if (i==1){
    
    z <- 56  # no. of column, related to the dim of the region
    # Qld 57 by 51
    # NSW 38 by 33
    
  } else {
    
    z <- 37 }
  
  if (x%%z!=0){ #& x%%z!=(z-1)) {
    y.1 <- y[[x]]
    y.2 <- y[[x+1]]
    y.3 <- y[[x+z]]
    y.4 <- y[[x+z+1]]
    
    if(!is.na(y.1[1])&!is.na(y.2[1])&!is.na(y.3[1])&!is.na(y.4[1])) { 
      if (i == 1) {
        n2 <- endyear-2004
        v.1 <- ts(y.1[c(229:288,313:length(y.1))],
                  start=c(1997,1),frequency=12)
        v.2 <- ts(y.2[c(229:288,313:length(y.1))],
                  start=c(1997,1),frequency=12)
        v.3 <- ts(y.3[c(229:288,313:length(y.1))],
                  start=c(1997,1),frequency=12)
        v.4 <- ts(y.4[c(229:288,313:length(y.1))],
                  start=c(1997,1),frequency=12)
        
      } else {
        
        n2 <- endyear-2003
        
        v.1 <- ts(y.1[c(229:length(y.1))],
                  start=c(1997,1),frequency=12)
        v.2 <- ts(y.2[c(229:length(y.2))],
                  start=c(1997,1),frequency=12)
        v.3 <- ts(y.3[c(229:length(y.3))],
                  start=c(1997,1),frequency=12)
        v.4 <- ts(y.4[c(229:length(y.4))],
                  start=c(1997,1),frequency=12)
      }
      
      ## rank by month
      for (j in 1:12) {
        assign(paste("r1",j,sep=""),rank(subset(v.1,cycle(v.1)==j)))
        assign(paste("r2",j,sep=""),rank(subset(v.2,cycle(v.2)==j)))
        assign(paste("r3",j,sep=""),rank(subset(v.3,cycle(v.3)==j)))
        assign(paste("r4",j,sep=""),rank(subset(v.4,cycle(v.4)==j)))
        
      }
      
      ## mann-whitney rank sum statistic
      rlist.1 <- list(r11,r12,r13,r14,r15,r16,r17,r18,r19,r110,r111,r112) ## full year
      rlist.2 <- list(r21,r22,r23,r24,r25,r26,r27,r28,r29,r210,r211,r212)
      rlist.3 <- list(r31,r32,r33,r34,r35,r36,r37,r38,r39,r310,r311,r312)
      rlist.4 <- list(r41,r42,r43,r44,r45,r46,r47,r48,r49,r410,r411,r412)
      rlist <- list(unlist(rlist.1),unlist(rlist.2),unlist(rlist.3),unlist(rlist.4)) 
      
      w.s1 <- 0
      w.s2 <- 0
      w.s3 <- 0
      w.s4 <- 0
      
      for (k in 1:length(rlist.1)) {
        w.s1 <- sum(rlist.1[[k]][1:n1])+w.s1
        w.s2 <- sum(rlist.2[[k]][1:n1])+w.s2
        w.s3 <- sum(rlist.3[[k]][1:n1])+w.s3
        w.s4 <- sum(rlist.4[[k]][1:n1])+w.s4
      }
      w.s <- sum(w.s1,w.s2,w.s3,w.s4) #,w.s5,w.s6,w.s7,w.s8,w.s9)
      
      ## other statistic
      # mean for full year
      mu <- n1*(n1+n2+1)/2
      mu.s <- 12*4*mu
      
      ## variance (standard variation). The autocorrelation of the residuals can be assumed to be null (checked, cor.test())
      # assume only cross correlation
      ss <- n1*n2*(n1+n2+1)/12
      ss.s <- 0
      for (kk in 1:4) {
        for (kkk in 1:4) {
          #         for (kkkk in 1:12) {
          ss.s <- 12*ss*cor(rlist[[kk]],rlist[[kkk]]) + ss.s
          #         }
        }
      }
      
      ## step trend statistic
      z <- (w.s - mu.s)/sqrt(ss.s)
      
      return(z) }
    
    else {return(NA)} }
  
  else {return(NA)}
  
}

## 30 years
## full year,
## central QLD
qld.z.new <- unlist(lapply(1:(length(residual_qld)-57),function(x) stepTrend2(x,residual_qld,1)))
qld.z.new <- c(qld.z.new,rep(NA,57))

## NSW
nswvic.z.new <- unlist(lapply(1:(length(residual_nswvic)-38),function(x) stepTrend2(x,residual_nswvic,2)))
nswvic.z.new <- c(nswvic.z.new,rep(NA,38))



#ns = 4 results
# positive
qld_origp.new <- length(subset(qld.z.new,qld.z.new>=1.645))/
  length(qld.z.new)
# 0.00034
# negative
qld_orign.new <- length(subset(qld.z.new,qld.z.new<=-1.645))/
  length(qld.z.new)
# 0

nsw_origp.z.new <- length(subset(nswvic.z.new,nswvic.z.new>=1.645))/
  length(nswvic.z.new)
# 0
nsw_orign.z.new <- length(subset(nswvic.z.new,nswvic.z.new<=-1.645))/
  length(nswvic.z.new)
# 0


# bootstrap ns = 4
## multiple cells
# Set up for the shorter period
NstepTrend <- function(x,y,m,i){
 #browser()

  n1 <- 2003-1997
  
  if (i==1){
    
    z <- 56  # no. of column, related to the dim of the region
    # Qld 57 by 51
    # NSW 38 by 33
    
  } else {
    
    z <- 37 }
  
  if (x%%z!=0){ #& x%%z!=(z-1)) {
    y.1 <- m[[x]]
    y.2 <- m[[x+1]]
    y.3 <- m[[x+z]]
    y.4 <- m[[x+z+1]]
    
    if(!is.na(y.1[1])&!is.na(y.2[1])&!is.na(y.3[1])&!is.na(y.4[1])) {
      if (i == 1) {
        n2 <- 2016-2004
        v.1 <- ts(y.1[c(229:288,313:length(y.1))],
                  start=c(1997,1),frequency=12)
        v.2 <- ts(y.2[c(229:288,313:length(y.2))],
                  start=c(1997,1),frequency=12)
        v.3 <- ts(y.3[c(229:288,313:length(y.3))],
                  start=c(1997,1),frequency=12)
        v.4 <- ts(y.4[c(229:288,313:length(y.4))],
                  start=c(1997,1),frequency=12)
        
      } else {
        
        n2 <- 2016-2002
        
        v.1 <- ts(y.1[c(229:length(y.1))],start=c(1997,1),frequency=12)
        v.2 <- ts(y.2[c(229:length(y.2))],start=c(1997,1),frequency=12)
        v.3 <- ts(y.3[c(229:length(y.3))],start=c(1997,1),frequency=12)
        v.4 <- ts(y.4[c(229:length(y.4))],start=c(1997,1),frequency=12)
      }
      
      ## rank by month
   #   browser()
      for (j in 1:12) {
        y1.mth <- data.frame(mth=as.vector(window(v.1,
                                                  start=c(1997,j),
                                                  frequency=1)),
                             o=y[[j]])[order(y[[j]]),]
        y2.mth <- data.frame(mth=as.vector(window(v.2,
                                                  start=c(1997,j),
                                                  frequency=1)),
                             o=y[[j]])[order(y[[j]]),]
        y3.mth <- data.frame(mth=as.vector(window(v.3,
                                                  start=c(1997,j),
                                                  frequency=1)),
                             o=y[[j]])[order(y[[j]]),]
        y4.mth <- data.frame(mth=as.vector(window(v.4,
                                                  start=c(1997,j),
                                                  frequency=1)),
                             o=y[[j]])[order(y[[j]]),]
        
        assign(paste("r1",j,sep=""),rank(y1.mth$mth))
        assign(paste("r2",j,sep=""),rank(y2.mth$mth))
        assign(paste("r3",j,sep=""),rank(y3.mth$mth))
        assign(paste("r4",j,sep=""),rank(y4.mth$mth))
        
      }
      
      ## mann-whitney rank sum statistic
      rlist.1 <- list(r11,r12,r13,r14,r15,r16,r17,r18,r19,r110,r111,r112) ## full year
      rlist.2 <- list(r21,r22,r23,r24,r25,r26,r27,r28,r29,r210,r211,r212)
      rlist.3 <- list(r31,r32,r33,r34,r35,r36,r37,r38,r39,r310,r311,r312)
      rlist.4 <- list(r41,r42,r43,r44,r45,r46,r47,r48,r49,r410,r411,r412)
      rlist <- list(unlist(rlist.1),unlist(rlist.2),
                    unlist(rlist.3),unlist(rlist.4))
      ## seasonal
      w.s1 <- 0
      w.s2 <- 0
      w.s3 <- 0
      w.s4 <- 0
      for (k in 1:length(rlist.1)) {
        #   for (k in c(9:11)) {
        w.s1 <- sum(rlist.1[[k]][1:n1])+w.s1
        w.s2 <- sum(rlist.2[[k]][1:n1])+w.s2
        w.s3 <- sum(rlist.3[[k]][1:n1])+w.s3
        w.s4 <- sum(rlist.4[[k]][1:n1])+w.s4
      }
      w.s <- sum(w.s1,w.s2,w.s3,w.s4) #,w.s5,w.s6,w.s7,w.s8,w.s9)
      
      ## other statistic
      # mean for full year
      mu <- n1*(n1+n2+1)/2
      mu.s <- 12*4*mu
      # ## mean for one season
      #     mu <- n1*(n1+n2+1)/2
      #     mu.s <- 3*4*mu
      
      ## variance (standard variation). 
      # assume only cross correlation
      ss <- n1*n2*(n1+n2+1)/12
      ss.s <- 0
      for (kk in 1:4) {
        for (kkk in 1:4) {
          #         for (kkkk in 1:12) {
          ss.s <- 12*ss*cor(rlist[[kk]],rlist[[kkk]]) + ss.s
          #         }
        }
      }
      ## variance for one season
      #     ss <- n1*n2*(n1+n2+1)/3
      #     ss.s <- 0
      #     for (kk in 1:4) {
      #       for (kkk in 1:4) {
      #           ss.s <- 3*ss*cor(rlist[[kk]],rlist[[kkk]]) + ss.s
      #       }
      #     }
      
      ## step trend statistic
      z <- (w.s - mu.s)/sqrt(ss.s)
      
      return(z) }
    
    else {return(NA)} }
  
  else {return(NA)}
  
}


R <- 1000
new.order <- list()

zpq.values = numeric(R)
znq.values = numeric(R)
period1 <- 2002 - 1997
period2 <- 2016 - 2004
mth.l <- period1 + period2

#ptm <- proc.time()
require(doParallel)
# cl <- makeCluster(4)
# registerDoParallel(cl)

# QLD_zvalues <- foreach (i=1:R, .combine = rbind) %do% {
  
  # for (j in 1:12) {
    # new.order[[j]]<-sample(1:mth.l)
  # }
  
  # temp <- unlist(lapply(1:(length(residual_qld)-57),
                        # function(x,y) NstepTrend(x,y,residual_qld,1),new.order))
 # if(i==1) {write.table(temp,file="bt_zvalues_qld_7y.txt",row.names=F)
  #          } else
   #           {write.table(temp,file="bt_zvalues_qld_7y.txt",append=T,row.names=F,col.names=F)}
  # data.frame(zpq.values =
               # length(subset(temp,temp>=1.645))/length(temp),
             # znq.values =
               # length(subset(temp,temp<=-1.645))/length(temp))
# }
# stopCluster(cl)

# saveRDS(QLD_zvalues,"QLD_zvalues.RDS")

zpq.values = numeric(R)
znq.values = numeric(R)
period1 <- 2002 - 1997
period2 <- 2016 - 2002
mth.l <- period1 + period2



cl <- makeCluster(4)

registerDoParallel(cl)
# NSW VIC
NSW_zvalues <- foreach (i=1:R, .combine = rbind) %do% {
  
  for (j in 1:12) {
    new.order[[j]]<-sample(1:mth.l)
  }
  
  temp <- unlist(lapply(1:(length(residual_nswvic)-38),
                        function(x,y) NstepTrend(x,y,residual_nswvic,2),new.order))
  # if(i==1) {write.table(temp,file="bt_zvalues_qld_7y.txt",row.names=F)
  #           } else
  #             {write.table(temp,file="bt_zvalues_qld_7y.txt",append=T,row.names=F,col.names=F)}
  data.frame(zpq.values =
               length(subset(temp,temp>=1.645))/length(temp),
             znq.values =
               length(subset(temp,temp<=-1.645))/length(temp))
}
stopCluster(cl)

saveRDS(NSW_zvalues,"NSW_zvalues.RDS")

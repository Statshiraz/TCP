rm(list=ls())

##############################Packages#######################
library(MASS)
library(shiny)
library(ggplot2)
library(gridExtra)
library(psych)
library(plot3D)
library(mvtnorm)
library(readxl)
library(datawizard)
#############################Functions#######################

des<-function(Data){
  desc1<-describe(Data)
  desc2<-desc1[,c(2:4,8:13)]
  cn1<-row.names(desc2)
  cn2<-c("N","Mean","Std.Deviation","Minimum","Maximum","Range","Skew","Kurtosis","std.Error of Mean")
  NN<-length(cn1)
  desc3<-empty.dump()
  for(i in 1:NN) desc3<-cbind(desc3,round(as.numeric(desc2[i,]),3))
  rbind(c("",cn1),cbind(cn2,desc3))
}

corr<-function(Data){
  n<-ncol(Data)
  if(n==1) print(paste("Error:The data must be multivariate"))
  if(n>=2){
    corr<-matrix(rep(0),nrow=n*2,ncol=n)
    corr1<-round(cor(Data),4)
    corr2<-corr1
    for(i in 1:(n-1)){ for(j in (i+1):n){corr2[i,j]=corr2[j,i]<-round(cor.test(Data[,i],Data[,j])$p.value,4)}}
    t=1
    cn<-rep(0,0)
    cn1<-row.names(describe(Data))
    for(i in 1:n){
      corr[t,]<-corr1[i,]
      corr[(t+1),]<-corr2[i,]
      t=2*i+1
      cn<-c(cn,cn1[i],"")
    }
    corr<-round(corr,4)
    rbind(c("","",cn1),cbind(cn,rep(c("correlation coefficient","p-value"),n),corr))
  }
}

plot2D<-function(Data,x1,x2){
  theme_set(theme_bw())
  N<-nrow(Data)
  T<-1:N
  d<-data.frame(T,Data)
  cn1<-row.names(describe(Data)[c(x1,x2),])
  p1<-ggplot(data=d) +  geom_point(aes(T,Data[,x1]), color="blue")+ geom_line(aes(T,Data[,x1]), color="black") +  ylab(cn1[1])+xlab("Time")
  p2<-ggplot(data=d) +  geom_point(aes(T,Data[,x2]), color="blue") + geom_line(aes(T,Data[,x2]), color="black")+  ylab(cn1[2])+xlab("Time")
  grid.arrange(p1, p2)
}

plot3D<-function(Data,x1,x2){
  N<-nrow(Data)
  T<-1:N
  cn1<-row.names(describe(Data)[c(x1,x2),])
  scatter3D(x=Data[,x1], y=Data[,x2], z=as.matrix(T),xlab=cn1[1],zlab="Time",ylab=cn1[2],phi = 0, bty = "g", ticktype = "detailed")
}

Pro<-function(Data,s){
  rescale1 <- function(x) rescale(x, to = c(0, 1), range = NULL, verbose = TRUE)
  Data=(apply(Data, 2,rescale1))
  n<-nrow(Data)
  pd1<-Data%*%s
  pd3<-s%*%s
  coff=rep(0,0)
  yhat<-matrix(0,nrow=n,ncol=ncol(Data))
  for(i in 1:n){
    coff[i]<-pd1[i]/pd3
  }
  coff
}

Propelt<-function(Data,s){
  n<-nrow(Data)
  pd1<-Data%*%s
  pd3<-s%*%s
  coff=rep(0,0)
  yhat<-matrix(0,nrow=n,ncol=ncol(Data))
  for(i in 1:n){
    coff[i]<-pd1[i]/pd3
  }
  coff
}


trend<-function(PData,chp){
  chp<-sort(chp[(chp)!=0])
  n<-length(PData)
  Time<-1:n
  if(length(chp)==0) {
    yl<-PData
    modl<-lm(formula = yl ~ Time)
    rp<-summary(modl)[[4]]
    rp1<-rp
    rp2<-rbind(c("","","Estimate","Std. Error","t value","Pr(>|t|)" ),cbind(matrix(c("Segment",""),ncol=1),matrix(c("Intercept","Time"),ncol=1),cbind(round(as.numeric(rp1[,1]),3),round(as.numeric(rp1[,2]),3),round(as.numeric(rp1[,3]),3),round(as.numeric(rp1[,4]),3))))
    fit<-fitted(modl)
  }
  if(length(chp)!=0) {
    rp<-list()
    rp1<-empty.dump()
    dd=cc=rep(0,0)
    fit<-rep(0,0)
    chpn<-c(0,chp,n)
    for(i in 1:(length(chp)+1)){
      yl<-PData[(chpn[i]+1):chpn[i+1]]
      Time<-c((chpn[i]+1):chpn[i+1])
      modl<-lm(formula = yl ~ Time)
      rp[[i]]<-summary(modl)[[4]]
      rp1<-rbind(rp1,rp[[i]])
      cc<-c(cc,"Segment","")
      dd<-c(dd,"Intercept","Time")
      fit[(chpn[i]+1):chpn[i+1]]<-fitted(modl)
    }
    rp2<-rbind(c("","","Estimate","Std. Error","t value","Pr(>|t|)" ),cbind(matrix(cc,ncol=1),matrix(dd,ncol=1),cbind(round(as.numeric(rp1[,1]),3),round(as.numeric(rp1[,2]),3),round(as.numeric(rp1[,3]),3),round(as.numeric(rp1[,4]),3))))
  }
  list(rp,fit,rp2) 
}


CL<-function(PData,a,b){
  n<-length(PData)
  Time<-(a+1):b
  yl<-PData[(a+1):b]
  modl<-lm(formula = yl ~ Time)
  res<-sqrt(sum((modl$residuals)^2))
  res
}



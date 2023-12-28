rm(list = ls())

#=================Package======================
library(mvtnorm)
library(readxl)

#================Function=======================
Pro<-function(Data,s){
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
  n<-length(PData)
  Time<-1:n
  if(length(chp)==0) {
    yl<-PData
    modl<-lm(formula = yl ~ Time)
    rp<-summary(modl)[[4]]
    fit<-fitted(modl)
    coff<-rp[2,1]
    see<-rp[2,2]
  }
  if(length(chp)!=0) {
    rp<-list()
    fit=coff=see<-rep(0,0)
    chpn<-c(0,chp,n)
    for(i in 1:(length(chp)+1)){
      if(length((chpn[i]+1):chpn[i+1])==1){
        yl<-PData[(chpn[(i-1)]+1):chpn[i+1]]
        Time<-c((chpn[(i-1)]+1):chpn[i+1])
        modl<-lm(formula = yl ~ Time)
        rp[[i]]<-summary(modl)[[4]]
        coff[i]<-rp[[i]][2,1]
        see[i]<-rp[[i]][2,2]
        fit[(chpn[(i-1)]+1):chpn[i+1]]<-fitted(modl)
      }
      if(length((chpn[i]+1):chpn[i+1])!=1){
        yl<-PData[(chpn[i]+1):chpn[i+1]]
        Time<-c((chpn[i]+1):chpn[i+1])
        modl<-lm(formula = yl ~ Time)
        rp[[i]]<-summary(modl)[[4]]
        coff[i]<-rp[[i]][2,1]
        see[i]<-rp[[i]][2,2]
        fit[(chpn[i]+1):chpn[i+1]]<-fitted(modl)
        
      }
    }
  }
  list(rp,fit,coff,see) 
}

WIN<-function(PData,CF){
  w=2
  Z<-rep(0,0)
  k=1
  n=length(PData)
  for(i in w:(n-w)){
    Z[k]<-CF(PData,i-w,i+w)-(CF(PData,i-w,i)+CF(PData,i,i+w))
    k=k+1
  }
  qq<-quantile(Z,.9)
  L1<-which(Z>qq)
  L<-L1[which(Z[L1]-mean(Z[-L1])>quantile(Z[L1],.7))]
  L=L[L>5]
  nl<-length(L)
  if(nl==0){
    LL=L
    errL<-sqrt(sum((trend(PData,L)[[2]]-PData)^2))
  }
  if(nl!=0){
    err=rep(0,0)
    for(i in 1:nl) err[i]<-sqrt(sum((trend(PData,L[i])[[2]]-PData)^2))
    difl<-abs(L[-nl]-L[-1])
    nnl<-c(0,which(difl>=5),nl)
    Lnew<-rep(0,0)
    cc=0
    for(i in 1:(length(nnl)-1)){
      Lnew[i]<-cc+which.min(err[c((nnl[i]+1):nnl[(i+1)])])
      cc<-nnl[(i+1)] 
    } 
    LL<-L[Lnew]
    errL<-sqrt(sum((trend(PData,LL)[[2]]-PData)^2))
  }
  list(LL,Z,errL)
}

CL<-function(PData,a,b){
  n<-length(PData)
  Time<-(a+1):b
  yl<-PData[(a+1):b]
  modl<-lm(formula = yl ~ Time)
  res<-sqrt(sum((modl$residuals)^2))
  res
}

trendest<-function(Data,chp){
  N<-nrow(Data)
  Time<-1:N
  T<-cbind(rep(1,N),Time)
  errhat=mhat<-Data
  if(length(chp)==0){
    etahat<-t(solve(t(T)%*%(T))%*%(t(T)%*%(Data)))
    mhat<-etahat%*%t(T)
    errhat<-Data-t(mhat)
  }
  if(length(chp)!=0){
    etahat<-list()
    seg<-c(0,chp,N)
    for(i in 1:(length(chp)+1)){
      etahat[[i]]<-t(solve(t((T[((seg[i]+1):seg[(i+1)]),]))%*%(T[((seg[i]+1):seg[(i+1)]),]))%*%(t((T[((seg[i]+1):seg[(i+1)]),]))%*%(Data[((seg[i]+1):seg[(i+1)]),])))
      mhat[((seg[i]+1):seg[(i+1)]),]<-t(etahat[[i]]%*%t(T[((seg[i]+1):seg[(i+1)]),]))
    }
    errhat<-Data-mhat
  } 
  list(etahat,mhat,errhat)
}

yhatf<-function(Data,s){ 
  chpdata=NULL
  P<-ncol(Data)
  if(P==1) pint(paste("error:univatiate"))
  if(P!=1){
    PData<-Pro(Data,s)
    rp<-trend(PData,rep(0,0))
    if(rp[[1]][2,4]>0.05){
      yhat<-Data
    }
    if(rp[[1]][2,4]<=0.05){
      chpdata<-WIN(PData,CL)[[1]]
      yhat<-trendest(Data,chpdata)[[3]]
    }
    list(yhat,chpdata,rp,PData) 
  }
}


#=============================Simulation===================
N=200
R=20
par<-read_excel("par3.xlsx")
par<-as.matrix(par,ncol=4)
cc<-read_excel("chp.xlsx")
cc<-as.matrix(cc,ncol=2)
eta1<-par[1:20,]
eta2<-par[21:40,]
eta3<-par[41:60,]  
phi<-par[61:80,]
corr<-par[81:100,]
chp1<-cc[,1]
chp2<-t(apply(cc,1,sort))  
s=c(sqrt(.5),sqrt(.5))
yhat1=yhat2=yhat3=yhat4=yhat5=yhat6=yhat7=yhat8=NULL
x1=x2=x3=x4=x5=x6=x7=x8=NULL
chphat1=chphat2=chphat3=chphat4=chphat5=chphat6=chphat7=chphat8=list()
count1=count2=count3=count4=count5=count6=count7=count8=rep(0,R)
err1=err2=err3=err4=err5=err6=err7=err8=rep(0,0)
tt<-1:N
T<-cbind(rep(1,N),tt)
mm1=mm2=mm3=matrix(rep(0,2*N),nrow=N)
count=ERRR=rep(0,8)
chpp5=chpp6=CHPP5=CHPP6=rep(0,R)
chpp7=chpp8=CHPP7=CHPP8<-matrix(rep(0,R*2),ncol=2)
M=100
set.seed(100)
for (m in 1:M){
  for(j in 1:R){
    for(i in 1:N) mm1[i,]<-matrix(eta1[j,],nrow=2)%*%T[i,]
    for(i in 1:chp1[j]) mm2[i,]<-matrix(eta1[j,],nrow=2)%*%T[i,]
    for(i in (chp1[j]+1):N) mm2[i,]<-matrix(eta2[j,],nrow=2)%*%T[i,]
    for(i in 1:chp2[j,1]) mm3[i,]<-matrix(eta1[j,],nrow=2)%*%T[i,]
    for(i in (chp2[j,1]+1):chp2[j,2]) mm3[i,]<-matrix(eta2[j,],nrow=2)%*%T[i,]
    for(i in (chp2[j,2]+1):N) mm3[i,]<-matrix(eta3[j,],nrow=2)%*%T[i,]
    y1<- rmvnorm(N,rep(0,2),matrix(corr[j,],nrow=2)) 
    y2<-y1
    y2[1,]<-rmvnorm(1,rep(0,2),diag(c(.5,.5)))
    for(i in 2:N) y2[i,]<-matrix(phi[j,],nrow=2)%*%y2[(i-1),]+y1[i,]
    x1[[j]]<-y1
    x2[[j]]<-y2
    x3[[j]]<-mm1+y1
    x4[[j]]<-mm1+y2
    x5[[j]]<-mm2+y1
    x6[[j]]<-mm2+y2
    x7[[j]]<-mm3+y1
    x8[[j]]<-mm3+y2
    yhat1[[j]]<-yhatf(x1[[j]],s)  
    yhat2[[j]]<-yhatf(x2[[j]],s)      
    yhat3[[j]]<-yhatf(x3[[j]],s)      
    yhat4[[j]]<-yhatf(x4[[j]],s)      
    yhat5[[j]]<-yhatf(x5[[j]],s)      
    yhat6[[j]]<-yhatf(x6[[j]],s)      
    yhat7[[j]]<-yhatf(x7[[j]],s)      
    yhat8[[j]]<-yhatf(x8[[j]],s) 
    if(yhat1[[j]][[3]][[1]][2,4]>0.01) count1[j]=1
    if(yhat2[[j]][[3]][[1]][2,4]>0.01) count2[j]=1
    if(yhat3[[j]][[3]][[1]][2,4]<=0.01&length(yhat3[[j]][[2]])==0)count3[j]=1
    if(yhat4[[j]][[3]][[1]][2,4]<=0.01&length(yhat4[[j]][[2]])==0)count4[j]=1
    if(yhat5[[j]][[3]][[1]][2,4]<=0.01&length(yhat5[[j]][[2]])==1)count5[j]=1
    if(yhat6[[j]][[3]][[1]][2,4]<=0.01&length(yhat6[[j]][[2]])==1)count6[j]=1
    if(yhat7[[j]][[3]][[1]][2,4]<=0.01&length(yhat7[[j]][[2]])==2)count7[j]=1
    if(yhat8[[j]][[3]][[1]][2,4]<=0.01&length(yhat8[[j]][[2]])==2)count8[j]=1
    err1[j]<-sqrt(sum((y1-yhat1[[j]][[1]])^2))
    err2[j]<-sqrt(sum((y2-yhat2[[j]][[1]])^2))  
    err3[j]<-sqrt(sum((y1-yhat3[[j]][[1]])^2))
    err4[j]<-sqrt(sum((y2-yhat4[[j]][[1]])^2))
    err5[j]<-sqrt(sum((y1-yhat5[[j]][[1]])^2))
    err6[j]<-sqrt(sum((y2-yhat6[[j]][[1]])^2))
    err7[j]<-sqrt(sum((y1-yhat7[[j]][[1]])^2))
    err8[j]<-sqrt(sum((y2-yhat8[[j]][[1]])^2))
    if(length(yhat5[[j]][[2]])==1) chpp5[j]<-yhat5[[j]][[2]]
    if(length(yhat6[[j]][[2]])==1) chpp6[j]<-yhat6[[j]][[2]]
    if(length(yhat7[[j]][[2]])==2) chpp7[j,]<-yhat7[[j]][[2]]
    if(length(yhat8[[j]][[2]])==2) chpp8[j,]<-yhat8[[j]][[2]]
  }
  count<-count+c(sum(count1),sum(count2),sum(count3),sum(count4),sum(count5),sum(count6),sum(count7),sum(count8))
  ERRR<-ERRR+c(mean(err1),mean(err2),mean(err3),mean(err4),mean(err5),mean(err6),mean(err7),mean(err8))
  CHPP5<-CHPP5+chpp5
  CHPP6<-CHPP6+chpp6
  CHPP7<-CHPP7+chpp7
  CHPP8<-CHPP8+chpp8
}


#===========Result=================
count/(R*M)
ERRR/M
CHPP5/M
CHPP6/M
CHPP7/M
CHPP8/M
((CHPP5/M)-chp1)
((CHPP6/M)-chp1)
(t(apply(CHPP7/M,1,sort))-chp2)
(t(apply(CHPP8/M,1,sort))-chp2)

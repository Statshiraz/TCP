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

slop<-function(PData,T){
  tu=T-2
  if(tu<=2) tu=3
  alpha=0.05
  k=length(PData)
  aa<-c(tu+1,k-tu)
  d<-rep(0,0)
  for(a in aa[1]:aa[2]){
    yl1<-PData[(a-tu):(a-1)]
    nl1<-length(yl1)
    xl1<-1:nl1
    yl2<-PData[(a+1):(a+tu)]
    nl2<-length(yl2)
    xl2<-1:nl2
    modl1<-lm(formula = yl1 ~ xl1)
    modl2<-lm(formula = yl2 ~ xl2)
    bl1=as.numeric(modl1$coefficients[2])
    bl2=as.numeric(modl2$coefficients[2])               
    yhatl1<-fitted(modl1)
    yhatl2<-fitted(modl2)
    N=sum((xl1-((nl1+1)/2))^2)
    M=sum((xl2-((nl2+1)/2))^2)
    C<-N*M/(N+M)
    S2<-(1/C)*(1/(nl1+nl2-4))*(sum((yl1-yhatl1)^2)+sum((yl2-yhatl2)^2))
    tslope<-(bl1-bl2)/sqrt(S2)
    d[a]<-sum(abs(tslope)>=qt(1-(alpha/2),nl1+nl2-4))
  }
  dd<-rep(0,0)
  r=1
  for(i in aa[1]:(aa[2]-1)){
    if(d[i]==1&d[i+1]==0){
      dd[r]=i
      r=r+1
    }
  }
  L1<-dd-1
  L1=L1[which(L1>3&L1<(k-3))]
  chpnew=chpoinchek3(PData,L1)
  L<-round(chpnew[[1]])
  err<-chpnew[[2]]
  M1=rbind(c("number of change point",length(which(L!=0))),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
  M2=cbind(c("number of change point",0:(nrow(chpnew[[3]])-1)),rbind(c("Change point",rep("",ncol(chpnew[[3]])-1),"Error"),cbind(chpnew[[3]],round(chpnew[[4]],3))))
  list(L,M1,M2)  
}

chpoinchek3<-function(PData,chp){
  k<-length(chp)
  n<-length(PData)
  if(k==0) {
    hh<-c(0)
    err<-rep(0,0)
    err[1]<-sqrt(sum((trend(PData,rep(0,0))[[2]]-PData)^2))
    vv=NULL
    pp1<-1
    qq1<-as.matrix(hh[pp1])
    qq2<-hh[pp1]
    err1=err2<-err[pp1]
  }
  if(k==1) {
    hh<-c(chp,0)
    err<-rep(0,0)
    err[1]<-sqrt(sum((trend(PData,hh[1])[[2]]-PData)^2))
    err[2]<-sqrt(sum((trend(PData,rep(0,0))[[2]]-PData)^2))
    dif<-abs(err[2]-err[1])
    if(dif<(max(err)/5)){
      vv=NULL
      pp1<-2
      qq1<-as.matrix(hh[pp1])
      qq2<-hh[pp1]
      err1=err2<-err[pp1]
    }
    if(dif>=(max(err)/5)){
      vv=NULL
      pp1<-which.min(err)
      qq1<-as.matrix(hh[pp1])
      qq2<-hh[pp1]
      err1=err2<-err[pp1]
    }
  }
  if(k>=2) {
    chpn=c(chp[1],chp[2])
    ll=i=1
    while (i<(k-1)){
      if((chpn[(ll+1)]-chpn[ll])>=round(n*4/100)) {chpn<-c(chpn,chp[(i+2)]);i=i+1;ll=ll+1}
      if((chpn[(ll+1)]-chpn[ll])<=(round(n*4/100)-1)) {
        if(sqrt(sum((trend(PData,chpn[ll])[[2]]-PData)^2))>sqrt(sum((trend(PData,chpn[(ll+1)])[[2]]-PData)^2))) chpn<-c(chpn[-ll],chp[i+2]);i=i+1
        if(sqrt(sum((trend(PData,chpn[ll])[[2]]-PData)^2))<=sqrt(sum((trend(PData,chpn[(ll+1)])[[2]]-PData)^2))) chpn<-c(chpn[-(ll+1)],chp[i+2]);i=i+1
      }
    }
    chp=chpn
    k=length(chp)
    gg=list()
    for(i in 1:k) gg[[i]]<-combn(k,i)
    xx1=rep(0,0)
    for(i in 1:k) xx1<-c(xx1,ncol(gg[[i]]))
    xx1<-c(1,xx1)
    xx<-sum(xx1)
    hh=vv<-matrix(rep(0,(xx)*k),ncol=k)
    ss=1
    err<-rep(0,0)
    hh[ss,]=vv[ss,]=matrix(rep(0,k),ncol=k)
    err[ss]<-sqrt(sum((trend(PData,rep(0,0))[[2]]-PData)^2))
    ss=ss+1
    for(i in 1:k) {
      for(j in 1:ncol(gg[[i]])) {
        z<-c(chp[gg[[i]][,j]])
        vv[ss,c(1:length(z))]<-z
        zz<-z
        if(length(zz)==0) {
          hh[ss,]<-hh[ss,]
          err[ss]=Inf
        }
        if(length(zz)!=0){
          hh[ss,c(1:length(zz))]<-zz
          err[ss]<-sqrt(sum((trend(PData,zz)[[2]]-PData)^2))
        }
        ss<-ss+1
      }
    }
    cs<-cumsum(xx1)
    pp1<-TRUE
    for(i in 1:(k-1)) pp1<-c(pp1,err[((cs[i]+1):cs[(i+1)])]==min(err[((cs[i]+1):cs[(i+1)])]))
    pp1<-c(pp1,TRUE)
    pp1<-which(pp1)
    qq1<-round(hh[pp1,])
    err1<-err[pp1]
    DDD=NULL
    for(i in 1:(length(err1))) DDD[i]<-(err1[(i)]-min(err1))/(max(err1)-min(err1))
    cc=which(DDD<.005)[1]
    qq2=qq1[cc,which(qq1[cc,]!=0)]
    err2=err1[cc]
  }
  
  M1=rbind(c("number of change point",length(which(qq2!=0))),cbind(c("Change point",rep("",length(qq2)-1),"Error"),c((qq2),round(err2,3))))
  M2=cbind(c("number of change point",0:(nrow(qq1)-1)),rbind(c("Change point",rep("",ncol(qq1)-1),"Error"),cbind(qq1,round(err1,3))))
  list(qq2,err2,qq1,err1,M1,M2)
}


CL<-function(PData,a,b){
  n<-length(PData)
  Time<-(a+1):b
  yl<-PData[(a+1):b]
  modl<-lm(formula = yl ~ Time)
  res<-sqrt(sum((modl$residuals)^2))
  res
}


PELT<-function(PData,CF){
  n<-length(PData)
  B=0.5*log(n)
  Z<-rep(0,n+1)
  Z[1]<-(-B)
  L<-NULL
  L[[1]]<-empty.dump()
  X<-c(1)
  for(i in 2:(n+1)){
    CC<-rep(0,0)
    for(j in 1:length(X)) CC[j]<-Z[X[j]]+CF(PData,(X[j]-1),(i-1))+B
    that<-X[which.min(CC)]
    Z[i]<-Z[that]+CF(PData,(that-1),(i-1))+B
    L[[i]]<-c(as.vector(unlist(L[[that]])),that)
    CCC=rep(0,0)
    for(j in 1:length(X)) {
      CCC[j]<-Z[X[j]]+CF(PData,(X[j]-1),(i-1))
    }
    X<-c(X[which(CCC<=Z[i])],i)
    B=0.5*log(n)*length(L[[i]])
  }
  L1<-round(L[[n+1]][-1]-1)
  L1=L1[which(L1>3&L1<(n-3))]
  chpnew=chpoinchek3(PData,L1)
  L<-round(chpnew[[1]])
  err<-chpnew[[2]]
  M1=rbind(c("number of change point",length(which(L!=0))),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
  M2=cbind(c("number of change point",0:(nrow(chpnew[[3]])-1)),rbind(c("Change point",rep("",ncol(chpnew[[3]])-1),"Error"),cbind(chpnew[[3]],round(chpnew[[4]],3))))
  list(L,M1,M2)  
}

WIN<-function(PData,CF){
  n=length(PData)
  w=round(n*.003)
  if(w==1||w==0) w=2
  Z<-rep(0,0)
  k=1
  for(i in w:(n-w)){
    Z[k]<-CF(PData,i-w,i+w)-(CF(PData,i-w,i)+CF(PData,i,i+w))
    k=k+1
  }
  qq<-quantile(Z,.8)
  L1<-which(Z>qq)
  L<-L1[which(Z[L1]-mean(Z[-L1])>quantile(Z[L1],.7))]
  L<-L[which(L>5&L<n-5)]
  nl<-length(L)
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
  LL<-round(L[Lnew])
  chpnew=chpoinchek3(PData,LL)
  L<-round(chpnew[[1]])
  err<-chpnew[[2]]
  M1=rbind(c("number of change point",length(which(L!=0))),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
  M2=cbind(c("number of change point",0:(nrow(chpnew[[3]])-1)),rbind(c("Change point",rep("",ncol(chpnew[[3]])-1),"Error"),cbind(chpnew[[3]],round(chpnew[[4]],3))))
  list(L,M1,M2)  
}

Binseq<-function(PData,CF,stt){
  n=length(PData)
  st<-round(n*.03+stt)
  XX<-matrix(rep(0,st*st),nrow=st)
  L=rep(0,0)
  nch<-1
  while(nch<=(st-1)){
    k=length(L)
    t<-rep(0,0)
    t[1]<-0
    t[k+2]=n
    if(k>0) {LL<-sort(L);t[2:(k+1)]<-LL}
    G<-rep(0,k+1)
    for(i in 1:(k+1)){
      GG<-rep(0,0)
      for(j in (t[i]+1):(t[i+1]-1)) GG[j]<-CF(PData,t[i],j)-CF(PData,j,t[i+1])
      G[i]<-CF(PData,t[i],t[i+1])- min(GG,na.rm=TRUE)
    }
    dhat<-which.max(G)
    GGG<-rep(0,0)
    for(j in (t[dhat]+1):(t[dhat+1]-1)) GGG[j]<-CF(PData,t[dhat],j)+CF(PData,j,t[dhat+1])
    that<-which.min(GGG)
    L<-c(L,that)
    nch<-length(L)
    XX[nch,c(1:nch)]<-sort(L)
  }
  XX<-round(rbind(rep(0,nch),XX))
  chpnew=chpoinchek3(PData,XX[st+1,])
  if(nrow(chpnew[[3]])>=(stt+1)){
    L<-chpnew[[3]][(stt+1),which(round(chpnew[[3]][(stt+1),])!=0)]
    err<-chpnew[[4]][stt+1]
    M1=rbind(c("number of change point",length(which(L!=0))),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
    M2=cbind(c("number of change point",0:(stt)),rbind(c("Change point",rep("",stt-1),"Error"),cbind(chpnew[[3]][1:(stt+1),1:stt],round(chpnew[[4]][1:(stt+1)],3))))
  }
  if(nrow(chpnew[[3]])<(stt+1)){
    L<-chpnew[[3]][nrow(chpnew[[3]]),]
    err<-chpnew[[4]][nrow(chpnew[[3]])]
    M1=rbind(c("number of change point",length(L)),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
    M2=cbind(c("number of change point",0:(nrow(chpnew[[3]])-1)),rbind(c("Change point",rep("",ncol(chpnew[[3]])-1),"Error"),cbind(chpnew[[3]],round(chpnew[[4]],3))))
  }
  list(L,M1,M2)  
}

Botup<-function(PData,CF,stt){
  n=length(PData)
  st<-round(n*.03+stt)
  delta=round(n*.009)
  if(delta==1||delta==0) delta=2
  L=c(1:(round(n/delta)-1))*delta
  nch<-length(L)
  XX<-matrix(rep(0,(nch+1)*(nch+1)),nrow=nch+1)
  dd=1
  XX[dd,c(1:nch)]<-L
  while(nch>=(st+1)){
    k=length(L)
    t<-rep(0,0)
    t[1]<-0
    t[k+2]=n
    LL<-sort(L);t[2:(k+1)]<-LL
    G<-rep(0,(k))
    for(i in 2:(k+1)){
      GG<-rep(0,0)
      G[i-1]<-CF(PData,t[i-1],t[i+1])-(CF(PData,t[i-1],t[i])+CF(PData,t[i],t[i+1]))
    }
    dhat<-which.min(G)
    L<-t[-c(1,dhat+1,(k+2))]
    nch<-length(L)
    dd=dd+1
    XX[dd,c(1:nch)]<-L
    L=round(L)
  }
  chpnew=chpoinchek3(PData,L)
  if(nrow(chpnew[[3]])>=(stt+1)){
    L<-chpnew[[3]][(stt+1),which(round(chpnew[[3]][(stt+1),])!=0)]
    err<-chpnew[[4]][stt+1]
    M1=rbind(c("number of change point",length(L)),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
    M2=cbind(c("number of change point",0:(stt)),rbind(c("Change point",rep("",stt-1),"Error"),cbind(chpnew[[3]][1:(stt+1),1:stt],round(chpnew[[4]][1:(stt+1)],3))))
  }
  if(nrow(chpnew[[3]])<(stt+1)){
    L<-chpnew[[3]][nrow(chpnew[[3]]),]
    err<-chpnew[[4]][nrow(chpnew[[3]])]
    M1=rbind(c("number of change point",length(L)),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
    M2=cbind(c("number of change point",0:(nrow(chpnew[[3]])-1)),rbind(c("Change point",rep("",ncol(chpnew[[3]])-1),"Error"),cbind(chpnew[[3]],round(chpnew[[4]],3))))
  }
  list(L,M1,M2)
}

OPT<-function(PData,CF,stt){
  n=length(PData)
  nchp=round(n*.03+stt)
  nrg=nchp+1
  C=NULL
  for(i in 1:nrg) C[[i]]=matrix(Inf,nrow=n,ncol=n)
  for(i in 1:(n-1)){for(j in (i+1):n){C[[1]][i,j]=CF(PData,i,j)}}
  for(k in 2:nrg){
    for(i in 1:(n-k-1)){for(j in (i+k):n){
      CC=min(C[[k-1]][i,((i+k-1):(j-1))]+C[[1]][((i+k):j),j])
      C[[k]][i,j]<-CC
    }}
  }
  L=rep(0,nrg)
  L[nrg]=n
  k=nrg
  while(k>1){
    s=L[k]
    tstar<-which.min(C[[k-1]][1,(k-1):(s-1)]+C[[1]][(k:s),s])
    L[k-1]=tstar
    k=k-1
  }
  L=L[-nrg]
  L=L[which(L!=1)]
  LL=L+c(1:length(L))
  LL=LL[which(LL<n)]
  chpnew=chpoinchek3(PData,LL)
  if(nrow(chpnew[[3]])>=(stt+1)){
    L<-chpnew[[3]][(stt+1),which(round(chpnew[[3]][(stt+1),])!=0)]
    err<-chpnew[[4]][stt+1]
    M1=rbind(c("number of change point",length(L)),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
    M2=cbind(c("number of change point",0:(stt)),rbind(c("Change point",rep("",stt-1),"Error"),cbind(chpnew[[3]][1:(stt+1),1:stt],round(chpnew[[4]][1:(stt+1)],3))))
  }
  if(nrow(chpnew[[3]])<(stt+1)){
    L<-chpnew[[3]][nrow(chpnew[[3]]),]
    err<-chpnew[[4]][nrow(chpnew[[3]])]
    M1=rbind(c("number of change point",length(L)),cbind(c("Change point",rep("",length(L)-1),"Error"),c(L,round(err,3))))
    M2=cbind(c("number of change point",0:(nrow(chpnew[[3]])-1)),rbind(c("Change point",rep("",ncol(chpnew[[3]])-1),"Error"),cbind(chpnew[[3]],round(chpnew[[4]],3))))
  }
  list(L,M1,M2) 
}

plotpd<-function(PData,chp){
  chp<-sort(chp[(chp)!=0])
  theme_set(theme_bw())
  n<-length(PData)
  Time<-1:n
  fit<-trend(PData,chp)[[2]]
  d<-data.frame(Time,PData,fit)
  if(length(chp)==0){
    pl=ggplot(d) + geom_point(aes(Time,PData), color="blue")  + geom_line(aes(Time,fit),linetype = 2,size=1.3)+ylab("Product")+xlab("Time")
  }
  if(length(chp)!=0){
    fit0<-trend(PData,0)[[2]]
    pl=ggplot(d) + geom_point(aes(Time,PData), color="blue")+ geom_line(aes(Time,fit),linetype = 2,size=1.3)+geom_vline(xintercept = chp,color="red",size=1)+ylab("Product")+xlab("Time")
  }  
  pl
}

plot2Derr<-function(Data,chp,x1,x2){
  chp<-sort(chp[(chp)!=0])
  theme_set(theme_bw())
  N<-nrow(Data)
  T<-1:N
  d<-data.frame(T,Data)
  cn1<-row.names(describe(Data)[c(x1,x2),])
  if(length(chp)==0){
    p11<-ggplot(data=d) +geom_point(aes(T,Data[,x1]),colour="blue", size=1) +geom_line(aes(T,trend(Data[,x1],chp)[[2]]),colour="black",linetype = 2, size=1.3) +  ylab(cn1[1]) +  xlab("Time")
    p21<-ggplot(data=d) + geom_point(aes(T,Data[,x2]),colour="blue", size=1)+geom_line(aes(T,trend(Data[,x2],chp)[[2]]),colour="black",linetype = 2, size=1.3) +  ylab(cn1[2]) +  xlab("Time")
    grid.arrange(p11,p21,nrow=2,ncol=1)
  }
  if(length(chp)!=0){
    p11<-ggplot(data=d) +geom_point(aes(T,Data[,x1]),colour="blue", size=1) +geom_line(aes(T,trend(Data[,x1],chp)[[2]]),colour="black",linetype = 2, size=1.3)+geom_vline(xintercept = chp,color="red",size=1)+ ylab(cn1[1]) +  xlab("Time")
    p21<-ggplot(data=d) + geom_point(aes(T,Data[,x2]),colour="blue", size=1) + geom_line(aes(T,trend(Data[,x2],chp)[[2]]),colour="black",linetype = 2, size=1.3)+geom_vline(xintercept = chp,color="red",size=1)+ylab(cn1[2]) +  xlab("Time")
    grid.arrange(p11,p21,nrow=2,ncol=1)
  }
}

sigtren<-function(Data,chp){
  cc=length(chp)
  alpha=0.2
  k=length(Data)
  if(cc==0) ddd=NULL
  chp=c(1,chp,k)
  ddd=rep(0,0)
  if(cc!=0){
    for(i in 1:(cc)){
      yl1<-Data[(chp[i]):chp[i+1]]
      nl1<-length(yl1)
      xl1<-1:nl1
      yl2<-Data[(chp[i+1]):chp[i+2]]
      nl2<-length(yl2)
      xl2<-1:nl2
      modl1<-lm(formula = yl1 ~ xl1)
      modl2<-lm(formula = yl2 ~ xl2)
      bl1=as.numeric(modl1$coefficients[2])
      bl2=as.numeric(modl2$coefficients[2])               
      yhatl1<-fitted(modl1)
      yhatl2<-fitted(modl2)
      N=sum((xl1-((nl1+1)/2))^2)
      M=sum((xl2-((nl2+1)/2))^2)
      C<-N*M/(N+M)
      S2<-(1/C)*(1/(nl1+nl2-4))*(sum((yl1-yhatl1)^2)+sum((yl2-yhatl2)^2))
      tslope<-(bl1-bl2)/sqrt(S2)
      z=(bl1-bl2)/sqrt(summary(modl1)[[4]][2,2]^2+summary(modl2)[[4]][2,2]^2)
      ddd[i]<-sum(max(abs(z),abs(tslope))>=qnorm(1-(alpha/2)))
    }
  }
  ddd
}

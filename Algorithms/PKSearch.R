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

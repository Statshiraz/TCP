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
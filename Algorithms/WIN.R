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
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

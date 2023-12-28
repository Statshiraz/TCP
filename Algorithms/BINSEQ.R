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
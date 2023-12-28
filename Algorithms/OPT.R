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

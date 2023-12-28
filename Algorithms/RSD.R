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

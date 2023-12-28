rm(list = ls(all=TRUE))

WD.PATH = paste(getwd(), "/Github", sep = "")

source(paste(WD.PATH, '/function.r', sep = ""))

library(ggplot2)
library(ggpubr)

D<-read.delim("D.txt")
Data=as.matrix(D)
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
d<-data.frame(T,Data)
cn1<-row.names(describe(Data)[c(1,2,3,4),])
p1<-ggplot(data=d) +  geom_point(aes(T,Data[,1]), color="blue", size=1)+ geom_line(aes(T,Data[,1]), color="blue") +  ylab(cn1[1])+xlab("Time")
p2<-ggplot(data=d) +  geom_point(aes(T,Data[,2]), color="blue", size=1) + geom_line(aes(T,Data[,2]), color="blue")+  ylab(cn1[2])+xlab("Time")
p3<-ggplot(data=d) +  geom_point(aes(T,Data[,3]), color="blue", size=1) + geom_line(aes(T,Data[,3]), color="blue")+  ylab(cn1[3])+xlab("Time")

X11()
ggarrange(p1, p2,p3,
               labels = c("(a)", "(b)", "(c)"),
               ncol = 1, nrow = 3,hjust=-0.1)


s=sqrt(rep(1/ncol(Data),ncol(Data)))
PData<-Propelt(Data,s)
tr0<-trend(PData,0)
CHPR<-PELT(PData,CL)
trf<-trend(PData,CHPR[[1]])
trest<-trend(Data,CHPR[[1]])

chp=CHPR[[1]]
chp<-sort(chp[(chp)!=0])
chp<-sort(chp[(chp)!=0])
theme_set(theme_bw())
n<-length(PData)
Time<-1:n
fit<-trend(PData,chp)[[2]]
d<-data.frame(Time,PData,fit)
if(length(chp)==0){
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=1)  + geom_line(aes(Time,fit),linetype = 2,size=1.4)+ylab("Product")+xlab("Time")
}
if(length(chp)!=0){
  fit0<-trend(PData,0)[[2]]
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=0.7)+ geom_line(aes(Time,fit),linetype = 1,size=1)+geom_vline(xintercept = chp,color="black",size=0.8)+ylab("Product")+xlab("Time")+scale_x_continuous( breaks=sort(c(chp,0,50,150,250)),labels=c(0,50,expression(t[1]^s[1]),150,expression(t[2]^s[1]),expression(t[3]^s[1]),250))
}  
X11()
pl+theme(axis.text = element_text(size = 13)) 


s1=sqrt(c(9/10,0,1/10))
PDatas1<-Propelt(Data,s1)
tr0s1<-trend(PDatas1,0)
CHPRs1<-PELT(PDatas1,CL)
chps1=CHPRs1[[1]]
chps1<-sort(chps1[(chps1)!=0])
Time<-1:n
fits1<-trend(PDatas1,chps1)[[2]]
ds1<-data.frame(Time,PDatas1,fits1,PData,fit)
fit0s1<-trend(PDatas1,0)[[2]]
trfs1<-trend(PDatas1,chps1)
trests1<-trend(Data,chps1)
pl=ggplot(ds1) + geom_point(aes(Time,PData), color="blue",size=0.7)+ geom_line(aes(Time,fit),linetype = 1,color="#E1095B",size=1)+geom_vline(xintercept = chp,color="#E1095B",size=0.8)+ylab("Product")+xlab("Time")
pl=pl+ geom_point(aes(Time,PDatas1), color="black",size=0.7,shape = 2)+ geom_line(aes(Time,fits1),linetype = 1,color="#26DC5A",size=1)+geom_vline(xintercept = chps1,color="#26DC5A",size=0.8)+ylab("Product")+xlab("Time")
X11()
pl+scale_x_continuous( breaks=sort(c(chps1,chp,0,50,150,250)),labels=c(0,50,expression(t[1]^s[1]),expression(t[1]^s[2]),150,expression(t[2]^s[1]),expression(t[3]^s[1]),250))+theme(axis.text = element_text(size = 13)) 

CHPRdata=list()
for(i in 1:ncol(Data)) CHPRdata[[i]]=PELT(as.matrix(D)[,i],CL)[[1]]
chpd=CHPRdata
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
chp<-sort(chp[(chp)!=0])
chpd1=sort(chpd[[1]][(chpd[[1]])!=0])
chpd2=sort(chpd[[2]][(chpd[[2]])!=0])
chpd3=sort(chpd[[3]][(chpd[[3]])!=0])
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
mhats1<-trend(Data,chps1)[[2]]
mhat1<-trend(Data[,1],chpd1)[[2]]
mhat2<-trend(Data[,2],chpd2)[[2]]
mhat3<-trend(Data[,3],chpd3)[[2]]
d<-data.frame(T,Data)
cn1<-row.names(describe(Data)[c(1,2,3),])
dd1=sigtren(Data[,1],chp)
dd2=sigtren(Data[,2],chp)
dd3=sigtren(Data[,3],chp)
p11<-ggplot(data=d) +geom_point(aes(T,Data[,1]),colour="blue", size=0.7) +geom_line(aes(T,fits1-20),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chps1,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd1,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat1),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[1]) +  xlab("Time")
p11<-p11+scale_x_continuous( breaks=sort(c(chps1,0,50,100,200,250)),labels=c(0,50,100,expression(t[1]==t[1]^s[2]),200,250))+theme(axis.text = element_text(size = 13)) 
p21<-ggplot(data=d) +geom_point(aes(T,Data[,2]),colour="blue", size=0.7) +geom_line(aes(T,fits1-20),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chps1,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd2,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat2),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[2]) +  xlab("Time")
p21<-p21+scale_x_continuous( breaks=sort(c(chps1,chpd2,0,50,200,150,250)),labels=c(0,50,expression(t[1]),expression(t[2]),expression(t[1]^s[2]),expression(t[3]),150,200,expression(t[4]),250))+theme(axis.text = element_text(size = 13)) 
p31<-ggplot(data=d) +geom_point(aes(T,Data[,3]),colour="blue", size=0.7) +geom_line(aes(T,fits1-20),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chps1,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd3,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat3),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[3]) +  xlab("Time")
p31<-p31+scale_x_continuous( breaks=sort(c(chps1,0,50,100,200,250)),labels=c(0,50,100,expression(t[1]==t[1]^s[2]),200,250))+theme(axis.text = element_text(size = 13)) 

X11()
ggarrange(p11, p21,p31,
               labels = c("(a)", "(b)", "(c)"),
               ncol = 1, nrow = 3,hjust=-0.1)

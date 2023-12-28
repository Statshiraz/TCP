rm(list = ls(all=TRUE))

WD.PATH = paste(getwd(),"/Github", sep = "")

source(paste(WD.PATH, '/function.r', sep = ""))

library(ggplot2)
library(ggpubr)

C<-read.delim("C.txt")
Data=as.matrix(C)
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
d<-data.frame(T,Data)
cn1<-row.names(describe(Data)[c(1,2,3,4),])
p1<-ggplot(data=d) +  geom_point(aes(T,Data[,1]), color="blue", size=1)+ geom_line(aes(T,Data[,1]), color="blue") +  ylab(cn1[1])+xlab("Time")
p2<-ggplot(data=d) +  geom_point(aes(T,Data[,2]), color="blue", size=1) + geom_line(aes(T,Data[,2]), color="blue")+  ylab(cn1[2])+xlab("Time")
p3<-ggplot(data=d) +  geom_point(aes(T,Data[,3]), color="blue", size=1) + geom_line(aes(T,Data[,3]), color="blue")+  ylab(cn1[3])+xlab("Time")
p4<-ggplot(data=d) +  geom_point(aes(T,Data[,4]), color="blue", size=1) + geom_line(aes(T,Data[,4]), color="blue")+  ylab(cn1[4])+xlab("Time")

X11()
ggarrange(p1, p2,p3,p4,
               labels = c("(a)", "(b)", "(c)","(d)"),
               ncol = 1, nrow = 4,hjust=-0.1)



rescale1 <- function(x) rescale(x, to = c(0, 1), range = NULL, verbose = TRUE)
Data=(apply(C, 2,rescale1))
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
d<-data.frame(T,Data)
cn1<-row.names(describe(Data)[c(1,2,3,4),])
p1<-ggplot(data=d) +  geom_point(aes(T,Data[,1]), color="blue", size=1)+ geom_line(aes(T,Data[,1]), color="blue") +  ylab(cn1[1])+xlab("Time")
p2<-ggplot(data=d) +  geom_point(aes(T,Data[,2]), color="blue", size=1) + geom_line(aes(T,Data[,2]), color="blue")+  ylab(cn1[2])+xlab("Time")
p3<-ggplot(data=d) +  geom_point(aes(T,Data[,3]), color="blue", size=1) + geom_line(aes(T,Data[,3]), color="blue")+  ylab(cn1[3])+xlab("Time")
p4<-ggplot(data=d) +  geom_point(aes(T,Data[,4]), color="blue", size=1) + geom_line(aes(T,Data[,4]), color="blue")+  ylab(cn1[4])+xlab("Time")

X11()
ggarrange(p1, p2,p3,p4,
               labels = c("(a)", "(b)", "(c)","(d)"),
               ncol = 1, nrow = 4,hjust=-0.1)

s=sqrt(rep(1/ncol(Data),ncol(Data)))
PData<-Pro(Data,s)
tr0<-trend(PData,0)
CHPR<-slop(PData,round(nrow(Data)*.12))
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
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=0.7)  + geom_line(aes(Time,fit),linetype = 1,size=1)+ylab("Product")+xlab("Time")
}
if(length(chp)!=0){
  fit0<-trend(PData,0)[[2]]
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=0.7)+ geom_line(aes(Time,fit),linetype = 1,size=1)+geom_vline(xintercept = chp,color="black",size=1)+ylab("Product")+xlab("Time")+scale_x_continuous( breaks=sort(c(chp,0,50,550,650)),labels=c(0,50,expression(t[1]^s[1]),expression(t[2]^s[1]),expression(t[3]^s[1]),expression(t[4]^s[1]),expression(t[5]^s[1]),550,expression(t[6]^s[1]),650))
}  

X11()
pl+theme(axis.text = element_text(size = 13))


CHPRdata=list()
for(i in 1:ncol(Data)) CHPRdata[[i]]=slop(Data[,i],round(nrow(Data)*.12))[[1]]
chpd=CHPRdata
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
chp<-sort(chp[(chp)!=0])
chpd1=sort(chpd[[1]][(chpd[[1]])!=0])
chpd2=sort(chpd[[2]][(chpd[[2]])!=0])
chpd3=sort(chpd[[3]][(chpd[[3]])!=0])
chpd4=sort(chpd[[4]][(chpd[[4]])!=0])
theme_set(theme_bw())
N<-nrow(Data)
T<-1:N
mhat<-trend(Data,chp)[[2]]
mhat1<-trend(Data[,1],chpd1)[[2]]
mhat2<-trend(Data[,2],chpd2)[[2]]
mhat3<-trend(Data[,3],chpd3)[[2]]
mhat4<-trend(Data[,4],chpd4)[[2]]
d<-data.frame(T,Data)
cn1<-row.names(describe(Data)[c(1,2,3,4),])
dd1=sigtren(Data[,1],chp)
dd2=sigtren(Data[,2],chp)
dd3=sigtren(Data[,3],chp)
dd4=sigtren(Data[,4],chp)

p11<-ggplot(data=d) +geom_point(aes(T,Data[,1]),colour="blue", size=0.7) +geom_line(aes(T,fit),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chp,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd1,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat1),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[1]) +  xlab("Time")
p11<-p11+scale_x_continuous( breaks=sort(c(chp[-c(2,6)],chp[2]-5,chp[6]+5,chpd1[-c(1,4)],0,50,650)),labels=c(0,50,expression(t[1]^s[1]==t[1]),expression(t[2]^s[1]),expression(t[2]),expression(t[3]),expression(t[3]^s[1]),expression(t[4]^s[1]==t[4]),expression(t[5]^s[1]),expression(t[5]),expression(t[6]),expression(t[6]^s[1]),expression(t[7]),650))+theme(axis.text = element_text(size = 13)) 
p21<-ggplot(data=d) +geom_point(aes(T,Data[,2]),colour="blue", size=0.7) +geom_line(aes(T,fit),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chp,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd2,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat2),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[2]) +  xlab("Time")
p21<-p21+scale_x_continuous( breaks=sort(c(chp,chpd2[-c(1,4,5,6,8)],0,50,650)),labels=c(0,50,expression(t[1]^s[1]==t[1]),expression(t[2]^s[1]),expression(t[2]),expression(t[3]),expression(t[3]^s[1]==t[4]),expression(t[4]^s[1]==t[5]),expression(t[5]^s[1]==t[6]),expression(t[7]),expression(t[6]^s[1]==t[8]),650))+theme(axis.text = element_text(size = 13)) 
p31<-ggplot(data=d) +geom_point(aes(T,Data[,3]),colour="blue", size=0.7) +geom_line(aes(T,fit),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chp,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd3,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat3),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[3]) +  xlab("Time")
p31<-p31+scale_x_continuous( breaks=sort(c(chp,chpd3[-c(1,2,4,5,6,7)],chpd3[2]+5,0,50,650)),labels=c(0,50,expression(t[1]^s[1]==t[1]),expression(t[2]^s[1]),expression(t[2]),expression(t[3]),expression(t[3]^s[1]==t[4]),expression(t[4]^s[1]==t[5]),expression(t[5]^s[1]==t[6]),expression(t[6]^s[1]==t[7]),650))+theme(axis.text = element_text(size = 13)) 
p41<-ggplot(data=d) +geom_point(aes(T,Data[,4]),colour="blue", size=0.7) +geom_line(aes(T,fit),colour="#E1095B",linetype = 1, size=1)+geom_vline(xintercept = chp,color="#E1095B",size=0.8) +geom_vline(xintercept = chpd4,color="#26DC5A",size=0.8)+geom_line(aes(T,mhat4),colour="#26DC5A",linetype = 1, size=1) +  ylab(cn1[4]) +  xlab("Time")
p41<-p41+scale_x_continuous( breaks=sort(c(chp,chpd4[-c(1,2,3,6)],0,50,650)),labels=c(0,50,expression(t[1]^s[1]==t[1]),expression(t[2]^s[1]==t[2]),expression(t[3]^s[1]==t[3]),expression(t[4]^s[1]),expression(t[4]),expression(t[5]^s[1]),expression(t[5]),expression(t[6]^s[1]==t[6]),expression(t[7]),650))+theme(axis.text = element_text(size = 13)) 

X11()
ggarrange(p11, p21,p31,p41,
               labels = c("(a)", "(b)", "(c)","(d)"),
               ncol = 1, nrow = 4,hjust=-0.1)




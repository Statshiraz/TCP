rm(list = ls(all=TRUE))

WD.PATH = paste(getwd(),"/Github", sep = "")

source(paste(WD.PATH, '/function.r', sep = ""))

library(ggplot2)
library(ggpubr)

IRAN<-read.delim("Data_GAG_Iran.txt")
Data=as.matrix(IRAN)
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



rescale1 <- function(x) rescale(x, to = c(0, 1), range = NULL, verbose = TRUE)
Data=(apply(IRAN, 2,rescale1))
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
PData<-Pro(Data,s)
tr0<-trend(PData,0)
CHPR<-OPT(PData,CL,4)
trf<-trend(PData,CHPR[[1]])
trest<-trend(Data,CHPR[[1]])


X11()
ggplot(d) + geom_point(aes(T,PData), color="blue",size=1)+xlab("Time")



chp=CHPR[[1]]
chp<-sort(chp[(chp)!=0])
chp<-sort(chp[(chp)!=0])
theme_set(theme_bw())
n<-length(PData)
Time<-1:n
fit<-trend(PData,chp)[[2]]
d<-data.frame(Time,PData,fit)
if(length(chp)==0){
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=1)  + geom_line(aes(Time,fit),linetype = 1,size=1)+ylab("Product")+xlab("Time")
}
if(length(chp)!=0){
  fit0<-trend(PData,0)[[2]]
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=1)+ geom_line(aes(Time,fit),linetype = 1,size=1)+geom_vline(xintercept = chp,color="black",size=0.8)+ylab("Product")+xlab("Time")+scale_x_continuous( breaks=sort(c(chp,0,10,15,25,30)),labels=c(0,expression(t[1]^s),10,expression(t[2]^s),15,expression(t[3]^s),25,expression(t[4]^s),30))
}  

X11()
pl+theme(axis.text = element_text(size = 13)) 


trf[[3]]


WD.PATH = paste(getwd(),"/Github", sep = "")

source(paste(WD.PATH, '/function.r', sep = ""))


# Generate mirrored univariate time-series with two TCPs
n <- 100  # Number of observations
time <- 1:n
Z=rnorm(100,0,2)
series1 <- c(0.2*1:30,-0.3*31:60,0.7*61:100)+Z
series2 <- -c(0.2*1:30,-0.3*31:60,0.7*61:100)+Z

# Plot the original and combined time-series
X11()
plot(time, series1, type = "l", col = "red", ylim=c(-60,60) ,xlab = "Time", ylab = "Value", main = "Combined Time-Series")
lines(time, series2, col = "blue")


# The PELT algorithm with a negative weight for series2 to identify TCPs
T<-1:n
Data=cbind(series1,series2)
corr(Data)
s=sqrt(c(0.5,0.5))*c(1,-1)
PData<-Propelt(Data,s)
tr0<-trend(PData,0)
CHPR<-PELT(PData,CL)
trf<-trend(PData,CHPR[[1]])
chp=CHPR[[1]]
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
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=0.7)+ geom_vline(xintercept = chp,color="black",size=1)+ylab("Product")+xlab("Time")
}  

X11()
pl+theme(axis.text = element_text(size = 13))


# The PELT algorithm with a positive weight for series2 to identify TCPs
T<-1:n
Data=cbind(series1,series2)
corr(Data)
s=sqrt(c(0.5,0.5))
PData<-Propelt(Data,s)
tr0<-trend(PData,0)
CHPR<-PELT(PData,CL)
trf<-trend(PData,CHPR[[1]])
chp=CHPR[[1]]
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
  pl=ggplot(d) + geom_point(aes(Time,PData), color="blue",size=0.7)+ geom_vline(xintercept = chp,color="black",size=1)+ylab("Product")+xlab("Time")
}  

X11()
pl+theme(axis.text = element_text(size = 13))



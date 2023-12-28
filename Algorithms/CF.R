CL<-function(PData,a,b){
  n<-length(PData)
  Time<-(a+1):b
  yl<-PData[(a+1):b]
  modl<-lm(formula = yl ~ Time)
  res<-sqrt(sum((modl$residuals)^2))
  res
}
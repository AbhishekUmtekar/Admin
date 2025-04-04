plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
boxplot(AirPassengers~cycle(AirPassengers))
acf(AirPassengers)
acf(log(AirPassengers))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))
d1<-ts(AirPassengers,start = c(1949, 1),end = c(1959, 12), frequency = 12)
mdl<-arima(log(d1), order = c(0,1,1), seasonal = list(order = c(0,1,1),period=12))
pred<-predict(mdl,n.ahead=120)
pr1<-exp(pred$pred)
pr1
ts.plot(d1,pr1,lty=c(1,3))
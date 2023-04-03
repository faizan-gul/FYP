#ETHBTC
myData = read.csv("C:\\Users\\faiza\\Downloads\\MS4090 - MS FYP\\ETHBTC.csv", 
                  header = TRUE)
plot(myData$close, cex=0.0001,
     main="ETHBTC Close Rates",
     xlim=c(1,39667),xaxt="n")
axis(1,at=c(1:39667),time_index[1:39667])
plot(myData$Logrets,cex=0.000001, type='l')
hist(myData$Logrets,xlim=range(-0.01,0.01),breaks=100,
     main="ETHBTC Logreturns Histogram")
gr1=xts(myData$close[1:39667], order.by=time_index[1:39667])
plot(gr1, xlab=seq(time_index[1:39667],10000))

time_index2 = seq(from = as.POSIXct(myData$date[1], tryFormats = c("%d/%m/%Y %H:%M")), 
                 to = as.POSIXct(myData$date[39668], tryFormats = c("%d/%m/%Y %H:%M")), 
                 length.out=39668)
fixed=xts(myData$Logrets[1:39668],order.by=time_index2)
df1=data.frame(c(time_index[1:39668]),c(myData$Logrets[1:39668]))
hist(df1,main="ETHBTC Logreturns Histogram")

timeseq=seq(from=2018.369,to=2022.893,length.out=39678)
df1=data.frame(c(timeseq),c(myData$Logrets))
timeser=ts(myData$Logrets, start=as.Date("2019-05-15 06:00:00"),
           end=as.Date("2022-11-23 00:00:00"),
           frequency=31)
plot(fixed)

library(rugarch,colnames(myData$close)[c(10000,20000,30000,39667)] <- c("jan","feb","mar","apr"))

garch_spec=ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(3,3)),
                     mean.model=list(armaOrder=c(0,0)))
?ugarchspec
fit_garch = ugarchfit(spec = garch_spec, data = na.omit(fixed))
fit_garch
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch, which = i)
}
#Goodness of fit(MSE) for mean prediction
e = residuals(fit_garch)
mean(e^2)
#Goodness of fit(MSE) for variance prediction
d = e^2 - sigma(fit_garch)^2
mean(d^2)
#goodness of fit for ditribution
likelihood(fit_garch)

coef(fit_garch)

#BTCUSD
myData2 = read.csv("C:\\Users\\faiza\\Downloads\\MS4090 - MS FYP\\BTCUSD.csv", 
                   header = TRUE)
plot(myData2$close,cex=0.001)
plot(myData2$Logrets,cex=0.001,type='l')
hist(myData2$Logrets,xlim=range(-0.01,0.01),breaks=500,
     main="BTCUSD Logreturns Histogram")

df1=data.frame(c(time_index[1:39667]),c(myData2$Logrets[1:39667]))
plot(df1,main="BTCUSD Logreturns",cex=0.001,type='l')

time_index2 = seq(from = as.POSIXct(myData2$date[1], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData2$date[39689], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by = "hour")
fixed=xts(myData2$Logrets[1:39690],order.by=time_index2)

garch_spec2 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
fit_garch2 = ugarchfit(spec = garch_spec2, data = na.omit(fixed))
fit_garch2
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch2, which = i)
}

?ugarchspec

#USDEUR
par(mfrow = c(1, 1))
myData3 = read.csv("C:\\Users\\faiza\\Downloads\\MS4090 - MS FYP\\USDEUR.csv", 
                   header = TRUE)
plot(myData3$Close, type='l')
plot(myData3$Logrets, type='l',cex=0.001)
hist(myData3$Logrets, breaks=100, xlim=range(-0.01,0.01),
     main="USDEUR Logreturns Histogram")

time_index2 = seq(from = as.POSIXct(myData$date[1], tryFormats = c("%d/%m/%Y")), 
                  to = as.POSIXct(myData$date[39667], tryFormats = c("%d/%m/%Y")), 
                  by = "day")
timeseq=seq(from=2018.369,to=2022.893,length.out=1184)
df1=data.frame(c(timeseq),c(myData3$Logrets))
plot(df1,main="USDEUR Logreturns",cex=0.001,type='l')

time_index2 = seq(from = as.POSIXct(myData3$Date[2], tryFormats = c("%d/%m/%Y")), 
                  to = as.POSIXct(myData3$Date[1184], tryFormats = c("%d/%m/%Y")), 
                  by = "day")
time_index2=time_index2[!weekdays(time_index2) %in% c('Saturday','Sunday')]
fixed=xts(myData3$Logrets[2:1183],order.by=time_index2)

garch_spec3 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0)))
fit_garch3 = ugarchfit(spec = garch_spec3, data = na.omit(fixed))
fit_garch3
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch3, which = i)
}

#VIX
myData4 = read.csv("C:\\Users\\faiza\\Downloads\\MS4090 - MS FYP\\VIX.csv", 
                   header = TRUE)
plot(myData4$Close, type='l')
plot(myData4$Logrets,type='l', cex=0.001)
hist(myData4$Logrets, xlim=range(-0.12,0.15), breaks=50,
     main="VIX Logreturns Histogram")

timeseq=seq(from=2018.369,to=2022.893,length.out=1143)
df1=data.frame(c(timeseq),c(myData4$Logrets))
plot(df1,main="VIX Logreturns",cex=0.001,type='l')


plot(myData$Logrets,cex=0.000001, type='l')
lines(myData4$Logrets, col='blue', cex=0.00000001)

#Fitting VIX
x=myData4$Logrets
v2=rep(1,length(x))
v2[c(FALSE, FALSE, FALSE, FALSE, TRUE)] <- 3
y=rep(x, v2)
write.csv(y, file ="VIX24hr.csv", row.names=FALSE)
y2 = read.csv("C:\\Users\\faiza\\Documents\\VIX24hr.csv", header = TRUE)
y3=rep(y2$x, each=24)
write.csv(y3, file ="VIX24hrs.csv", row.names=FALSE)
myData5 = read.csv("C:\\Users\\faiza\\Documents\\VIX24hrs.csv", header = TRUE)

#Make USDEUR hourly also
x=myData3$Logrets[2:1185]
v2=rep(1,length(x))
v2[c(FALSE, FALSE, FALSE, FALSE, TRUE)] <- 3
y=rep(x, v2)
write.csv(y, file ="USDEUR24hr.csv", row.names=FALSE)
y2 = read.csv("C:\\Users\\faiza\\Documents\\USDEUR24hr.csv", header = TRUE)
y3=rep(y2$x, each=24)
write.csv(y3, file ="USDEUR24hrs.csv", row.names=FALSE)
myData6 = read.csv("C:\\Users\\faiza\\Documents\\USDEUR24hrs.csv", header = TRUE)


#Regress VIX with USDEUR
x2=lm(abs(myData6$x[0:38322])~abs(myData5$x[0:38322]))
plot(abs(myData5$x[0:38322]),abs(myData6$x[0:38322]), xlab="VIX Logrets", 
     ylab="USDEUR Logrets")
abline(x2, col="red", lwd=3)
plot.new()

cor(myData5$x[25000:38322],myData6$x[25000:38322])
?cor
?abline

#Now with ETHBTC
x2=lm(abs(myData$Logrets[2:38322])~abs(myData5$x[2:38322]))
plot(abs(myData5$x[2:38322]), abs(myData$Logrets[2:38322]),  xlab="VIX Logrets", 
     ylab="ETHBTC Logrets")
abline(x2, col="red", lwd=3)
cor(abs(myData5$x[2:38322]),abs(myData$Logrets[2:38322]))

#VIX into ETHBTC as exogeneous covariate
x=matrix(myData5$x[2:38322])
y=na.omit(myData5$x[2:38322])
min(y)

time_index2 = seq(from = as.POSIXct(myData3$Date[2], tryFormats = c("%d/%m/%Y")), 
                  to = as.POSIXct(myData3$Date[1184], tryFormats = c("%d/%m/%Y")), 
                  by = "day")
time_index2=time_index2[!weekdays(time_index2) %in% c('Saturday','Sunday')]
fixed=xts(myData3$Logrets[2:1183],order.by=time_index2)

garch_specVIX=ugarchspec(variance.model=list(model="sGARCH", external.regressors=x, 
                      garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
y=matrix(myData$Logrets[2:38322])
fit_garchVIX = ugarchfit(spec = garch_specVIX, data = na.omit(fixed)[2:38322])
fit_garchVIX
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch, which = i)
}
?ugarchspec

#plot VIX Logrets over ETHBTC, BTCUSD, USDEUR
plot.new()
par(mfrow=c(1,1))
plot(df1, cex=0.000001, type='l', col="black",
     main="VIX Logreturns")
lines(df2,cex=0.0001, type='l', col="red")

time_index2 = seq(from = as.POSIXct(myData2$date[1], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData2$date[38322], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by = "hour")
fixed=xts(myData5$x[1:38322],order.by=time_index2)
df1=data.frame(c(time_index[1:38322]),c(myData5$x[1:38322]))
df2=data.frame(c(time_index[1:38322]),4*c(myData$Logrets[1:38322]))



#ETHBTC split in 3 timeframes
#first 4 months - 2-3000
par(mfrow = c(1, 1))
plot(myData$close[3:3000], cex=0.0001)
plot(myData$Logrets[3:3000],cex=0.000001, type='l')
hist(myData$Logrets[3:3000],xlim=range(-0.01,0.01),breaks=100)

time_index2 = seq(from = as.POSIXct(myData$date[3], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData$date[3000], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by = "hour")
fixed=xts(myData$Logrets[3:3000],order.by=time_index2)

garch_spec=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                      mean.model=list(armaOrder=c(0,0)))
fit_garch = ugarchfit(spec = garch_spec, data = na.omit(fixed))
fit_garch
?ugarchspec

par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch, which = i)
}
plot.new()

#next 2 years 6 months - 3000-25000
plot(myData$close[3001:25000], cex=0.0001)
plot(myData$Logrets[3001:25000],cex=0.000001, type='l')
hist(myData$Logrets[3001:25000],xlim=range(-0.01,0.01),breaks=100)

time_index2 = seq(from = as.POSIXct(myData$date[3001], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData$date[24999], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by = "hour")
fixed=xts(myData$Logrets[3001:25000],order.by=time_index2)

garch_spec=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                      mean.model=list(armaOrder=c(0,0)))
fit_garch = ugarchfit(spec = garch_spec, data = na.omit(fixed))
fit_garch
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch, which = i)
}

#next 1 year 8 months - 25000-39667
plot(myData$close[25001:39667], cex=0.0001)
plot(myData$Logrets[25001:39667],cex=0.000001, type='l')
hist(myData$Logrets[25001:39667],xlim=range(-0.01,0.01),breaks=100)

time_index2 = seq(from = as.POSIXct(myData$date[25001], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData$date[39667], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by = "hour")
fixed=xts(myData$Logrets[25001:39667],order.by=time_index2)

garch_spec=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                      mean.model=list(armaOrder=c(0,0)))
fit_garch = ugarchfit(spec = garch_spec, data = na.omit(fixed))
fit_garch
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(fit_garch, which = i)
}

#Forecasting
garch_spec=ugarchspec(variance.model=list(model="sGARCH",
          garchOrder=c(1,1)), fixed.pars=list(omega=0),
          mean.model=list(armaOrder=c(0,0)))
?ugarchspec

require(xts)
time_index = seq(from = as.POSIXct(myData$date[1], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData$date[39667], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by = "hour")
traff = xts(myData$Logrets[1:39668], order.by = time_index)
ff=traff[2:39668]
tail(ff[,1])
tail(sigma(fit_garch),2)


fit_garch = ugarchfit(spec = garch_spec, data = na.omit(ff[,1]))
fit_garch2 = ugarchfit(spec = garch_spec, data = na.omit(fixed[0:39667]))
fit_garch

set.seed(43)
fb=ugarchboot(fit_garch, method = "Partial", 
              n.ahead = 2329, n.bootpred = 2)
plot(fb,which="all", type='l')



f1=ugarchforecast(fit_garch, n.ahead=2329)
f2=ugarchforecast(fit_garch2,
                 n.ahead=2329)
tail(f1@forecast$seriesFor)
plot(f1)
plot(f2)
plot(f1@forecast$seriesFor)



f3=(f1@forecast$seriesFor+f2@forecast$seriesFor)/2
plot(f3,type="l")

head(fitted(f))
plot((f1+f2)/2, xlab="c(Nov, Dec, Jan, Feb")


tail(fitted(f))

#ug_f = f@forecast$sigmaFor
#plot(ug_f, type = 'l') 

fb=ugarchboot(fit_garch, method = "Partial", 
              n.ahead = 2329, n.bootpred = 10, out.sample=2329)
?ugarchboot
plot(fb,which="all", type='l')
fb

set.seed(43)
fb=ugarchboot(fit_garch, method = "Partial", 
              n.ahead = 2329, n.bootpred = 2, out.sample=2329,
              na.omit(ff[,1]))

set.seed(43)
fb=ugarchboot(fit_garch, method = "Partial", 
              n.ahead = 2329, n.bootpred = 2)
a=as.data.frame(fb, which = "series")
b=colMeans(a)
c=as.numeric(b)
plot(c,type='l')

e=c(myData$Logrets,c)
plot(myData8$date[39566:41995],e[39568:41997], type='l', col="blue", ylim=c(-0.013,0.013))
lines(0:100, e[39568:39668], col = "black", lwd=1.4)


set.seed(77)
fb=ugarchboot(fit_garch, method = "Partial", 
              n.ahead = 2329, n.bootpred = 2)
a=as.data.frame(fb, which = "series")
b=colMeans(a)
c=as.numeric(b)
plot(c,type='l')


time_index3 = seq(from = as.POSIXct(myData8$date[1], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct(myData8$date[41995], tryFormats = c("%d/%m/%Y %H:%M")), 
                  by="hour")

dff=data.frame(c(time_index3[39667:41995]),c)
tail(dff)
plot(dff,type='l',col='blue',
     main="ETHBTC Logreturns Predicted",
     ylim=c(-0.01,0.01))

e=c(myData$Logrets,c)

dff2=data.frame(c(time_index3[0:41995]),e[0:41995])

plot(dff2[39569:41995,1],dff2[39569:41995,2],col='blue',type='l',
     ylim=c(-0.01,0.01),main='ETHBTC Logreturns Prediction')
lines(dff2[39568:39667,1], dff2[39568:39667,2], col = "black", lwd=1)

#vs real
dff3=data.frame(c(time_index3[0:41995]),myData8$Logrets[0:41995])
plot(dff3[39569:41995,1],dff3[39569:41995,2],col='red',type='l',
     ylim=c(-0.01,0.01),main="ETHBTC Logreturns Test Data")
lines(dff2[39568:39667,1], dff2[39568:39667,2], col = "black", lwd=1)



c3
plot(myData8$Logrets[39764:41995]-c3,type='l',
     main="Prediction Residuals")

#percentage
duh=data.frame(myData8$Logrets[39764:41995]-c3/myData8$Logrets[39764:41995])
duh2=duh[is.finite(rowSums(duh)),]
mean(duh2)
#rmse
sqrt(mean((myData8$Logrets[39764:41995]-c3)^2))
#mae
duh3=data.frame(myData8$Logrets[39764:41995]-c3)
duh4=duh3[is.finite(rowSums(duh)),]
sum(abs(duh4))/2222
#mape
sum(abs(duh2))/2222
#goodness of fit mse
mean(duh4^2)



plot(c, type='l', ylab="predicted Logrets", xlab="time from 23/11/2022")
plot(c(myData$Logrets[39669:39778],c),type='l')
plot(c(myData8$date[39667:41995]),c,col='red',type='l')
set.seed(24)

#fit_garch@fit$fitted.values
#f@forecast$seriesFor
#fit_garch@fit$var
#fit_garch@fit$sigma
#str(fit_garch)

#dont know if i need all this
f=ugarchforecast(fit_garch2,
                 n.ahead=2000)
fb=ugarchboot(fit_garch, method = "Partial", 
              n.ahead = 120, n.bootpred = 2000)
plot(sigma(f))

f=ugarchforecast(fit_garch3,
                 n.ahead=2000)
fb=ugarchboot(fit_garch3, method = "Partial", 
              n.ahead = 120, n.bootpred = 2000)
plot(sigma(f), type='l')
mean(fb$Series(summary))

as.data.frame(fb)


#combining earlier data with forecast
d=rep(c, each=24)
e=c(myData$Logrets, d)
e=c(myData$Logrets,c)
plot(e[38668:40668],type='l')
dff3=data.frame
plot(e, type='l')
plot(dff2,type='l')
lines(dff2[39669:41996,1], dff2[39669:41996,2], col = "blue")
lines(39668:42007, e[39668:42007], col = "blue")
#zoom in to forecast
plot(e[39568:42007], type='l', col="blue", ylim=c(-0.01,0.01))
lines(0:100, e[39568:39668], col = "black", lwd=1.5)

#earlier data with real forward data
myData7 = read.csv("C:\\Users\\faiza\\Downloads\\MS4090 - MS FYP\\ETHBTCp2.csv", 
                  header = TRUE)
myData8 = read.csv("C:\\Users\\faiza\\Downloads\\MS4090 - MS FYP\\ETHBTCcb.csv", 
                   header = TRUE)
dff3=data.frame(c(time_index3[0:41995]),myData8$Logrets[0:41995])
plot(dff3, type='l')
lines(dff3[39669:41996,1], dff3[39669:41996,2], col = "red")

#zoom in to forward data
plot(myData8$Logrets[39569:41996], type='l', col="blue", ylim=c(-0.01,0.01))
lines(0:100, myData8$Logrets[39569:39669], col = "black", lwd=1.5)

#predicted and real forward data together
plot(dff2[39568:41995,1],dff2[39568:41995,2], type='l', col="blue",
     ylim=c(-0.01,0.01),main="Predicted vs Test")
lines(dff3[39568:41995,1],dff3[39568:41995,2], type='l', col="red")
lines(dff2[39568:39668,1], dff2[39568:39668,2], col = "black")
lines(dff3[39568:39668,1], dff3[39568:39668,2], col = "black")

#forecast with roll back method (using Nov-Feb data)
garch_spec2=ugarchspec(variance.model=list(model="sGARCH",
                      garchOrder=c(1,1)), fixed.pars=list(omega=0),
                      mean.model=list(armaOrder=c(0,0)))
require(xts)
time_index2 = seq(from = as.POSIXct(myData8$date[2], tryFormats = c("%d/%m/%Y %H:%M")), 
                 to = as.POSIXct(myData8$date[41994], tryFormats = c("%d/%m/%Y %H:%M")), 
                 by = "hour")
traff2 = xts(myData8$Logrets[2:41995], order.by = time_index2)
ff2=traff2[0:41994]
tail(ff2[,1])

fit_garch2 = ugarchfit(spec = garch_spec2, data = na.omit(ff2[,1]))
f2=ugarchforecast(fit_garch2,
                 n.ahead=2329, n.roll=2329)
plot(f2)

#bootstrap
fb2=ugarchboot(fit_garch2, method = "Partial", 
              n.ahead = 2232, n.bootpred = 2)

plot(fb2,which="all", type='l')

a2=as.data.frame(fb2, which = "series")
b2=colMeans(a2)
c2=as.numeric(b2)
e2=c(myData8$Logrets,c2)
plot(c2, type='l', ylab="predicted Logrets", xlab="days from 20/11/2022")


#forecast Feb-May (use combined data)
garch_spec2=ugarchspec(variance.model=list(model="sGARCH",
                      garchOrder=c(1,1)), fixed.pars=list(omega=0),
                       mean.model=list(armaOrder=c(0,0)))
fit_garch3 = ugarchfit(spec = garch_spec2, data = na.omit(ff2[,1]))
f3=ugarchforecast(fit_garch3, n.ahead=2232)
tail(f3)
plot(f3)


tail(ff2[,1])


#bootstrap
set.seed(57)

fb3=ugarchboot(fit_garch3, method = "Partial", 
               n.ahead = 2232, n.bootpred = 2)


a3=as.data.frame(fb3, which = "series")
b3=colMeans(a3)
c3=as.numeric(b3)
e3=c(myData8$Logrets,c3)


time_index4 = seq(from = as.POSIXct(myData8$date[1], tryFormats = c("%d/%m/%Y %H:%M")), 
                  to = as.POSIXct("2023-06-01 00:00:00 BST"), 
                  length.out=44227)

dff12=data.frame(c(time_index4[41996:44227]),c3)
dff22=data.frame(c(time_index4[0:44227]),e3)

plot(dff12, type='l', col='blue',main="Forecasted Logreturns")

plot(dff22,type='l',col='blue',main="ETHBTC Train+Test+Forecasted Logreturns")
lines(dff22[1:39667,1],dff22[1:39667,2],col='black')
lines(dff3[39667:41996,1],dff3[39667:41996,2],col='red')

plot(fb3,which="all", type='l')


#Summary Stats
allofthem=c(myData$Logrets[1:38322],
  myData2$Logrets[1:38322],
  myData6$x[1:38322],
  myData5$x[1:38322])
allofthemdf=data.frame(c(myData$Logrets)[1:38322],
                     c(myData2$Logrets)[1:38322],
                     c(myData6$x)[1:38322],
                     c(myData5$x)[1:38322])
allofthemm=as.matrix(c(myData$Logrets)[1:38322],
            c(myData2$Logrets)[1:38322],
            c(myData6$x)[1:38322],
            c(myData5$x)[1:38322])

ss01=c(min(na.omit(myData$Logrets)),
  min(na.omit(myData2$Logrets)),
  min(na.omit(myData6$x)),
  min(na.omit(myData5$x)) )

ss02=c(quantile(na.omit(myData$Logrets),prob=0.25),
  quantile(na.omit(myData2$Logrets),prob=0.25),
  quantile(na.omit(myData6$x),prob=0.25),
  quantile(na.omit(myData5$x),prob=0.25) )

ss03=c(median(na.omit(myData$Logrets)),
  median(na.omit(myData2$Logrets)),
  median(na.omit(myData6$x)),
  median(na.omit(myData5$x)) )

ss04=c(mean(na.omit(myData$Logrets)),
  mean(na.omit(myData2$Logrets)),
  mean(na.omit(myData6$x)),
  mean(na.omit(myData5$x)) )

ss05=c(quantile(na.omit(myData$Logrets),prob=0.75),
  quantile(na.omit(myData2$Logrets),prob=0.75),
  quantile(na.omit(myData6$x),prob=0.75),
  quantile(na.omit(myData5$x),prob=0.75) )

ss06=c(max(na.omit(myData$Logrets)),
  max(na.omit(myData2$Logrets)),
  max(na.omit(myData6$x)),
  max(na.omit(myData5$x)) )

?quantile




summary(allofthemdf)
ss1=c(summary(na.omit(myData$Logrets)),
summary(na.omit(myData2$Logrets)),
summary(na.omit(myData6$x)),
summary(na.omit(myData5$x)) )

ss2=c(var(na.omit(myData$Logrets)),
  var(na.omit(myData2$Logrets)),
  var(na.omit(myData6$x)),
  var(na.omit(myData5$x)))

ss3=c(sd(na.omit(myData$Logrets)),
  sd(na.omit(myData2$Logrets)),
  sd(na.omit(myData6$x)),
  sd(na.omit(myData5$x)))
length(na.omit(myData$Logrets))
#std error
sd(na.omit(myData$Logrets)) / sqrt(length(na.omit(myData$Logrets)))
ss4=c(sd(na.omit(myData$Logrets)) / sqrt(length(na.omit(myData$Logrets))),
  sd(na.omit(myData2$Logrets)) / sqrt(length(na.omit(myData2$Logrets))),
  sd(na.omit(myData6$x)) / sqrt(length(na.omit(myData6$x))),
  sd(na.omit(myData5$x)) / sqrt(length(na.omit(myData5$x))) )
#coef variance
sd(na.omit(myData$Logrets)) / mean(na.omit(myData$Logrets))
ss5=c(sd(na.omit(myData$Logrets)) / mean(na.omit(myData$Logrets)),
  sd(na.omit(myData2$Logrets)) / mean(na.omit(myData2$Logrets)),
  sd(na.omit(myData6$x)) / mean(na.omit(myData6$x)),
  sd(na.omit(myData5$x)) / mean(na.omit(myData5$x)) )

library(moments)
skewness(na.omit(myData$Logrets))
ss6=c(skewness(na.omit(myData$Logrets)),
  skewness(na.omit(myData2$Logrets)),
  skewness(na.omit(myData6$x)),
  skewness(na.omit(myData5$x)))
kurtosis(na.omit(myData$Logrets))
ss7=c(kurtosis(na.omit(myData$Logrets)),
  kurtosis(na.omit(myData2$Logrets)),
  kurtosis(na.omit(myData6$x)),
  kurtosis(na.omit(myData5$x)) )
#VaR
exp(quantile(na.omit(myData$Logrets), probs = c(0.01,0.05)))-1

ss8=c(exp(quantile(na.omit(myData$Logrets), probs = c(0.01,0.05)))-1,
exp(quantile(na.omit(myData2$Logrets), probs = c(0.01,0.05)))-1,
exp(quantile(na.omit(myData6$x), probs = c(0.01,0.05)))-1,
exp(quantile(na.omit(myData5$x), probs = c(0.01,0.05)))-1 )

ss1
print(format(t(data.frame(Minimum=ss01,Q1=ss02,
             Median=ss03,Mean=ss04,
             Q3=ss05,Maximum=ss06,
             Variance=ss2,StdDev=ss3,
             StdError=ss4,CoefVar=ss5,Skew=ss6,Kurtosis=ss7,
             row.names = c("ETHBTC","BTCUSD","USDEUR","VIX"))),
             scientific=FALSE, digits=1), quote=FALSE)
?format


#correlation matrix
c1=cor(na.omit(myData$Logrets)[1:39666],
    na.omit(myData2$Logrets)[1:39666])
c2=cor(na.omit(myData$Logrets)[1:39666],
    na.omit(myData6$x)[1:39666])
c3=cor(na.omit(myData$Logrets)[1:38322],
    na.omit(myData5$x)[1:38322])

c4=cor(na.omit(myData2$Logrets)[1:39666],
    na.omit(myData6$x)[1:39666])
c5=cor(na.omit(myData2$Logrets)[1:38322],
    na.omit(myData5$x)[1:38322])

c6=cor(na.omit(myData6$x)[1:38322],
    na.omit(myData5$x)[1:38322])

f=data.frame(ETHBTC=c(1,c1,c2,c3),
          BTCUSD=c(c1,1,c4,c5),
          USDEUR=c(c2,c4,1,c6),
          VIX=c(c3,c5,c6,1),
          row.names = c("ETHBTC","BTCUSD",
                        "USDEUR","VIX"))
print(format(as.matrix(f),digits=4),quote=FALSE)
?as.matrix


length(myData5$x)


#Engle ARCH Test
install.packages("MTS")
library(MTS)
archTest(na.omit(myData$Logrets))

#ADF Stats
library(tseries)
adf.test(myData$Logrets[3:39667])
ur.df(myData$Logrets[3:39667])
?adf.test

#Summary Stats
install.packages("psych")
library(Hmisc)
library(palmerpenguins)
library(tidyverse)
library(mnormt)
library(psych)
describe(myData$Logrets)






#Jarque-Bera Tests

library(moments)
library(tseries)
jarque.test(myData$Logrets[2:39667])
jarque.bera.test(myData$Logrets[2:39667])

jarque.test(myData2$Logrets[2:39667])

jarque.test(myData3$Logrets[3:1180])

jarque.test(myData6$x[2:39667])

jarque.test(myData4$Logrets[2:1142])

jarque.test(myData5$x[2:38322])




a=1
f=merge(myData$Logrets, myData4$Logrets)
f

# Plotting daily data together with hourly data


# Read in the data
data <- read.csv('data.csv')

# Convert daily data to hourly data
daily_data <- myData$Logrets
  group_by(day)
  summarize(mean_daily_value = mean(value))

# Plot the data
ggplot(myData$Logrets, aes(x = time, y = value)) +
  geom_line(color = 'blue', group = 1) +
  geom_line(data = daily_data, aes(x = day, y = mean_daily_value), color = 'red', group = 2) +
  labs(x = 'Time', y = 'Value') +
  ggtitle('Hourly vs Daily Data')


#close
new1=10^(myData$Logrets)
for (i in 3:39678){
  ds[i]=c(10^(myData$Logrets[i])*myData$close[i-1])
}
head(ds)
#forecastclose
for(i in 3:41996){
  ds2[i]=c(10^(myData$Logrets[i])*myData$close[i-1])
}

citation(package="rugarch")
R.Version()

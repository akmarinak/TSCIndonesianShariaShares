#####Package#####
library(zoo)
library(ggplot2)
library(MASS)
library(car)
library(tseries)
library(forecast)
library(FitARMA)
library(lmtest)
library(TSA)
library(stats)
library(nnfor)
library(Metrics)

#####Import Data Perwakilan Cluster#####
setwd("E:/KULIAH/SKRIPSI")
hasilcluster=read.csv("clust.csv",header=TRUE,stringsAsFactors = FALSE)
cluster_saham=zoo(hasilcluster[,-1],order.by=as.Date(as.character(hasilcluster[,1]),"%m/%d/%Y"))
datacluster=ts(cluster_saham)

#####Definisi Variabel#####
###Data Training & Testing
training=cluster_saham[1:726,1:12]
testing=cluster_saham[727:906,1:12]
Y_in=ts(training)
Y_out=ts(testing)
c1tr=training$MRAT
c1tr=ts(c1tr)
c1ts=testing$MRAT
c1ts=ts(c1ts)
c2tr=training$BUKK
c2tr=ts(c2tr)
c2ts=testing$BUKK
c2ts=ts(c2ts)
c3tr=training$KINO
c3tr=ts(c3tr)
c3ts=testing$KINO
c3ts=ts(c3ts)
c4tr=training$WINS
c4tr=ts(c4tr)
c4ts=testing$WINS
c4ts=ts(c4ts)
c5tr=training$PICO
c5tr=ts(c5tr)
c5ts=testing$PICO
c5ts=ts(c5ts)
c6tr=training$ARTA
c6tr=ts(c6tr)
c6ts=testing$ARTA
c6ts=ts(c6ts)
c7tr=training$LSIP
c7tr=ts(c7tr)
c7ts=testing$LSIP
c7ts=ts(c7ts)
c8tr=training$JIHD
c8tr=ts(c8tr)
c8ts=testing$JIHD
c8ts=ts(c8ts)
c9tr=training$LMPI
c9tr=ts(c9tr)
c9ts=testing$LMPI
c9ts=ts(c9ts)
c10tr=training$BIPP
c10tr=ts(c10tr)
c10ts=testing$BIPP
c10ts=ts(c10ts)
c11tr=training$GAMA
c11tr=ts(c11tr)
c11ts=testing$GAMA
c11ts=ts(c11ts)
c12tr=training$MIRA
c12tr=ts(c12tr)
c12ts=testing$MIRA
c12ts=ts(c12ts)
###Data Tanpa Pemisahan
MRAT=ts(cluster_saham$MRAT)
BUKK=ts(cluster_saham$BUKK)
KINO=ts(cluster_saham$KINO)
WINS=ts(cluster_saham$WINS)
PICO=ts(cluster_saham$PICO)
ARTA=ts(cluster_saham$ARTA)
LSIP=ts(cluster_saham$LSIP)
JIHD=ts(cluster_saham$JIHD)
LMPI=ts(cluster_saham$LMPI)
BIPP=ts(cluster_saham$BIPP)
GAMA=ts(cluster_saham$GAMA)
MIRA=ts(cluster_saham$MIRA)

#####PLOT KESELURUHAN#####
#jika diperlukan gunakan dev.off()
options(repr.plot.width=8, repr.plot.height=5)
autoplot(datacluster)+ggtitle('12 Saham ISSI')+xlab('Hari')+ylab('Harga Penutupan')
autoplot(Y_in)+ggtitle('Plot Gabungan Data Training')+xlab('Hari')+ylab('Harga Penutupan')


#####ANN CLUSTER SATU#####
###Identifikasi###
##Plot Deret Waktu##
autoplot(c1tr)+ggtitle('MRAT')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##Plot ACF##
autoplot(acf(c1tr,plot=FALSE))+ labs(title="Plot ACF Saham MRAT")
##Uji ADF##
adf.test(c1tr)
##pembedaan##
c1_lag=diff(c1tr,lag=1)
adf.test(c1_lag)
diffc1=cbind(c1tr,c1_lag)
plot.ts(diffc1)
##BoxCox##
BoxCox.ts(c1_lag)
##plot PACF##
autoplot(pacf(c1_lag,plot=FALSE))+ labs(title="Plot PACF Saham MRAT")

###ANN###
set.seed(1)
fit1=mlp(c1_lag,rep=1,hd=1,algorithm="backprop", learningrate=0.001,lags=c(1,2,3,5,6,8),sel.lag=F,stepmax=10e+06)
fit1
###Perhitungan MAPE###
##transformasi balik data training##
dpredtr1=ts(fit1$fitted[1:717])
xt.1=c1tr[9:725]
predtr1=dpredtr1+xt.1
acttr1=ts(c1tr[10:726])
dftr1=data.frame(predtr1,acttr1)
##MAPEin##
MAPEin1=mape(dftr1$acttr1,dftr1$predtr1)
MAPEin1
##Forecast##
fcast1=forecast(fit1,h=180)
y726_c1=c1tr[726]
fcast1
y726_c1
#Transformasi balik di Excel
##MAPEout##
predts1=read.csv("ts_c1.csv",header=TRUE,stringsAsFactors = FALSE)
predts1=predts1$peramalan
actts1=c1ts
MAPEout1=mape(actts1,predts1)
MAPEout1


#####ANN CLUSTER DUA#####
###Identifikasi###
##plot deret waktu##
autoplot(c2tr)+ggtitle('BUKK')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c2tr,plot=FALSE))+ labs(title="Plot ACF Saham BUKK")
##Uji ADF##
adf.test(c2tr)
##Pembedaan##
c2_lag=diff(c2tr,lag=1)
adf.test(c2_lag)
diffc2=cbind(c2tr,c2_lag)
plot.ts(diffc2)
##BoxCox##
min(c2_lag)
c2_lag_140.01=c2_lag+140.01
BoxCox.ts(c2_lag_140.01)
T.c2=bcPower(c2_lag_140.01,0.6)
BoxCox.ts(T.c2)
##Uji ADF##
adf.test(T.c2)
##plot PACF##
autoplot(pacf(T.c2,plot=FALSE))+ labs(title="Plot PACF Saham BUKK")

###ANN###
set.seed(1)
fit2=mlp(T.c2,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2,3,4,8),sel.lag=F,stepmax=10e+06)
fit2
##transformasi balik data training##
dpredtr2=ts(fit2$fitted[1:717])
predtr2_=InvBoxCox(dpredtr2,0.6,biasadj=FALSE,fvar=NULL)
predtr2.T=predtr2_-140.01
xt.2=c2tr[9:725]
predtr2=predtr2.T+xt.2
acttr2=ts(c2tr[10:726])
dftr2=data.frame(predtr2,acttr2)
##MAPEin##
MAPEin2=mape(dftr2$acttr2,dftr2$predtr2)
MAPEin2
##forecast##
fcast2=as.data.frame(forecast(fit2,h=180))
predts2_=InvBoxCox(fcast2$`Point Forecast`,0.6,biasadj=FALSE,fvar=NULL)
predts2.T=predts2_-140.01
fcast2_=data.frame(point=c(727:906),Forecast=predts2.T,stringsAsFactors=FALSE)
y726_c2=c2tr[726]
fcast2_
y726_c2
#Transformasi balik di Excel
##MAPEout##
predts2=read.csv("ts_c2.csv",header=TRUE,stringsAsFactors = FALSE)
predts2=predts2$peramalan
actts2=c2ts
MAPEout2=mape(actts2,predts2)
MAPEout2


#####ANN CLUSTER TIGA#####
###Identifikasi###
##plot deret waktu##
autoplot(c3tr)+ggtitle('KINO')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c3tr,plot=FALSE))+ labs(title="Plot ACF Saham KINO")
##uji ADF##
adf.test(c3tr)
###pembedaan##
c3_lag=diff(c3tr,lag=1)
adf.test(c3_lag)
diffc3=cbind(c3tr,c3_lag)
plot.ts(diffc3)
##BoxCox##
BoxCox.ts(c3_lag)
##plot PACF##
autoplot(pacf(c3_lag,plot=FALSE))+ labs(title="Plot PACF Saham KINO")

###ANN###
set.seed(1)
fit3=mlp(c3_lag,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,5,6),sel.lag=F,stepmax=10e+06)
fit3
##transformasi balik data training##
dpredtr3=ts(fit3$fitted[1:719])
xt.3=c3tr[7:725]
predtr3=dpredtr3+xt.3
acttr3=ts(c3tr[8:726])
dftr3=data.frame(predtr3,acttr3)
##MAPEin##
MAPEin3=mape(dftr3$acttr3,dftr3$predtr3)
MAPEin3
##forecast##
fcast3=forecast(fit3,h=180)
y726_c3=c3tr[726]
fcast3
y726_c3
#Transformasi balik di Excel
###MAPEout##
predts3=read.csv("ts_c3.csv",header=TRUE,stringsAsFactors = FALSE)
predts3=predts3$peramalan
actts3=c3ts
MAPEout3=mape(actts3,predts3)
MAPEout3


#####ANN CLUSTER EMPAT#####
###Identifikasi###
##Plot Deret Waktu##
autoplot(c4tr)+ggtitle('WINS')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##Uji ADF##
adf.test(c4tr)
##Pembedaan##
c4_lag=diff(c4tr,lag=1)
adf.test(c4_lag)
diffc4=cbind(c4tr,c4_lag)
plot.ts(diffc4)
#BoxCox
BoxCox.ts(c4_lag)
min(c4_lag)
c4_lag_36.01=c4_lag+36.01
BoxCox.ts(c4_lag_36.01)
T.c4=bcPower(c4_lag_36.01,0.9)
BoxCox.ts(T.c4)
##plot PACF##
autoplot(pacf(T.c4,plot=FALSE))+ labs(title="Plot PACF Saham WINS")

###ANN###
set.seed(1)
fit4=mlp(T.c4,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(9,10),sel.lag=F,stepmax=10e+06)
fit4
##transformasi balik data training##
dpredtr4=ts(fit4$fitted[1:715])
predtr4_=InvBoxCox(dpredtr4,0.9,biasadj=FALSE,fvar=NULL)
predtr4.T=predtr4_-36.01
xt.4=c4tr[11:725]
predtr4=predtr4.T+xt.4
acttr4=ts(c4tr[12:726])
dftr4=data.frame(predtr4,acttr4)
##MAPEin##
MAPEin4=mape(dftr4$acttr4,dftr4$predtr4)
MAPEin4
##Forecast##
fcast4=as.data.frame(forecast(fit4,h=180))
predts4_=InvBoxCox(fcast4$`Point Forecast`,0.9,biasadj=FALSE,fvar=NULL)
predts4.T=predts4_-36.01
fcast4_=data.frame(point=c(727:906),Forecast=predts4.T,stringsAsFactors=FALSE)
y726_c4=c4tr[726]
fcast4_
y726_c4
#Transformasi balik di Excel
##MAPEout##
predts4=read.csv("ts_c4.csv",header=TRUE,stringsAsFactors = FALSE)
predts4=predts4$peramalan
actts4=c4ts
MAPEout4=mape(actts4,predts4)
MAPEout4


#####ANN CLUSTER LIMA#####
###Identifikasi###
##plot deret waktu##
autoplot(c5tr)+ggtitle('PICO')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c5tr,plot=FALSE))+ labs(title="Plot ACF Saham PICO")
#Uji ADF
adf.test(c5tr)
##Pembedaan##
c5_lag=diff(c5tr,lag=1)
adf.test(c5_lag)
diffc5=cbind(c5tr,c5_lag)
plot.ts(diffc5)
##BoxCox##
BoxCox.ts(c5_lag)
min(c5_lag)
c5_lag_32.01=c5_lag+32.01
BoxCox.ts(c5_lag_32.01)
T.c5=bcPower(c5_lag_32.01,0.9)
BoxCox.ts(T.c5)
##plot PACF##
autoplot(pacf(T.c5,plot=FALSE))+ labs(title="Plot PACF Saham PICO")

###ANN###
set.seed(1)
fit5=mlp(T.c5,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2,3,4),sel.lag=F,stepmax=10e+06)
fit5
##transformasi balik data training##
dpredtr5=ts(fit5$fitted[1:721])
predtr5_=InvBoxCox(dpredtr5,0.9,biasadj=FALSE,fvar=NULL)
predtr5.T=predtr5_-32.01
xt.5=c5tr[5:725]
predtr5=predtr5.T+xt.5
acttr5=ts(c5tr[6:726])
dftr5=data.frame(predtr5,acttr5)
##MAPEin##
MAPEin5=mape(dftr5$acttr5,dftr5$predtr5)
MAPEin5
##forecast##
fcast5=as.data.frame(forecast(fit5,h=180))
predts5_=InvBoxCox(fcast5$`Point Forecast`,0.9,biasadj=FALSE,fvar=NULL)
predts5.T=predts5_-32.01
fcast5_=data.frame(point=c(727:906),Forecast=predts5.T,stringsAsFactors=FALSE)
y726_c5=c5tr[726]
fcast5_
y726_c5
#Transformasi balik di Excel
##MAPEout##
predts5=read.csv("ts_c5.csv",header=TRUE,stringsAsFactors = FALSE)
predts5=predts5$peramalan
actts5=c5ts
MAPEout5=mape(actts5,predts5)
MAPEout5


#####ANN CLUSTER ENAM#####
###Identifikasi###
##plot deret waktu##
autoplot(c6tr)+ggtitle('ARTA')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c6tr,plot=FALSE))+ labs(title="Plot ACF Saham ARTA")
##Uji ADF##
adf.test(c6tr)
##pembedaan##
c6_lag=diff(c6tr,lag=1)
adf.test(c6_lag)
diffc6=cbind(c6tr,c6_lag)
plot.ts(diffc6)
##BoxCox##
BoxCox.ts(c6_lag)
min(c6_lag)
c6_lag_157.01=c6_lag+157.01
BoxCox.ts(c6_lag_157.01)
T.c6=bcPower(c6_lag_157.01,0.8)
BoxCox.ts(T.c6)
##Uji ADF##
adf.test(T.c6)
##plot PACF##
autoplot(pacf(T.c6,plot=FALSE))+ labs(title="Plot PACF Saham ARTA")

###ANN###
set.seed(1)
fit6=mlp(T.c6,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,3,4,6),sel.lag=F,stepmax=10e+06)
fit6
##transformasi balik data training##
dpredtr6=ts(fit6$fitted[1:719])
predtr6_=InvBoxCox(dpredtr6,0.8,biasadj=FALSE,fvar=NULL)
predtr6.T=predtr6_-157.01
xt.6=c6tr[7:725]
predtr6=predtr6.T+xt.6
acttr6=ts(c6tr[8:726])
dftr6=data.frame(predtr6,acttr6)
##MAPEin##
MAPEin6=mape(dftr6$acttr6,dftr6$predtr6)
MAPEin6
##Forecast##
fcast6=as.data.frame(forecast(fit6,h=180))
predts6_=InvBoxCox(fcast6$`Point Forecast`,0.8,biasadj=FALSE,fvar=NULL)
predts6.T=predts6_-157.01
fcast6_=data.frame(point=c(727:906),Forecast=predts6.T,stringsAsFactors=FALSE)
y726_c6=c6tr[726]
fcast6_
y726_c6
#Transformasi balik di Excel
##MAPEout##
predts6=read.csv("ts_c6.csv",header=TRUE,stringsAsFactors = FALSE)
predts6=predts6$peramalan
actts6=c6ts
MAPEout6=mape(actts6,predts6)
MAPEout6

###CEK PLOT C6###
c6=cluster_saham$ARTA
autoplot(c6)+ggtitle('ARTA')+xlab('Hari')+ylab('Harga Penutupan')


#####ANN CLUSTER TUJUH#####
###Identifikasi###
##Plot Deret Waktu##
autoplot(c7tr)+ggtitle('LSIP')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c7tr,plot=FALSE))+ labs(title="Plot ACF Saham LSIP")
##uji ADF##
adf.test(c7tr)
##BoxCox##
BoxCox.ts(c7tr)
T.c7=bcPower(c7tr,0.5)
BoxCox.ts(T.c7)
##plot PACF##
autoplot(pacf(T.c7,plot=FALSE))+ labs(title="Plot PACF Saham LSIP")

###ANN###
set.seed(1)
fit7=mlp(T.c7,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1),sel.lag=F,stepmax=10e+06)
fit7
##transformasi balik data training##
dpredtr7=ts(fit7$fitted[1:725])
predtr7=InvBoxCox(dpredtr7,0.5,biasadj=FALSE,fvar=NULL)
acttr7=ts(c7tr[2:726])
dftr7=data.frame(predtr7,acttr7)
##MAPEin##
MAPEin7=mape(dftr7$acttr7,dftr7$predtr7)
MAPEin7
##Forecast##
fcast7=as.data.frame(forecast(fit7,h=180))
predts7_=InvBoxCox(fcast7$`Point Forecast`,0.5,biasadj=FALSE,fvar=NULL)
fcast7_=data.frame(point=c(727:906),Forecast=predts7_,stringsAsFactors=FALSE)
y726_c7=c7tr[726]
fcast7_
##MAPEout##
predts7=read.csv("ts_c7.csv",header=TRUE,stringsAsFactors = FALSE)
predts7=predts7$peramalan
actts7=c7ts
MAPEout7=mape(actts7,predts7)
MAPEout7


#####ANN CLUSTER delapan#####
###Identifikasi###
##plot deret waktu##
autoplot(c8tr)+ggtitle('JIHD')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c8tr,plot=FALSE))+ labs(title="Plot ACF Saham JIHD")
##uji ADF##
adf.test(c8tr)
##pembedaan##
c8_lag=diff(c8tr,lag=1)
adf.test(c8_lag)
diffc8=cbind(c8tr,c8_lag)
plot.ts(diffc8)
BoxCox.ts(c8_lag)
##plot PACF##
autoplot(pacf(c8_lag,plot=FALSE))+ labs(title="Plot PACF Saham JIHD")

###ANN###
set.seed(1)
fit8=mlp(c8_lag,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2,3,5),sel.lag=F,stepmax=10e+06)
fit8
##transformasi balik data training##
dpredtr8=ts(fit8$fitted[1:720])
xt.8=c8tr[6:725]
predtr8=dpredtr8+xt.8
acttr8=ts(c8tr[7:726])
dftr8=data.frame(predtr8,acttr8)
##MAPEin##
MAPEin8=mape(dftr8$acttr8,dftr8$predtr8)
MAPEin8
##Forecast##
fcast8=forecast(fit8,h=180)
y726_c8=c8tr[726]
fcast8
y726_c8
#Transformasi balik di Excel
##MAPEout##
predts8=read.csv("ts_c8.csv",header=TRUE,stringsAsFactors = FALSE)
predts8=predts8$peramalan
actts8=c8ts
MAPEout8=mape(actts8,predts8)
MAPEout8


#####ANN CLUSTER SEMBILAN#####
###Identifikasi###
##plot deret waktu##
autoplot(c9tr)+ggtitle('LMPI')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c9tr,plot=FALSE))+ labs(title="Plot ACF Saham LMPI")
##uji ADF##
adf.test(c9tr)
##Pembedaan##
c9_lag=diff(c9tr,lag=1)
adf.test(c9_lag)
diffc9=cbind(c9tr,c9_lag)
plot.ts(diffc9)
##BoxCox##
BoxCox.ts(c9_lag)
min(c9_lag)
c9_lag_33.01=c9_lag+33.01
BoxCox.ts(c9_lag_33.01)
T.c9=bcPower(c9_lag_33.01,0.8)
BoxCox.ts(T.c9)
##Uji ADF##
adf.test(T.c9)
##plot PACF##
autoplot(pacf(T.c9,plot=FALSE))+ labs(title="Plot PACF Saham LMPI")

###ANN###
set.seed(1)
fit9=mlp(T.c9,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2,4,5,7),sel.lag=F,stepmax=10e+06)
fit9
##transformasi balik data training##
dpredtr9=ts(fit9$fitted[1:718])
predtr9_=InvBoxCox(dpredtr9,0.8,biasadj=FALSE,fvar=NULL)
predtr9.T=predtr9_-33.01
xt.9=c9tr[8:725]
predtr9=predtr9.T+xt.9
acttr9=ts(c9tr[9:726])
dftr9=data.frame(predtr9,acttr9)
##MAPEin##
MAPEin9=mape(dftr9$acttr9,dftr9$predtr9)
MAPEin9
##Forecast##
fcast9=as.data.frame(forecast(fit9,h=180))
predts9_=InvBoxCox(fcast9$`Point Forecast`,0.8,biasadj=FALSE,fvar=NULL)
predts9.T=predts9_-33.01
fcast9_=data.frame(point=c(727:906),Forecast=predts9.T,stringsAsFactors=FALSE)
y726_c9=c9tr[726]
fcast9_
y726_c9
#Transformasi balik di Excel
##MAPEout##
predts9=read.csv("ts_c9.csv",header=TRUE,stringsAsFactors = FALSE)
predts9=predts9$peramalan
actts9=c9ts
MAPEout9=mape(actts9,predts9)
MAPEout9


#####ANN CLUSTER SEPULUH#####
###Identifikasi###
##plot deret waktu##
autoplot(c10tr)+ggtitle('BIPP')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c10tr,plot=FALSE))+ labs(title="Plot ACF Saham BIPP")
##uji ADF##
adf.test(c10tr)
##pembedaan##
c10_lag=diff(c10tr,lag=1)
adf.test(c10_lag)
diffc10=cbind(c10tr,c10_lag)
plot.ts(diffc10)
##BoxCox##
BoxCox.ts(c10_lag)
min(c10_lag)
c10_lag_10.01=c10_lag+10.01
BoxCox.ts(c10_lag_10.01)
T.c10=bcPower(c10_lag_10.01,0.7)
BoxCox.ts(T.c10)
##Uji ADF##
adf.test(T.c10)
##plot PACF##
autoplot(pacf(T.c10,plot=FALSE))+ labs(title="Plot PACF Saham BIPP")

###ANN###
set.seed(1)
fit10=mlp(T.c10,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2,5),sel.lag=F,stepmax=10e+06)
fit10
##transformasi balik data training##
dpredtr10=ts(fit10$fitted[1:720])
predtr10_=InvBoxCox(dpredtr10,0.7,biasadj=FALSE,fvar=NULL)
predtr10.T=predtr10_-10.01
xt.10=c10tr[6:725]
predtr10=predtr10.T+xt.10
acttr10=ts(c10tr[7:726])
dftr10=data.frame(predtr10,acttr10)
##MAPEin##
MAPEin10=mape(dftr10$acttr10,dftr10$predtr10)
MAPEin10
##Forecast##
fcast10=as.data.frame(forecast(fit10,h=180))
predts10_=InvBoxCox(fcast10$`Point Forecast`,0.7,biasadj=FALSE,fvar=NULL)
predts10.T=predts10_-10.01
fcast10_=data.frame(point=c(727:906),Forecast=predts10.T,stringsAsFactors=FALSE)
y726_c10=c10tr[726]
fcast10_
y726_c10
#Transformasi balik di Excel
##MAPEout##
predts10=read.csv("ts_c10.csv",header=TRUE,stringsAsFactors = FALSE)
predts10=predts10$peramalan
actts10=c10ts
MAPEout10=mape(actts10,predts10)
MAPEout10


#####ANN CLUSTER SEBELAS#####
###Identifikasi###
##plot deret waktu##
autoplot(c11tr)+ggtitle('GAMA')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c11tr,plot=FALSE))+ labs(title="Plot ACF Saham GAMA")
##uji ADF##
adf.test(c11tr)
#BoxCox
BoxCox.ts(c11tr)
T.c11<-bcPower(c11tr,-1)
adf.test(T.c11)
diffc11=cbind(c11tr,T.c11)
plot.ts(diffc11)
##plot PACF##
autoplot(pacf(T.c11,plot=FALSE))+ labs(title="Plot PACF Saham GAMA")

###ANN###
set.seed(1)
fit11=mlp(T.c11,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2),sel.lag=F,stepmax=10e+06)
fit11
##transformasi balik data training##
dpredtr11=ts(fit11$fitted[1:724])
predtr11=InvBoxCox(dpredtr11,-1,biasadj=FALSE,fvar=NULL)
acttr11=ts(c11tr[3:726])
dftr11=data.frame(predtr11,acttr11)
##MAPEin##
MAPEin11=mape(dftr11$acttr11,dftr11$predtr11)
MAPEin11
##Forecast##
fcast11=as.data.frame(forecast(fit11,h=180))
predts11_=InvBoxCox(fcast11$`Point Forecast`,-1,biasadj=FALSE,fvar=NULL)
fcast11_=data.frame(point=c(727:906),Forecast=predts11_,stringsAsFactors=FALSE)
y726_c11=c11tr[726]
fcast11_
y726_c11
#Transformasi balik di Excel
##MAPEout##
predts11=read.csv("ts_c11.csv",header=TRUE,stringsAsFactors = FALSE)
predts11=predts11$peramalan
actts11=c11ts
MAPEout11=mape(actts11,predts11)
MAPEout11


#####ANN CLUSTER DUA BELAS#####
###Identifikasi###
##plot deret waktu##
autoplot(c12tr)+ggtitle('MIRA')+xlab('Hari')+ylab('Harga Penutupan')
##Stasioneritas##
##plot ACF##
autoplot(acf(c12tr,plot=FALSE))+ labs(title="Plot ACF Saham MIRA")
##uji ADF##
adf.test(c12tr)
##BoxCox##
BoxCox.ts(c12tr)
T.c12<-bcPower(c12tr,-1)
adf.test(T.c12)
diffc12=cbind(c12tr,T.c12)
plot.ts(diffc12)
##plot PACF##
autoplot(pacf(T.c12,plot=FALSE))+ labs(title="Plot PACF Saham MIRA")

###ANN###
set.seed(1)
fit12=mlp(T.c12,rep=1,hd=1,algorithm="backprop",learningrate=0.001,lags=c(1,2,4,7),sel.lag=F,stepmax=10e+06)
fit12
##transformasi balik data training##
dpredtr12=ts(fit12$fitted[1:719])
predtr12=InvBoxCox(dpredtr12,-1,biasadj=FALSE,fvar=NULL)
acttr12=ts(c12tr[8:726])
dftr12=data.frame(predtr12,acttr12)
##MAPEin##
MAPEin12=mape(dftr12$acttr12,dftr12$predtr12)
MAPEin12
##Forecast##
fcast12=as.data.frame(forecast(fit12,h=180))
predts12_=InvBoxCox(fcast12$`Point Forecast`,-1,biasadj=FALSE,fvar=NULL)
fcast12_=data.frame(point=c(727:906),Forecast=predts12_,stringsAsFactors=FALSE)
fcast12_
#Transformasi balik di Excel
##MAPEout##
predts12=read.csv("ts_c12.csv",header=TRUE,stringsAsFactors = FALSE)
predts12=predts12$peramalan
actts12=c12ts
MAPEout12=mape(actts12,predts12)
MAPEout12


#####Rangkuman MAPE#####
MAPEin=rbind(MAPEin1,MAPEin2,MAPEin3,MAPEin4,MAPEin5,MAPEin6,MAPEin7,MAPEin8,MAPEin9,MAPEin10,MAPEin11,MAPEin12)
MAPEout=rbind(MAPEout1,MAPEout2,MAPEout3,MAPEout4,MAPEout5,MAPEout6,MAPEout7,MAPEout8,MAPEout9,MAPEout10,MAPEout11,MAPEout12)
MAPE=cbind(c(1:12),MAPEin,MAPEout)
MAPE

#####Rangkuman 12 Model ANN#####
fit1$net["result.matrix"]
fit2$net["result.matrix"]
fit3$net["result.matrix"]
fit4$net["result.matrix"]
fit5$net["result.matrix"]
fit6$net["result.matrix"]
fit7$net["result.matrix"]
fit8$net["result.matrix"]
fit9$net["result.matrix"]
fit10$net["result.matrix"]
fit11$net["result.matrix"]
fit12$net["result.matrix"]

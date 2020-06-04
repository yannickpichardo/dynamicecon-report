library('readxl')  # To read Excel files
library('fpp2')    # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm')   # To estimate ARDL models
library('urca')    # For the Dickey Fuller test
library('corrplot')# For plotting correlation matrices
library('quadprog')# For quadratic optimization
library('forecast')
library('dplyr')
library('xts')
library('zoo')

lowfreq      <- zoo(,time(ffr_q))
highfreq     <- zoo(ffr,time(ffr))
ffr3         <- merge(lowfreq,highfreq)
ffr3$lowfreq <- na.approx(ffr3$FEDFUNDS.lowfreq, rule=2)
autoplot(ffr3)
ffr_ts       <- as.ts(ffr3$lowfreq)


CCI = read.csv('DP_LIVE_07052020144921792.csv') 
CCI = CCI[158:length(CCI$Value),]
CCI = CCI %>% mutate(percentage = Value - 100)
CCI$percentage

CCI = ts(CCI[,9], start= c(2008), frequency = 12)
autoplot(CCI)
autoplot(diff(CCI))
months = c(31,28,31,30,31,30,31,31,30,31,30,31)
CCI_days = c()
for (i in 1:length(CCI$percentage)){
  j = (i %% 12) 
  if (i %% 12 == 0){
    j=12
  }
  CCI_days = c(CCI_days, rep(CCI$percentage[i],months[j]))
}
CCI_days    


CCI_week_mean = c()
for (i in 1:floor(length(CCI_days) /7)){
  CCI_week_mean = c(CCI_week_mean, mean(CCI_days[(7*i +1):(7*(i+1))]))
}
CCI_week_mean[length(CCI_week_mean)] = CCI_week_mean[length(CCI_week_mean)-1]
CCI_week_mean = c(CCI_week_mean,CCI_week_mean[length(CCI_week_mean)-1])
CCI_week_mean
length(CCI_week_mean)
CCI_unique =c()
for (i in 1:length(unique(CCI_week_mean))) {
  CCI_unique[i] = min(which(CCI_week_mean == unique(CCI_week_mean)[i]))
  
}
CCI_week = c()
for (i in 1:(length(CCI_unique)-1)){
  seq = seq(from = CCI_week_mean[CCI_unique[i]], to =  CCI_week_mean[CCI_unique[i+1]], length.out = CCI_unique[i+1]-CCI_unique[i]+1)
  
  CCI_week = c(CCI_week,seq[1:(length(seq)-1)])
}
CCI_week = c(CCI_week,seq(from = CCI_week_mean[CCI_unique[length(CCI_unique)-1]], to = CCI_week_mean[CCI_unique[length(CCI_unique)]],length.out = 6)[2:6])
CCI_week

CCI = ts(CCI_week, start = c(2008), frequency = 52)
autoplot(CCI)

d_CCI <- diff(CCI)
autoplot(d_CCI)


cor(CCI,WEI)

data <- read_excel("WEI.xlsx", sheet = 'Weekly Data (2008-)')

WEI = ts(data[,4], start= c(2008), frequency = 52)

#WEI_no_corona = ts(data[1:629,4], start= c(2008), frequency = 365.25/7)
#voor dicky fuller zonde corona maanden
autoplot(WEI) #figure 1 lijn rond 0
autoplot(diff(WEI)) # naast figure 1
d_WEI <- diff(WEI)
ts.plot(d_CCI, d_WEI)

WEI = ts(data[,4], start= c(2008), frequency = 365.25/7)
autoplot(WEI)
autoplot(diff(WEI))



#pacf en acf
Acf(WEI) #figure 2 
Pacf(WEI)  #figure 2
#AR gedeelte vanwege Pacf ongeveer 5 
#MA gedeelte erg groot maar we hebben vraag gesteld op discussion board

#Dickey Fuller test
pmax    <- floor(12*((length(data_1$WEI)/100)^0.25))
dft     <- ur.df(data_1$WEI,type=c("drift"),lags=pmax,selectlags=c("BIC"))
summdf  <- summary(dft)
print(summdf@test.name)
print(c("Test statistic: ", summdf@teststat[1]))
print(c("Crit. vals", summdf@cval[1,]))
1#crit vals 1pct 5pct 10pct  figure 3



#information criteria
bic_WEI = matrix(NA,7,6)
aic_WEI = matrix(NA,7,6)
T_est = matrix(NA,7,6)
for (i in seq(2,8)){
  for (j in seq(0,5)){
    fit = Arima(WEI, order = c(i,0,j))
    T_est[i-1,j+1] = length(fit$residuals)
    bic_WEI[i-1,j+1] = fit$bic
    aic_WEI[i-1,j+1] = fit$aic
  }
}
T_est

colnames(bic_WEI) <- c("MA(0)","MA(1)","MA(2)","MA(3)","MA(4)",'MA(5)')
rownames(bic_WEI) <- c('AR(2)',"AR(3)","AR(4)","AR(5)","AR(6)","AR(7)",'AR(8)')
bic_WEI #table 1
min_values_bic= sort(bic_WEI)[1:3]
min_index_bic=c() 
for (i in 1:3){
  min_index_bic[i] = which(bic_WEI==min_values_bic[i])
}
min_index_bic


colnames(aic_WEI) <- c("MA(0)","MA(1)","MA(2)","MA(3)","MA(4)",'MA(5)')
rownames(aic_WEI) <- c('AR(2)',"AR(3)","AR(4)","AR(5)","AR(6)","AR(7)",'AR(8)')
aic_WEI #table 2
min_values_aic= sort(aic_WEI)[1:3]
min_index_aic=c() 
for (i in 1:3){
  min_index_aic[i] = which(aic_WEI==min_values_aic[i])
}
min_index_aic

#residual autocorrelation
fit_1 <- Arima(WEI, order = c(2,0,3)) #figure 4
checkresiduals(fit_1)

fit_2 <- Arima(WEI, order = c(3,0,0))
checkresiduals(fit_2)

fit_3 <- Arima(WEI, order = c(2,0,0))
checkresiduals(fit_3)

fit_4 <- Arima(WEI, order = c(5,0,4)) #figure 5
checkresiduals(fit_4)

fit_5 <- Arima(WEI, order = c(6,0,4))
checkresiduals(fit_5)

fit_6 <- Arima(WEI, order = c(7,0,5))
checkresiduals(fit_6)

fit_7 = Arima(WEI, order = c(52,0,3), fixed=c(NA,NA,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA,NA,NA))
checkresiduals(fit_7) #figure 6

which(fit$residual>0.08)
#the residual gaan vrij goed behalve bij 2020 omdat hier een schrok gebeurt,
#die niet te economisch niet te voorspellen was met de data van de WEI alleen
#wij willen nu variabelen zoeken die deze schrok wel kunnen voorspellen
#en deze toevoegen aan het model.
#Het dal rond 2020 komt gedeeltelijk omdat unemplyement insurance claims
#deel zijn van de WEI deze zijn enorm gestegen https://fred.stlouisfed.org/series/ICSA
#
#Er is een grote autocorrelition tussen alle residuals precies na een jaar,
#Waarom zou dit komen? Dit komt waarschijnlijk doordat de WEI series 
#aangepast is op seasonality en hierdoor een foutje ontstaat in de acf
#zie ook het document op nestor waar dit wordt uitgelegd in het einde van 4.3

#kijken naar invertibility en stability door middel van de unit cirkel (ze moeten er in liggen)
#en klein summary overzicht met de waardes van de coefficienten.

autoplot(fit_1)
fit_1

autoplot(fit_2)
fit_2

autoplot(fit_3)
fit_3

autoplot(fit_4)
fit_4

autoplot(fit_5)
fit_5

autoplot(fit_6)
fit_6

autoplot(fit_7)
fit_7





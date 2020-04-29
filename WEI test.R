library('readxl')  # To read Excel files
library('fpp2')    # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm')   # To estimate ARDL models
library('urca')    # For the Dickey Fuller test
library('corrplot')# For plotting correlation matrices
library('quadprog')# For quadratic optimization
library('forecast')
library('dplyr')


data <- read_excel("WEI.xlsx", sheet = 'Weekly Data (2008-)')

WEI = ts(data[,4], start= c(2008), frequency = 365.25/7)

autoplot(WEI)

#pacf en acf
Acf(WEI)
Pacf(WEI)
#AR gedeelte vanwege Pacf ongeveer 5 
#MA gedeelte erg groot maar we hebben vraag gesteld op discussion board

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
bic_WEI
min_values_bic= sort(bic_WEI)[1:3]
min_index_bic=c() 
for (i in 1:3){
  min_index_bic[i] = which(bic_WEI==min_values_bic[i])
}
min_index_bic


colnames(aic_WEI) <- c("MA(0)","MA(1)","MA(2)","MA(3)","MA(4)",'MA(5)')
rownames(aic_WEI) <- c('AR(2)',"AR(3)","AR(4)","AR(5)","AR(6)","AR(7)",'AR(8)')
aic_WEI
min_values_aic= sort(aic_WEI)[1:3]
min_index_aic=c() 
for (i in 1:3){
  min_index_aic[i] = which(aic_WEI==min_values_aic[i])
}
min_index_aic

#residual autocorrelation
fit_1 <- Arima(WEI, order = c(3,0,0))
checkresiduals(fit_1)

fit_2 <- Arima(WEI, order = c(6,0,0))
checkresiduals(fit_2)

fit_3 <- Arima(WEI, order = c(4,0,2))
checkresiduals(fit_3)

fit_4 <- Arima(WEI, order = c(5,0,4))
checkresiduals(fit_4)

fit_5 <- Arima(WEI, order = c(6,0,4))
checkresiduals(fit_5)

fit_6 <- Arima(WEI, order = c(4,0,1))
checkresiduals(fit_6)

fit_7 = Arima(WEI, order = c(52,0,2), fixed=c(NA,NA,NA,NA,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA))
p = checkresiduals(fit_7)

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
#en klein summary overzicht met de waardes van de coefficienten

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





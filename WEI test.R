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
bic_WEI = matrix(NA,4,5)
aic_WEI = matrix(NA,4,5)
T_est = matrix(NA,4,5)
for (i in seq(3,6)){
  for (j in seq(0,4)){
    fit = Arima(WEI, order = c(i,0,j))
    T_est[i-2,j+1] = length(fit$residuals)
    bic_WEI[i-2,j+1] = fit$bic
    aic_WEI[i-2,j+1] = fit$aic
  }
}
T_est

colnames(bic_WEI) <- c("MA(0)","MA(1)","MA(2)","MA(3)","MA(4)")
rownames(bic_WEI) <- c("AR(3)","AR(4)","AR(5)","AR(6)")
bic_WEI
min_values_bic= sort(bic_WEI)[1:3]
min_index_bic=c() 
for (i in 1:3){
  min_index_bic[i] = which(bic_WEI==min_values_bic[i])
}
min_index_bic


colnames(aic_WEI) <- c("MA(0)","MA(1)","MA(2)","MA(3)","MA(4)")
rownames(aic_WEI) <- c("AR(3)","AR(4)","AR(5)","AR(6)")
aic_WEI
min_values_aic= sort(aic_WEI)[1:3]
min_index_aic=c() 
for (i in 1:3){
  min_index_aic[i] = which(aic_WEI==min_values_aic[i])
}
min_index_aic

#residual autocorrelation
fit <- Arima(WEI, order = c(3,0,5))
checkresiduals(fit)

fit <- Arima(WEI, order = c(4,0,5))
checkresiduals(fit)

fit <- Arima(WEI, order = c(5,0,5))
checkresiduals(fit)

fit <- Arima(WEI, order = c(6,0,5))
checkresiduals(fit)

fit <- Arima(dlog_gnp, order = c(0,0,2))
checkresiduals(fit)

fit <- Arima(dlog_gnp, order = c(0,0,2))
checkresiduals(fit)









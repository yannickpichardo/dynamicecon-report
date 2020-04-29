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
bic_WEI = matrix(NA,4,16)
aic_WEI = matrix(NA,4,16)
T_est = matrix(NA,4,16)
for (i in seq(3,6)){
  for (j in seq(5,20)){
    fit = Arima(WEI, order = c(i,0,j))
    T_est[i+1,j+1] = length(fit$residuals)
    bic_WEI[i+1,j+1] = fit$bic
    aic_WEI[i+1,j+1] = fit$aic
  }
}


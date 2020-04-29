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

Acf(WEI)
Pacf(WEI)

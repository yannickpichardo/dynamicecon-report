###################### ALL THE R CODE WE USED ####################################################

################################## Library ####################################################
library("DataCombine")
library('ggplot2')
library("corrplot")
library("tidyverse")
library("dplyr")
library("openxlsx")
library("tseries")
library('fpp2')    # For forecasting
library('dynlm')   # To estimate ARDL models
library('urca')    # For the Dickey Fuller test
library('corrplot')# For plotting correlation matrices
library('quadprog')# For quadratic optimization
library('forecast')
library('readxl')  # To read Excel files
library('fpp2')    # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm')   # To estimate ARDL models
library('urca')    # For the Dickey Fuller test
library('corrplot')# For plotting correlation matrices
library('quadprog')# For quadratic optimization
library('forecast')# Lots of handy forecasting routines
library('vars')    # VARs
library('zoo')   
library('lubridate')

############# Data loading #######################################################################

data_1 <- read.xlsx("WEI.xlsx", sheet = 2, detectDates = TRUE)
sp500_newdata <- read.csv("sp500newdata.csv")
CCI       <- read.csv('CCI.csv')

########### Data manipulation #####################################################################

sp500_newdata <- sp500_newdata %>% 
  mutate(average_open_close = (Open + Close) / 2)

data <- data %>% cbind(sp500data$average_open_close)
colnames(data)[11] <- "average_open_close"

sp500_52week_change <- PercChange(data = sp500_newdata, Var = "average_open_close", NewVar = "sp500_52week_change", slideBy = -52)
sp500_52week_change <- sp500_52week_change$sp500_52week_change
sp500_52week_change <- sp500_52week_change[!is.na(sp500_52week_change)]
data_1$sp500_52week_change <- sp500_52week_change

#read CSV file and obtain data from 2007-2020, with values around 0
CCI_data = CCI %>% slice(3:nrow(CCI)) %>% mutate(percentage = Value - 100)
CCI_2007       <- ts(CCI_data[,9],start = 2007,frequency=12)

#Take difference with respect to the value of last year
diff_CCI = diff(CCI_2007, 12)
diff_CCI = ts(as.vector(diff_CCI), start = 2008, frequency = 12)

# Merge low and high freq time series
lowfreq      <- zoo(diff_CCI,time(diff_CCI))
highfreq     <- zoo(WEI,time(WEI))
merged       <- merge(lowfreq,highfreq)

# Approximate the NAs and output at the dates of the WEI
CCIw         <- na.approx(merged$lowfreq, xout = time(WEI),rule=2)
CCIw         <- ts(CCIw,start = 2008,frequency=52)

data_1$CCIw =as.vector(CCIw) 

#preparing all time series
WEI_365       <- ts(data_1$WEI, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
CCIw_365       <- ts(data_1$CCIw, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
sp500_52week_change_365        <- ts(data_1$sp500_52week_change, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
sp_500_52week_diff_365       <- ts(data_1$sp_500_52week_diff, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
noise<-ts(rnorm(length(CCIw_365))*sqrt(sd((CCIw_365)/100)),decimal_date(ymd("2008-01-05")),frequency=365.25/7)
CCIn <- CCIw_365+noise

########################### Plots we used ###########################################################

plot_WEI_SP500_CCI <- ggplot(data = data_1, aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500")) +
  geom_line(aes(y = CCIw * 1.5, colour = "CCI")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500" = "blue", "CCI" = "red")) + 
  ggtitle("WEI vs S&P500 52 week % change scaled by 10 vs CCI scaled by 1.5") + 
  ylab("WEI, S&P500 and CCI") + 
  xlab("Date") 
plot_WEI_SP500_CCI

plot_WEI_SP500_CCI_2008_2010 <- ggplot(data = data_1[1:105, ], aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500")) +
  geom_line(aes(y = CCIw * 1.5, colour = "CCI")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500" = "blue", "CCI" = "red")) + 
  ggtitle("WEI vs S&P500 52 week % change scaled by 10 vs CCI scaled by 1.5 during 2008-2010") + 
  ylab("WEI, S&P500 and CCI") + 
  xlab("Date") 
plot_WEI_SP500_CCI_2008_2010

plot_WEI_SP500_CCI_before_covid <- ggplot(data = data_1[560:630,], aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500")) +
  geom_line(aes(y = CCIw * 1.5, colour = "CCI")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500" = "blue", "CCI" = "red")) + 
  ggtitle("WEI vs S&P500 52 week % change scaled by 10 vs CCI scaled by 1.5 before covid") + 
  ylab("WEI, S&P500 and CCI") + 
  xlab("Date") 
plot_WEI_SP500_CCI_before_covid

plot_WEI_SP500_CCI_during_covid <- ggplot(data = data_1[630:639, ], aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500")) +
  geom_line(aes(y = CCIw * 1.5, colour = "CCI")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500" = "blue", "CCI" = "red")) + 
  ggtitle("WEI vs S&P500 52 week % change scaled by 10 vs CCI scaled by 1.5 during covid") + 
  ylab("WEI, S&P500 and CCI") + 
  xlab("Date") 
plot_WEI_SP500_CCI_during_covid

################################## Corrplot ########################################################
data_2 <- data_1
colnames(data_2)[11:13] <- c("S&P500 %change" ,"S&P500 difference", "CCI")
z <- cbind(data_2[4:8], data_2[11:13])
Z <- cor(z)
corrplot(Z, method = "color")

################################# (P)ACF plots and Dicky-Fuller tests ###############################

acf(data_1$WEI)
pacf(data_1$WEI)

# From the ACF plots we suspected non covariance stationarity, so we conducted some Dicky-Fuller tests. #
# For the WEI
pmax    <- floor(12*((length(data_1$WEI)/100)^0.25))
dft     <- ur.df(data_1$WEI,type=c("drift"),lags=pmax,selectlags=c("BIC"))
summdf  <- summary(dft)
print(summdf@test.name)
print(c("Test statistic: ", summdf@teststat[1]))
print(c("Crit. vals", summdf@cval[1,]))

# For the S&P500
pmax    <- floor(12*((length(data_1$sp500_52week_change)/100)^0.25))
dft     <- ur.df(data_1$sp500_52week_change,type=c("drift"),lags=pmax,selectlags=c("BIC"))
summdf  <- summary(dft)
print(summdf@test.name)
print(c("Test statistic: ", summdf@teststat[1]))
print(c("Crit. vals", summdf@cval[1,]))

#For the CCI
pmax    <- floor(12*((length(data_1$CCIw)/100)^0.25))
dft     <- ur.df(data_1$CCIw,type=c("drift"),lags=pmax,selectlags=c("BIC"))
summdf  <- summary(dft)
print(summdf@test.name)
print(c("Test statistic: ", summdf@teststat[1]))
print(c("Crit. vals", summdf@cval[1,]))

################################# BIC and AIC ##########################################################

# Information criteria for the WEI.
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

# Information criteria for the VAR model.
colnames(aic_WEI) <- c("MA(0)","MA(1)","MA(2)","MA(3)","MA(4)",'MA(5)')
rownames(aic_WEI) <- c('AR(2)',"AR(3)","AR(4)","AR(5)","AR(6)","AR(7)",'AR(8)')
aic_WEI #table 2
min_values_aic= sort(aic_WEI)[1:3]
min_index_aic=c() 
for (i in 1:3){
  min_index_aic[i] = which(aic_WEI==min_values_aic[i])
}
min_index_aic

# BIC graph
Y           <- cbind(CCIw_365, sp500_52week_change_365 , WEI_365)
colnames(Y) <- c('CCI', 'SP500', 'WEI')
VARmodel_ic <- VARselect(Y,type=c("const"),lag.max=8)
ic          <- as.data.frame(t(VARmodel_ic$criteria))
ic
ggplot(data=ic, aes(x=seq(1,8),y=`SC(n)`))+geom_line()+ylab("BIC")+xlab("VAR(p)")
ggplot(data=ic, aes(x=seq(1,8),y=`AIC(n)`))+geom_line()+ylab("AIC")+xlab("VAR(p)")

############################################ Forecasting ##########################################################

## ARMA forecasts:
# ARMA(2,3)
fit_1 <- Arima(WEI_365, order = c(2,0,3))
fARMA_1 <- forecast(fit_1,h=208)
autoplot(fARMA_1) 

# VAR(3)
Y <- cbind(WEI_365, CCIw_365 , sp500_52week_change_365 )
VAR3 <- VAR(Y,p=3,type = c('const'))
fVAR3 <- forecast(VAR3, h=208)
autoplot(fVAR3$forecast$WEI)
VAR3$varresult$WEI$coefficients

########################################## MSE/MAE #########################################################
#MSE of the ARMA models
es     <- as.Date("2008/1/5") # Estimation start
fs     <- as.Date("2016/1/2") # First forecast 
fe     <- as.Date("2020/03/21")# Final forecast

convert_date <- function(date){
  c(as.numeric(format(date,'%Y')),
    ceiling(as.numeric(format(date,'%W')))) 
  # Use %W for weeks and do not divide by 3.
}

dates   <- seq(fs,fe,by="week") # (or "week"...)
n       <- length(dates)                 # number of forecasts
qF      <- convert_date(fs)
qL      <- convert_date(fe)
target  <- window(WEI_365,start=qF,end=qL)

in_out_ARMA = function(hor, p, q){
  fc    <- ts(data=matrix(NA,n,1),start=qF,frequency=365.25/7)
  fce   <- ts(data=matrix(NA,n,1),start=qF,frequency=365.25/7)
  
  for (i_d in seq(1,n)){
    # Define estimation sample (ends h periods before 1st forecast)
    # Start at the first forecast date, 
    # Then move back h+1 quarters back in time
    est   <- seq(dates[i_d],length=hor+1, by = "-1 week")[hor+1]
    # Now define the data we can use to estimate the model
    yest  <- window(WEI_365,end=convert_date(est))
    # Fit the AR models using Arima
    fit            <- Arima(yest,order=c(p,0,q))   #Fit model
    fc[i_d,1]      <- forecast(fit,h=hor)$mean[hor]#Get forecast
    fce[i_d,1]     <- fc[i_d,1]-target[i_d]        #Get forecast error
    
  }
  results         <- list()
  results$fc      <- fc
  results$fce     <- fce
  results$target  <- target
  return(results)
}

h_all     <- c(26,52,104)      # Which horizons to consider
lh        <- length(h_all)
mseARMA   <- matrix(NA,lh,3) # Full sample
p = c(2,3,5)
q = c(3,0,4)
parameters = as.data.frame(cbind(p,q))
for (p in 1:3){
  for (i in seq(1,lh)){
    fcARMA             <- in_out_ARMA(h_all[i],parameters[p,1],parameters[p,2])
    mseARMA[i,p]    <- colMeans(fcARMA$fce^2, na.rm = T)
  }
}
rownames(mseARMA)  <- c("26-step","52-step","104-step")
colnames(mseARMA)  <- c('ARMA(2,3)','ARMA(3,0)','ARMA(5,4)')
mseARMA

# Absolute error/MAE

h_all     <- c(26,52,104)      # Which horizons to consider
lh        <- length(h_all)
abeARMA   <- matrix(NA,lh,3)
p = c(2,3,5)
q = c(3,0,4)
parameters = as.data.frame(cbind(p,q))
for (p in 1:3){
  for (i in seq(1,lh)){
    fcARMA             <- in_out_ARMA(h_all[i],parameters[p,1],parameters[p,2])
    abeARMA[i,p]    <- colMeans(abs(fcARMA$fce), na.rm = T)
  }
}
rownames(abeARMA)  <- c("26-step","52-step","104-step")
colnames(abeARMA)  <- c('ARMA(2,3)','ARMA(3,0)','ARMA(5,4)')

abeARMA

######################################### IRF ############################################################

Y           <- cbind(sp500_52week_change_365 , CCIw_365  ,  WEI_365)
colnames(Y) <- c('CCI','SP500', 'WEI' )
VARmodel    <- VAR(Y,p=3,type=c("const"))
roots(VARmodel) # computes eigenvalues of companion matrix


irf_WEI <- irf(VARmodel,impulse=c("SP500"),
               response=c("WEI"),ortho=T, n.ahead = 208)
plot(irf_WEI,plot.type=c("single"))

irf_WEI_CCI <- irf(VARmodel,impulse=c("CCI"),
                   response=c("WEI"),ortho=T, n.ahead = 208)
plot(irf_WEI_CCI,plot.type=c("single"))

######################################## Combined forecast ###############################################

fit_1 <- Arima(WEI_365, order = c(2,0,3))
fARMA_1 <- forecast(fit_1,h=208)
autoplot(fARMA_1) 

Y <- cbind(WEI_365, CCIw_365 , sp500_52week_change_365 )
VAR4 <- VAR(Y,p=3,type = c('const'))
fVAR4 <- forecast(VAR4, h=208)
autoplot(fVAR4$forecast$WEI)
VAR4$varresult$WEI$coefficients

fcombined = matrix(0,length(fARMA_1$mean),6)
for (i in 1:208){
  fcombined[i,2] = 0.5*as.numeric(fVAR4$forecast$WEI_365$mean[i])+0.5*as.numeric(fARMA_1$mean[i])
  fcombined[i,3] = 0.5*as.numeric(fVAR4$forecast$WEI_365$lower[i,1])+0.5*as.numeric(fARMA_1$lower[i,1])
  fcombined[i,4] = 0.5*as.numeric(fVAR4$forecast$WEI_365$lower[i,2])+0.5*as.numeric(fARMA_1$lower[i,2])
  fcombined[i,5] = 0.5*as.numeric(fVAR4$forecast$WEI_365$upper[i,1])+0.5*as.numeric(fARMA_1$upper[i,1])
  fcombined[i,6] = 0.5*as.numeric(fVAR4$forecast$WEI_365$upper[i,2])+0.5*as.numeric(fARMA_1$upper[i,2])
}

combinedForecast_1 = ts( c(as.vector(WEI_365),fcombined[,2]), decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
combinedForecast_low1 = ts( c(as.vector(WEI_365),fcombined[,3]), decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
combinedForecast_low2 = ts( c(as.vector(WEI_365),fcombined[,4]), decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
combinedForecast_high1 = ts( c(as.vector(WEI_365),fcombined[,5]), decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
combinedForecast_high2 = ts( c(as.vector(WEI_365),fcombined[,6]), decimal_date(ymd("2008-01-05")), frequency = 365.25/7)

ts.plot(combinedForecast_low1, combinedForecast_low2, combinedForecast_high1, combinedForecast_high2, combinedForecast_1, 
        col= c('#4842f5','#00b5af','#4842f5', '#00b5af','#000000'), ylab = 'WEI', main = 'Combined Var(3) and ARMA(2,3) froecasts') 
legend('bottomleft', legend = c('95% low', '80 low', '95% high' ,'80% high','forecast'), col =  c('#4842f5','#00b5af','#4842f5', '#00b5af','#000000'), lty=1)

# The calculation of the SSR of the combined model.
fcombined2 = matrix(0,636,2)
for (i in 4:639){
  fcombined2[i-3,2] = 0.5*as.numeric(VAR4$varresult$WEI_365$fitted.values[i-3])+0.5*as.numeric(fit_1$fitted[i])
}
residuals_combined = c()
for(i  in 4:639){
  residuals_combined[i-3] = as.vector(WEI_365)[i] - fcombined2[i-3,2]
}
SSR_c = sum(residuals_combined^2)
SSR_VAR = sum(as.numeric(VAR4$varresult$WEI_365$residuals)^2)
SSR_ARMA = sum(as.numeric(fit_1$residuals)[4:639]^2)
SSR = matrix(c(SSR_c, SSR_VAR, SSR_ARMA),1,3)
rownames(SSR)  <- c("SSR")
colnames(SSR)  <- c('Combined','VAR(3)','ARMA(2,3)')
SSR

############################## This is code we did not use in the paper, but gave different insights #############

# From these plots we suspected non covariance stationarity 
acf(data_1$sp500_52week_change)
pacf(data_1$sp500_52week_change)

acf(data_1$CCIw)
pacf(data_1$CCIw)




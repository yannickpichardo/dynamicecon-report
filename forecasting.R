


#install.packages('vars')
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

acf(data_1$sp500_52week_change)
acf(data_1$CCIw)
pacf(data_1$CCIw)
pacf(data_1$sp500_52week_change)

data_1 <- read.xlsx("WEI.xlsx", sheet = 2, detectDates = TRUE)
data <- read.xlsx("WEI.xlsx", sheet = 2, detectDates = TRUE)
sp500data <- read.csv("GSPC.csv")
sp500_newdata <- read.csv("sp500newdata.csv")

sp500data <- sp500data %>% 
  mutate(average_high_low = (High + Low) / 2)
sp500data <- sp500data %>% 
  mutate(average_open_close = (Open + Close) / 2)

sp500_newdata <- sp500_newdata %>% 
  mutate(average_open_close = (Open + Close) / 2)

data <- data %>% cbind(sp500data$average_open_close)
colnames(data)[11] <- "average_open_close"

BBchange <- PercChange(data = data, Var = "BB", NewVar = "BBchange")
BBchange <- BBchange$BBchange
data$BBchange <- BBchange

M1change <- PercChange(data = data, Var = "M1", NewVar = "M1change")
M1change <- M1change$M1change
data$M1change <- M1change

WEIchange <- PercChange(data = data, Var = "WEI", NewVar = 'WEIchange')
WEIchange <- WEIchange$WEIchange
data$WEIchange <- WEIchange


sp500_52week_change <- PercChange(data = sp500_newdata, Var = "average_open_close", NewVar = "sp500_52week_change", slideBy = -52)
sp500_52week_change <- sp500_52week_change$sp500_52week_change
sp500_52week_change <- sp500_52week_change[!is.na(sp500_52week_change)]
data_1$sp500_52week_change <- sp500_52week_change

sp_500_52week_diff <- diff(sp500_newdata$average_open_close, lag = 52)
data_1$sp_500_52week_diff <- sp_500_52week_diff


WEI       <- ts(data_1$WEI, start = 2008, frequency = 52)

#read CSV file and obtain data from 2007-2020, with values around 0
CCI       <- read.csv('CCI.csv')
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

# Corrplot of all the relevant variables
correlation <- cor(select(data_1, 4:8, 11:13))
corrplot(correlation, method = "color", na.remove = TRUE)

#preparing all time series
WEI_365       <- ts(data_1$WEI, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
CCIw_365       <- ts(data_1$CCIw, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
sp500_52week_change_365        <- ts(data_1$sp500_52week_change, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
sp_500_52week_diff_365       <- ts(data_1$sp_500_52week_diff, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
noise<-ts(rnorm(length(CCIw_365))*sqrt(sd((CCIw_365)/100)),decimal_date(ymd("2008-01-05")),frequency=365.25/7)
CCIn <- CCIw_365+noise

#WEI       <- ts(data_1$WEI, decimal_date(ymd("2008-01-05")), frequency = 52)
#CCIw       <- ts(data_1$CCIw, decimal_date(ymd("2008-01-05")), frequency = 52)
#sp500_52week_change        <- ts(data_1$sp500_52week_change, decimal_date(ymd("2008-01-05")), frequency = 52)
#sp_500_52week_diff       <- ts(data_1$sp_500_52week_diff, decimal_date(ymd("2008-01-05")), frequency = 52)


#forecasting with arma
fit_1 <- Arima(WEI_365, order = c(2,0,3))
fARMA_1 <- forecast(fit_1,h=208)
autoplot(fARMA_1) 

fit_2 <- Arima(WEI, order = c(5,0,4)) #figure 5
fARMA_2 <- forecast(fit_2,h=208)
autoplot(fARMA_2)

fit_3 = Arima(WEI, order = c(52,0,3), fixed=c(NA,NA,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA,NA,NA))
fARMA_3 <- forecast(fit_3,h=208)
autoplot(fARMA_3)


#forecasting with VAR
Y <- cbind(WEI_365, CCIw_365 , sp500_52week_change_365 )
VAR3 <- VAR(Y,p=3,type = c('const'))
fVAR3 <- forecast(VAR3, h=208)
autoplot(fVAR3$forecast$WEI)
VAR3$varresult$WEI$coefficients

#comparing forecasts
autoplot(fARMA_1$mean,series="ARMA(2,3)")+ autolayer(fVAR4$forecast$WEI,series="VAR(4)")+labs(y="WEI")
#+L(CCIw_365,(1:4))
#ARDL model
ARDL4 <- dynlm(WEI_365 ~L(WEI_365,(1:4)) +L(sp500_52week_change_365 ,(1:4)) + L(CCIw_365,(1:4)))
summ<-summary(ARDL4)
print(summ$coefficients,digits=1)


Y <- cbind(WEI_365, CCIw_365, sp500_52week_change_365 )
VAR4 <- VAR(Y,p=3,type = c('const'))
corder1  <- order(names(VAR4$varresult$WEI$coefficients))
corder2  <- order(names(summ$coefficients[,1]))
coefVAR  <- cbind(VAR4$varresult$WEI$coefficients[corder1],
                  summ$coefficients[corder2])
colnames(coefVAR)<- c("VAR(4)","ARDL(4,4,4)")

print(coefVAR,digits=3)

#in and out of sample ARMA
es     <- as.Date("2008/1/5") # Estimation start
fs     <- as.Date("2016/1/2") # First forecast 
fe     <- as.Date("2020/2/1")# Final forecast

maxARp <- 6 # Consider AR(p) models with p=1,...,maxARlag

# Helper function to get dates into helpful format c(yr,qtr)
convert_date <- function(date){
  c(as.numeric(format(date,'%Y')),
    ceiling(as.numeric(format(date,'%W')))) 
  # Use %W for weeks and do not divide by 3.
}



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

# Absolute error

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


#IRF analysis
Y           <- cbind(sp500_52week_change_365 , CCIw_365  ,  WEI_365)
colnames(Y) <- c('CCI','SP500', 'WEI' )
VARmodel    <- VAR(Y,p=3,type=c("const"))
roots(VARmodel) # computes eigenvalues of companion matrix



irf_WEI <- irf(VARmodel,impulse=c("SP500"),
               response=c("WEI"),ortho=T, n.ahead = 208)
plot(irf_WEI,plot.type=c("single"))

irf_CCI <- irf(VARmodel,impulse=c("SP500"),
               response=c("CCI"),ortho=T, n.ahead = 208)
plot(irf_CCI,plot.type=c("single"))

irf_WEI_CCI <- irf(VARmodel,impulse=c("CCI"),
                   response=c("WEI"),ortho=T, n.ahead = 208)
plot(irf_WEI_CCI,plot.type=c("single"))


Y           <- cbind(CCIw_365  , sp500_52week_change_365 , WEI_365)
colnames(Y) <- c('CCI', 'SP500', 'WEI')
VARmodel_ic <- VARselect(Y,type=c("const"),lag.max=8)
ic          <- as.data.frame(t(VARmodel_ic$criteria))
ic
ggplot(data=ic, aes(x=seq(1,8),y=`SC(n)`))+geom_line()+ylab("BIC")+xlab("VAR(p)")
ggplot(data=ic, aes(x=seq(1,8),y=`AIC(n)`))+geom_line()+ylab("AIC")+xlab("VAR(p)")

#restricted VAR
p1        <- 6;
VARr     <- VAR( Y,p=p1,type=c("const"))
nseries  <- 3;
#mones    <- matrix(1,nrow = nseries,ncol=nseries) 
#mzero    <- matrix(0,nrow = nseries,ncol=nseries) 
vones    <- matrix(1,nrow = nseries,ncol=1)
lag1mat <- matrix(c(1, 1, 1,
                    1, 1, 1,
                    1, 1, 1)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE) # lag matrix cols = cci, sp500 and WEI. Rows are the same but indicate the equation. E.g. if [1,3] = 1 then the CCI equation will include lag 1 of the WEI
lag2mat <- matrix(c(0, 0, 0,
                    0, 0, 0,
                    0, 0, 0)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag3mat <- matrix(c(1, 1, 1,
                    1, 1, 1,
                    1, 1, 1)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag4mat <- matrix(c(0, 0, 0,
                    0, 0, 0,
                    0, 0, 0)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag5mat <- matrix(c(1, 1, 1,
                    1, 1, 1,
                    1, 1, 1)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag6mat <- matrix(c(0, 0, 0,
                    0, 0, 0,
                    0, 0, 0)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag7mat <- matrix(c(1, 1, 1,
                    1, 1, 1,
                    1, 1, 1)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag8mat <- matrix(c(0, 0, 0,
                    0, 0, 0,
                    0, 0, 0)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
lag9mat <- matrix(c(1, 1, 1,
                    1, 1, 1,
                    1, 1, 1)
                  ,nrow = nseries,ncol=nseries, byrow = TRUE)
restrict <- matrix(cbind(lag1mat, lag2mat, lag3mat, lag4mat, lag5mat, lag6mat,  vones), nrow = 3, ncol = p1*3+1) # order is: lag 1, ..., lag p and then the constant


VARr     <- restrict(VARr, method = "man", resmat = restrict)

# Somehow BIC has to be calculated by hand
resid    <- residuals(VARr)
T        <- length(resid[,1])
BIC      <- log(det(t(resid)%*%resid/T)) + (log(T)/T)*sum(restrict)
BIC

fVARr <- forecast(VARr, h=200)
autoplot(fVARr$forecast$WEI)
VARr$varresult$WEI$coefficients
# You can check that now the third lag is omitted by typing
summary(VARr)
roots(VARr)

irf_WEI <- irf(VARr,impulse=c("SP500"),
               response=c("WEI"),ortho=T, n.ahead = 300)
plot(irf_WEI,plot.type=c("single"))

irf_CCI <- irf(VARr,impulse=c("SP500"),
               response=c("CCI"),ortho=T, n.ahead = 300)
plot(irf_CCI,plot.type=c("single"))

irf_WEI_CCI <- irf(VARr,impulse=c("CCI"),
                   response=c("WEI"),ortho=T, n.ahead = 300)
plot(irf_WEI_CCI,plot.type=c("single"))


#Ftest <- matrix(NA,4,2)
#lags <- 4 # number of lags
#nvar <- 3 # number of variables
#for (i in seq(4)){
#  y    <- ardl.list[[i]]$residuals
#  T    <- length(y)
#  # Fit ARDL models with and without lags of y
#  fit1 <- dynlm(y ~ L(y,(1:lags)) + L(dgnp_T,(1:i)) + L(ddef_T,(1:i)) + L(ffr_T,(1:i)))
#  fit2 <- dynlm(y ~                 L(dgnp_T,(1:i)) + L(ddef_T,(1:i)) + L(ffr_T,(1:i)))
#  SSR1 <- sum(fit1$residuals^2)
#  SSR0 <- sum(fit2$residuals^2)
#  Ftest[i,1] <- ((SSR0-SSR1)/lags)/(SSR1/(T-lags-nvar*i))
#  Ftest[i,2] <- qf(0.95,lags,T-lags-nvar*i)
#}
#print(Ftest)
#

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

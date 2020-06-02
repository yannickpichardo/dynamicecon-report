library("DataCombine")
library("corrplot")
library("tidyverse")
library("dplyr")
library("openxlsx")
library("hexbin")
library("mapproj")
library("viridisLite")
library("viridis")
library("RColorBrewer")
library("lattice")
library("microbenchmark")
library("moments")
library("stats")
library("faraway")
library("ggpubr")
library("wordcloud")
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

#preparing all time series
WEI_365       <- ts(data_1$WEI, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
CCIw_365       <- ts(data_1$CCIw, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
sp500_52week_change_365        <- ts(data_1$sp500_52week_change, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)
sp_500_52week_diff_365       <- ts(data_1$sp_500_52week_diff, decimal_date(ymd("2008-01-05")), frequency = 365.25/7)

WEI       <- ts(data_1$WEI, decimal_date(ymd("2008-01-05")), frequency = 52)
CCIw       <- ts(data_1$CCIw, decimal_date(ymd("2008-01-05")), frequency = 52)
sp500_52week_change        <- ts(data_1$sp500_52week_change, decimal_date(ymd("2008-01-05")), frequency = 52)
sp_500_52week_diff       <- ts(data_1$sp_500_52week_diff, decimal_date(ymd("2008-01-05")), frequency = 52)


#forecasting with arma
fit_1 <- Arima(WEI, order = c(2,0,3))
fARMA_1 <- forecast(fit_1,h=200)
autoplot(fARMA_1)

fit_2 <- Arima(WEI, order = c(5,0,4)) #figure 5
fARMA_2 <- forecast(fit_2,h=200)
autoplot(fARMA_2)

fit_3 = Arima(WEI, order = c(52,0,3), fixed=c(NA,NA,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                              0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA,NA,NA))
fARMA_3 <- forecast(fit_3,h=200)
autoplot(fARMA_3)


#forecasting with VAR
Y <- cbind(WEI_365, CCIw_365, sp500_52week_change_365 )
VAR4 <- VAR(Y,p=3,type = c('const'))
fVAR4 <- forecast(VAR4, h=200)
autoplot(fVAR4$forecast$WEI)
VAR4$varresult$WEI$coefficients

#comparing forecasts
autoplot(fARMA_1$mean,series="ARMA(2,3)")+ autolayer(fVAR4$forecast$WEI,series="VAR(4)")+labs(y="WEI")
#+L(CCIw_365,(1:4))
#ARDL model
ARDL4 <- dynlm(WEI_365 ~L(WEI_365,(1:4)) +L(sp500_52week_change_365 ,(1:4)) + L(CCIw_365,(1:4)))
summ<-summary(ARDL4)
print(summ$coefficients,digits=1)


Y <- cbind(WEI_365, CCIw_365, sp500_52week_change_365 )
VAR4 <- VAR(Y,p=4,type = c('const'))
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


# TEST IF LOOP IS WRONG OR CODE IN LOOP IS WRONG
#dates   <- seq(fs,fe,by="week")
#qF      <- convert_date(fs)
#qL      <- convert_date(fe)
#target  <- window(WEI,start=qF,end=qL)
#est   <- seq(dates[20],length=40+1, by = "-1 week")[40+1]
#yest = window(WEI,end=convert_date(est))
#fit <- Arima(yest,order=c(3,0,0))
#fc    <- ts(data=matrix(NA,length(dates),3),start=qF,frequency=365.25/7)
#fce   <- ts(data=matrix(NA,length(dates),3),start=qF,frequency=365.25/7)
#fc[20,3] <- forecast(fit,h=40)$mean[40]
#fc
#fce[20,3]     <- fc[20,3]-target[20]
#fce

forecastARMA <- function(y,es,fs,fe,maxARp,hor){
  dates   <- seq(fs,fe,by="week") # (or "week"...)
  n       <- length(dates)                 # number of forecasts
  qF      <- convert_date(fs)
  qL      <- convert_date(fe)
  target  <- window(y,start=qF,end=qL)     # What we are forecasting.
  
  # Define ts objects where forecasts/forecast errors are saved.
  # (Note that frequency=4 applies to quarterly data!)
  fc    <- ts(data=matrix(NA,n,maxARp),start=qF,frequency=365.25/7)
  fce   <- ts(data=matrix(NA,n,maxARp),start=qF,frequency=365.25/7)
  
  for (i_d in seq(1,n)){
    # Define estimation sample (ends h periods before 1st forecast)
    # Start at the first forecast date, 
    # Then move back h+1 quarters back in time
    est   <- seq(dates[i_d],length=hor+1, by = "-1 week")[hor+1]
    # Now define the data we can use to estimate the model
    yest  <- window(y,end=convert_date(est))
    # Fit the AR models using Arima
    for (j in seq(1,maxARp)){
      fit            <- Arima(yest,order=c(j,0,0))   #Fit model
      fc[i_d,j]      <- forecast(fit,h=hor)$mean[hor]#Get forecast
      fce[i_d,j]     <- fc[i_d,j]-target[i_d]        #Get forecast error
    }
  }
  results         <- list()
  results$fc      <- fc
  results$fce     <- fce
  results$target  <- target
  return(results)
}




#fcARMA             <- forecastARMA(WEI,es,fs,fe,maxARp,h_all[1])
#fcARMA
#mseARMA[1,] = colMeans(fcARMA$fce^2, na.rm = T)
#mseARMA
h_all     <- c(26,52,200)      # Which horizons to consider
lh        <- length(h_all)
mseARMA   <- matrix(NA,lh,maxARp) # Full sample
for (i in seq(1,lh)){
  fcARMA             <- forecastARMA(WEI,es,fs,fe,maxARp,h_all[i])
  mseARMA[i,]    <- colMeans(fcARMA$fce^2, na.rm = T)
}
colnames(mseARMA)  <- c("AR(1)","AR(2)","AR(3)","AR(4)","AR(5)","AR(6)")
rownames(mseARMA)  <- c("26-step","52-step","200-step")

mseARMA

#in and out of sample ARDL

forecastARDL <- function(y,X,es,fs,fe,maxARp,hor){
  dates   <- seq(fs,fe,by="week") # (or "week"...)
  n       <- length(dates)                 # number of forecasts
  qF      <- convert_date(fs)
  qL      <- convert_date(fe)
  target  <- window(y,start=qF,end=qL)     # What we are forecasting.
  
  # Define ts objects where forecasts/forecast errors are saved.
  # (Note that frequency=4 applies to quarterly data!)
  fc    <- ts(data=matrix(NA,n,maxARp),start=qF,frequency=365.25/7)
  fce   <- ts(data=matrix(NA,n,maxARp),start=qF,frequency=365.25/7)
  
  for (i_d in seq(1,n)){
    # Define estimation sample (ends h periods before 1st forecast)
    # Start at the first forecast date, 
    # Then move back h+1 quarters back in time
    est   <- seq(dates[i_d],length=hor+1, by = "-1 week")[hor+1]
    # Now define the data we can use to estimate the model
    Y = cbind(y,X)
    Yest  <- window(Y,end=convert_date(est))
    # Fit the AR models using Arima
    for (j in seq(1,maxARp)){
      fit            <- VAR(Yest,p=j,type=c('const'))   #Fit model
      fc[i_d,j]      <- forecast(fit,h=hor)$forecast$y$mean[hor]#Get forecast
      fce[i_d,j]     <- fc[i_d,j]-target[i_d]        #Get forecast error
    }
  }
  results         <- list()
  results$fc      <- fc
  results$fce     <- fce
  results$target  <- target
  return(results)
}

# Get forecasts
X_SP <- cbind(WEI_365,sp500_52week_change_365 )
X_CCI <- cbind(WEI_365,CCIw_365)
fcARDLh1_SP  <- forecastARDL(WEI_365,X_SP,es,fs,fe,maxARp,1)
fcARDLh1_CCI  <- forecastARDL(WEI_365,X_CCI,es,fs,fe,maxARp,1)

# Calculate MSE and compare
mseARDL_SP     <- colMeans(fcARDLh1_SP$fce^2)
mseARDL_CCI     <- colMeans(fcARDLh1_CCI$fce^2)
compare_SP     <- rbind(mseARMA[1,],mseARDL_SP)
compare_CCI     <- rbind(mseARMA[1,],mseARDL_CCI)
rownames(compare_SP) <- c("AR","ARDL")
colnames(compare_SP) <- c("p=1","p=2","p=3","p=4",'p=5','p=6')
rownames(compare_CCI) <- c("AR","ARDL")
colnames(compare_CCI) <- c("p=1","p=2","p=3","p=4",'p=5','p=6')
round(compare_SP,digits=3)
round(compare_CCI,digits=3)


#IRF analysis
Y           <- cbind(sp500_52week_change_365 , CCIw_365,  WEI_365)
colnames(Y) <- c('CCI','SP500', 'WEI' )
VARmodel    <- VAR(Y,p=4,type=c("const"))
roots(VARmodel) # computes eigenvalues of companion matrix



irf_WEI <- irf(VARmodel,impulse=c("SP500"),
               response=c("WEI"),ortho=T, n.ahead = 300)
plot(irf_WEI,plot.type=c("single"))

irf_CCI <- irf(VARmodel,impulse=c("SP500"),
               response=c("CCI"),ortho=T, n.ahead = 300)
plot(irf_CCI,plot.type=c("single"))

irf_WEI_CCI <- irf(VARmodel,impulse=c("CCI"),
               response=c("WEI"),ortho=T, n.ahead = 300)
plot(irf_WEI_CCI,plot.type=c("single"))


Y           <- cbind(CCIw_365, sp500_52week_change_365 , WEI_365)
colnames(Y) <- c('CCI', 'SP500', 'WEI')
VARmodel_ic <- VARselect(Y,type=c("const"),lag.max=8)
ic          <- as.data.frame(t(VARmodel_ic$criteria))
ic
ggplot(data=ic, aes(x=seq(1,8),y=`SC(n)`))+geom_line()+ylab("BIC")+xlab("VAR(p)")
ggplot(data=ic, aes(x=seq(1,8),y=`AIC(n)`))+geom_line()+ylab("AIC")+xlab("VAR(p)")


#restricted VAR
p        <- 6;
VARr     <- VAR(Y,p=6,type=c("const"))
nseries  <- 3;
mones    <- matrix(1,nrow = nseries,ncol=nseries) 
mzero    <- matrix(0,nrow = nseries,ncol=nseries) 
vones    <- matrix(1,nrow = nseries,ncol=1)
restrict <- cbind(mones,mones,mzero,mones,mzero,mones,vones) # order is: lag 1, ..., lag p and then the constant
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



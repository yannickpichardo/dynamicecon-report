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
WEI       <- ts(data_1$WEI, decimal_date(ymd("2008-01-05")), frequency = 52)
CCIw       <- ts(data_1$CCIw, decimal_date(ymd("2008-01-05")), frequency = 52)
sp500_52week_change       <- ts(data_1$sp500_52week_change, decimal_date(ymd("2008-01-05")), frequency = 52)
sp_500_52week_diff      <- ts(data_1$sp_500_52week_diff, decimal_date(ymd("2008-01-05")), frequency = 52)

#forecasting with arma
fit_1 <- Arima(WEI, order = c(2,0,3))
fARMA_1 <- forecast(fit_1,h=200)
autoplot(fARMA_1)

#forecasting with VAR
Y <- cbind(WEI, CCIw, sp_500_52week_diff)
VAR4 <- VAR(Y,p=4,type = c('const'))
fVAR4 <- forecast(VAR4, h=200)
autoplot(fVAR4$forecast$WEI)
VAR4$varresult$WEI$coefficients

#comparing forecasts
autoplot(fARMA_1$mean,series="ARMA(2,3)")+ autolayer(fVAR4$forecast$WEI,series="VAR(4)")+labs(y="WEI")

#ARDL model
ARDL4 <- dynlm(WEI ~L(WEI,(1:4)) +
                 L(CCIw,(1:4))+L(sp_500_52week_diff,(1:4)))
summ<-summary(ARDL4)
print(summ$coefficients,digits=1)


Y <- cbind(WEI, CCIw, sp_500_52week_diff)
VAR4 <- VAR(Y,p=4,type = c('const'))
corder1  <- order(names(VAR4$varresult$WEI$coefficients))
corder2  <- order(names(summ$coefficients[,1]))
coefVAR  <- cbind(VAR4$varresult$WEI$coefficients[corder1],
                  summ$coefficients[corder2])
colnames(coefVAR)<- c("VAR(4)","ARDL(4,4)")

print(coefVAR,digits=1)

#in and out of sample
es     <- as.Date("2008/1/5") # Estimation start
fs     <- as.Date("2016/1/2") # First forecast 
fe     <- as.Date("2020/2/1")# Final forecast
cs     <- c(2008,1)           # Starting date for crisis period analysis
ce     <- c(2012,4)           # End date for crisis period analysis
maxARp <- 4 # Consider AR(p) models with p=1,...,maxARlag

# Helper function to get dates into helpful format c(yr,qtr)
convert_date <- function(date){
  c(as.numeric(format(date,'%Y')),
    ceiling(as.numeric(format(date,'%W')))) 
  # Use %W for weeks and do not divide by 3.
}

forecastARMA <- function(y,es,fs,fe,maxARp,hor){
  dates   <- seq(fs,fe,by="week") # (or "week"...)
  n       <- length(dates)                 # number of forecasts
  qF      <- convert_date(fs)
  qL      <- convert_date(fe)
  target  <- window(y,start=qF,end=qL)     # What we are forecasting.
  
  # Define ts objects where forecasts/forecast errors are saved.
  # (Note that frequency=4 applies to quarterly data!)
  fc    <- ts(data=matrix(NA,n,maxARp),start=qF,end=qL,frequency=52)
  fce   <- ts(data=matrix(NA,n,maxARp),start=qF,end=qL,frequency=52)
  
  for (i_d in seq(1,n)){
    # Define estimation sample (ends h periods before 1st forecast)
    # Start at the first forecast date, 
    # Then move back h+1 quarters back in time
    est   <- seq(dates[i_d],length=hor+1, by = "-1 quarter")[hor+1]
    # Now define the data we can use to estimate the model
    yest  <- window(y,start=convert_date(es),end=convert_date(est))
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

h_all     <- c(12,26,52)      # Which horizons to consider
lh        <- length(h_all)
mseARMA   <- matrix(NA,lh,maxARp) # Full sample
mseARMAc  <- matrix(NA,lh,maxARp) # Crisis only
for (i in seq(1,lh)){
  fcARMA             <- forecastARMA(WEI,es,fs,fe,maxARp,h_all[i])
  mseARMA[i,]    <- colMeans(fcARMA$fce^2)
  # Focusing on the crisis period
  crisis             <- window(WEI$fce,start=cs,end=ce)
  mseARMAc[i,]   <- colMeans(crisis^2)
}
colnames(mseARMA)  <- c("AR(1)","AR(2)","AR(3)","AR(4)")
rownames(mseARMA)  <- c("1-step","4-step","12-step")
colnames(mseARMAc) <- c("AR(1)","AR(2)","AR(3)","AR(4)")
rownames(mseARMAc) <- c("1-step","4-step","12-step")


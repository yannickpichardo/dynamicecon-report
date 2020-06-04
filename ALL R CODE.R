###################### ALL THE R CODE WE USED ####################################################

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

acf(data_1$sp500_52week_change)
pacf(data_1$sp500_52week_change)

acf(data_1$WEI)
pacf(data_1$WEI)

acf(data_1$CCIw)
pacf(data_1$CCIw)

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

Y           <- cbind(CCIw_365, sp500_52week_change_365 , WEI_365)
colnames(Y) <- c('CCI', 'SP500', 'WEI')
VARmodel_ic <- VARselect(Y,type=c("const"),lag.max=8)
ic          <- as.data.frame(t(VARmodel_ic$criteria))
ic
ggplot(data=ic, aes(x=seq(1,8),y=`SC(n)`))+geom_line()+ylab("BIC")+xlab("VAR(p)")
ggplot(data=ic, aes(x=seq(1,8),y=`AIC(n)`))+geom_line()+ylab("AIC")+xlab("VAR(p)")

############################################   ##########################################################

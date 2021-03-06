
install.packages("forecast")
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


install.packages("tseries")

## Data manipulation ##
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

plot240 <- ggplot(data = data_1) + 
  geom_line(mapping = aes(x = Date, y = sp_500_52week_diff / 100), colour = "red") +
  geom_line(mapping = aes(x = Date, y = WEI), colour = "blue") + 
  geom_line(aes(x = Date, y = 0), colour = "black")
plot240

plot239 <- ggplot(data = data_1) +
  geom_line(mapping = aes(x = Date, y = sp500_52week_change / 10), colour = "red") + 
  geom_line(mapping = aes(x = Date, y = WEI), colour = "blue") +
  geom_hline(yintercept = 0, colour = 'black') + 
  ggtitle("WEI vs S&P500 52 week percentage change") + 
  ylab("S&P500 scaled by 10")
plot239

correlation <- cor(select(data_1, 4:8, 11:12))
print(correlation)
corrplot(correlation, method = "color", na.remove = TRUE)

#SP500 zorgen dat je een 52 weken percentage change difference neemt, die plotten tegenover WEI zonder veranderingen.
#Oil price, 52 weken percentage change dan verschillen over 52 weken (misschien doet deze stap te weinig voor verlies aan data, dit zelf bekijken)
#(vanwege inflatie)
#midas modellen



SP500change <- PercChange(data = data, Var = "S&P500", NewVar = "SP500change")
SP500change <- SP500change$SP500change
data$SP500change <- SP500change

sp500_perc_change<- PercChange(data = data, Var = "average_open_close", NewVar = "sp500_perc_change")
sp500_perc_change <- sp500_perc_change$sp500_perc_change
data$sp500_perc_change <- sp500_perc_change

data$lnSP500 <- log(data$`S&P500`)
data$lnBB <- log(data$BB)
data$lnM1 <- log(data$M1)
data$lnOil <- log(data$Oil)

WEI_time_series_change <- diff(data$WEI)
WEI_time_series_change <- append(WEI_time_series_change, 0, after = 0)
data <- data %>% cbind(WEI_time_series_change)


## Plotting several variables against the WEI to identify some correlation ##

plot1 <- ggplot(data = data, mapping = aes(x = BB, y = WEI)) +
  geom_point()
plot1

plot2 <- ggplot(data = data, mapping = aes(x = T10Y3M, y = WEI)) +
  geom_point() 
plot2

plot3 <- ggplot(data = data, mapping = aes(x = M1, y = WEI)) +
  geom_point()
plot3

plot4 <- ggplot(data = data) +
  geom_line(mapping = aes(x = Date, y = WEI), color = "blue") +
  geom_line(mapping = aes(x = Date, y = T10Y3M, color = "red"))
plot4

plot5 <- ggplot(data = data) +
  geom_line(mapping = aes(x = Date, y = WEI), color = "blue", label = "WEI") +
  geom_line(mapping = aes(x = Date, y = sp500_perc_change, color = "red", label = "Change in BB")) +
  geom_line(mapping = aes(x = Date, y = T10Y3M, color = "black", label = "Bond rates")) + 
  geom_line(aes(x = Date, y = 0))
plot5

plot6 <- ggplot(data = data) +
  geom_line(aes(x = Date, y = lnOil, color = "red")) +
  geom_line(aes(x = Date, y = lnSP500, color = "blue")) +
  geom_line(aes(x = Date, y = lnBB, color = "black")) +
  geom_line(aes(x = Date, y = lnM1, color = "white"))
plot6

plot7 <- ggplot(data = data, aes(x = Date)) + 
  geom_line(aes(y = M1change, color = "Money supply")) +
  geom_line(aes(y =WEIchange, colour = "WEI"))+
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Money supply"="blue", "WEI"="green")) +
  ggtitle("WEI vs Money supply percentage change") +
  ylab("WEI and Money supply percentage change") 
plot7

plot8 <- ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y =BBchange, colour = "Bank borrowings")) +
  geom_line(aes(y =WEI, colour = "WEI"))+ 
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Bank borrowings"="blue", "WEI"="green")) +
  ggtitle("WEI vs Bank borrowings percentage change") +
  ylab("WEI and the BB percentage percentage change")  
plot8

plot9 <- ggplot(data = data) +
  geom_line(aes(x = Date, y = BBchange, color = "darkred")) + 
  geom_line(aes(x = Date, y = M1change, color = "lightblue"))
plot9

plot10 <- ggplot(data = data) +
  geom_line(aes(x = Date, y = WEI, color = "Red")) + 
  geom_line(aes(x = Date, y = SP500change, color = "Blue"))
plot10

plot11 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = Oil*10, color = "red")) + 
  geom_line(aes(x = Date, y = WEI*100, color = "blue"))
plot11

plot12 <- ggplot(data = data) +
  geom_line(aes(x = Date, y = WEI*1000, color = "red")) +
  geom_line(aes(x = Date, y = `S&P500`*10, color = "blue"))
plot12

plot13 <- ggplot(data = data) + 
  geom_line(mapping = aes(x = Date, y = BBchange, color = "red"))
plot13

plot14 <- ggplot(data = sp500data) + 
  geom_line(aes(x = Date, y = average_high_low, group = 1, color = "darkred")) +
  geom_line(aes(x = Date, y = average_open_close, group = 1, color = "lightblue"))
plot14

plot15 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = SP500change, color = "darkred")) + 
  geom_line(aes(x = Date, y = sp500_perc_change, color = "lightblue"))
plot15


plot16 <- ggplot(data = data, aes(x= Date)) + 
  geom_line(aes(y = sp500_perc_change, colour = "S&P500")) + 
  geom_line(aes(y = WEI, colour = "WEI")) +
  geom_hline(yintercept = 0, colour = 'black') + scale_colour_manual("", values = c("S&P500"="blue", "WEI"="green")) +
  ggtitle("The WEI vs S&P500 percentage changes") +
  ylab("WEI and S&P500 percentage changes") 
plot16

data[(which.min(data$WEIchange) - 1):(which.min(data$WEIchange) + 1), ]
plot(x = data$Date, y = data$WEI, type = "l")
data(length(data$WEIchange - 10))

par(mfcol = c(2,2))
plot(data$BB ,data$WEI)
plot(data$T10Y3M, data$WEI)
plot(data$M1, data$WEI)



plot18 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = BBchange / 10, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs BB growth rate")
plot18

plot19 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = M1change / 2, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs M1 growth rate")
plot19

plot20 <- ggplot(data = data[560:630, ]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = diff_oil_price / 10, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("diff in WEI vs diff in Oil price")
plot20

plot17 <- ggplot(data = data[560:628, ]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = SP500_diff_change / 50, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs S&P500 growth rate")
plot17

plottest <- ggplot(data = data[560:628, ]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = sp500_perc_change / 5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs S&P500 growth rate")
plottest

plothoertje <- ggplot(data = data[500:600, ]) + 
  geom_line(aes(x = Date, y = sp500_perc_change, color = "darkred")) +
  geom_line(aes(x = Date, y = SP500_diff_change / 50, color = 'lightblue'))
plothoertje

  plot17 <- ggplot(data = data, aes(x = Date)) + 
  geom_line(aes(y =WEI_difference100, colour = "WEI difference scaled by 100")) + 
  geom_line(aes(y =SP500_time_series_change, colour = "S&P500 difference")) + 
  geom_hline(yintercept = 0, colour = 'black') + scale_colour_manual("", values = c("WEI difference scaled by 100"= "red",
                                                                                    "S&P500 difference"= "blue")) +
  ggtitle("Difference within WEI vs S&P500 2008-2020") + ylab("Difference with WEI scaled by 100")
plot21

plot_WEI_SP500 <- ggplot(data = data_1, aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500 52 week %change scaled by 10")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500 52 week %change scaled by 10" = "blue")) + 
  ggtitle("WEI vs 52 week % change of S&P500") + 
  ylab("WEI and 52 week % change S&P500") + 
  xlab("Date")
plot_WEI_SP500

plot_WEI_SP500_2008_2010 <- ggplot(data = data_1[1:105, ], aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500 52 week %change scaled by 10")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500 52 week %change scaled by 10" = "blue")) + 
  ggtitle("WEI vs 52 week % change of S&P500 in 2008-2010") + 
  ylab("WEI and 52 week % change S&P500") + 
  xlab("Date")
plot_WEI_SP500_2008_2010

plot_WEI_SP500_before_covid <- ggplot(data = data_1[560:630, ], aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500 52 week %change")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500 52 week %change" = "blue")) + 
  ggtitle("WEI vs 52 week % change of S&P500 before COVID-19") + 
  ylab("WEI and 52 week % change S&P500") + 
  xlab("Date")
plot_WEI_SP500_before_covid

plot_WEI_SP500_during_covid <- ggplot(data = data_1[630:639, ], aes(x = Date)) +
  geom_line(aes(y = WEI, colour = "WEI")) + 
  geom_line(aes(y = sp500_52week_change / 10, colour = "S&P500 52 week %change")) +
  geom_hline(yintercept = 0, colour = 'black') +
  scale_color_manual("", values = c("WEI" = "green", "S&P500 52 week %change" = "blue")) + 
  ggtitle("WEI vs 52 week % change of S&P500 before COVID-19") + 
  ylab("WEI and 52 week % change S&P500") + 
  xlab("Date")
plot_WEI_SP500_during_covid

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

plot17_2008_2010 <- ggplot(data = data[1:63,], aes(x = Date)) + 
  geom_line(aes(y =WEI_difference100, colour = "WEI difference scaled by 100")) + 
  geom_line(aes(y =SP500_time_series_change, colour = "S&P500 difference")) + 
  geom_hline(yintercept = 0, colour = 'black') + scale_colour_manual("", values = c("WEI difference scaled by 100"= "red",
                                                                                    "S&P500 difference"= "blue")) +
  ggtitle("Difference within WEI vs S&P500 2008-2010") + ylab("Difference with WEI scaled by 100")
plot17_2008_2010

plot17_before_covid <- ggplot(data = data[560:630,], aes(x = Date)) + 
  geom_line(aes(y =WEI_difference100, colour = "WEI difference scaled by 100")) + 
  geom_line(aes(y =SP500_time_series_change, colour = "S&P500 difference")) + 
  geom_hline(yintercept = 0, colour = 'black') + scale_colour_manual("", values = c("WEI difference scaled by 100"= "red",
                                                                                    "S&P500 difference"= "blue")) +
  ggtitle("Difference within WEI vs S&P500 before COVID-19") + ylab("Difference with WEI scaled by 100")
plot17_before_covid

plot17_during_covid <- ggplot(data = data[630:639,], aes(x = Date)) + 
  geom_line(aes(y =WEI_difference100, colour = "WEI difference scaled by 100")) + 
  geom_line(aes(y =SP500_time_series_change, colour = "S&P500 difference")) + 
  geom_hline(yintercept = 0, colour = 'black') + scale_colour_manual("", values = c("WEI difference scaled by 100"= "red",
                                                                                    "S&P500 difference"= "blue")) +
  ggtitle("Difference within WEI vs S&P500 during COVID-2019") + ylab("Difference with WEI scaled by 100")
plot17_during_covid 


## Gekloot met time series ##

?autoplot

BB_time_series = ts(data[, 9], start = c(2008), frequency = 365.25/7)
BB_ts_change = diff(BB_time_series)
data$BB_t
?rep
y <- data.frame(rep(c(0), times = 639))

WEI_time_series = ts(data[,4], start= c(2008), frequency = 365.25/7)
WEI_ts_change = diff(WEI_time_series)
time_series_data <- data.frame(WEI_ts_change)
data$WEI_time_series <- WEI_time_series

WEI_time_series_change <- diff(data$WEI)
WEI_time_series_change <- append(WEI_time_series_change, 0, after = 0)
data <- data %>% cbind(WEI_time_series_change)

ggplot(data = data) + 
  geom_line(aes(x = Date, y = diff(WEI)))

autoplot(diff(BB))
p1 <- autoplot(diff(WEI))
plot(x = data$Date, y = data$WEI)
plot(x = data$Date, y = diff(data$WEI))

p1$layers +
  geom_line(data = data, 
            mapping = aes(x = Date, y = sp500_perc_change),
            inherit.aes = F)

een_variabele_naam <- cbind(diff(WEI), diff(BB_time_series / 100000))
plot.ts(een_variabele_naam, plot.type = "single", col = c("blue", "red"))

#########################################################################

## Correlation matrix and plots ##

x <- c(cor(data$WEI, data$T10Y3M), cor(data$WEI, data$BB), cor(data$WEI, data$M1))
print(x)
cor(data[4:10])
corrplot(data[4:10], type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
cor(data[4:11])
cor(data[-1])
cor.test(x = data$WEI, y = data$`S&P500`, method=c("pearson", "kendall", "spearman"))

data_2 <- data_1
colnames(data_2)[11:13] <- c("S&P500 %change" ,"S&P500 difference", "CCI")
cor_all <- cor(select(data_2, 4:8, 11:13)) 
corrplot(cor_all, method = "color", na.rm = T)
x <- as.data.frame(cor(data_2[11:13]))
y <- as.data.frame(cor(data_2[4:8]))
z <- as.data.frame(rbind(x, y))
z <- cbind(data_2[4:8], data_2[11:13])
Z <- cor(z)
corrplot(Z, method = "color")

## Regressing several variables to identify statsitical significance ##

model1 <- lm(WEI ~ T10Y3M + BBchange, data = data)
summary(model1)
anova(model1)
plot(model1)
summary(data)

model2 <- lm(WEI ~ T10Y3M, data = data)
summary(model2)

model3 <- lm(WEI ~ `S&P500` + Oil + FFR, data = data)
summary(model3)
plot(model3)

model4 <- lm(WEI ~ lnSP500 + Oil + FFR, data = data)
summary(model4)
plot(model4)

model5 <- lm(WEI ~ lnSP500 + lnBB + lnM1 + lnOil, data = data)
summary(model5)


model6 <- lm(WEI ~ lnOil + FFR + T10Y3M, data = data)
summary(model5)

## Autocorrelation models (acf and pacf) ##

?acf
acf(data$sp500_perc_change, na.action = na.pass)
pacf(data$sp500_perc_change, na.action = na.pass)

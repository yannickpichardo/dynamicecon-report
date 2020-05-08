
install.packages("DataCombine")
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

## Data manipulation ##

data <- read.xlsx("WEI.xlsx", sheet = 2, detectDates = TRUE)
sp500data <- read.csv("GSPC.csv")

sp500data <- sp500data %>% 
  mutate(average_high_low = (High + Low) / 2)
sp500data <- sp500data %>% 
  mutate(average_open_close = (Open + Close) / 2)

data <- data %>% cbind.(sp500data$average_open_close)
colnames(data)[20] <- "average_open_close"

BBchange <- PercChange(data = data, Var = "BB", NewVar = "BBchange")
BBchange <- BBchange$BBchange
data$BBchange <- BBchange

M1change <- PercChange(data = data, Var = "M1", NewVar = "M1change")
M1change <- M1change$M1change
data$M1change <- M1change

WEIchange <- PercChange(data = data, Var = "WEI", NewVar = 'WEIchange')
WEIchange <- WEIchange$WEIchange
data$WEIchange <- WEIchange


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

## Plotting several variables against the WEI to identify some correlation ##

plot1 <- ggplot(data = data, mapping = aes(x = BB, y = WEI)) +
            geom_point()
plot1

plot2 <- ggplot(data = data, mapping = aes(x = T10Y3M, y = wEI)) +
            geom_point() 
plot2

plot3 <- ggplot(data = data, mapping = aes(x = M1, y = WEI)) +
            geom_point()
plot3

plot4 <- ggplot(data = data) +
  geom_line(mapping = aes(x = Date, y = WEI), color = "blue") +
  geom_line(mapping = aes(x = Date, y = T10Y30M, color = "red"))
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

plot7 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = WEI, color = "darkred")) +
  geom_line(aes(x = Date, y = M1change, color = "lightblue")) +
  geom_line(aes(x = Date, y = 0, color = "black")) +
  ggtitle("WEI vs Money supply growth") +
  ylab("WEI and the M1 growth") + 
  theme(legend.position = "none")
plot7

plot8 <- ggplot(data = data) +
  geom_line(aes(x = Date, y = WEI, color = "darkred")) +
  geom_line(aes(x = Date, y = BBchange, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "black")) + 
  ggtitle("WEI vs Bank borrowings growth") +
  ylab("WEI and the BB growth") + 
  theme(legend.position = 'none')
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
  geom_line(mapping = aes(x = Date, y = WEIchange, color = "red"))
plot13

plot14 <- ggplot(data = sp500data) + 
  geom_line(aes(x = Date, y = average_high_low, group = 1, color = "darkred")) +
  geom_line(aes(x = Date, y = average_open_close, group = 1, color = "lightblue"))
plot14

plot15 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = SP500change, color = "darkred")) + 
  geom_line(aes(x = Date, y = sp500_perc_change, color = "lightblue"))
plot15

plot16 <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = WEI, color = "darkred")) + 
  geom_line(aes(x = Date, y = sp500_perc_change, color = "lightblue")) +
  geom_line(aes(x = Date, y = 0), color = "black") +
  ggtitle("The WEI vs S&P500 growth rates") +
  ylab("WEI and S&P500 percentage changes") +
  theme(legend.position = "none")
plot16

data[(which.min(data$WEIchange) - 1):(which.min(data$WEIchange) + 1), ]
plot(x = data$Date, y = data$WEI, type = "l")
data(length(data$WEIchange - 10))

par(mfcol = c(2,2))
plot(data$BB ,data$WEI)
plot(data$T10Y3M, data$WEI)
plot(data$M1, data$WEI)

WEI = ts(data[,4], start= c(2008), frequency = 365.25/7)
plot(x = data$Date, y = data$WEI)
plot(x = data$Date, y = diff(data$WEI))

## Correlation matrix and plots ##

x <- c(cor(data$WEI, data$T10Y3M), cor(data$WEI, data$BB), cor(data$WEI, data$M1))
print(x)
cor(data[4:10])
corrplot(data[4:10], type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
cor(data[4:11])
cor(data[-1])
cor.test(x = data$WEI, y = data$`S&P500`, method=c("pearson", "kendall", "spearman"))

cor_all <- cor(data[4:10])
corrplot(cor_all, method = "color", na.rm = T)

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

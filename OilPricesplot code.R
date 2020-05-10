##Libraries
install.packages('ggfortify')
library(ggfortify)

## Data input
data = read.xlsx("WEI.xlsx", sheet = 2, detectDates = T)
oilp = data %>% select(6)
## 



##Wei change
WEI_time_series = ts(data[,4], start= c(2008), frequency = 365.25/7)
WEI_ts_dataframe = data.frame(WEI_time_series)

oil_time_series = ts(data[,6], start = c(2008), frequency = 365.25/7)

oil_time_series_change = diff(oil_time_series)

autoplot(oil_time_series_change)


time_series = ts(cbind(data[,4],data[,6]), start = c(2008), frequency = 365.25/7)
autoplot(time_series)



##Differences
oil_time_series_change_append = append(oil_time_series_change, 0, after = 0)
data = data %>% mutate(oil_diff = oil_time_series_change_append)


WEI_time_series_change <- diff(data$WEI)
WEI_time_series_change <- append(WEI_time_series_change, 0, after = 0)
data <- data %>% cbind(WEI_time_series_change)


data = PercChange(data = data, Var = "WEI", NewVar = "WEIpercchange")


##Percentage Differences
data = PercChange(data = data, Var = "Oil", NewVar = "Oilchange")


##plots

wei_vs_changeoil <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = WEI, color = "darkred")) + 
  geom_line(aes(x = Date, y = Oilchange/5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("WEI vs Change in Oil Price")
wei_vs_changeoil


changewei_vs_changeoil <- ggplot(data = data) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = Oilchange/5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs Change in Oil Price")
changewei_vs_changeoil

up2010changewei_vs_changeoil <- ggplot(data = data[1:63,]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = Oilchange/5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs Change in Oil Price (2008-2010)")
up2010changewei_vs_changeoil

beforecovchangewei_vs_changeoil <- ggplot(data = data[560:630,]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = Oilchange/5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs Change in Oil Price (Before Corona)")
beforecovchangewei_vs_changeoil

duringcovchangewei_vs_changeoil <- ggplot(data = data[630:639,]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = Oilchange/5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs Change in Oil Price (After Corona)")
duringcovchangewei_vs_changeoil

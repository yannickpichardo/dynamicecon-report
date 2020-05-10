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
data = PercChange(data = data, Var = "Oil", NewVar = "Oilperchange")


##plots
wei_vs_changeoil <- ggplot(data = data, aes(x = Date)) + 
   geom_line(aes(y = Oilperchange/5, color = "Oil perchantage change")) + geom_line(aes(y = WEI, color = "WEI")) +
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Oil perchantage change"= "blue", "WEI"= "green")) +
  ggtitle("WEI vs Change in Oil Price") + ylab("WEI and Oil price percentage change") 
wei_vs_changeoil


changewei_vs_changeoil <- ggplot(data = data, aes(x = Date)) + 
  geom_line(aes(y = oil_diff, color = "Oil price difference")) + 
  geom_line(aes(y = WEI_time_series_change*10, color = "WEI difference scaled by 10")) +
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Oil price difference"= "blue", "WEI difference scaled by 10"= "green")) +
  ggtitle("Difference within WEI vs Oil Price") + ylab("Difference with WEI scaled by 10") 
changewei_vs_changeoil

up2010changewei_vs_changeoil <- ggplot(data = data[1:63,], aes(x = Date)) + 
  geom_line(aes(y = oil_diff, color = "Oil price difference")) + 
  geom_line(aes(y = WEI_time_series_change*10, color = "WEI difference scaled by 10")) +
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Oil price difference"= "blue", "WEI difference scaled by 10"= "green")) +
  ggtitle("Difference within WEI vs Oil Price") + ylab("Difference with WEI scaled by 10") 
up2010changewei_vs_changeoil

beforecovchangewei_vs_changeoil <- ggplot(data = data[560:630,], aes(x = Date)) + 
  geom_line(aes(y = oil_diff, color = "Oil price difference")) + 
  geom_line(aes(y = WEI_time_series_change*10, color = "WEI difference scaled by 10")) +
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Oil price difference"= "blue", "WEI difference scaled by 10"= "green")) +
  ggtitle("Difference within WEI vs Oil Price") + ylab("Difference with WEI scaled by 10") 
beforecovchangewei_vs_changeoil

duringcovchangewei_vs_changeoil <- ggplot(data = data[630:639,], aes(x = Date)) + 
  geom_line(aes(y = oil_diff, color = "Oil price difference")) + 
  geom_line(aes(y = WEI_time_series_change*10, color = "WEI difference scaled by 10")) +
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("Oil price difference"= "blue", "WEI difference scaled by 10"= "green")) +
  ggtitle("Difference within WEI vs Oil Price") + ylab("Difference with WEI scaled by 10") 
duringcovchangewei_vs_changeoil

duringcovchangewei_vs_changeoil <- ggplot(data = data[630:639,]) + 
  geom_line(aes(x = Date, y = WEI_time_series_change, color = "darkred")) + 
  geom_line(aes(x = Date, y = Oilperchange/5, color = "lightblue")) + 
  geom_line(aes(x = Date, y = 0 , color = "green")) +
  ggtitle("Change in WEI vs Change in Oil Price (After Corona)")
duringcovchangewei_vs_changeoil

library('readxl')  # To read Excel files
library('fpp2')    # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm')   # To estimate ARDL models
library('urca')    # For the Dickey Fuller test
library('corrplot')# For plotting correlation matrices
library('quadprog')# For quadratic optimization
library('forecast')# Lots of handy forecasting routines
library('vars')    # VARs
library('zoo')     # Handy time series package


# Read data from the data folder
sheets    <- excel_sheets("data/WEI.xlsx") # Get sheets from excel
WEI_data  <- read_excel("data/WEI.xlsx", sheet = sheets[2]) # Load data
WEI       <- ts(WEI_data$WEI, start = 2008, frequency = 365.25/7)

#read CSV file and obtain data from 2007-2020, with values around 0
CCI       <- read.csv('CCI.csv')
CCI_data = CCI %>% slice(3:nrow(CCI)) %>% mutate(percentage = Value - 100)
CCI_2007       <- ts(CCI_data[,9],start = 2007,frequency=12)

#Take difference with respect to the value of last year
diff_CCI = diff(CCI_2007, 12)
diff_CCI = ts(as.vector(diff_CCI), start = 2008, frequency = 12)
autoplot(diff_CCI)

#Use data to create percentage change with respect to last year
CCI_2007_2019 = CCI_data %>% slice(1:(nrow(CCI)-14))
CCI_2007_2019 <- ts(CCI_2007_2019[,9],start = 2007,frequency=12) 
CCI_52_perc = as.vector(diff_CCI)/as.vector(CCI_2007_2019)
CCI_52_perc = ts(CCI_52_perc, start = 2008,frequency=12) 
autoplot(CCI_52_perc)

# Merge low and high freq time series
lowfreq      <- zoo(diff_CCI,time(diff_CCI))
highfreq     <- zoo(WEI,time(WEI))
merged       <- merge(lowfreq,highfreq)

# Approximate the NAs and output at the dates of the WEI
CCIw         <- na.approx(merged$lowfreq, xout = time(WEI),rule=2)
autoplot(CCIw) + autolayer(diff_CCI)
CCIw         <- ts(CCIw,start = 2008,frequency=365.25/7)
autoplot(CCIw)
autoplot(diff(CCIw))


#plotje
data <- read.xlsx("WEI.xlsx", sheet = 2, detectDates = TRUE)
data$CCIw =as.vector(CCIw) 
plot <- ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y =CCIw, colour = "CCI")) +
  geom_line(aes(y =WEI, colour = "WEI"))+ 
  geom_hline(yintercept = 0, color = 'black') + scale_colour_manual("", 
                                                                    values = c("CCI"="blue", "WEI"="green")) +
  ggtitle("WEI vs CCI 52 weekly difference") +
  ylab("WEI and CCI 52 weekly difference")  
plot

#correlatie
cor(data$WEI, data$CCIw)
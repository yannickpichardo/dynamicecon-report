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
CCI       <- read_excel("data/cci.xlsx")
CCI       <- ts(CCI,start = 2008,frequency=12)

# Merge low and high freq time series
lowfreq      <- zoo(CCI,time(CCI))
highfreq     <- zoo(WEI,time(WEI))
merged       <- merge(lowfreq,highfreq)
# Approximate the NAs and output at the dates of the WEI
CCIw         <- na.approx(merged$CCI, xout = time(WEI),rule=2)
autoplot(CCIw) + autolayer(CCI)
CCIw         <- ts(CCIw,,start = 2008,frequency=365.25/7)
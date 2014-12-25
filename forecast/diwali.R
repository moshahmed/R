# Example on how to process google trends forecast data in R.
# Data correlates the query 'diwali' with queries -2 months before.
# 2014-08-16 Sat 05:44

library(xts)
library(TTR)
setwd(dirname(sys.frame(1)$ofile))  
# Read the data downloaded from google trends.
diwali <- read.csv(file.choose(), header=T, skip=11, fill=T)  
# Convert date from string to numeric.
dates <- as.Date(diwali$Date, format="%Y-%m-%d")  
# Merge dates with the data.
data <- xts( diwali, order.by=dates)
# Pick a few columns to analyze.
data1 <- subset(data, select = c(2:5) )

# Convert the date into a time series.
dts <- ts(data1, frequency=12, start=c(2004,1))
plot.ts(dts) 
#| *graphic* of 4 timeseries

data2 <- xts( sapply( data1[, c(1:4)], as.numeric ), order.by=dates)
# correlate the columns makhar and diwali = 0.8356527
cor(data2$diwali, data2$makhar) 

# Use the SMA package to smooth the series.
# dts2 <- SMA(dts, n=2) .. error
dts2 <- SMA(ts(data2,frequency=12,start=c(2004,1)),n=4)  
plot.ts(dts2)
# Decompose the the time series into seasonal and non-seasonal components.
ddts2 <- decompose(dts2)
plot(ddts2)  
#| *graphics* with seasonal trend           


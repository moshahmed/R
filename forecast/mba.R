# R program to analyse Google trends forecast data.
library(xts) # Extended Time Series.
library(TTR) # Technical Trading Rules.

# Read the data downloaded from Google trends.
# Data correlates the query 'mba admissions'
# with queries -2 months before.
setwd(dirname(sys.frame(1)$ofile))
mbadata <- read.csv(
    "data/correlate-mba_admission-month-minus-2-in-4col-trimmed.csv",
    header=T, skip=11, fill=T)

# Convert date from string to numeric.
dates <- as.Date(mbadata$Date, format="%Y-%m-%d")
# Merge dates with the data.
data <- xts( mbadata, order.by=dates)

# Pick two columns to analyze.
data1 <- subset(data, select = c(2:3) )

# Convert the date into a time series.
dts <- ts(data1, frequency=12, start=c(2004,1))
plot.ts(dts) # graphics of 2 timeseries

data2 <- xts( sapply( data1[, c(1:2)], as.numeric ), order.by=dates)
# Correlate the columns 'mba admissions' and 'internships'.
cor(data2$mba.admission, data2$internships) # r=0.7

# Use the SMA package to smooth the series.
dts2 <- SMA(ts(data2,frequency=12,start=c(2004,1)),n=4)
plot.ts(dts2) # graphics of dts2

# Decompose the time series into
# seasonal and non-seasonal components.
ddts2 <- decompose(dts2)
plot(ddts2)  # graphics with seasonal trend


setwd("/root/git/RelianceCableFaults")

# Install and loading libraries

if(!require(forecast)){
     install.packages("forecast")
  }
 
library(forecast)

# loading Data

cable_fault_data <- read.csv("Cable Faults.csv")

# Function for forecasting cable faults

Cable_Fault_FC <- function(type = NULL, zone = NULL,horizon = NULL){
  
mydata <- cable_fault_data[cable_fault_data$Zone == zone & cable_fault_data$Type == type,]

# Convering Month variable in date format

mydata$Month <- paste0("01-",mydata$Month)

mydata$Month <- as.Date(mydata$Month, format = "%d-%b-%y")

# Ordering Data in ascending order

mydata <- mydata[order(mydata$Month,decreasing = F),]

# determine last month

last_month <- as.numeric(format(tail(mydata$Month,1),"%m"))

last_year <- as.numeric(format(tail(mydata$Month,1),"%Y"))
 
# creating time series

myts <-  ts(mydata$Cases,end = c(last_year,last_month), frequency = 12)

# detecting outliers

outliers <- tsoutliers(myts)

# replacement for outliers

myts[outliers$index] <- outliers$replacements

# fitting ARIMA model

fit_arima <- auto.arima(myts)

# fitting exponential smoothing model

fit_expo <- ets(myts)

# Compare accuracy

acc_arima <- accuracy(fit_arima)[,"RMSE"]
acc_expo <- accuracy(fit_expo)[,"RMSE"]

# forecasting with best method

if( acc_arima < acc_expo){
  
FC <- forecast(fit_arima, as.numeric(horizon))

FC <- FC$mean

} else {
  FC <- forecast(fit_expo, as.numeric(horizon))
  
  FC <- FC$mean
}
  
# output
 return (FC) 
  
}


# running forecasting function

# input the values for the variables

message("Enter the  type of the Fault (All Fault/HT Fault/LT Fault)...")
type <- "All Fault" #readLines(n = 1)

message("Enter the name of zone (SOUTH/SOUTH CENTRAL/CENTRAL/NORTH/EAST TOTAL/TOTAL)...")
zone <- "SOUTH" #readLines(n = 1)

message("Enter the number of months fore forecasting(1/2/3/4/.....)...")
horizon <- 4 #readLines(n = 1)

# Forecasting using  defined  function :Cable_Fault_FC 

FC <- Cable_Fault_FC(type = type,zone = zone,horizon = horizon)

print(FC)




  

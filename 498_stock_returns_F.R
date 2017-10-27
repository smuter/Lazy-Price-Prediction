# Stuart Muter
# Predict 498
# Capstone Assignment
# Stock & portfolio return calculation 

#10/20/17
##
##



# If necessary, install packages
install.packages("psych")
install.packages("ggplot2")
install.packages("ElemStatLearn")
install.packages("multilevel")
install.packages("lsr")
install.packages("xlsx")
install.packages("XML")
install.packages("data.table") 
install.packages("plyr")
install.packages("zoo")
install.packages("pscl")
install.packages("rpart")
install.packages("fma")
install.packages("forecast")
install.packages("car")
install.packages("MASS")
install.packages("TTR")
install.packages("lubridate")
install.packages("DataCombine")
install.packages("party")
install.packages("randomForest")
install.packages("dyn")
install.packages("Ecdat")
install.packages("fGarch")
install.packages("copula")
install.packages("quantmod")
install.packages("VineCopula")
install.packages("tseries")
install.packages("rgl")
install.packages("rugarch")
install.packages("Matrix")
install.packages("quadprog")
install.packages("quantmod")
install.packages("tidyquant")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)
library(lsr)
library(xlsx)
library(XML)
library(data.table) 
library(plyr) 
library(zoo)
library(pscl) 
library(rpart)
library(fma)
library(forecast)
library(car)
library(MASS)
library(TTR)
library(lubridate)
library(DataCombine)
library(party)
library(randomForest)
library(dyn)
library(Ecdat)
library(fGarch)
library(copula)
library(VineCopula)
library(tseries)
library(rgl)
library(rugarch)
library(Matrix)
library(Ecdat)
library(quadprog)
library(quantmod)
library(tidyquant)





###### Read in the data ################################################

### list of stock tickers to read in for testing  #########################################
symbols <- c("INTC","IBM","EBAY")

string_begin <- 'https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY&symbol='
string_end <- '&outputsize=full&apikey=NTRDFP32SIWS6GU0&datatype=csv'

### iterate over the tickers in symbols to fill up the dataframe with stock prices
stock_data <- data.frame()
datalist = list()
for(i in seq_along(symbols)) {
  URL <- paste0(string_begin, symbols[i], string_end)
  dat <- read.csv(URL)
  print(symbols[i])
  colnames(dat) <- c('timestamp', 'open', 'high', 'low', symbols[i], 'volume')
#  assign(paste0(symbols[i],"_data"), dat)
  dat2 <- dat[, c('timestamp', symbols[i])]
  if (i <2){
    stock_data<- dat2
  } else{
    stock_data <- cbind(stock_data, dat2[2])
  }
  
}

head(stock_data)
head(dat2)
print(stock_data)

## convert timestamp to Dates.
stock_data$timestamp <- as.Date(stock_data$timestamp)

#### sort into descending order ####################
stock_data2<- stock_data[order(stock_data$timestamp),]
### reset the index(rownames) for the ordered stock_data2 ###########
rownames(stock_data2) <- seq(length=nrow(stock_data2))
print(stock_data2)
### Begin process of converting from stock prices to returns

# compute difference of log stock prices (log differences), need to get FF RF for
# excess return calculation
#stocks_diff = as.data.frame(100*apply(log(stocks_subset), 2, diff) - FF_data_3$RF) # Excess returns
stocks_log_prices <- as.data.frame(apply(stock_data2[,2:ncol(stock_data2)], 2, log))
symbols_log <- lapply(symbols, function(x) paste0(x,"_logprice"))

#add names to stocks_log_prices
names(stocks_log_prices) <- symbols_log
# Check
head(stocks_log_prices)
#print(stock_data2$timestamp[-1])
# Apply the difference function to compute log differnces
stocks_log_diff <- as.data.frame(apply(stocks_log_prices, 2, diff))

# add names to stocks_log_diff
symbols_log_diff <- lapply(symbols, function(x) paste0(x,"_log_diff"))
names(stocks_log_diff) <- symbols_log_diff
# Check
head(stocks_log_diff)
tail(stocks_log_diff)
# Add the timestamp.
stocks_log_diff_2 <- cbind(stock_data2$timestamp[-1], stocks_log_diff)
names(stocks_log_diff_2) <- c('timestamp',symbols_log_diff)
# check
head(stocks_log_diff_2)
print(stocks_log_diff_2)


### Now create functions to return a cumulative return.
## arguments = stock ticker and date (month ending date)
## returns the stock return for that month (so filing occured in the month before)

###### testing ######
test_date <- c('2010-03-31')
test_ticker <- c('MSFT')

###### 3 month return function #######
## arguments = stock ticker and date (month ending date, called stock_month)
## returns the stock return for three month period starting from stock_month. So the filing occured sometime 
## in the month before stock_month.

three_month_return <- function(stock_ticker, stock_month) { 
  begin_row<- which(stocks_log_diff_2 == stock_month)
  print(stocks_log_diff_2[begin_row,])
  print(stocks_log_diff_2[(begin_row + 1),])
  end_3_month_row <- begin_row + 2
  print(stocks_log_diff_2[end_3_month_row,])
  column_name_end <- c('_log_diff')
  col_name <-  paste0(stock_ticker, column_name_end)
  id_col<- which(names(stocks_log_diff_2) == col_name)
  sum_3<- sum(stocks_log_diff_2[begin_row:end_3_month_row, id_col])
  exp_3 <- expm1(sum_3) 
  return(exp_3)
}

### test function
three_month_return('IBM', '2010-02-26')

#### 6 month return function  #######
## arguments = stock ticker and date (month ending date, called stock_month)
## returns the stock return for six month period starting from stock_month. So the filing occured sometime 
## in the month before stock_month.
six_month_return <- function(stock_ticker, stock_month) { 
  stock_row_begin<- which(stocks_log_diff_2 == stock_month)
  stock_row_end <- stock_row_begin + 5
  print(stocks_log_diff_2[stock_row_begin,])
  print(stocks_log_diff_2[stock_row_end,])
  column_name_end <- c('_log_diff')
  col_name <-  paste0(stock_ticker, column_name_end)
  id_col<- which(names(stocks_log_diff_2) == col_name)
  sum_6<- sum(stocks_log_diff_2[stock_row_begin:stock_row_end, id_col])
  exp_6 <- expm1(sum_6)
  return(exp_6)
}

### test function
six_month_return('EBAY', '2010-02-26')

#### 12 month return function  #######
## arguments = stock ticker and date (month ending date, called stock_month)
## returns the stock return for 12 month period starting from stock_month. So the filing occured sometime 
## in the month before stock_month.
twelve_month_return <- function(stock_ticker, stock_month) { 
  stock_row_begin<- which(stocks_log_diff_2 == stock_month)
  stock_row_end <- stock_row_begin+ 11
  print(stocks_log_diff_2[stock_row_begin,])
  print(stocks_log_diff_2[stock_row_end,])
  column_name_end <- c('_log_diff')
  col_name <-  paste0(stock_ticker, column_name_end)
  id_col<- which(names(stocks_log_diff_2) == col_name)
  sum_12<- sum(stocks_log_diff_2[stock_row_begin:stock_row_end, id_col])
  exp_12 <- expm1(sum_12)
  return(exp_12)
}

### test function
twelve_month_return('EBAY', '2010-02-26')

#### 1 month return function  #######
## arguments = stock ticker and date (month ending date)
## returns the stock return for that month (so filing occured in the month before)
one_month_return <- function(stock_ticker, return_date) { 
  file_row<- which(stocks_log_diff_2 == return_date)
  begin_row <- file_row 
  end_one_month_row <- file_row 
  column_name_end <- c('_log_diff')
  col_name <-  paste0(stock_ticker, column_name_end)
  id_col<- which(names(stocks_log_diff_2) == col_name)
  print(stocks_log_diff_2[begin_row,])
  print(stocks_log_diff_2[end_one_month_row,])
  sum_one<- sum(stocks_log_diff_2[begin_row:begin_row, id_col])
#  print(sum_one)
  exp_one <- expm1(sum_one) 
  return(exp_one)
}


### test function
one_month_return('EBAY', '2010-02-26')
print(one_month_return)


##### Write portfolio return functions ########
### equal weighted portfolio return from stock returns
#### one month portfolio return function #####
### given ticker list and month the portfolio is active,  returns equal weighted portfolio return for that month
### Note file_date = month of filing, so this function returns the return for the following month

one_month_port_return <- function(ticker_list, port_month) { 
  
  # find stock correct row in log difference DF based on file_date    
  
  ## initialize matrix n x 1, n = number of tickers
  return_list <- c(0, "2000-01-01")
  n<- length(ticker_list)
  stock_returns <- matrix(ncol = 1, nrow = n)
  ## loop over the list of stocks in ticker_list and get return for each ticker
  ## write into a list called stock_returns
  
  for(i in seq_along(ticker_list)) {
    
    stock_returns[i,1] <- one_month_return(ticker_list[i], port_month)
  }
  ### return the mean of element in stock_returns, as equal weighted portfolio
  ### return.
  port_average<- mean(stock_returns)
  return_list<- c(port_average, port_month)
  return(return_list)
}

#### test this function #######

test_tickers = c("IBM","EBAY", "INTC")
test_date <- c('2013-07-31')

#test_date_3<- as.yearmon(test_date)
#print(test_date_3)

port_return<- one_month_port_return(test_tickers, test_date)
print(port_return)


###### read in fremch fama data #############

#french_fama <- read.csv("F_F_Research_Data_Factors.CSV", header = TRUE)
french_fama <- read.table("F_F_Research_Data_Factors.txt", header = TRUE)

head(french_fama)
str(french_fama)
### trim french fama data to begin with 2010-01-01 (defined by start_date variable)
## format in French Fama data is 192601 for Jan 1926

start_date <- as.yearmon(c('2010-01'))

#### convert integer date (YYYYmm) to yearmon class (month year)
french_fama$Date <- as.yearmon(as.character(french_fama$Date), "%Y%m")

id_month_row<- which(french_fama$Date == start_date)

#print(french_fama[1003,])

#### subset french_fama data to start from Jan 2010 (start_date variable)

returns_dataframe<- french_fama[id_month_row:nrow(french_fama),]
head(returns_dataframe)
tail(returns_dataframe)

##### Add in empty column for monthly portfolio return (port_return) and adj port returns (port return - RF)
returns_dataframe[,"port_return"] <- NA
returns_dataframe[,"adj_port_return"] <- NA
### reset the index(rownames) for the returns_dataframe subset ###########
rownames(returns_dataframe) <- seq(length=nrow(returns_dataframe))


### create a function that is given a portfolio return and date (YYYY-MM-DD) and
### writes this into the returns_dataframe DATAFRAME at the right row location
### this will be looped across all months of filings to build up the 
## returns_dataframe that can be used for the multiple linear regression modeling.

portfolio_filler <- function(port_return_list) { 
  port_date<- as.yearmon(port_return_list[2])
  id_month_row2<- which(returns_dataframe$Date == port_date)
  returns_dataframe[id_month_row2, 6] <<- as.numeric(port_return_list[1])  
  returns_dataframe[id_month_row2, 7] <<- as.numeric(port_return_list[1]) - returns_dataframe[id_month_row2, 5] 
}


#### test, by calling the function
portfolio_filler(port_return)

#### check  #####################
head(returns_dataframe)

##### Now create lineasr regression models

linear_model1<- lm(adj_port_return ~ Mkt.RF + SMB + HML, data = returns_dataframe)

####### END ##############################################

##### have a look Reeds flat files ##################

sim_data <- read.csv("stocksWithSimilarityMetrics.txt", header = TRUE)
sim_data_quintiles <- read.csv("stocksWithSimilarityMetricsQuintiles.txt", header = TRUE)
head(sim_data)
print(sim_data)
str(sim_data)
head(sim_data_quintiles)
tail(sim_data_quintiles)
str(sim_data_quintiles)


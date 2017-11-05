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
install.packages("bsts")
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
library(bsts)
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
symbols <- c("AAPL", "ADM", "ADI", "AMGN", "AIV", "ALB", "AET", "AMZN", "ADP", 
             "BA", "BAC", "C", "CAT", "CCI", "CINF", "CSCO", "CELG", "CHD", "CHRW", "COF", "CTSH", "DHI", "DGX", 
              "EQR", "EBAY", "ESRX", "F", "FAST", "FCX", "FLIR", "FFIV", "FDX", "FLS", "GE", "GLW", 
              "IBM", "INTC", "IFF", "IRM", "JNPR", "KO", "K", "LUK", "MGM", "MYL", "M", "MRK", "MCD", "MAT", "MAA", 
              "MCK", "MDT", "MMC", "MMM", "MSFT", "MTD", "NTAP", "NKE", "ORCL", "PCAR", "PAYX", "PBCT", "PDCO", "PH", "PPG", "PRGO", "PNW", "QCOM",
              "RHT", "ROP", "SBUX", "SPG", "SYMC", "SRE", "TSCO", "TGT", "TROW", "TXT", "UPS", "VZ",  
              "VFC", "VRTX", "WFC", "WMT", "XL", "XRX", "XOM")



string_begin <- 'https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY&symbol='
string_end <- '&outputsize=full&apikey=NTRDFP32SIWS6GU0&datatype=csv'

### iterate over the tickers in symbols to fill up the dataframe with stock prices
stock_data <- data.frame()
datalist = list()
for(i in seq_along(symbols)) {
  URL <- paste0(string_begin, symbols[i], string_end)
  dat <- read.csv(URL)
  print(symbols[i])
  print(nrow(dat))
  colnames(dat) <- c('timestamp', 'open', 'high', 'low', symbols[i], 'volume')
#  assign(paste0(symbols[i],"_data"), dat)
  dat2 <- dat[, c('timestamp', symbols[i])]
#  length(dat2)<- 213
  if (i <2){
    stock_data<- dat2
  } else{
    stock_data <- cbind(stock_data, dat2[2])
  }
  
}

head(stock_data)
str(stock_data)
head(dat2)
print(stock_data$AAPL)

## convert timestamp to Dates.
stock_data$timestamp <- as.Date(stock_data$timestamp)

#### sort into descending order ####################
stock_data2<- stock_data[order(stock_data$timestamp),]
### reset the index(rownames) for the ordered stock_data2 ###########
rownames(stock_data2) <- seq(length=nrow(stock_data2))
print(stock_data2$timestamp)
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
#  print(stocks_log_diff_2[begin_row,])
#  print(stocks_log_diff_2[end_one_month_row,])
  sum_one<- sum(stocks_log_diff_2[begin_row:begin_row, id_col])
#  print(sum_one)
  exp_one <- expm1(sum_one) 
  return(exp_one)
}


### test function
one_month_return('AAPL', '2014-06-30')
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
    print(c(stock_returns[i,1], ticker_list[i]))
  }
  ### return the mean of element in stock_returns, as equal weighted portfolio
  ### return.
  port_average<- mean(stock_returns)
  print(port_average)
  return_list<- c(port_average, port_month)
  # just return the portfolio return. 
  return(return_list[1])
}

#### test this function #######

test_tickers = c("IBM","EBAY", "INTC")
test_tickers_2 = c("SYMC","KO",   "AMZN", "ESRX", "VFC" , "FDX",  "CSCO" ,"BAC")
print(test_tickers)

print(test_tickers[[1]])
test_date <- c('2013-07-31')

#test_date_3<- as.yearmon(test_date)
#print(test_date_3)

port_return<- one_month_port_return(test_tickers, test_date)
port_return_2<- one_month_port_return(test_tickers_2, test_date)
print(port_return)







#### import monthly Quintile rankings data from Reed/etc module #######

quintiles_data <- read.csv("mock_dataSheet_D.csv", header = TRUE)
print(quintiles_data)
tail(quintiles_data)
str(quintiles_data)


### change date into yearmon 
quintiles_data$Date <- as.Date(sim_data$Date, format = "%m/%d/%y")
#mock_data$Date <- as.yearmon(as.character(mock_data$Date),  "%Y%m")

### get begninning and ending month of the similarity data
begin_month <- as.yearmon(quintiles_data$Date[1])
end_month <- as.yearmon(quintiles_data$Date[nrow(quintiles_data)])

#print(LastDayInMonth(as.Date(sim_data$Date[1])))

#### Loop through the data dataframe to prep data as character
quintiles_data$Q1_clean <- lapply(quintiles_data$Q1_quintile, as.character)
quintiles_data$Q5_clean <- lapply(quintiles_data$Q5_quintile, as.character)


####### portfolio generator code #######################################
## takes monthly quintile sort and creates and tracks portfolios according to rules:
## first month Q1 and Q5 go into portfolios
## stocks stay in the portfolio for 3 months, or 1 month or 2 months (a variable) then
## removed and each month assess this and see which stocks adding and which deleting
## i = 1 is the first month. in this case then portfolio = quintiles
## i = 2, in this case, Q1_curent_port = Q1_current_port (prior month) + any new names from Q1, Q5 quintiles
### loop over the entire DF going month by month creating the portfolios

# set variable for number of month a stock is in a portfolio
duration <- 3

Q1_current_port <- c()
Q5_current_port <- c()
for (i in 1:nrow(quintiles_data)){
  if (i == 1) {
    Q1_string<- quintiles_data$Q1_clean[1]
    Q1_current_port<- strsplit(Q1_string[[1]], " ")[[1]]
    quintiles_data$Q1_port_clean[i] <- list(c(Q1_current_port))
    
    Q5_string<- quintiles_data$Q5_clean[1]
    Q5_current_port<- strsplit(Q5_string[[1]], " ")[[1]]
    quintiles_data$Q5_port_clean[i] <- list(c(Q5_current_port))
    
  } else if (2 <= i && i <= 3){
    Q1_string<- quintiles_data$Q1_clean[i]
    Q1_string_new<- strsplit(Q1_string[[1]], " ")[[1]]
    Q1_adds <- Q1_string_new[which(!(Q1_string_new%in%Q1_current_port))]
    
    Q1_current_port <- c(Q1_current_port, Q1_adds)
    quintiles_data$Q1_port_clean[i] <- list(Q1_current_port)
   
    Q5_string<- quintiles_data$Q5_clean[i]
    Q5_string_new<- strsplit(Q5_string[[1]], " ")[[1]]
    Q5_adds <- Q5_string_new[which(!(Q5_string_new%in%Q5_current_port))]

    Q5_current_port <- c(Q5_current_port, Q5_adds)
    quintiles_data$Q5_port_clean[i] <- list(Q5_current_port)
    
  } else{
    Q1_string<- quintiles_data$Q1_clean[i]
    Q1_string_new<- strsplit(Q1_string[[1]], " ")[[1]]
    Q1_subtract<- quintiles_data$Q1_port_clean[[(i -duration)]]
    Q1_updated_port<- Q1_current_port[!(Q1_current_port %in% Q1_subtract)]
    Q1_adds <- Q1_string_new[which(!(Q1_string_new%in%Q1_updated_port))]

    Q1_current_port<- append(Q1_updated_port, list(Q1_adds))
    quintiles_data$Q1_port_clean[i] <- list(Q1_current_port)
    
    Q5_string<- quintiles_data$Q5_clean[i]
    Q5_string_new<- strsplit(Q5_string[[1]], " ")[[1]]
    Q5_subtract<- quintiles_data$Q5_port_clean[[(i -duration)]]
    Q5_updated_port<- Q5_current_port[!(Q5_current_port %in% Q5_subtract)]
    Q5_adds <- Q5_string_new[which(!(Q5_string_new%in%Q5_updated_port))]
    
    Q5_current_port<- append(Q5_updated_port, list(Q5_adds))
    quintiles_data$Q5_port_clean[i] <- list(Q5_current_port)
  }
}






###### testing ###########
string_1<- sim_data$Q1_portfolio_clean[1]
print(string_1)
new_string<- strsplit(string_1[[1]], " ")[[1]]

test_date <- c('2014-02-28')
print(test_date)
port_return_2<- one_month_port_return(new_string, test_date)

print(LastDayInMonth(as.Date(sim_data$Date[1])))
##################################################




##### link sim_data to the output of the portfolio generator ADD CODE HERE


###### create a dataframe with the returns data for each month in sim_data #####
##### create a dataframe with Q1 and Q5 returns ##########
##### run the loop here ####### 
# initialize two empty matrices to take the data
port_return <- matrix(, nrow = nrow(sim_data), ncol = 2)
#Q5_port_return <- matrix(, nrow = nrow(sim_data), ncol = 1)

for (i in 1:nrow(sim_data)){
  port_calc_date<- as.character(sim_data$Date[i])
  print(port_calc_date)
  string_Q1<- sim_data$Q1_portfolio_clean[i]
  new_string_Q1<- strsplit(string_Q1[[1]], " ")[[1]]
  print(new_string_Q1)
  string_Q5<- sim_data$Q5_portfolio_clean[i]
  new_string_Q5<- strsplit(string_Q5[[1]], " ")[[1]]
  print(new_string_Q5)
### call port return function ######
  port_return[i,1] <- as.numeric(one_month_port_return(new_string_Q1, port_calc_date))
  port_return[i,2] <- as.numeric(one_month_port_return(new_string_Q5, port_calc_date))
#  portfolio_filler(port_return_data)
}

#### convert matrix into a dataframe
port_return_DF <- data.frame(port_return)
colnames(port_return_DF) <- c("Q1_return", "Q5_return")
print(port_return_DF)
mean(port_return_DF$Q1_return)
mean(port_return_DF$Q5_return)

###### read in fremch fama data #############################################

#french_fama <- read.csv("F_F_Research_Data_Factors.CSV", header = TRUE)
french_fama <- read.table("F_F_Research_Data_Factors.txt", header = TRUE)

head(french_fama)
str(french_fama)

## format in French Fama data is 192601 for Jan 1926

#### convert integer date (YYYYmm) to yearmon class (month year)
french_fama$Date <- as.yearmon(as.character(french_fama$Date), "%Y%m")

### figure out he begin and end row for french fama data to match sim_data dataframe months
id_month_row_start<- which(french_fama$Date == begin_month)
id_month_row_end<- which(french_fama$Date == end_month)

#### subset french_fama data to start from begin date in sim_data and end date in sim_data
returns_dataframe<- french_fama[id_month_row_start:id_month_row_end,]

head(returns_dataframe)
tail(returns_dataframe)

### reset the index(rownames) for the returns_dataframe subset ###########
rownames(returns_dataframe) <- seq(length=nrow(returns_dataframe))

### Now combine the Q1 and Q5 portfolio returns with the fremnch-fama dataframe
#### add portfolio returns (port_return_DF) to french-fama dataframe (returns_dataframe)

combined_dataframe <- cbind(returns_dataframe, port_return_DF)

##### create Q1_adj_return and Q5_adj_return columns in combined_dataframe
## Adjusted return = port return -RF (risk free rate)

combined_dataframe$Q1_adj_return <- combined_dataframe$Q1_return -combined_dataframe$RF
combined_dataframe$Q5_adj_return <- combined_dataframe$Q5_return -combined_dataframe$RF

#### Check ####
print(combined_dataframe)

#### compute means of the adjusted returns

mean(combined_dataframe$Q1_adj_return)
mean(combined_dataframe$Q5_adj_return)

#### write completed DF into a CSV file
write.csv(combined_dataframe, file = "combined_DF.csv")

##### Now create linear regression model for Q1 portfolio 

linear_model_Q1<- lm(Q1_adj_return ~ Mkt.RF + SMB + HML, data = combined_dataframe)

summary(linear_model_Q1)

sink("summaryQ1Model.txt")
summary(linear_model_Q1)
sink()

plot(combined_dataframe$Date, combined_dataframe$Q1_adj_return)


##### Now create linear regression model for Q5

linear_model_Q5<- lm(Q5_adj_return ~ Mkt.RF + SMB + HML, data = combined_dataframe)

summary(linear_model_Q5)

sink("summaryQ5Model.txt")
summary(linear_model_Q5)
sink()

plot(combined_dataframe$Date, combined_dataframe$Q5_adj_return)

###### Modeling ends here ###########################


##### have a look Reeds flat files ##################

sim_data <- read.csv("stocksWithSimilarityMetrics.txt", header = TRUE)
sim_data_quintiles <- read.csv("stocksWithSimilarityMetricsQuintiles.txt", header = TRUE)
head(sim_data)
print(sim_data)
str(sim_data)
head(sim_data_quintiles)
tail(sim_data_quintiles)
str(sim_data_quintiles)

##### have a look Reeds flat file for November2017 ##################

nov_data <- read.csv("November2017.txt", header = TRUE)
print(nov_data)

####### END ##############################################

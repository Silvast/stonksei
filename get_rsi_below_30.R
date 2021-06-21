suppressPackageStartupMessages({
library(quantmod)
library(PerformanceAnalytics)
library(jsonlite)
library(TTR)
library(dplyr)
})

symbols <- jsonlite::fromJSON("tickers_sto.json")
symbols_stockholm <- filter(as.data.frame(symbols), has_eod == "TRUE")
tickers_st <- symbols_stockholm$symbol
apikey <- Sys.getenv('r_apikey')

get_full_name <- function(ticker, symbols_list){
  getElement(symbols_list[symbols_list$symbol == ticker,], "name")
}

find_tobuy <- function(stock_name, symbols_list){
url <- paste("http://api.marketstack.com/v1/eod?access_key=", apikey, "&symbols=",stock_name, sep="")
  y <- jsonlite::fromJSON(url)
  y_data <- y$data
  price <- y_data[,"close"]
  #rsi <- RSI(price)
  rsi <- try(RSI(price), silent=TRUE)
  
  last_rsi <- tail(rsi, n=5)
  
  if (length(last_rsi[last_rsi < 30]) > 2){
  print(paste(stock_name, "---", get_full_name(stock_name,symbols_list)))
  }
}

print("****STOCKHOLM STOCKS RSI BELOW 30****")
print("Current date:")
print(Sys.Date())
for (i in 1:length(tickers_st)) {
  try(find_tobuy(tickers_st[i], symbols_stockholm))
}

symbols_hel <- jsonlite::fromJSON("tickers_hel.json")
symbols_hel <- filter(as.data.frame(symbols_hel), has_eod == "TRUE")
tickers_hel <- symbols_hel$symbol

print("***HELSINKI STOCKS RSI BELOW 30***")

for (i in 1:length(tickers_hel)) {
  try(find_tobuy(tickers_hel[i], symbols_hel))
}




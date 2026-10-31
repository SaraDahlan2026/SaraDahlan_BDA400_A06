install.packages("shiny")
install.packages("ggplot2")
install.packages("quantmod")
library(shiny)
library(ggplot2)
library(quantmod)
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

head(stock_data)
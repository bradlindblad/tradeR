
library(dtplyr)
library(tidyverse)
library(quantmod)
library(TTR)
library(tidyquant)
library(lubridate)
# library(alphavantager)
# alphavantager::av_api_key("LEZWEWZ2MNDGVOPM")




# Get all symbol data for last 6 mo ---------------------------------------

all.nasdaq <- tidyquant::tq_exchange("NASDAQ")
all.nyse <- tidyquant::tq_exchange("NYSE")
all.symbols <- rbind(all.nasdaq, all.nyse)

# Smaller, riskier subset
my.symbols <- all.symbols %>%
  dplyr::filter(last.sale.price < 15) %>%
  dplyr::filter(industry == 'Biotechnology: Biological Products (No Diagnostic Substances)')


# Loop over all and assign to data frames ---------------------------------
date.6mo.ago <- lubridate::today() - 30.42 * 6

# alldatas <- tidyquant::tq_get(x = all.symbols$symbol, get = "alphavantager", av_fun = "TIME_SERIES_DAILY")
#
# base.df <- tidyquant::tq_get(x = "SPY", get = "alphavantager", av_fun = "TIME_SERIES_DAILY")
# base.df$symbol <- "SPY"
#
#
# loopSymbols <- function(symbol) {
#
#   output <- tidyquant::tq_get(x = symbol, get = "alphavantager", av_fun = "TIME_SERIES_DAILY")
#   output$symbol <- symbol
#   Sys.sleep(15)
#   print("Looping through all symbols...")
#   rbind(base.df, output)
#
# }
#
# final.data <- purrr::map(my.symbols$symbol, loopSymbols) %>%
#   dplyr::bind_rows(.id = 'symbol')
#
#
# tq_get("AAPL", get = "stock.prices")
#






# sp <- tq_get("SP500", get = "stock.index")
#
# # Map get_stock_prices() to list of stock symbols
# from <- "2017-01-01"
# to   <- today()
# Series
biotech <- my.symbols$symbol[1:8] %>%
  tq_get(get = "stock.prices",
         from = date.6mo.ago,
         to = lubridate::today())

# Mutate and get a MACD
with.macd <- biotech %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_mutate(select = close,
                       mutate_fun = MACD,
                       nFast = 12,
                       nSlow = 20,
                       nSig = 15,
                       maType = SMA)

ggplot(with.macd) +
  geom_line(aes(date, close)) +
  geom_line(aes(date, macd), color = 'blue') +
  facet_wrap(~ symbol)




#
# input <- test.symbols$symbol
#
#
#
# df_total = data.frame()
# for (i in input){
#   # vector output
#
#   output <- tidyquant::tq_get(x = i, get = "alphavantager", av_fun = "TIME_SERIES_DAILY")
#   output$symbol <- i
#   Sys.sleep(2)
#   # add vector to a dataframe
#
#   df_total <- rbind(df_total,output)
# }
#
#
#
#
#
# quantmod::getSymbols("SPY", subset = 'last 4 months')
#
#
# tq_ge
#
#
# quantmod::getFin("SPY")
#
# quantmod::getSymbols.alphavantage(Symbols = c("SPY", "AAPL"), api.key = "LEZWEWZ2MNDGVOPM", )
#
#
#
# quantmod::getSymbols("IBM", src="av", api.key="LEZWEWZ2MNDGVOPM", output.size="full",
#
#                      periodicity="daily")

## ------------------------------------------------------------------------
(p1 <- structure(rnorm(4), names=c('IBM', 'T', 'AAPL')))

## ----include=FALSE, cache=FALSE------------------------------------------
library(testR)

## ------------------------------------------------------------------------
x1 <- list(
  `2000-01-01`=structure(1:20, names=letters[1:20]),
  `2001-01-01`=structure((-1)^(1:10), names=letters[1:10])
)
x2 <- list(
  `2000-01-01`=structure(1:20, names=letters[7:26]),
  `2001-01-01`=structure(sin(1:10), names=letters[11:20])
)

## ------------------------------------------------------------------------
S1 <- as.strategy(x1)
S2 <- as.strategy(x2) 

## ------------------------------------------------------------------------
x1_m <- l2m(x1)
head(x1_m)

## ------------------------------------------------------------------------
x1_df <- x1_m %>% 
  melt %>% 
  na.omit %T>% 
  {names(.) <- c('date','asset', 'value')}
# The column names do not have to be date, asset, value, but their order needs to be date-asset-value.
head(x1_df)

## ------------------------------------------------------------------------
# from matrix
S1a <- as.strategy(x1_m)
# from data frame
S1b <- as.strategy(x1_df)

## ------------------------------------------------------------------------
x1 <- as.sos(str1=S1)
x2 <- as.sos(str1=S1,str2=S2)

## ------------------------------------------------------------------------
names(x2) <-c('first', 'second')

## ------------------------------------------------------------------------
summary(x2)
summary(S1a)

## ------------------------------------------------------------------------
x3 <- as.sos(str1=S1) %>%
  mutate(str3=2*str1, str4=long(str1)) %>% 
  mutate(str5=3*str3)

## ------------------------------------------------------------------------
x4 <- add_strategy_list(x3, list(str6=2*x3$str1, str7=dollar_neutral.strategy(x3$str1, bench_id='spy')))

## ------------------------------------------------------------------------
x3$str1$`2000-01-01`
x3$str1$`2000-01-01`['a']  #for the latter, $ won't work, because portfolios are vectors

## ------------------------------------------------------------------------
x3 %>% get_strat('str1', 'str2')

## ------------------------------------------------------------------------
x2 %>% filter(date <= '2000-01-01')

## ------------------------------------------------------------------------
set.seed(314)
data(RETS)
asset_id <- colnames(RETS)
dates <- structure(row.names(RETS), names=row.names(RETS))
# # generates a random strategy
str <- lapply(dates, function(x) structure(rnorm(500), names=sample(asset_id, 500, replace=FALSE))) %>%
  as.strategy
STRAT <- as.sos(strategy1=str, strategy2=long(str))
# 
# v <- backtest(STRAT, RETS)
# names(v)

## ---- warning = FALSE, error =FALSE, fig.width = 7, fig.height = 4-------
# library(financieRaid)
# plotts(v$ret, cumulative = T, geometric = T)
# spy <- getSPY()
# # partial application of benchmark and country
# compute_alphabeta_US <- function(rets) compute_alphabeta(rets, spy, '')
# 
# make_report_list(v$ret, compute_sharpe , compute_alphabeta_US, compute_drawdown)


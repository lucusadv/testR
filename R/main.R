#' @export
compute_gmv <- function(x) UseMethod("compute_gmv")

#' @export
compute_turnover <- function(x) UseMethod("compute_turnover")

#' @export
long <- function(x) UseMethod("long")

#' @export
short <- function(x) UseMethod("short")

#' @export
beta_neutralize <- function(x, BETA, bench_id) UseMethod("beta_neutralize")

#' @export
get_betas <- function(x, y) UseMethod("get_betas")

#' Computes performance of a strategy object.
#'
#' All strategy dates must be contained in the returns dates.
#' The performance of the strategy is comprised of pnl, turnover
#' and the time series of the strategy.
#'
#' @param PORT list, holdings of strategy (one element per date)
#' @param RETS matrix of returns
#' @param shortfall numeric, market impact in bps
#' @param borrowrate numeric, borrow rate in bps
#' @return vector of pnl
#'
#' @export
.compute_performance <- function(PORT, RETS, shortfall=0, borrow=0, keep_strategy=FALSE){
  stopifnot(all(names(PORT) %in% row.names(RETS)))
  dtes_start <- min(names(PORT))
  dtes_end   <- max(row.names(RETS))
  ids <- PORT %>% lapply(names) %>% Reduce(union, .)
  R <- filter(RETS, date >= dtes_start, id %in% ids)
  dtes <- row.names(R)
  pnl <- structure(rep(NA, nrow(R)), names=dtes)
  work_portfolio <- list()
  work_portfolio[[dtes_start]] <- PORT[[dtes_start]]

  turnover <- structure(rep(0, nrow(R)), names=dtes)
  for (d in dtes){
    v <- R[d, ] + 1
    v[is.na(v)] <- 1
    if (d %in% names(PORT)) {
      tmp_portfolio <- opVectors(work_portfolio[[d]], v, FUN=`*`)
      scaling_factor <- ifelse(d > dtes_start, sum(abs(tmp_portfolio))/sum(abs(PORT[[d]])), 1)
      work_portfolio[[d]] <- scaling_factor * PORT[[d]]
      turnover[d] <- sum(abs(opVectors(work_portfolio[[d]], tmp_portfolio, FUN=`-`))) / sum(abs(tmp_portfolio))
    }
    pnl[d] <- opVectors(work_portfolio[[d]], R[d, ], FUN = `*`) %>% sum(na.rm=TRUE)
    if (is.na(pnl[d])) stop('missing dates')
    if (d < dtes_end){
      d_next <- succ(d, dtes)
      v <- R[d, ] + 1
      v[is.na(v)] <- 1
      work_portfolio[[d_next]] <- opVectors(work_portfolio[[d]], v, FUN=`*`)
    }
  }
  # creates adjusted pnl. I am using an approximate number of trading days in
  # order to avoid dependencies on date libraries
  days_trading <- 252/365*as.numeric(as.Date(first(dtes))-as.Date(last(dtes)))
  portfolio_gmv <- sapply(work_portfolio, function(x) sum(abs(x)))
  portfolio_shortnmv <- sapply(work_portfolio, function(x) -sum(pmin(x,0)))
  turnover_costs <- shortfall*sum(turnover)*portfolio_gmv/days_trading
  borrow_costs <- portfolio_shortnmv*borrow
  pnl_adj <- pnl - borrow_costs - turnover_costs
  class(work_portfolio) <- c('strategy', 'list')
  if (!keep_strategy) work_portfolio <- NULL
  list(ret = pnl/portfolio_gmv,
       ret_adj = pnl_adj/portfolio_gmv,
       turnover = turnover,
       strategy = work_portfolio)
}


#' Converts a list of vectors, a data frame or a matrix into a strategy
#' object.
#'
#' If input is a list, it must be comprised of numeric vector elements.
#' The names of the elements are date characters of format "YYYY-MM-DD",
#' and the names of the vectors are asset ids.
#' If input is a data frame, it must have three columns: date, asset id,
#' weight. The data frame names need not be 'date', 'asset', 'id'.
#' If input is a matrix, it must have row.names equal to dates, and colnames
#' equal to asset ids.
#'
#' @param x a list, a matrix or a data frame
#' @return a strategy object
#' @examples
#' PORT1 <- list(`2000-01-01`=structure(1:20, names=letters[1:20]),
#' `2001-01-01`=structure((-1)^(1:10), names=letters[1:10]))
#' PORT2 <- list(`2000-01-01`=structure(1:20, names=letters[7:26]),
#' `2001-01-01`=structure(sin(1:10), names=letters[11:20]))
#' x <- as.strategy(PORT1)
#' @export
#'
as.strategy <- function(x) UseMethod('as.strategy')

#' converts a strategy to a strategy (identity)
#'
#' @param x strategy
#' @export
as.strategy.list <- function(x){
  class(x) <- c('strategy', 'list')
  x
}

#' converts a list to a strategy
#'
#' @param x list of vectors
#' @export
as.strategy.strategy <- function(x) x

#' Convert a matrix to a strategy
#'
#' @param x matrix
#' @export
as.strategy.matrix <- function(x) x %>% m2l %>% lapply(function(x) x[!is.na(x)]) %>% as.strategy

#' Convert a data frame to a strategy
#'
#' @param x data frame
#' @export
as.strategy.data.frame <- function(x) {
  names(x) <- c('date','asset','value')
  y <- dlply(x, .(date), function(x) structure(x$value, names=x$asset)) %>% as.strategy
  attr(y, 'split_type') <- NULL
  attr(y, 'split_labels') <- NULL

  y
}

#' Converts list, data.frame, matrix to a set of strategies object.
#'
#' @param data a list of objects that can be converted into strategies
#' @return a sos object
#' @examples
#' PORT1 <- list(`2000-01-01`=structure(1:20, names=letters[1:20]),
#' `2001-01-01`=structure((-1)^(1:10), names=letters[1:10]))
#' PORT2 <- list(`2000-01-01`=structure(1:20, names=letters[7:26]),
#' `2001-01-01`=structure(sin(1:10), names=letters[11:20]))
#' x <- as.sos(list(p1=PORT1, p2=PORT2))
#' x2 <- as.sos(list(PORT1, PORT2))
#' @export
#'
as.sos <- function(data){
  # attempts to capture the case where input is a strategy
  if (any(c('data.frame', 'matrix', 'strategy') %in% class(data))){
    data <- list(data)
  }
  stopifnot('list' %in% class(data))
  data %<>% lapply(as.strategy)
  strategies_names <- names(data)
  ctr <- 1
  if (is.null(strategies_names)) strategies_names <- rep('', length(data))
  ind <- names(data) %>% str_match('^S([0-9]*)$') %>% na.omit
  if (length(ind)){
      offset_ind  <- {.[,2]} %>% as.numeric %>% max
      ctr <- ctr + offset_ind
  }
  for (i in seq(strategies_names)) {
    if (strategies_names[i] == ''){
      strategies_names[i] <- paste0('S', ctr)
      ctr <- ctr + 1
    }
  }
  names(data) <- strategies_names
  class(data) <- c('sos','list')
  data
}


#' Add one or more portfolios, as separate arguments, to a set-of-strategies (sos) object.
#'
#' @param data set of strategies (sos) object
#' @param ..., objects, one or more objects that can be converted into strategies
#' @return a sos object
#' @export
#'
add_strategy_all <- function(data, ...){
  x <- list(...)
  add_strategy_list(data, x)
}

#' Add one or more portfolios, wrapped into a list, to a set-of-portfolios (sos)
#' object.
#'
#' @param data set of strategies (sos) object
#' @param X, a list of objects taht can be converted into strategies
#' @return a sos object
#' @export
#'
add_strategy_list <- function(data, X){
  X %<>% as.sos
  strategies_names <- names(X)
  ctr <- 1
  if (is.null(strategies_names)) strategies_names <- rep('', length(data))
  ind <- names(data) %>% str_match('S([0-9]*)$') %>% na.omit
  if (length(ind)){
    offset_ind  <- ind[,2] %>% as.numeric %>% max
    ctr <- ctr + offset_ind
  }
  for (i in seq(X)){
    if (grepl('S([0-9]*)$', names(X)[i])) {
      names(X)[i] <- paste0('S', ctr)
      ctr <- ctr + 1
    }
  }
  OUT <- c(data, X)
  class(OUT) <- c('sos', 'list')
  OUT
}


#' Summarizes a set of strategies.
#'
#' @param sos object
#' @return a data frame with strategy name, first date, last date, n.dates, min no.assets, max no.assets, avg no.assets
#' @export
#'
summary.sos <- function(data){
  lapply(data, summary) %>% do.call(rbind,.)
}

#' Summarizes a strategy.
#'
#' @param strategy object
#' @return a data frame with strategy name, first date, last date, n.dates, min no.assets, max no.assets, avg no.assets
#' @export
#'
summary.strategy <- function(data){
  num_assets <- sapply(data, length)
  data.frame(
    start         = min(names(data)),
    end           = max(names(data)),
    no.dates      = length(data),
    min.no.assets = min(num_assets),
    max.no.assets = max(num_assets),
    avg.no.assets = mean(num_assets)
  )
}




#' Computes returns for a set of strategies and turnover.
#'
#' @param X set of strategies
#' @param RETS matrix of returns
#' @return list of performance objects
#'
#' @export
#'
backtest <- function(X, RETS, shortfall=0, borrow=0){
  X <- lapply(X, .compute_performance, RETS=RETS, shortfall=shortfall, borrow=borrow)
  ret      <- lapply(X, '[[', 'ret')      %>% do.call(cbind, .)
  ret_adj  <- lapply(X, '[[', 'ret_adj')  %>% do.call(cbind, .)
  turnover <- lapply(X, '[[', 'turnover') %>% do.call(cbind, .)
  list(ret = ret, ret_adj=ret_adj, turnover = turnover)
}



#' Computes gmv for a strategy.
#'
#' @param strat strategy object
#' @return a vector of gmvs
#'
#' @export
#'
compute_gmv.strategy <- function(strat){
  sapply(strat, function(x) sum(abs(x), na.rm=TRUE))
}
#' Computes gmv for a set of strategiesr.
#'
#' @param X set of strategies
#' @return a matrix of gross market values; rows are dates, columns are strategies
#'
#' @export
#'
compute_gmv.sos <- function(X){
  lapply(X, compute_gmv) %>% l2m(by.row = FALSE)
}

#' Computes returns for a set of strategies.
#'
#' @param X set of strategy object
#' @return matrix with turnover per period
#' @export
#'
compute_turnover.sos <- function(X){
  lapply(X, compute_turnover) %>% l2m(by.row = FALSE)
}

#' Computes returns for a strategy.
#'
#' @param X strategy object
#' @return matrix with turnover per period
#' @export
#'
compute_turnover.strategy <- function(X){
  N <- length(X) - 1
  y <- structure(rep(NA_real_, N), names=names(X)[-1])
  for (i in 2:length(X)){
    y[i-1] <- (opVectors(X[[i]], X[[i-1]], all=TRUE, FUN=`-`) %>% abs %>% sum) / (X[[i-1]] %>% abs %>% sum)
  }
  y
}


binary_op_strategy <- function(x, y, op){
  #   browser()
  if ('strategy' %in% class(x) & 'strategy' %in% class(y)){
    common_names <- intersect(names(x), names(y))
    out <- .lapply(common_names, function(n) opVectors(x[[n]], y[[n]], FUN=op, all=TRUE))
  } else if ('strategy' %in% class(x) & 'numeric' %in% class(y) & length(y)==1){
    out <- lapply(x, function(v) op(v, y))
  } else if ('strategy' %in% class(y) & 'numeric' %in% class(x) & length(x)==1){
    out <- lapply(y, function(v) op(x,v))
  } else {
    stop('Invalid argument.')
  }
  class(out) <- c('strategy', 'list')
  out
}

#' Sums two strategy objects.
#'
#' @param x strategy
#' @param y strategy
#' @return strategy
#'
#' @export
#'
`+.strategy` <- function(x, y){
  binary_op_strategy(x, y, `+`)
}

#' Subtracts two portfolios.
#'
#' @param x strategy
#' @param y strategy
#' @return strategy
#'
#' @export
#'
`-.strategy` <- function(x, y){
  binary_op_strategy(x, y, `-`)
}

#' Multiplies two portfolios, or a strategy and a scalar.
#'
#' @param x strategy or numeric
#' @param y strategy or numeric
#' @return strategy
#'
#' @export
#'
`*.strategy` <- function(x, y){
  binary_op_strategy(x, y, `*`)
}

#' Divides two portfolios, or a strategy by a scalar or a scalar by a
#' strategy
#'
#' @param x strategy
#' @param y strategy
#' @return strategy
#'
#' @export
#'
`/.strategy` <- function(x, y){
  binary_op_strategy(x, y, `/`)
}


#' apply trunc() to a strategy's holdings.
#'
#' @param x strategy
#' @return strategy
#'
#' @export
#'
`trunc.strategy` <- function(x){
  out <- .lapply(names(x), function(n) trunc(x[[n]]))
}

#' apply ceiling() to a strategy's holdings.
#'
#' @param x strategy
#' @return strategy
#'
#' @export
#'
`ceiling.strategy` <- function(x){
  out <- .lapply(names(x), function(n) ceiling(x[[n]]))
}

#' apply round() to a strategy's holdings.
#'
#' @param x strategy
#' @param digits integer
#' @return strategy
#'
#' @export
#'
`round.strategy` <- function(x, digits = 0){
  out <- .lapply(names(x), function(n) round(x[[n]], digits))
}


#' Longs of a strategy's holdings.
#'
#' @param x strategy
#' @return strategy
#'
#' @export
#'
long.strategy <- function(.data){
  y <- lapply(.data, function(x) x[x>0] )
  class(y) <- c('strategy', 'list')
  y
}

#' Longs of a strategy's holdings.
#'
#' @param x strategy
#' @return strategy
#'
#' @export
#'
short.strategy <- function(.data){
  y <- lapply(.data, function(x) x[x<0] )
  class(y) <- c('strategy', 'list')
  y
}



#' Dollar-neutral version of a strategy.
#'
#' @param x strategy
#' @param bench_id character, identifier of the benchmark
#' @return strategy
#'
#' @export
#'
dollar_neutral.strategy <- function(.data, bench_id){
  y <- lapply(.data, function(x) {
    x <- c(x, -sum(x))
    names(x)[length(x)] <- bench_id
    x
  })
  class(y) <- c('strategy', 'list')
  y
}

#' Betas of a strategy.
#'
#' @param x strategy
#' @param BETA matrix of the betas of the assets (dates/assets)
#' @return a vector
#'
#' @export
#'
get_betas.strategy <- function(.data, BETA){
  .data <- l2m(.data) %>% set_axes(axes(BETA))
  .data[is.na(.data)] <- 0
  dtes_missing <- setdiff(row.names(.data), row.names(BETA))
  id_missing <- setdiff(colnames(.data), colnames(BETA))
  if (length(dtes_missing)){
    stop('These date are missing in the BETA matrix: ', paste(dtes_missing, collapse=', '))
  }
  if (length(id_missing)){
    stop('These assets are missing in the BETA matrix: ', paste(id_missing, collapse=', '))
  }
  betas <- align_array(list(.data, BETA), all.dim=c(F,F)) %>% {.[[1]] * .[[2]]} %>%
    apply(1, sum, na.rm=TRUE)
  betas
}

#' Betas of a set of strategies
#'
#' @param .data set of strategies
#' @param BETA matrix of the betas of the assets (dates/assets)
#' @return a matrox of betas
#'
#' @export
#'
get_betas.sos <- function(.data, BETA){
  lapply(.data, get_betas.strategy, BETA=BETA) %>% l2m(by.row=FALSE)
}

#' Beta-neutral version of a strategy.
#'
#' @param x strategy
#' @param BETA matrix of the betas of the assets (dates/assets)
#' @param bench_id character, identifier of the benchmark
#' @return strategy
#'
#' @export
#'
beta_neutralize.strategy <- function(.data, BETA, bench_id){
  betas <- get_betas(.data, BETA)
  dtes <- names(.data)
  strat_neutral <- lapply(dtes, function(d) {
    y <- c(.data[[d]], -betas[d])
    names(y)[length(y)] <- bench_id
    y
    })
  names(strat_neutral) <- dtes
  class(strat_neutral) <- c('strategy', 'list')
  strat_neutral
}


#' Beta-neutral version of a set of strategies
#'
#' @param x sos
#' @param BETA matrix of the betas of the assets (dates/assets)
#' @param bench_id character, identifier of the benchmark
#' @return sos
#'
#' @export
#'
beta_neutralize.sos <- function(.data, BETA, bench_id){
  sos_neutral <- lapply(.data, function(x) beta_neutralize(x, BETA, bench_id))
  class(sos_neutral) <- c('sos', 'list')
  sos_neutral
}

#' Adds portfolios to a sos objects (safe hatch).
#'
#' @param data a set of strategies (sos) object
#' @param .... expressions
#' @return a sos object
#' @export
#'
mutate_.sos <- function(.data, .dots){
  #   dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  X <- lazy_eval(.dots, .data)
  add_strategy_list(.data, X)
}

#' Filter portfolios in a sos object by date (safe hatch).
#'
#' @param data a set of strategies (sos) object
#' @param ... expressions using the variable date, returning a boolean
#'
#' @export
#'
filter_.sos <- function(.data, .dots){
  X <- lapply(.data, function(x){
    tmp <- data.frame(date=names(x), stringsAsFactors=FALSE)
    ind <- lazy_eval(.dots, tmp)[[1]]
    x[ind]
  })
  class(X) <- c('sos','list')
  X
}

#' selects portfolios in a sos object by date (safe hatch).
#'
#' @param data a set of strategies (sos) object
#' @param ... unquoted names of strategies
#'
#' @export
#'
select_.sos <- function(.data, .dots){
#   browser()
  X <- lazy_eval(.dots, .data)
  names(X) <- vapply(.dots, function(x) deparse(x$expr), '')
  class(X) <- c('sos', 'list')
  X
}

#' Selects portfolios in a sos object (safe hatch).
#'
#' @param data a set of strategies (sos) object
#' @param ... names of the portfolios
#'
#' @export
#'
get_strat <- function(.data, ...){
  .dots <- c(...)
  .data[.dots]
}

#' Summarizes a set-of-strategies object.
#'
#' @param S set of strategies
#' @return a data frame with summary satistics
#' @export
#'
summary.sos <- function(X){
  lapply(X, function(x){
    elem <- sapply(x, length)
    data.frame(first_date = min(names(x)),
               last_date=max(names(x)),
               mean_nassets= mean(elem),
               min_nassets= min(elem),
               max_nassets= max(elem))
  }) %>%
    bind_rows %>%
    {data.frame(strategy =names(X),.)}
}



#' Makes a summary report table.
#'
#' @param data matrix, returns (or adjusted returns) arranged by date/assets (rows/asset ids)
#' @param ... functions operating of a time series of returns
#' @export
#'
make_report_list <- function(data, ...){
  fn_list <- list(...)
  make_report_all(data, fn_list)
}


#' Make a summary report table.
#'
#' @param data matrix, returns (or adjusted returns) arranged by date/assets (rows/asset ids)
#' @param fn_list, list of functions operating of a time series of returns
#' @export
#'
make_report_all <- function(data, fn_list){
  S <- data.frame(strategy=colnames(data))
  lapply(fn_list, function(fn) adply(data, 2, fn) %>% select(-X1))  %>%
    bind_cols %>%
    {cbind(S,.)}
}


#' Computes maximum drawdown.
#'
#' @param r numeric vector, returns
#' @param startDate character, earliest date, if specified
#' @param endDate character, latest date, if specified
#' @return a data frame containing drawdown, its start
#' and end dates
#' @export
compute_drawdown <- function(r, startDate=NULL, endDate=NULL){
  if (!is.null(startDate)) r <- r[names(r) >= startDate]
  if (!is.null(endDate)) r <- r[names(r) <= startDate]
  x_len <- length(r)
  max_drawdown <- 0
  drawdown_startdate <- ''
  drawdown_enddate <- ''
  for (n in seq(r)){
    x2 <- 1 + r[n:x_len]
    x_cumret <- cumprod(x2)
    x_min <- min(x_cumret)
    temp_drawdown <- x_min - 1
    if (temp_drawdown < max_drawdown){
      max_drawdown <- temp_drawdown
      drawdown_startdate <- names(r)[n]
      drawdown_enddate <- names(x2)[x_cumret == x_min][1]
    }
  }
  if (max_drawdown == 0) {
    maxdrawdown <- 0
    drawdown_startdate <- NA_character_
    drawdown_enddate <- NA_character_
  }
  out <- data.frame(maxdrawdown=max_drawdown,
             start.date=drawdown_startdate,
             end.date=drawdown_enddate,
             stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

#' Computes Sharpe Ratio for a set of returns, assuming iid returns.
#'
#' @param r numeric, returns
#' @param period numeric, average interval between return data
#' @param volAdjPeriod integer or NULL, if integer the returns are normalized by
#'   vol estimates of window volAdjPeriod centered at the return
#' @param startDate character, earliest date, if specified
#' @param endDate character, latest date, if specified
#' @author G.A.Paleologo
#' @return numeric, sharpe ratio computed as mean(r)/sd(r)*sqrt(period)
#' @export
compute_sharpe <- function(r, period=1, volAdjPeriod=NULL,
                           startDate=NULL, endDate=NULL){
  if (!is.null(volAdjPeriod)){
    r <- zoo(r, as.Date(names(r)))
    r <- rollapply(r, volAdjPeriod, function(x)
      x[floor(length(x))/2]/sd(x, na.rm=T), align='center')
  }
  if (!is.null(startDate)) r <- r[names(r) >= startDate]
  if (!is.null(endDate)) r <- r[names(r) <= startDate]
  SR <- (mean(r, na.rm=TRUE) / sd(r, na.rm=TRUE) * sqrt(252/period)) %>% round(2)
  if (is.null(volAdjPeriod)){
    out <- data.frame(Sharpe=SR, row.names=NULL)
  } else  {
    out <- data.frame(Adj.Sharpe=SR, row.names=NULL)
  }
  out
}


#' Computes alphas and betas of a strategy with respect to a benchmark.
#'
#' @param rets numeric, returns
#' @param benchmark numeric, time series of benchmarks
#' @param country character, country
#' @return data frame, alpha and beta of the strategy
#' @export
compute_alphabeta <- function(rets, benchmark, country='US'){
  benchmark %<>% v2df(c('date', 'benchmark'))
  rets %<>% v2df(c('date','ret'))
  A <- left_join(rets, benchmark, by='date') %>%
    dplyr::arrange(desc(date))
  stopifnot(all(!is.na(A)))
  numdays <- numTD(min(A$date), max(A$date), country=country)
  period <- numdays / nrow(rets)
  tmp <- lm(ret ~ benchmark, data=A)$coefficients
  data.frame(alpha = round(tmp[1]*252/period*100,2), beta = round(tmp[2], 2), row.names=FALSE)
}

#' Computes average annual returns for a time series of returns.
#'
#' @param rets numeric, returns
#' @param startDate character, earliest date, if specified
#' @param endDate character, latest date, if specified
#' @return data frame, annual returns
#' @export
compute_returns <- function(r, startDate=NULL, endDate=NULL, country='US'){
  if (!is.null(startDate)) r <- r[names(r) >= startDate]
  if (!is.null(endDate))   r <- r[names(r) <= startDate]
  dte_start <- min(names(r))
  dte_end   <- max(names(r))
  n <- 252/length(getTD(dte_start, dte_end, country=country))
  data.frame(return=(prod(r + 1))^n - 1)
}

#' Rademacher complexity.
#'
#' @param X matrix, data arranged by observations (rows) and hypotheses
#'   (columns). Row names of X must be dates of format "YYYY-MM-DD".
#'   Column names must not be empty
#' @param n integer, number of samples
#' @param delta numeric, probability
#' @return a list with estimates and best bound
#' @export
rademacher <- function(X=NULL, n=1e4L, delta=1e-3, period=12){
  nperiods <- nrow(X)
  dtes <- getDates(X)
  period <- sapply(2:length(dtes), function(i) numTD(dtes[i-1], dtes[i])) %>% mean
  scaling_factor <- sqrt(252/period)
  X_SD <- outer(rep(1,nrow(X)), apply(X, 2, sd))
  X_SIGN <- outer(rep(1,nrow(X)), apply(X, 2, function(x) sign(sum(x))))
  X_norm <- X / X_SD * X_SIGN
  S <- matrix(sample(c(1,-1), n*nperiods, replace=T), nrow=n)

  a_range <- seq(.001, .25, length=20)
  R <- rep(NA_real_,length=length(a_range))
  i <- 1
  for (a in a_range){
    tmp <- (S %*% pmax(pmin(X_norm,a),-a)/nperiods) %>%
      apply(., 1, max) %>%
      mean
    R[i] <-  tmp
    i <- i + 1
  }
  bounds <- (R + 2*a_range*sqrt(log(1/delta)/2/nperiods) + 2/nperiods/a_range)
  bestbound <- min(bounds)*scaling_factor
  #   cutoff <- a_range[BOUND==min(bounds)]
  estim <- apply(X_norm,2,mean)*scaling_factor
  list(SR=estim, bound=bestbound)
}

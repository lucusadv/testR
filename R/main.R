#' @export
compute_gmv <- function(x) UseMethod("compute_gmv")

#' @export
compute_turnover <- function(x) UseMethod("compute_turnover")

#' @export
long <- function(x) UseMethod("long")

#' @export
short <- function(x) UseMethod("short")

#' @export
dollar_neutralize <- function(x) UseMethod("dollar_neutralize")

#' @export
beta_neutralize <- function(x, BETA, fraction, bench_id) UseMethod("beta_neutralize")

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
.compute_performance <- function(PORT, RETS, shortfall=0, borrow=0, 
                                 keep_strategy=FALSE){
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
    # message(Sys.time(), ': backtest for date ', d)
    if (d %in% names(PORT)) {
      tmp_portfolio <- work_portfolio[[d]]
      scaling_factor <- ifelse(d > dtes_start, sum(abs(tmp_portfolio))/sum(abs(PORT[[d]])), 1)
      work_portfolio[[d]] <- scaling_factor * PORT[[d]]
      turnover[d] <- sum(abs(opVectors(work_portfolio[[d]], tmp_portfolio, FUN=`-`))) / 
        sum(abs(tmp_portfolio))
    }
    # browser()
    pnl[d] <- sum(work_portfolio[[d]] * R[d, names(work_portfolio[[d]])] )
    if (is.na(pnl[d])) stop('missing dates')
    if (d < dtes_end){
      d_next <- succ(d, dtes)
      v <- R[d, names(work_portfolio[[d]])] + 1
      work_portfolio[[d_next]] <- work_portfolio[[d]]*v
    }
  }
  # creates adjusted pnl. I am using an approximate number of trading days in
  # order to avoid dependencies on date libraries
  days_trading <- 252/365*as.numeric(as.Date(first(dtes))-as.Date(last(dtes)))
  portfolio_gmv <- sapply(work_portfolio, function(x) sum(abs(x)))
  portfolio_shortnmv <- sapply(work_portfolio, function(x) -sum(pmin(x,0)))
  turnover_costs <- shortfall*turnover*portfolio_gmv / 100
  borrow_costs <- portfolio_shortnmv*borrow
  pnl_adj <- pnl - borrow_costs - turnover_costs
  class(work_portfolio) <- c('strategy', 'list')
  if (!keep_strategy) work_portfolio <- NULL
  list(ret = pnl/portfolio_gmv,
       ret_adj = pnl_adj/portfolio_gmv,
       turnover = turnover,
       strategy = work_portfolio)
}

#' Computes L1 norm. Internal use only
#' 
#' @param x numeric vector
#' @return numeric
L1 <- function(x) sum(abs(x), na.rm = TRUE)

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
#' x0 <- as.sos(p1=PORT1, p2=PORT2)
#' x1 <- as.sos(PORT1)
#' x2 <- as.sos(PORT1, PORT2)
#' @export
#'
as.sos <- function(...){
  # attempts to capture the case where input is a strategy
  data <- list(...)
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

#' Add one or more portfolios, wrapped into a list, to a set-of-portfolios / 
#' set-of-strategies (sos) object.
#'
#' @param data set of strategies (sos) object
#' @param X, a list of objects that can be converted into strategies
#' @return a sos object
#' @note #REV!2 JWP 2015-08-11
#' @export
#'
add_strategy_list <- function(data, X){
  class(X) <- c('sos', 'list')
  strategies_names <- names(X)
  ctr <- 1
  if (is.null(strategies_names)) strategies_names <- rep('', length(data))
  ind <- names(data) %>% str_match('^S([0-9]*)$') %>% na.omit
  if (length(ind)){
    offset_ind  <- ind[,2] %>% as.numeric %>% max
    ctr <- ctr + offset_ind
  }
  for (i in seq(X)){
    if (grepl('^S([0-9]*)$', names(X)[i])) {
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

#' extracts a strategy from a set-of-strategies
#'
#' @param .data sos object
#' @param x string, strategy name
#' @return strategy
#'
#' @export
#'
`$.sos` <- function(.data, x){
  class(.data) <- 'list'
  y <- .data[[x]]
  class(y) <- c('strategy', 'list')
  y
}

#' extracts a strategy from a set-of-strategies
#'
#' @param .data sos object
#' @param x string, strategy name
#' @return strategy
#'
#' @export
#'
`[[.sos` <- function(.data, x){
  class(.data) <- 'list'
  y <- .data[[x]]
  class(y) <- c('strategy', 'list')
  y
}

#' extracts a strategy from a set-of-strategies
#'
#' @param .data sos object
#' @param x vector of strategies, strategy names
#' @return sos object
#'
#' @export
#'
`[.sos` <- function(.data, x){
  y <- list()
  class(y) <- c('sos', 'list')
  for (v in x)
    y[[v]] <- .data[[v]]
  y
}


#' Computes returns for a set of strategies and turnover.
#'
#' @param X set of strategies object
#' @param RETS matrix of returns
#' @param shortfall numeric expected shortfall (e.g. 0.20 = 20 bps)
#' @param borrow numeric borrow costs (e.g. 0.5 = 0.5%/yrs)
#' @return list of performance objects
#'
#' @export
#'
backtest <- function(X, RETS, shortfall=0, borrow=0){
  X <- lapply(X, 
                .compute_performance, 
                RETS = RETS, 
                shortfall = shortfall, 
                borrow = borrow)
  ret <- lapply(X, '[[', 'ret') %>% 
    do.call(cbind, .) %>% 
    set_axes(c('date', 'id'))
  ret_adj <- lapply(X, '[[', 'ret_adj') %>% 
    do.call(cbind, .) %>% 
    set_axes(c('date', 'id'))
  turnover <- lapply(X, '[[', 'turnover') %>% 
    do.call(cbind, .) %>% 
    set_axes(c('date', 'id'))
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
    common_names <- intersect(names(x), names(y)) %>% withNames
    out <- lapply(common_names, function(n) opVectors(x[[n]], y[[n]], FUN=op, all=TRUE))
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

#' Shorts of a strategy's holdings.
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
dollar_neutralize.strategy <- function(.data, bench_id = "78462F103"){
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
beta_neutralize.strategy <- function(.data, BETA, fraction = 1, 
                                     bench_id = "78462F103"){
  beta_hedges <- get_betas(.data, BETA) * fraction
  dtes <- names(.data)
  strat_neutral <- lapply(dtes, function(d) {
    y <- .data[[d]] 
    y[bench_id] <- ifelse(is.na(y[bench_id]), 
      -beta_hedges[d], 
      y[bench_id] - beta_hedges[d]
    )
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
beta_neutralize.sos <- function(.data, BETA, fraction = 1, 
                                bench_id = "78462F103"){
  sos_neutral <- lapply(.data, function(x) beta_neutralize(x, BETA, fraction, bench_id))
  class(sos_neutral) <- c('sos', 'list')
  sos_neutral
}

#' Adds portfolios to a sos objects (safe hatch).
#'
#' @param .data a set of strategies (sos) object
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
#' @param .dots expressions using the variable date, returning a boolean
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

#' Filter portfolios in a strategy object by date (safe hatch).
#'
#' @param .data strategy object 
#' @param ... expressions using the variable date, returning a boolean
#'
#' @export
#'
filter_.strategy <- function(.data, .dots){
  tmp <- data.frame(date=names(.data), stringsAsFactors=FALSE)
  ind <- lazy_eval(.dots, tmp)[[1]]
  x <- .data[ind]
  class(x) <- c('strategy','list')
  x
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
#' @param .data set of strategies
#' @return a data frame with summary satistics
#' @export
#'
summary.sos <- function(.data){
  .data %>% lapply(function(x){
    elem <- sapply(x, length)
    data.frame(first_date   = min(names(x)),
               last_date    = max(names(x)),
               mean_nassets = mean(elem),
               min_nassets  = min(elem),
               max_nassets  = max(elem))
  }) %>%
    bind_rows %>%
    {data.frame(strategy = names(.data),.)}
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
  axes(data) <- c('date', 'strategy')
  lapply(fn_list, function(fn) adply(data, 2, fn) %>% select(-strategy))  %>%
    bind_cols %>%
    {cbind(S,.)}
}



#' Computes date range
#'
#' @param r numeric vector, returns
#' @return a data frame containing start date and end date of the return data
#' @export
compute_daterange <- function(r){
  data.frame(start.date = min(names(r)),
             end.date = max(names(r)))
}




#' Computes maximum drawdown.
#'
#' @param r numeric vector, returns
#' @param startDate character, earliest date, if specified
#' @param endDate character, latest date, if specified
#' @return a data frame containing drawdown (in percentage points), its start
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
  out <- data.frame(maxdrawdown = round(100 * max_drawdown,2),
             start.date = drawdown_startdate,
             end.date = drawdown_enddate,
             stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

#' Computes Sharpe Ratio for a set of returns, assuming iid returns.
#'
#' @param rets numeric, vector of returns, element names are dates in format
#'   YYYY-MM-DD
#' @param period numeric, average interval between return data
#' @param volAdjPeriod integer or NULL, if integer the returns are normalized by
#'   vol estimates of window volAdjPeriod centered at the return
#' @param startDate character, earliest date, if specified
#' @param endDate character, latest date, if specified
#' @author G.A.Paleologo
#' @return numeric, sharpe ratio computed as mean(r)/sd(r)*sqrt(period)
#' @export
compute_sharpe <- function(rets, period=1, volAdjPeriod=NULL,
                           startDate=NULL, endDate=NULL){
  if (!is.null(volAdjPeriod)){
    rets <- zoo(rets, as.Date(names(rets)))
    rets <- rollapply(rets, volAdjPeriod, function(x)
      x[floor(length(x))/2]/sd(x, na.rm=T), align='center')
  }
  if (!is.null(startDate)) rets <- rets[names(rets) >= startDate]
  if (!is.null(endDate)) rets <- rets[names(rets) <= startDate]
  SR <- (mean(rets, na.rm=TRUE) / sd(rets, na.rm=TRUE) * sqrt(252/period)) %>% round(2)
  if (is.null(volAdjPeriod)){
    out <- data.frame(Sharpe=SR, row.names=NULL)
  } else  {
    out <- data.frame(Adj.Sharpe=SR, row.names=NULL)
  }
  out
}

#' Computes monthly hit rate of a strategy. Works only with daily backtests
#'
#' @param rets numeric, vector of returns, element names are dates in format
#'   YYYY-MM-DD
#' @return data frame, hit rate of the strategy by year-month
#' @export
compute_hitrate_monthly <- function(rets){
  rets %>% sign %>% compute_returns_monthly(what = 'mean') %>% {(. + 1)/2}
}

#' Computes monthly hit rate of a strategy. Works only with daily backtests
#'
#' @param rets numeric, vector of returns, element names are dates in format
#'   YYYY-MM-DD
#' @return data frame, return of the strategy by year-month
#' @export
compute_hitrate_yearly <- function(rets){
  rets %>% sign %>% compute_returns_yearly(what = 'mean') %>% {(. + 1)/2}
}

#' Computes monthly returns of a strategy. Works only with daily backtests
#'
#' @param rets numeric, vector of returns, element names are dates in format
#'   YYYY-MM-DD
#' @return data frame, return of the strategy by year-month
#' @export
compute_returns_monthly <- function(rets, what = 'product'){
  if (what =='product') {
    compounder <- function(x) prod(x + 1) - 1
  } else if (what == 'sum'){
    compounder <- sum
  } else if (what == 'mean'){
    compounder <- mean
  } else {
    stop('what argument must be product, sum, or mean.')
  }
  dtes_returns <- names(rets) %>%
    ensure_that(!any(duplicated(.)), err_desc = 'duplicated dates')
  dtes_returns %>% as.Date %>% diff %>% as.numeric %>%
    ensure_that(mean(.) <= 2, err_desc = 'returns do not appear to be daily')
  rets %>% v2df(c('date','ret')) %>%
    mutate(date=substr(date,1 , 7)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(nobs = length(ret), ret=compounder(ret)) %>%
    # ensure_that(mean(nobs) >= 4, err_desc = 'fewer than 4 returns per month') %>%
    {x <- data.frame(matrix(.$ret, nrow=1)); names(x) <- .$date;x}
}

#' Computes annual returns of a strategy for each year. It is stronly recommended that this be
#' run on daily backtests, although the function will run on monthly backtests.
#'
#' @param rets numeric, vector of returns, element names are dates in format
#'   YYYY-MM-DD
#' @return data frame, return of the strategy by year
#' @export
compute_returns_yearly <- function(rets, what = 'product'){
  if (what =='product') {
    compounder <- function(x) prod(x + 1) - 1
  } else if (what == 'sum'){
    compounder <- sum
  } else if (what == 'mean'){
    compounder <- mean
  } else {
    stop('what argument must be product, sum, or mean.')
  }
  dtes_returns <- names(rets) %>%
    ensure_that(!any(duplicated(.)), err_desc = 'duplicated dates') 
  dtes_returns %>%
    as.Date %>% diff %>% as.numeric %>%
    ensure_that(mean(.) <= 22, err_desc = 'returns do not appear to be monthly')
  rets %>% v2df(c('date','ret')) %>%
    mutate(date=substr(date, 1, 4)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(nobs = length(ret), ret=compounder(ret)) %>%
    {x <- data.frame(matrix(.$ret, nrow=1)); names(x) <- .$date; x}
}


#' Computes alphas and betas of a strategy with respect to a benchmark.
#'
#' @param rets numeric, vector of returns, element names are dates in format #'   YYYY-MM-DD
#' @param benchmark numeric, time series of benchmarks
#' @param country character, country
#' @return data frame, alpha and beta of the strategy
#' @note #REV!2 JWP 2015-08-11
#' @export
compute_alphabeta <- function(rets, benchmark, country='US'){
  dtes_returns <- names(rets) %>% 
    ensure_that(!any(duplicated(.)), err_desc = 'duplicated dates') %>%
    as.Date %>% diff %>% as.numeric %>%
    ensure_that(mean(.) <= 2, err_desc = 'returns do not appear to be daily')
  benchmark %<>% v2df(c('date', 'benchmark'))
  rets %<>% v2df(c('date','ret'))
  A <- left_join(rets, benchmark, by='date') %>%
    na.omit %>%
    dplyr::arrange(desc(date)) 
  numdays <- numTD(min(A$date), max(A$date), country=country)
  period <- numdays / nrow(rets)
  tmp <- lm(ret ~ benchmark, data=A)$coefficients
  data.frame(alpha = round(tmp[1]*252/period*100,2), beta = round(tmp[2], 2), row.names=FALSE)
}

#' Computes average average returns for a time series of returns. The default is
#' annualized, but it can be changed through the horizon parameter.
#' 
#' @param rets numeric, vector of returns, element names are dates in format 
#'   YYYY-MM-DD
#' @param startDate character, earliest date, if specified
#' @param endDate character, latest date, if specified
#' @param horizon numeric, number of trading dates over which the return is 
#'   computed. Use 252 for yearly returns, 126 for half-year, 63 for quarterly, 
#'   21 for monthly
#' @return data frame, annual returns
#' @export
compute_returns <- function(r, startDate=NULL, endDate=NULL, horizon=252, country='US'){
  if (!is.null(startDate)) r <- r[names(r) >= startDate]
  if (!is.null(endDate))   r <- r[names(r) <= startDate]
  dte_start <- min(names(r))
  dte_end   <- max(names(r))
  n <- horizon / length(getTD(dte_start, dte_end, country=country))
  data.frame(return=round(100*(prod(r + 1)^n - 1), 2))
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
  estim <- apply(X_norm * X_SIGN, 2, mean)*scaling_factor
  list(SR=estim, bound=bestbound)
}


#' Converts a matrix of subjects/observations to a Markov transition matrix
#'
#' The conversion follows the ideas of Stochastic Neighbor Embedding; see the
#' the papers by Hinton and Roweis, "Stochastic Neighbor Embedding", and Van Der
#' Maaten and Hinton, " Visualizing data using t-SNE". The basic formula is
#' given by:
#' p(j | i) = exp(-gamma * |x_j - x_i|^2)/ sum_k exp(-gamma * |x_k - x_i|^2)
#'
#' optimal width by requiring that the conditional gaussian conditional entropy
#' be identical across points. Let:
#' H_gamma_i(i) = - sum_k p(k | i) * log(p(k |i))
#' H_0 = log(N) H_infty = 0
#' Then when set gamma_i so that, for all i, exp(H_gamma_i(i))) = perplexity
#'
#' @param X matrix of observations rows are points, columns are coordinates
#' @param perplexity numeric
#' @return matrix, transition probability with X[i,j] =p(j|i)
#'
SNEmbed <- function(X, perplexity=30){
  perplexity_log <- log(perplexity)
  n <- nrow(X)
  e <- matrix(rep(1, n), ncol=1)
  Y <- X %*% t(X)
  v <- matrix(diag(Y),ncol=1)
  W <- v %*% t(e)
  DIST <- W  + t(W) - 2*Y
  gamma_min <- matrix(rep(0, n), ncol=1)
  DIST_median <- median(DIST)
  gamma_max <- matrix(20, nrow=n,ncol=1)
  tol <- 1e-5
  D <- exp(-DIST/DIST_median)
  # vectorized bisection method
  .fn <- function(g){
    (D ^ (g %*% t(e))) %>%
    {. / ( rowSums(.) %*% t(e) - 1)} %>%
    {-rowSums(. * log(.)) - perplexity_log}
  }
  obj_value <- rep(1, n)
  stopifnot(all(.fn(gamma_max) < 0))
  stopifnot(all(.fn(gamma_min) > 0))
  # browser()
  while (max(abs(obj_value)) > tol) {
    message(max(abs(obj_value)))
    gamma_test <- 0.5*(gamma_min + gamma_max)
    obj_value <- .fn(gamma_test)
    gamma_min <- ifelse(obj_value > 0, gamma_test, gamma_min)
    gamma_max <- ifelse(obj_value < 0, gamma_test, gamma_max)
  }
  (D ^ (gamma_test %*% t(e))) %>%
  {. / ( rowSums(.) %*% t(e) - 1)} %>%
  {diag(.) <- 0; .}
}




#' Estimates the trailing volatility of a return matrix. Exponential weighting. 
#' 
#' @param R matrix, asset returns
#' @param halflife numeric, half-life in weighting returns
#' @return a matrix of estimated volatilities
#'   
#' @author G.A.Paleologo
#' @export
estimate_volatility <- function(R, VOLT=NULL, halflife=126, country='US'){
  lambda <- 1-1/halflife
  dtes <- getDates(R)
  alldtes <- getTD(country=country)
  dte_interval <- (alldtes %in% dtes) %>% which %>% diff %>% mean
  if (dte_interval > 1.01) warning('return data do not appear to be daily.')
  ESTIMATE <- R^2
  ESTIMATE[1,] <- median(ESTIMATE[1,], na.rm=TRUE) # median x-sec vol
  for (i in 2:nrow(ESTIMATE)){
    message('estimating volatility for date ', dtes[i])
    ESTIMATE[i, ] <- ifelse(is.na(ESTIMATE[i, ]) , ESTIMATE[i-1, ], (1-lambda)*ESTIMATE[i, ] + lambda*ESTIMATE[i-1, ])  
  }
  ESTIMATE <- sqrt(ESTIMATE * 252/dte_interval) 
  ESTIMATE[is.na(R)] <- NA_real_
  # The last steps tapers new observations
  ESTIMATE <-  ( tail(ESTIMATE, -5) + 
                   tail(head(ESTIMATE, -1), -4) + 
                   tail(head(ESTIMATE, -2), -3) + 
                   tail(head(ESTIMATE, -3), -2) +
                   tail(head(ESTIMATE, -4), -1) +
                   head(ESTIMATE, -5)    ) / 6
  ESTIMATE
}

#' Estimates the trailing mean of a return matrix. Exponential weighting. 
#'
#' 
#' @param R matrix, asset returns
#' @param halflife numeric, half-life in weighting returns
#' @return a matrix of setimated mean
#'   
#' @author G.A.Paleologo
#' @export
estimate_mean <- function(R, MEAN=NULL, halflife=126, country='US'){
  lambda <- 1-1/halflife
  dtes <- getDates(R)
  alldtes <- getTD(country=country)
  dte_interval <- (alldtes %in% dtes) %>% which %>% diff %>% mean
  if (dte_interval > 1.01) warning('return data do not appear to be daily.')
  ESTIMATE <- R
  ESTIMATE[1,] <- 0
  for (i in 2:nrow(ESTIMATE)){
#     message('estimating mean for date ', dtes[i])
    ESTIMATE[i, ] <- ifelse(is.na(ESTIMATE[i, ]) , ESTIMATE[i-1, ], (1-lambda)*ESTIMATE[i, ] + lambda*ESTIMATE[i-1, ])  
  }
  ESTIMATE <- ESTIMATE * 252/dte_interval
  ESTIMATE[is.na(R)] <- NA_real_
  # The last steps tapers new observations
  ESTIMATE <-  ( tail(ESTIMATE, -5) + 
                   tail(head(ESTIMATE, -1), -4) + 
                   tail(head(ESTIMATE, -2), -3) + 
                   tail(head(ESTIMATE, -3), -2) +
                   tail(head(ESTIMATE, -4), -1) +
                   head(ESTIMATE, -5)    ) / 5
  ESTIMATE
}




# 
# 
# .compute_performance_fast <- function(STRAT, CUMRETS, shortfall=0, borrow=0){
#   stopifnot(all(names(STRAT) %in% row.names(RETS)))
#   rebal_dates <- names(STRAT)
#   start_date <- rebal_dates[1]
#   # R <- filter(CUMRETS, date >= start_date)
#   
#   bind[R, STRAT] <-
#     backtest_dates <- row.names(R)
#   num_dates <- length(backtest_dates)
#   backtest_ids <- intersect(colnames(R), Reduce(union, lapply(STRAT, names)))
#   num_id <- length(backtest_ids)
#   R <- R[, backtest_ids]
#   STRAT2 <- array(0, dim = c(num_dates, length(backtest_ids)), 
#                   dimnames = list(date = backtest_dates, id = backtest_ids))
#   pnl <- structure(rep(NA, num_dates), names=backtest_dates)
#   turnover <- structure(rep(0, length(rebal_dates) -1), names=rebal_dates[-1])
#   scaling_factor <- 1
#   for (i in seq(rebal_dates)){
#     d <- rebal_dates[i]
#     d_next <- rebal_dates[i+1]
#     if (is.na(d_next)) d_next <- tail(backtest_dates, 1)
#     ind <- which(backtest_dates == d):(which(backtest_dates == d_next) - 1)
#     work_portfolio <- structure(rep(0, num_id), names = backtest_ids)
#     work_portfolio[names(STRAT[[d]])] <- STRAT[[d]]
#     if (i > 1) {
#       j <- ind[1] - 1
#       project_portfolio <- STRAT2[j,] * R[j+1,]/R[j, ]
#       scaling_factor <- L1(project_portfolio)/L1(work_portfolio) 
#       work_portfolio <- scaling_factor * work_portfolio
#       turnover[d] <- L1(project_portfolio - work_portfolio) / 
#         L1(work_portfolio)
#       e <- matrix(rep(1, length(ind)), ncol = 1)
#       STRAT2[ind,] <- R[ind, ] * (e %*% (work_portfolio/R[j,]))
#     } else {
#       e <- matrix(rep(1, length(ind)), ncol = 1)
#       STRAT2[ind,] <- R[ind, ] * (e %*% work_portfolio)
#     }
#   } 
#   portfolio_gmv <- STRAT2 %>% abs %>% apply(1, sum)
#   pnl <- STRAT2 %>% apply(1, sum) %>% diff %>% set_names(head(backtest_dates,-1))
#   portfolio_shortnmv <- apply(pmax(-STRAT2, 0), 1, sum)
#   turnover_costs <- 1e-2*shortfall*opVectors(turnover,portfolio_gmv, all = FALSE, FUN = `*`)
#   borrow_costs <- portfolio_shortnmv*borrow
#   pnl_adj <- pnl - tail(borrow_costs,-1) - sum(turnover_costs)/length(pnl)
#   class(work_portfolio) <- c('strategy', 'list')
#   list(ret = pnl/head(portfolio_gmv, -1),
#        ret_adj = pnl_adj/head(portfolio_gmv, -1),
#        turnover = turnover
#   )
# }

#' Computes the alpha curve of a strategy.
#'
#' The alpha curve of a strategy is defined as the pnl
#' of the strategy after n days after trading. 
#' All strategy dates must be contained in the returns dates.
#'
#' @param PORT list or strategy, holdings of strategy (one element per date)
#' @param RETS matrix of returns
#' @param nperiods numeric, number of days over which the alpha curve is
#' estimated
#' @return alpha curve
#' 
#' @export
compute_alphacurve <- function(PORT, RETS, nperiods = 21){
  R_cum <- RETS %>% 
    {.[is.na(.)] <- 0; .} %>% 
    {apply(. + 1, 2, cumprod)}
  dtes <- names(PORT) %>% 
    ensure_that(length(.) == length(unique(.)), err_desc = 'duplicated dates in strategy')
  perf <- list()
  for (d in dtes){
    w <- PORT[[d]] %>% {./sum(abs(.), na.rm = TRUE)}
    ids <- names(w) 
    i <- which(row.names(R_cum) == d)
    if (i > 1 & i < nrow(R_cum)-nperiods+1){
      N <- min(nrow(R_cum), i+nperiods)
      R_cum2 <- R_cum[i:N, ids] %>% apply(1, function(x) x/ R_cum[i-1, ids]) %>% t
      perf[[d]] <- (R_cum2 %*% w)[,] %>% {. - sum(w)} %>% head(nperiods) 
      names(perf[[d]]) <- sprintf('X%4.4i', 1:length(perf[[d]]))
    }
  } 
  data.frame(period =1:nperiods, 
             alpha = perf %>% 
               l2m(by.row=FALSE) %>%
               {.[order(row.names(.)),] } %>%
               apply(1, mean, na.rm=TRUE) %>%
               as.numeric
  )
}


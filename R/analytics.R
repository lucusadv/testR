#' Estimates the trailing beta of a return matrix  
#' 
#' 
#' @param R matrix, asset returns
#' @param BETA matrix, existing beta matrix. If NULL, the matrix is estimated
#'   from scratch; otherwise only dates following the latest date in BETA are
#'   estimated.
#' @param halflife numeric, half-life in weighting returns
#' @param width numeric, window width used for estimation
#' @param bench_id character, id of the benchmark
#' @return a matrix
#' @note #REV!1 JWP 2015-08-10 | #REV!2 JWP 2015-08-10
#'   
#' @author G.A.Paleologo
#' @export
estimate_beta <- function(R, BETA=NULL, halflife=126, width=253, bench_id="78462F103"){
  w <- exp(-(width:1)/halflife)
  R_dates <- getDates(R)
  if (!is.null(BETA)){ 
    beta_dates <- R_dates[R_dates > max(getDates(BETA))]
  } else {
    beta_dates <- tail(R_dates, -63) # at least three months of data
  }
  # this has nrows = ( n_dates - halflife) 
  BETA_est <- array(NA_real_, 
                    dim=c(length(beta_dates), ncol(R)), 
                    dimnames= list(date = beta_dates, id=colnames(R))) 
  bench_R <- R[, bench_id] 
  # this function returns the index range prior to date d
  validInterval <- function(d){
    i <- which(R_dates == d) - 1
    j <- max(i-width + 1, 1)
    i:j
  }
  for (d in beta_dates){ 
    message(Sys.time(), ': estimating beta for date ', d)
    ind <- validInterval(d)
    width_effective <- length(ind)
    w_temp <- w[( width - width_effective + 1 ):width]
    R_temp <- R[ind,, drop = FALSE]
    bench_R_temp <- bench_R[ind]
    var_bench_temp <- sum(w_temp * bench_R_temp^2, na.rm=TRUE) / sum(w_temp*!is.na(bench_R_temp)) #??! This assumes that the E[bench_R] = 0, correct?
    BETA_est[d, ] <- apply(R_temp, 2, function(x) {
      tmp <- w_temp * x * bench_R_temp
      sum(tmp, na.rm = T) / sum( w_temp * !is.na(tmp)) #??! This assumes that the E[x] = 0, correct?
      }) / var_bench_temp
  }
  # smooths BETA over 5 days to reduce impact of new observations
  BETA_est <-  ( tail(BETA_est, -5) + 
    tail(head(BETA_est, -1), -4) + 
    tail(head(BETA_est, -2), -3) + 
    tail(head(BETA_est, -3), -2) +
    tail(head(BETA_est, -4), -1) +
         head(BETA_est, -5)    ) / 6
  if (!is.null(BETA)){
    BETA_est <- overlay(BETA, BETA_est)
  }
  BETA_est
}

#' Estimates the momentum of a return matrix
#' 
#' 
#' @param R matrix, asset returns
#' @param width numeric, window width used for estimation
#' @param bench_id character, id of the benchmark. If id is NULL, then returns 
#'   are used
#' @return a data frame with fields date (format YYYY-MM-DD), id, and raw
#'   loadings
#' @note #REV!2 JWP 2015-08-11   
#' @author G.A.Paleologo
#' @export
estimate_momentum <- function(R, width = 253, bench_id = "78462F103"){
  if (!is.null(bench_id)){
    bench_id %>% ensure_that(. %in% getID(R), 
                             err_desc = "benchmark is missing") 
    spy <- R[, bench_id]
    R %<>% apply(2, function(x) x - spy )
  }
  R %<>% {.[is.na(.)] <- 0; .} %>% {. + 1} %>% 
    apply(2, cumprod) 
  R_dates <- getDates(R)
  ndates <- nrow(R)
  MOME_est <- R[-1:-width, ] / R[1:(ndates - width), ]
  # smooths momentum est over 5 days to reduce impact of new observations
  MOME_est <-  ( tail(MOME_est, -5) + 
                   tail(head(MOME_est, -1), -4) + 
                   tail(head(MOME_est, -2), -3) + 
                   tail(head(MOME_est, -3), -2) +
                   tail(head(MOME_est, -4), -1) +
                   head(MOME_est, -5)    ) / 6
  MOME_est %>% 
    melt(as.is = TRUE) %>% 
    set_names(c('date','id', 'momentum'))
}




# TheilSenEstimator
estimate_theilsen <- function(x, y) {
  A <- outer(x, x, `-`)[lower.tri(A)]
  B <- outer(y, y, `-`)[lower.tri(B)]
  median(B/A, na.rm=TRUE)
}

#' Estimates the trailing beta of a return matrix using a Theil-Sen Estimator.
#' Not for practical use yet.
#' Specifically, this is too slow at o(width^2*N_assets*Nperiods). One should
#' take advantage of the updating (over time), and of the fact that there are
#' algorithms that replace width^2 with width*log(width).
#' 
#' 
#' @param R matrix, asset returns
#' @param width numeric, window width used for estimation
#' @param bench_id character, id of the benchmark
#' @return a matrix
#'   
#' @author G.A.Paleologo 
estimate_beta_theilsen <- function(R, width=253, bench_id="78462F103"){
  R_dates <- getDates(R)
  beta_dates <- tail(R_dates, -63) # at least three months of data
  BETA_est <- array(NA_real_, 
                    dim=c(length(beta_dates), ncol(R)), 
                    dimnames= list(date = beta_dates, id=colnames(R))) 
  bench_R <- R[, bench_id] 
  # this function returns the index range prior to date d
  validInterval <- function(d){
    i <- which(R_dates == d) - 1
    j <- max(i-width + 1, 1)
    i:j
  }
  for (d in beta_dates){ 
    message(Sys.time(), ': estimating beta for date ', d)
    ind <- validInterval(d)
    width_effective <- length(ind)
    R_temp <- R[ind,, drop = FALSE]
    bench_R_temp <- bench_R[ind]
    BETA_est[d, ] <- mclapply(1:ncol(R), function(n) {
      y <- R_temp[ , n]
      estimate_theilsen(x, y)
    }, mc.preschedule = FALSE, mc.cores = 4) 
  } 
  BETA_est
}


# unused function in the absence of a risk model
riskDecomp <- function(dte, 
                       portfolio,
                       model = 'AXUS3',
                       type = c('MH', 'MH-S', 'MH-M'),
                       workDir = file.path(getOption('axiomaus'),'AxiomaRiskModels-FlatFiles'),
                       cacheDir = file.path(getOption('axiomaus'),'AxiomaRiskModels-Rdata'),
                       idtable = file.path(getOption('stockdir'), 'cusip.ticker.Rdata')){
  B <- getAxioma(dte, model = model, type = type, workDir = workDir, cacheDir = cacheDir, idtable = idtable)
  Omega <- getAxiomaCov(dte, model = model, type = type, workDir = workDir, cacheDir = cacheDir)
  D <- getAxiomaFld(dte, model = model, type = type, field = "SpecificRisk", workDir = workDir, cacheDir = cacheDir, idtable = idtable)
  cusips <- names(portfolio)
  EXP <- portfolio %*% B[cusips, ]
  D <- D[cusips]
  IDIOVAR <- sum((portfolio*D)^2)
  FACTVAR <- (matrix(EXP, nrow=1) %*% Omega %*% matrix(EXP, ncol=1))[,] 
  TOTVAR <- FACTVAR + IDIOVAR
  PERCIDIO <- IDIOVAR/TOTVAR
  STYLEVAR <- (matrix(EXP[, 1:11], nrow=1) %*% Omega[1:11,1:11] %*% matrix(EXP[,1:11], ncol=1))[,] 
  INDUSTRYVAR <- (matrix(EXP[, -(1:11)], nrow=1) %*% Omega[-(1:11), -(1:11)] %*% matrix(EXP[, -(1:11)], ncol=1))[,] 
  SINGLEFACVAR <- (EXP[]^2 *diag(Omega))[,] 
  list(PERCIDIO = PERCIDIO, TOTVAR = TOTVAR, IDIOVAR = IDIOVAR, 
       FACTVAR = FACTVAR, STYLEVAR = STYLEVAR, INDUSTRYVAR = INDUSTRYVAR, 
       SINGLEFACVAR = SINGLEFACVAR)
}

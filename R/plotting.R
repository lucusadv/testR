#' plot a vector with date time-stamps
#'
#' The data are in list form
#'
#' @param x list or matrix, each element a numeric vector  
#' @param main character, title
#' @return side effect: plot 
#' @author G.A.Paleologo 
#' @export
#' 
plotrets <- function(x, main = 'asset', ylim = NULL){
  #labelopt <- theme(legend.position = "none") 
  plotts(x, main = main, ylim = ylim, cumulative=TRUE, geometric=TRUE, ylabel='return')
}


#' plot list of time series
#'
#' The data are in list form
#'
#' @param x list or matrix, each element a numeric vector of period returns
#' @param main character, title
#' @param ylim numeric, length-two vector of range of y values
#' @param cumulative boolean, should cumulative data should be computed
#' @return side effect: plot 
#' @author G.A.Paleologo 
#' @export
#' 
plotts <- function(x, main='asset', ylim = NULL, 
                   cumulative=FALSE, geometric=FALSE, 
                   ylabel='y'){
  if (is.list(x))  x <- l2m(x, by.row=FALSE)  
  if (is.vector(x)) {
    labelopt <- theme(legend.position = "none") 
    x <- matrix(x, ncol=1, dimnames = list(names(x), main))
  } else {
    labelopt <- theme(legend.position = "right") 
  }
  x <- x[order(row.names(x)), , drop=FALSE]
  DS <- x
  DS[is.na(DS)] <- 0
  stopifnot(cumulative | (!cumulative & !geometric))
  if (cumulative & !geometric)  {
    DS <- apply(DS, 2, cumsum)
  }
  if (cumulative & geometric)  {
    DS <- apply(DS, 2, function(x) log(cumprod(1+x)))
    ylabel <- sprintf("log(%s)", ylabel)             
  }
  DS[is.na(x)] <- NA
  DS <- melt(DS)
  names(DS) <- c('Date', 'Type', 'y')
  DS$Date <- as.Date(as.character(DS$Date))
  if (is.null(ylim)) ylim <- range(DS$y, na.rm=TRUE)
  p <- ggplot(DS, aes(x=Date, y=y, group=Type, color=Type, linetype=Type)) + 
    geom_line() + 
    ggtitle(main) + 
    scale_y_continuous(limits = ylim) +
    ylab(ylabel) +
    scale_x_date(labels = date_format("%m-%Y"), minor_breaks='1 year')  
  print(p)  
}


#' plot IR from list of returns
#'
#' IRs are estimated based on time series of returns. The empirical volatility is
#' estimated using a gaussian kernel of standard deviation equal to width.
#' The per-period IRs are based on the average of the estimated IRs between two periods
#' and are associated to the preceding period.
#'
#' @param x list, each element a numeric vector of period returns
#' @param main character, title 
#' @param ylim numeric, pair vector with range of y values
#' @param width numeric, width of periods of the kernel for centered volatility estimation
#' @param interval numeric, number of days between IR
#' @param collapse boolean, should the plot have as x-axis lags or dates? 
#'  In the latter case, IR starts on relevant date and is plotted for each lag afterward
#' @return side effect: plot 
#' @author G.A.Paleologo 
#' @export
#'  
rI_plot <- function(x, main='', ylim = NULL, width = 42, interval=252, collapse=TRUE){
  interval <- interval + (width %% 2 == 1) # converts to nearest even
  width <- width + (width %% 2 == 0) # converts to nearest odd
  halfwidth <- floor(width/2) 
  # tapered mean
  intervalweights <- c( (1:3)/3, rep(1, interval-6), (3:1)/3)
  x <- l2m(x, by.row=FALSE) 
  dtes <- row.names(x)
  x <- m2zoo(x)
  weights <- dnorm(-halfwidth:halfwidth, sd=halfwidth/2)
  DS <- rollapply(x, width=width, function(x) {
    x[halfwidth] / sqrt(weighted.mean(x^2, w=weights, na.rm=TRUE))
  }, align='center')
  DS <- rollapply(DS, width=interval, function(x) {
    weighted.mean(x, w=intervalweights, na.rm=TRUE) 
  }, align='left') * sqrt(252)
  ind <- seq(from=interval/2, to=nrow(DS), by=interval)
  
  DS <- t(as.matrix(DS[ind,])) #columns are dates, centered, rows are lags label
  DS <- melt(DS)
  names(DS) <- c('Lag', 'Date', 'IR')
  DS$Lag <- sub('^[lL]ag','', as.character(DS$Lag))
  DS$Lag <- as.integer(DS$Lag)
  if (!collapse){
    lags <- DS$Lag
    for (i in 1:nrow(DS)){
      DS$Lag[i] <- dtes[which(dtes==as.character(DS$Date[i]))+lags[i]-1]
    }
    DS$Lag <- as.Date(DS$Lag)
  }
  p <- ggplot(DS, aes(x=Lag, y=IR, group=Date, color=Date)) + 
    geom_line() + 
    ggtitle(main) 
  print(p)  
}

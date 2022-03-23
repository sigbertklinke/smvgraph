#' @rdname trend_season
#' @title Trend and aeasonality estimation of a univariate time series
#' @description Estimate a trend and seasonaliyt for a time series.
#' Available functions:
#' * `trend_season` to generate an estimate
#' * `print` to print the estimate
#' * `summary` to summarize the etsimate result
#' * `plot` to plot the time series, its estimation and the residuals
#' * `coef` to extract the coefficients if a seasonality estimation was done
#' * `residuals` to extract the residuals of the model
#' * `fitted` to the fitted values
#' @param t ts: time series object
#' @param trend character: trend method, either `none` (default), `linear` or `exponential`
#' @param season character: seasonality method, either `none` (default), `additive` or `multiplicative`
#' @param which integer: what to plot, `1` time series and estimation (default) or `2` residuals
#' @param x,object trend_season: estimated time series 
#' @param y unused
#' @param ...  unused
#'
#' @return `trend_season` returns a `trend_season` object with 
#' * `call` the function call
#' * `ts` the input time series
#' * `trend` the trend estimation (`ts` object)
#' * `trend.residuals` the residuals of the trend estimation (`ts` object)
#' * `season` the trend and season estimation (`ts` object)
#' * `season.residuals` the residuals of the trend and season estimation (`ts` object)
#' * `coefficients` the coefficients used in the seasonality estimation
#' * `residuals` the residuals of the model
#' * `fitted.values` the fitted values of the model
#' @export
#'
#' @examples
#' tts <- trend_season(austres, "linear")
#' print(tts)
#' summary(tts)
#' plot(tts)
#' plot(tts, which=2)
#' residuals(tts)
#' fitted(tts)
#' coef(tts)  # if NULL then no seasonality was estimated
trend_season <- function(t, ...) { UseMethod("trend_season") }

#' @rdname trend_season
#' @export
trend_season.default <- function(t, trend=c("constant", "linear", "exponential"), season=c("none", "additive", "multiplicative"), ...) {
  newts <- function(val, t) { ts(val, start=stats::start(t), end=stats::end(t), frequency = stats::frequency(t)) }
  #
  stopifnot('ts' %in% class(t))
  ret          <- list(call=deparse(match.call()), ts=t)
  trend.param  <- match.arg(trend)
  season.param <- match.arg(season)
  T      <- length(t)
  ts     <- t
  if (trend.param=="constant") ret$trend <- newts(rep(mean(t), T), t)
  if (trend.param=="linear") {
    lmo       <- stats::lm(y~x, data=data.frame(x=1:T, y=t))
    ret$trend <- newts(stats::fitted(lmo), t)
  }
  if (trend.param=="exponential") {
    lmo       <- stats::lm(log(y)~x, data=data.frame(x=1:T, y=t))
    ret$trend <- newts(exp(stats::fitted(lmo)), t)
  }
  ret$trend.residuals <- newts(t-ret$trend, t)
  #
  if(season.param=="additive") {
    index <- rep_len(1:stats::frequency(t), T)
    ret$coefficients <- tapply(t-ret$trend, index, mean)
    ret$season <- newts(ret$trend + rep_len(ret$coefficients, T), t)
    ret$season.residuals <- newts(t-ret$season, t)
  }
  if(season.param=="multiplicative") {
    index <- rep_len(1:stats::frequency(t), T)
    ret$coefficients <- tapply(t/ret$trend, index, mean)
    ret$season <- newts(ret$trend * rep_len(ret$coefficients, T), t)
    ret$season.residuals <- newts(t-ret$season, t)
  }
  ret$residuals <- if (is.null(ret$season.residuals)) ret$trend.residuals else ret$season.residuals 
  ret$fitted.values <- if (is.null(ret$season)) ret$trend else ret$season   
  ret$r.square <- 1-sum(ret$residuals^2)/sum((t-mean(t))^2)
  structure(ret, class="trend_season")
}

#' @rdname trend_season
#' @export
print.trend_season <- function (x, ...) {
  for (i in 1:length(x)) {
    if ("ts" %in% class(x[[i]])) print(x[[i]])
  }
} 

#' @rdname trend_season
#' @export
summary.trend_season <- function (object, ...) {
  print(object$call)
  ret <- NULL
  retn <- c()
  for (i in 1:length(object)) {
    if ("ts" %in% class(object[[i]])) {
      ret <- rbind(ret, stats::quantile(object[[i]]))
      retn <- c(retn, names(object)[i])
    }
  }
  row.names(ret) <- retn
  ret
} 

#' @rdname trend_season
#' @export
plot.trend_season <- function (x,  y, which=1, ...) {
  stopifnot (all(which %in% 1:2))
  args <- list(...)
  lwd  <- getval(args$lwd, 1)
  lty  <- getval(args$lty, 1)
  pch  <- getval(args$pch, 19)
  cex  <- getval(args$cex, 1)
  type <- getval(args$type, "b")
  if (1 %in% which) {
    ylim <- range(x$ts, x$trend, x$season, na.rm=TRUE)
    plot(x$ts, ylab="", main="Time series & estimation", sub="blue = trend / green = trend+season", ylim=ylim,
         type=type, lwd=lwd, lty=lty, pch=pch, cex=cex)
    lines(x$trend, col="blue", type=type, lwd=lwd, lty=lty, pch=pch, cex=cex)
    lines(x$season, col="green", type=type, lwd=lwd, lty=lty, pch=pch, cex=cex)
  }
  if (2 %in% which) {
    ylim <- range(x$trend.residuals, x$season.residuals, na.rm=TRUE)
    plot(x$trend.residuals,  col="blue", ylab="", ylim=ylim, main="Residuals", sub="blue = trend / green = trend+season",
         type=type, lwd=lwd, lty=lty, pch=pch, cex=cex)
    lines(x$season.residuals, col="green", type=type, lwd=lwd, lty=lty, pch=pch, cex=cex)
  }
}
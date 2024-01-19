# source: https://gitlab.com/cees.de.valk/trend_knmi/-/blob/master/R/climatrend.R?ref_type=heads
#' @name  climatrend
#'
#' @title climatrend
#'
#' @description Fit a trendline to an annually sampled time-series by local linear regression (LOESS)
#'
#' @param t years, increasing by 1 (double(n))
#' @param y annual values; missing values as blanks are allowed near beginning and end (double(n))
#' @param p (optional) confidence level for error bounds (default: 0.95)) (double)
#' @param t1 (optional) first year for which trendline value is compared in test (double)
#' @param t2 (optional) second year (see t1)
#' @param ybounds (optional) lower/upper bound on value range of y (default: c(-Inf, Inf)) (double(2))
#' @param drawplot (optional): FALSE/TRUE or label on y-axis (default: FALSE) (logical or character)
#' @param draw30 (optional): add 30-year moving averages to plot (default: FALSE) (logical).
#'
#' @usage Value <- climatrend(t, y, p, t1, t2, ybounds, drawplot, draw30)
#'
#' @return A list, with members:
#'   \item{t}{years}
#'   \item{trend}{trendline in y for years in t}
#'   \item{p}{confidence level}
#'   \item{trendubound}{lower confidence limit}
#'   \item{trendlbound}{upper confidence limit}
#'   \item{averaget}{central value of t in a 30-year interval}
#'   \item{averagey}{30-year average of y}
#'   \item{t1}{first year for which trendline value is compared in test}
#'   \item{t2}{second year for which trendline value is compared in test}
#'   \item{pvalue}{of test of no longterm change}
#'   \item{ybounds}{bounds on value range of y}
#'
#' @details
#'   The trendline can be regarded as an approximation of a 30-year average, which has a smooth
#'   appearance and is extended toward the beginning and end of the time-series.
#'
#'   It is based on linear local regression, computed using the R-function loess.
#'   It uses a bicubic weight function over a 42-year window. In the central part
#'   of the time-series, the variance of the trendline estimate is approximately equal to
#'   the variance of a 30-year average.
#'
#'   To test the proposition of no long-term change between the years t1 and t2, these years
#'   need to be supplied. The result is the p-value: the probability (under the proposition)
#'   that the estimated trendline values in t2 and t1 differ more than observed.
#'
#'   version: 09-Mar-2021
#'
#' @references
#' KNMI Technical report TR-389 (see http://bibliotheek.knmi.nl/knmipubTR/TR389.pdf)
#'
#' @author Cees de Valk \email{cees.de.valk@knmi.nl}
#'
#' @export
climatrend <- function(t, y, p, t1, t2, ybounds, drawplot, draw30)
{
  # fixed parameters
  width <- 42
  control <- loess.control(surface = "direct", statistics= "exact",
                           iterations= 1)
  # check input
  # t and y
  if (missing(t) | missing(y)) {
    list()
    stop("t or y missing.")
  }
  if (length(t)< 3 | length(y)!= length(t)) {
    list()
    stop("t and y arrays must have equal lengths greater than 2.")
  }
  # check input
  if (any(is.na(t)) | sum(!is.na(y))< 3) {
    list()
    stop("t or y contain too many NA.")
  }
  
  # p
  if (missing(p)) {
    p <- NA
  }
  if (length(p)!= 1) {
    p <- NA
  }
  if (is.na(p) | !(p> 0 & p< 1)) {
    p <- 0.95   # default confidence level
  }
  # t1 and t2
  if (missing(t1) | missing(t2)) {
    t1 <- Inf; t2 <- -Inf
  }
  if (length(t1)!= 1 | length(t2)!= 1) {
    t1 <- Inf; t2 <- -Inf
  }
  if (is.na(t1) | is.na(t2)) {
    t1 <- Inf; t2 <- -Inf
  }
  # ybounds
  ybounds0 <- c(-Inf, Inf)
  if (missing(ybounds)) {
    ybounds <- ybounds0
  }
  if (length(ybounds)!= 2) {
    ybounds <- ybounds0
  }
  id <- is.na(ybounds)
  ybounds[id] <- ybounds0[id]
  ybounds <- sort(ybounds)
  
  # drawplot
  ylab= ""
  if (missing(drawplot)) {
    drawplot <- FALSE
  }
  drawplot <- drawplot[1]
  if (is.character(drawplot)) {
    ylab <- drawplot
  } else if (is.na(drawplot) | !is.logical(drawplot)) {
    drawplot <- FALSE
  }
  
  # draw30
  if (missing(draw30)) {
    draw30 <- FALSE
  }
  draw30 <- draw30[1]
  if (!is.logical(draw30)) {
    draw30 <- FALSE
  }
  
  # dimensions etc.
  t <- as.vector(t, mode= "double")
  y <- as.vector(y, mode= "double")
  dt <- diff(t[1:2])
  n <- length(y)
  ig <- !is.na(y)
  yg <- y[ig]
  tg <- t[ig]
  ng <- sum(ig)
  # if (any(diff(tg)!= dt)) {
  #   stop("NA may only occur in y only at beginning and/or end of time-series")
  # }
  
  # check values of bounds
  if (any(yg< ybounds[1]) | any(yg> ybounds[2])) {
    stop("Stated bounds are not correct: y takes values beyond bounds.")
  }
  
  # averages over 30 time-steps
  avt <- avy <- avysd <- NULL
  if (ng> 29) {
    avt <- tg+dt/2  # time (end of time-step, for 30-year averages)
    avy <- filter(yg, rep(1, 30)/30, method = "convolution")
    avy2 <- filter(yg^2, rep(1, 30)/30, method = "convolution")
    avysd <- sqrt(avy2-avy^2)
    ind <- 15:(ng-15)
    avt <- avt[ind]
    avy <- avy[ind]
    avysd <- avysd[ind]
  }
  
  # linear LOESS trendline computation
  span <- width/ng
  mdl <- loess(y ~ t, data= data.frame(t= tg, y=yg), span= span,
               degree= 1, control= control)
  # mdl <- loess(y ~ t, data= data.frame(t= t, y=y), span= span, degree= 1)
  pre <- predict(mdl, newdata= data.frame(t= t), se= TRUE)
  trend <- pre$fit          # trendline
  trendsd <- pre$se.fit     # standard deviation of trendline
  
  # confidence limits (normal approximation)
  trendub=  trend + trendsd*qnorm(1-(1-p)/2)
  trendlb=  trend - trendsd*qnorm(1-(1-p)/2)
  
  # apply bounds
  trend <- pmin(trend, ybounds[2])
  trend <- pmax(trend, ybounds[1])
  trendub <- pmin(trendub, ybounds[2])
  trendub <- pmax(trendub, ybounds[1])
  trendlb <- pmin(trendlb, ybounds[2])
  trendlb <- pmax(trendlb, ybounds[1])
  
  # pvalue for trend
  pvalue <- NULL
  if (t2 %in% t & t1 %in% t & t2>= t1+30) {
    y1 <- trend[t1== t]
    y2 <- trend[t2== t]
    y1sd <- trendsd[t1== t]
    y2sd <- trendsd[t2== t]
    # two-sided test for absence of trend
    pvalue <- (1-pnorm(abs(y2-y1)/sqrt(y1sd^2+y2sd^2)))*2
  } else if (t2> -Inf & t1< Inf) {
    warning("no p-value: t1 or t2 not in t, or t2 too close to t1.")
  } else {
    t1 <- NULL
    t2 <- NULL
  }
  
  # plotting
  if (!(drawplot== FALSE)) {
    par(pty= "s", cex= 1)
    ylim <- c(min(pmin(y, trendlb), na.rm = TRUE), max(pmax(y, trendub), na.rm = TRUE))
    ylim[2] <- ylim[1] + (ylim[2]-ylim[1])*1.0
    plot(t, y, type= "l", xlab= "", ylab= ylab, ylim= ylim)
    grid()
    if (draw30== TRUE) {
      points(avt, avy, pch= 18, cex= 0.5, col= "black")
    }
    points(t, y, pch= 15, cex= 0.75)
    lines(t, trend, lwd= 2)
    lines(t, trendub, lwd= 1, lty= 2)
    lines(t, trendlb, lwd= 1, lty= 2)
    legendpos <- "topleft"
    tr <- y[!is.na(y)]
    lr <- length(tr)
    ir <- 1:(lr/2)
    id <- which.max(c(mean(tr[ir])-ylim[1], mean(tr[lr+1-ir])-ylim[1],
                      ylim[2]-mean(tr[ir]), ylim[2]-mean(tr[lr+1-ir])))
    pid <- c(min(tr[ir])-ylim[1], min(tr[lr+1-ir])-ylim[1],
             ylim[2]-max(tr[ir]), ylim[2]-max(tr[lr+1-ir]))
    id <- which.max(pid)
    legendpos <- c("bottomleft", "bottomright", "topleft", "topright")[id]
    if (draw30== TRUE) {
      legend(legendpos, c("trendline", paste("its ", 100*p, "% confid.", sep=""), "30-yr average"),
             lwd= c(2, 1, NA), lty= c(1, 2, NA), pch= c(NA, NA, 18), cex= 0.75, bty= "o",
             y.intersp= 0.8)
    } else {
      legend(legendpos, c("trendline", paste("its ", 100*p, "% confid.", sep="")),
             lwd= c(2, 1), lty= c(1, 2), cex= 0.75, bty= "o",
             y.intersp= 0.8)
    }
  }
  list(t= t, trend= trend, p= p, trendubound= trendub, trendlbound= trendlb,
       averaget= avt, averagey= avy,
       t1= t1, t2= t2, pvalue= pvalue, ybounds= ybounds, enp= mdl$enp)
}

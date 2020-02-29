# Methods for Kernel Regression & Local Linear Regression

est_bandwidth <- function(x, method) {

}

#' Estimate global 4th order polynomial using normalized frequencies
smooth_mccrary <- function(x, bw = NULL) {

}


function (runvar, cutpoint, bin = NULL, bw = NULL, verbose = FALSE,
          plot = TRUE, ext.out = FALSE, htest = FALSE)
{
  runvar <- runvar[complete.cases(runvar)]
  rn <- length(runvar)
  rsd <- sd(runvar)
  rmin <- min(runvar)
  rmax <- max(runvar)
  if (missing(cutpoint)) {
    if (verbose)
      cat("Assuming cutpoint of zero.\n")
    cutpoint <- 0
  }
  if (cutpoint <= rmin | cutpoint >= rmax) {
    stop("Cutpoint must lie within range of runvar")
  }
  if (is.null(bin)) {
    bin <- 2 * rsd * rn^(-1/2)
    if (verbose)
      cat("Using calculated bin size: ", sprintf("%.3f",
                                                 bin), "\n")
  }

  l <- floor((rmin - cutpoint)/bin) * bin + bin/2 + cutpoint

  r <- floor((rmax - cutpoint)/bin) * bin + bin/2 + cutpoint

  lc <- cutpoint - (bin/2)

  rc <- cutpoint + (bin/2)

  j <- floor((rmax - rmin)/bin) + 2

  binnum <- round((((floor((runvar - cutpoint)/bin) * bin +
                       bin/2 + cutpoint) - l)/bin) + 1)

  cellval <- rep(0, j)

  for (i in seq(1, rn)) {
    cnum <- binnum[i]
    cellval[cnum] <- cellval[cnum] + 1
  }

  cellval <- (cellval/rn)/bin

  cellmp <- seq(from = 1, to = j, by = 1)

  cellmp <- floor(((l + (cellmp - 1) * bin) - cutpoint)/bin) *
    bin + bin/2 + cutpoint

  if (is.null(bw)) {
    leftofc <- round((((floor((lc - cutpoint)/bin) * bin +
                          bin/2 + cutpoint) - l)/bin) + 1)
    rightofc <- round((((floor((rc - cutpoint)/bin) * bin +
                           bin/2 + cutpoint) - l)/bin) + 1)
    if (rightofc - leftofc != 1) {
      stop("Error occurred in bandwidth calculation")
    }
    cellmpleft <- cellmp[1:leftofc]
    cellmpright <- cellmp[rightofc:j]

    P.lm <- lm(cellval ~ poly(cellmp, degree = 4, raw = T),
               subset = cellmp < cutpoint)

    mse4 <- summary(P.lm)$sigma^2
    lcoef <- coef(P.lm)
    fppleft <- 2 * lcoef[3] + 6 * lcoef[4] * cellmpleft +
      12 * lcoef[5] * cellmpleft * cellmpleft
    hleft <- 3.348 * (mse4 * (cutpoint - l)/sum(fppleft *
                                                  fppleft))^(1/5)
    P.lm <- lm(cellval ~ poly(cellmp, degree = 4, raw = T),
               subset = cellmp >= cutpoint)
    mse4 <- summary(P.lm)$sigma^2
    rcoef <- coef(P.lm)
    fppright <- 2 * rcoef[3] + 6 * rcoef[4] * cellmpright +
      12 * rcoef[5] * cellmpright * cellmpright
    hright <- 3.348 * (mse4 * (r - cutpoint)/sum(fppright *
                                                   fppright))^(1/5)
    bw = 0.5 * (hleft + hright)
    if (verbose)
      cat("Using calculated bandwidth: ", sprintf("%.3f",
                                                  bw), "\n")
  }
  if (sum(runvar > cutpoint - bw & runvar < cutpoint) == 0 |
      sum(runvar < cutpoint + bw & runvar >= cutpoint) == 0)
    stop("Insufficient data within the bandwidth.")
  if (plot) {
    d.l <- data.frame(cellmp = cellmp[cellmp < cutpoint],
                      cellval = cellval[cellmp < cutpoint], dist = NA,
                      est = NA, lwr = NA, upr = NA)
    pmin <- cutpoint - 2 * rsd
    pmax <- cutpoint + 2 * rsd
    for (i in 1:nrow(d.l)) {
      d.l$dist <- d.l$cellmp - d.l[i, "cellmp"]
      w <- kernelwts(d.l$dist, 0, bw, kernel = "triangular")
      newd <- data.frame(dist = 0)
      pred <- predict(lm(cellval ~ dist, weights = w, data = d.l),
                      interval = "confidence", newdata = newd)
      d.l$est[i] <- pred[1]
      d.l$lwr[i] <- pred[2]
      d.l$upr[i] <- pred[3]
    }
    d.r <- data.frame(cellmp = cellmp[cellmp >= cutpoint],
                      cellval = cellval[cellmp >= cutpoint], dist = NA,
                      est = NA, lwr = NA, upr = NA)
    for (i in 1:nrow(d.r)) {
      d.r$dist <- d.r$cellmp - d.r[i, "cellmp"]
      w <- kernelwts(d.r$dist, 0, bw, kernel = "triangular")
      newd <- data.frame(dist = 0)
      pred <- predict(lm(cellval ~ dist, weights = w, data = d.r),
                      interval = "confidence", newdata = newd)
      d.r$est[i] <- pred[1]
      d.r$lwr[i] <- pred[2]
      d.r$upr[i] <- pred[3]
    }
    plot(d.l$cellmp, d.l$est, lty = 1, lwd = 2, col = "black",
         type = "l", xlim = c(pmin, pmax), ylim = c(min(cellval[cellmp <=
                                                                  pmax & cellmp >= pmin]), max(cellval[cellmp <=
                                                                                                         pmax & cellmp >= pmin])), xlab = NA, ylab = NA,
         main = NA)
    lines(d.l$cellmp, d.l$lwr, lty = 2, lwd = 1, col = "black",
          type = "l")
    lines(d.l$cellmp, d.l$upr, lty = 2, lwd = 1, col = "black",
          type = "l")
    lines(d.r$cellmp, d.r$est, lty = 1, lwd = 2, col = "black",
          type = "l")
    lines(d.r$cellmp, d.r$lwr, lty = 2, lwd = 1, col = "black",
          type = "l")
    lines(d.r$cellmp, d.r$upr, lty = 2, lwd = 1, col = "black",
          type = "l")
    points(cellmp, cellval, type = "p", pch = 20)
  }
  cmp <- cellmp
  cval <- cellval
  padzeros <- ceiling(bw/bin)
  jp <- j + 2 * padzeros
  if (padzeros >= 1) {
    cval <- c(rep(0, padzeros), cellval, rep(0, padzeros))
    cmp <- c(seq(l - padzeros * bin, l - bin, bin), cellmp,
             seq(r + bin, r + padzeros * bin, bin))
  }
  dist <- cmp - cutpoint
  w <- 1 - abs(dist/bw)
  w <- ifelse(w > 0, w * (cmp < cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatl <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  w <- 1 - abs(dist/bw)
  w <- ifelse(w > 0, w * (cmp >= cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatr <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  thetahat <- log(fhatr) - log(fhatl)
  sethetahat <- sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) +
                                                 (1/fhatl)))
  z <- thetahat/sethetahat
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  if (verbose) {
    cat("Log difference in heights is ", sprintf("%.3f",
                                                 thetahat), " with SE ", sprintf("%.3f",
                                                                                 sethetahat), "\n")
    cat("  this gives a z-stat of ", sprintf("%.3f",
                                             z), "\n")
    cat("  and a p value of ", sprintf("%.3f",
                                       p), "\n")
  }
  if (ext.out)
    return(list(theta = thetahat, se = sethetahat, z = z,
                p = p, binsize = bin, bw = bw, cutpoint = cutpoint,
                data = data.frame(cellmp, cellval)))
  else if (htest) {
    structure(list(statistic = c(z = z), p.value = p, method = "McCrary (2008) sorting test",
                   parameter = c(binwidth = bin, bandwidth = bw, cutpoint = cutpoint),
                   alternative = "no apparent sorting"), class = "htest")
  }
  else return(p)
}

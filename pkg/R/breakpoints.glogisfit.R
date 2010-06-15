###########################
## Breakpoint estimation ##
###########################

## This is somewhat experimental...
## glue code for gbreakpoints() in fxregime
breakpoints.glogisfit <- function(obj, hpc = "none", ...) {
  stopifnot(require("fxregime"))
  dat <- data.frame(x = as.vector(obj$x))
  myfit <- function(formula, data, ...) glogisfit.formula(formula, data, fixed = obj$fixed, x = FALSE, ...)
  rval <- fxregime:::gbreakpoints(x ~ 1, data = dat, order.by = time(obj$x), fit = myfit, hpc = hpc)
  rval$null <- obj
  class(rval) <- c("breakpoints.glogisfit", class(rval))
  return(rval)
}

refit.breakpoints.glogisfit <- function(object, ...) {
  bf <- breakfactor(object, ...)
  rval <- tapply(object$null$x, bf, glogisfit, fixed = object$null$fixed)
  names(rval) <- paste(tapply(format(object$index), bf, head, 1), "--",
    tapply(format(object$index), bf, tail, 1), sep = "")
  return(rval)
}

coef.breakpoints.glogisfit <- function(object, log = TRUE, ...) {
  rf <- refit(object, ...)
  t(sapply(rf, coef, log = log))
}

fitted.breakpoints.glogisfit <- function(object,
  type = c("mean", "variance", "skewness"), ...)
{
  type <- as.vector(sapply(type, match.arg, choices = c("mean", "variance", "skewness")))
  rf <- refit(object, ...)
  mom <- t(sapply(rf, "[[", "moments"))
  rval <- mom[breakfactor(object, ...), type]
  if(inherits(object$null$x, "zoo")) rval <- zoo(rval, time(object$null$x))
  if(inherits(object$null$x, "ts")) rval <- ts(rval, start = start(object$null$x), frequency = frequency(object$null$x))
  return(rval)
}


#########################################################
## Model fitting for generalized logistic distribution ##
#########################################################

## generic function
glogisfit <- function(x, ...) UseMethod("glogisfit")

## convenience formula method
glogisfit.formula <- function(formula, data, subset, na.action, weights,
  x = TRUE, ...)
{
  ## call
  cl <- match.call()
  cl[[1]] <- as.name("glogisfit")

  ## process formula to data
  if(missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  y <- model.response(mf)
  mt <- terms(formula, data = data)
  weights <- model.weights(mf)
  if(length(weights) == 0L) weights <- rep(1L, length(y))

  ## call default function
  rval <- glogisfit.default(y, weights = weights, ...)

  ## post-process and return
  rval$call <- cl
  if(!x) rval$x <- NULL
  rval$terms <- mt
  return(rval)
}

## workhorse default method
glogisfit.default <- function(x, weights = NULL,
  start = NULL, fixed = c(NA, NA, NA),
  method = "BFGS", ...)
{
  ## call
  cl <- match.call()
  cl[[1]] <- as.name("glogisfit")

  ## data
  x_orig <- x
  x <- as.vector(x)

  ## weights
  w <- weights
  if(is.null(w)) w <- rep.int(1L, length(x))
  stopifnot(length(w) == length(x))

  ## process method
  method <- match.arg(method, c(c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")))

  ## negative log-likelihood function
  llfun <- function(par) {
    p1 <- if(is.na(fixed[1])) par[1] else fixed[1]
    p2 <- if(is.na(fixed[2])) par[is.na(fixed[1]) + 1] else log(fixed[2])
    p3 <- if(is.na(fixed[3])) par[sum(is.na(fixed[1:2])) + 1] else log(fixed[3])
    -sum(w * dglogis(x, location = par[1], scale = exp(p2), shape = exp(p3), log = TRUE))
  }
  
  ## neagtive gradient function
  gradfun <- function(par) {
    p1 <- if(is.na(fixed[1])) par[1] else fixed[1]
    p2 <- if(is.na(fixed[2])) par[is.na(fixed[1]) + 1] else log(fixed[2])
    p3 <- if(is.na(fixed[3])) par[sum(is.na(fixed[1:2])) + 1] else log(fixed[3])
    -colSums(w * t(t(sglogis(x, location = p1, scale = exp(p2), shape = exp(p3))) * c(1, exp(p2), exp(p3)))[, is.na(fixed)])
  }

  ## choose starting values
  if(is.null(start)) start <- c(mean(x), log(sd(x)), 0)[is.na(fixed)]
  
  ## call optim
  opt <- if(method == "L-BFGS-B") {
    optim(par = start, fn = llfun, gr = gradfun, hessian = TRUE,
      lower = c(-Inf, 0, 0), upper = rep(Inf, 3), method = method, control = list(...))
  } else {
    optim(par = start, fn = llfun, gr = gradfun, hessian = TRUE,
      method = method, control = list(...))  
  }
  
  ## post-process optim result
  cf <- opt$par
  vc <- solve(opt$hessian)
  names(cf) <- colnames(vc) <- rownames(vc) <- c("location", "log(scale)", "log(shape)")[is.na(fixed)]  

  ## full parameter vector
  par <- fixed
  par[2:3] <- log(par[2:3])
  par[is.na(fixed)] <- cf
  par[2:3] <- exp(par[2:3])
  names(par) <- c("location", "scale", "shape")

  ## moments
  mom <- c(  
    "mean"      = as.vector(par[1] + (digamma(par[3]) - digamma(1)) * par[2]),
    "variance"  = as.vector((psigamma(par[3], deriv = 1) + psigamma(1, deriv = 1)) * par[2]^2),
    "skewness"  = as.vector((psigamma(par[3], deriv = 2) - psigamma(1, deriv = 2)) / 
                  (psigamma(par[3], deriv = 1) + psigamma(1, deriv = 1))^(3/2))
  )

  ## collect and return everything
  rval <- list(
    coefficients = cf,
    vcov = vc,
    loglik = -opt$value,
    df = 3,
    weights = if(isTRUE(all.equal(as.vector(w), rep.int(1L, length(x))))) NULL else w,
    optim = opt,
    method = method,
    par = par,
    moments = mom,
    start = start,
    fixed = fixed,
    call = cl,
    x = x_orig,
    converged = opt$convergence < 1
  )      
  class(rval) <- "glogisfit"
  return(rval)
}

## extractor functions (coef works by default)
coef.glogisfit <- function(object, log = TRUE, ...) {
  cf <- object$coefficients
  if(!log & any(c("log(scale)", "log(shape)") %in% names(cf))) {
    cf <- object$par[is.na(object$fixed)]
  }
  return(cf)
}
vcov.glogisfit <- function(object, log = TRUE, ...) {
  cf <- object$coefficients
  vc <- object$vcov
  if(!log & any(c("log(scale)", "log(shape)") %in% names(cf))) {
    D <- diag(c(1, object$par[2:3])[is.na(object$fixed)])    
    vc <- D %*% vc %*% t(D)
    colnames(vc) <- rownames(vc) <- names(object$par)[!is.na(object$fixed)]
  }
  return(vc)
}
logLik.glogisfit <- function(object, ...) structure(object$loglik, df = object$df, class = "logLik")
estfun.glogisfit <- function(x, ...) {
  rval <- t(t(sglogis(x$x, x$par[1], x$par[2], x$par[3])) * c(1, x$par[2], x$par[3]))[, is.na(x$fixed)]
  colnames(rval) <- names(coef(x))
  return(rval)
}

## printing and summary
print.glogisfit <- function(x, digits = max(3, getOption("digits") - 3), ...) 
{
  cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")

  if(!x$converged) {
    cat("model did not converge\n")
  } else {
    if(length(coef(x))) {
      cat(paste("Coefficients:\n", sep = ""))
      print.default(format(x$coefficients, digits = digits), print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
      cat(paste("Distribution parameters:\n", sep = ""))
      print.default(format(x$par, digits = digits), print.gap = 2, quote = FALSE)
  }
  
  invisible(x)
}

summary.glogisfit <- function(object, log = TRUE, ...)
{
  ## extend coefficient table
  cf <- coef(object, log = log)
  se <- sqrt(diag(vcov(object, log = log)))
  
  cf <- cbind(cf, se, cf/se, 2 * pnorm(-abs(cf/se)))
  colnames(cf) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  object$coefficients <- cf
  
  ## number of iterations
  object$iterations <- as.vector(tail(na.omit(object$optim$count), 1))
  
  ## return
  class(object) <- "summary.glogisfit"
  object
}

print.summary.glogisfit <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")
  
  if(!x$converged) {
    cat("model did not converge\n")
  } else {
    cat(paste("\nCoefficients:\n", sep = ""))
    printCoefmat(x$coefficients, digits = digits)
  
    cat("\nLog-likelihood:", formatC(x$loglik, digits = digits),
      "on", sum(sapply(x$coefficients, NROW)), "Df")
    cat(paste("\nNumber of iterations in", x$method, "optimization:", x$iterations, "\n"))
  }
  
  invisible(x)
}

## visualization
plot.glogisfit <- function(x, main = "", xlab = deparse(x$call), fill = "lightgray",
  col = "blue", lwd = 1, lty = 1, ylim = NULL, legend = "topright", ...)
{
  if(is.null(ylim)) {
    aux1 <- seq(min(x$x) - 3 * x$par[2], max(x$x) + 3 * x$par[2], length = 100)
    aux2 <- hist(x$x, plot = FALSE)$density
    ylim <- range(c(dglogis(aux1, x$par[1], x$par[2], x$par[3]), aux2))
  }
  rval <- hist(x, main = main, xlab = xlab, col = fill, ylim = ylim, ...)
  lines(x, col = col, lwd = lwd, lty = lty)
  if(identical(legend, TRUE)) legend <- "topleft"
  if(!identical(legend, FALSE)) legend(legend,
    paste(c("location", "scale", "shape"), ": ", format(round(x$par, pmax(getOption("digits") - 4, 1))),
      ifelse(is.na(x$fixed), "", " (fixed)"), sep = ""),
    bty = "n")

  invisible(rval)
}

hist.glogisfit <- function(x, main = "", xlab = deparse(x$call),
  col = "lightgray", freq = FALSE, ...)
{
  hist(x$x, main = main, xlab = xlab, col = col, freq = freq, ...)
}


lines.glogisfit <- function(x, ...)
{
  aux <- seq(min(x$x) - 3 * x$par[2], max(x$x) + 3 * x$par[2], length = 100)
  lines(aux, dglogis(aux, x$par[1], x$par[2], x$par[3]), ...)
}

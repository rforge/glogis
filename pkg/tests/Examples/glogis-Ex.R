pkgname <- "glogis"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('glogis')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("HICP")
### * HICP

flush(stderr()); flush(stdout())

### Name: HICP
### Title: Harmonised Index of Consumer Prices (1990-2010, OECD)
### Aliases: HICP hicps
### Keywords: datasets

### ** Examples

## price series
data("HICP", package = "glogis")

## corresponding raw unadjusted inflation rates (in percent)
hicp <- 100 * diff(log(HICP))

## seasonal adjustment of inflation rates (via STL)
hicps1 <- do.call("merge", lapply(1:ncol(hicp), function(i) {
  z <- na.omit(hicp[,i])
  coredata(z) <- coredata(as.ts(z) - stl(as.ts(z), s.window = 13)$time.series[, "seasonal"])
  z
}))
colnames(hicps1) <- colnames(hicp)

## load X-12-ARIMA adjusted inflation rates
data("hicps", package = "glogis")

## compare graphically for one country (Austria)
plot(hicp[, "Austria"], lwd = 2, col = "lightgray")
lines(hicps1[, "Austria"], col = "red")
lines(hicps[, "Austria"], col = "blue")
legend("topleft", c("unadjusted", "STL", "X-12-ARIMA"), lty = c(1, 1, 1),
  col = c("lightgray", "red", "blue"), bty = "n")

## compare graphically across all countries (via lattice)
if(require("lattice")) {
trellis.par.set(theme = canonical.theme(color = FALSE))
xyplot(merge(hicp, hicps1, hicps), 
  screen = names(hicp)[rep(1:ncol(hicp), 3)],
  col = c("lightgray", "red", "blue")[rep(1:3, each = ncol(hicp))],
  lwd = c(2, 1, 1)[rep(1:3, each = ncol(hicp))])
}





cleanEx()
nameEx("breakpoints.glogisfit")
### * breakpoints.glogisfit

flush(stderr()); flush(stdout())

### Name: breakpoints.glogisfit
### Title: Segmented Fitting of the Generalized Logistic Distribution
### Aliases: breakpoints.glogisfit coef.breakpoints.glogisfit
###   fitted.breakpoints.glogisfit refit.breakpoints.glogisfit
###   index.breakpoints.glogisfit confint.breakpoints.glogisfit
###   breakdates.confint.breakpoints.glogisfit
###   print.confint.breakpoints.glogisfit
###   lines.confint.breakpoints.glogisfit
### Keywords: regression

### ** Examples

## artifical data with one structural change
set.seed(1071)
x <- c(rglogis(50, -1, scale = 0.5, shape = 3), rglogis(50, 1, scale = 0.5, shape = 1))
x <- zoo(x, yearmon(seq(2000, by = 1/12, length = 100)))

## full sample estimation
gf <- glogisfit(x)

if(require("strucchange")) {

## structural change testing
gf_scus <- gefp(gf, fit = NULL)
plot(gf_scus, aggregate = FALSE)
plot(gf_scus, functional = meanL2BB)
sctest(gf_scus)
sctest(gf_scus, functional = meanL2BB)

}



cleanEx()
nameEx("glogis")
### * glogis

flush(stderr()); flush(stdout())

### Name: glogis
### Title: The Generalized Logistic Distribution (Type I: Skew-Logitic)
### Aliases: dglogis pglogis qglogis rglogis sglogis
### Keywords: distribution

### ** Examples

## PDF and CDF
par(mfrow = c(1, 2))
x <- -100:100/10
plot(x, dglogis(x, shape = 2), type = "l", col = 4, main = "PDF", ylab = "f(x)")
lines(x, dglogis(x, shape = 1))
lines(x, dglogis(x, shape = 0.5), col = 2)
legend("topleft", c("generalized (0, 1, 2)", "standard (0, 1, 1)",
  "generalized (0, 1, 0.5)"), lty = 1, col = c(4, 1, 2), bty = "n")
plot(x, pglogis(x, shape = 2), type = "l", col = 4, main = "CDF", ylab = "F(x)")
lines(x, pglogis(x, shape = 1))
lines(x, pglogis(x, shape = 0.5), col = 2)

## artifical empirical example
set.seed(2)
x <- rglogis(1000, -1, scale = 0.5, shape = 3)
gf <- glogisfit(x)
plot(gf)
summary(gf)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("glogisfit")
### * glogisfit

flush(stderr()); flush(stdout())

### Name: glogisfit
### Title: Fitting the Generalized Logistic Distribution
### Aliases: glogisfit glogisfit.default glogisfit.formula bread.glogisfit
###   coef.glogisfit estfun.glogisfit hist.glogisfit lines.glogisfit
###   logLik.glogisfit plot.glogisfit print.glogisfit
###   print.summary.glogisfit residuals.glogisfit summary.glogisfit
###   vcov.glogisfit
### Keywords: regression

### ** Examples

## simple artificial example
set.seed(2)
x <- rglogis(1000, -1, scale = 0.5, shape = 3)
gf <- glogisfit(x)
plot(gf)
summary(gf)

## query parameters and associated moments
coef(gf)
coef(gf, log = FALSE)
gf$parameters
gf$moments



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

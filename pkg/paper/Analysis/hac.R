## packages and data
library("glogis")
library("fxregime")
data("hicps", package = "glogis")

## compute supLM statistic with/without HAC correction
pval <- t(sapply(1:ncol(hicps), function(i) {
  x <- na.omit(hicps[,i])
  x_gf <- glogisfit(x)
  x_efp <- gefp(x_gf, fit = NULL)
  x_efp_hac <- gefp(x_gf, fit = NULL, vcov = vcovHAC)
  c("vcov" = sctest(x_efp, functional = supLM(0.1))$p.value,
    "vcovHAC" = sctest(x_efp_hac, functional = supLM(0.1))$p.value)
}))
rownames(pval) <- colnames(hicps)

## significance only changes for France and Estonia
## -> Estonia has a very poor fit anyway
## -> France has only a very short first segment,
##    results would not change much if no break was fitted
subset(as.data.frame(pval), vcov <= 0.05 & vcovHAC > 0.05)
##          vcov vcovHAC
## Estonia 0.000   0.176
## France  0.044   0.110

## compute various statistics for each segment:
## autocorrelation at lag 1, Ljung-Box test at lag 1,
## Chi-squared goodness of fit test
segstat <- lapply(colnames(hicps), function(nam) {
  load(paste(nam, ".rda", sep = ""))
  sapply(x_rf, function(obj) c(
    "ACF(1)" = acf(obj$x, plot = FALSE)$acf[2],
    "Ljung-Box" = Box.test(obj$x, type = "Ljung-Box")$p.value,
    "Chisq-GOF" = summary(obj)$chisq.test$p.value
  ))
})
names(segstat) <- colnames(hicps)

## -> GOF tests are mostly ok, only a few poor fits (e.g., Estonia)
## -> ACF(1) typically small and Ljung-Box test non-significant, i.e.,
##    most autocorrelation captured. Several exceptions though, but all
##    smaller than 0.5, typicall smaller than 0.2 even. Thus, no
##    high persistence.

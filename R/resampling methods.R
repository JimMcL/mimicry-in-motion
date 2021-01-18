# Boostrapping and randomisation

# Bootstrap 95% confidence interval of correlation coefficient
BootstrapCorCI <- function(x, y, nreps = 10000) {
  stopifnot(length(x) == length(y))
  
  rboot <- 0
  n <- length(x)
  for(i in 1:nreps) {
    samp <- sample(1:n, n, replace = TRUE)
    rboot[i] <- cor.test(x[samp], y[samp])$estimate
  }
  quantile(rboot, c(0.025, 0.975))
}

# Randomisation test (yields upper bound on p value, two sided test)
RandomisationCorTest <- function(x, y, nreps = 10000) {
  stopifnot(length(x) == length(y))

  rboot <- 0
  n <- length(x)
  for(i in 1:nreps) {
    samp <- sample(1:n, n, replace = FALSE)
    rboot[i] <- cor.test(x, y[samp])$estimate
  }
  sum(abs(cor.test(x,y)$estimate) < abs(rboot)) / nreps # approximate two-sided test
}

######################################################
# Tests

# # random bivariate data
# n <- 15
# x <- rnorm(n)
# y <- 3 * x + rnorm(n)
# 
# summary(lm(y ~ x))
# cor.test(x,y)
# BootstrapCorCI(x, y)
# RandomisationCorTest(x, y)

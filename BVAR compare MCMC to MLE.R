# install.packages("BVAR")

# Load the package
library("BVAR")

# Access a subset of the fred_qd dataset
data <- fred_qd[, c("GDPC1", "CPIAUCSL", "UNRATE", "FEDFUNDS")]
# Transform it to be stationary
data <- fred_transform(data, codes = c(5, 5, 5, 1), lag = 4)

# Estimate using default priors and MH step
set.seed(248)
x1 <- bvar(data, 
           lags = 1,
           priors = bv_priors(mn = bv_minnesota(
                                        lambda = bv_lambda(),
                                        alpha = bv_alpha(),
                                        psi = bv_psi(),
                                        var = 10000000,
                                        b = 1)
                             )
           )
set.seed(248)
x2 <- bvar(data, lags = 6)

set.seed(248)
source("~your path~/bvar_mle.R")
x3 <- bvar_mle(data, lags = 6)
x3[["beta"]]
x3[["sigma"]]
x3[["hyper"]]

plot(x2)
plot(x3)
plot(x2, type = "dens",vars_response = "GDPC1", vars_impulse = "GDPC1-lag1")
plot(x2, type = "dens",vars_response = "GDPC1")
plot(x3, type = "dens",vars_response = "GDPC1")

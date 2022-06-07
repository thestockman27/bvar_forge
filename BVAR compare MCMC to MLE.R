#install.packages("BVAR")

# Load the package
library("BVAR")

# Access a subset of the fred_qd dataset
data_raw <- fred_qd[, c("GDPC1", "CPIAUCSL", "UNRATE", "FEDFUNDS")]
# Transform it to be stationary
data <- fred_transform(data_raw, codes = c(2, 2, 2, 1), lag = 1)

# Estimate using default priors and MH step
set.seed(248)
x1 <- bvar(data, 
           lags = 6,
           priors = bv_priors(mn = bv_minnesota(
                                        lambda = bv_lambda(),
                                        alpha = bv_alpha(),
                                        psi = bv_psi(),
                                        var = 10000000,
                                        b = 1)
                             )
           )

set.seed(248)
source("K:/INCORE/Derek Stockman/DCS/R/projects/BVAR/bvar_mle.R")
x2 <- bvar_mle(data, lags = 6)

summary(x1)
coef(x1)
vcov(x1)

x2[["beta"]]
x2[["sigma"]]
x2[["hyper"]]
x2[["ml"]]


plot(x1)
plot(x2)
plot(x1, type = "dens",vars_response = "GDPC1", vars_impulse = "GDPC1-lag1")
plot(x1, type = "dens",vars_response = "GDPC1", vars_impulse = "UNRATE-lag1")
plot(x1, type = "dens",vars_response = "UNRATE", vars_impulse = "constant")
plot(x1, type = "trace",vars_response = "UNRATE", vars_impulse = "constant")

fitted(x1, type = "mean")

# generate forecast
y1 <- predict(x1) <- predict(x1, horizon = 5)
# store forecast
pred <- y1$quants[2,1,1]
plot(y1,vars = "GDPC1")
# generate forecast
y2 <- predict(x2) <- predict(x2, horizon = 5)
# store forecast
pred <- y2$quants[2,1,1]


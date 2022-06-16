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
source("~your_path/bvar_mle.R")
x2 <- bvar_mle(data, lags = 6)

summary(x1)
#coef(x1)
#vcov(x1)

x2[["hyper"]]
x2[["beta"]]
x2[["sigma"]]
x2[["ml"]]


plot(x1)
plot(x2)
plot(x1, type = "dens",vars_response = "GDPC1", vars_impulse = "GDPC1-lag1")
plot(x1, type = "dens",vars_response = "GDPC1", vars_impulse = "UNRATE-lag1")
plot(x1, type = "dens",vars_response = "UNRATE", vars_impulse = "constant")
plot(x1, type = "trace",vars_response = "UNRATE", vars_impulse = "constant")
# Only plot the marginal likelihood's trace
plot(x1, "trace", "ml")
fitted(x1, type = "mean")

# generate forecast
y1 <- predict(x1) <- predict(x1, horizon = 5)
# store forecast
y1_pred <- y1$quants[2,1,]
plot(y1,vars = "GDPC1")
# generate forecast
y2 <- predict(x2) <- predict(x2, horizon = 5) # error
# store forecast
y2_pred <- y2$quants[2,1,]

## decomposed the predict() to generate a forecast

M = x2[["meta"]]["M"][["M"]] # number of variables
N = x2[["meta"]]["N"][["N"]] # number of observations
Y = x2[["meta"]]["Y"][["Y"]] # our data, N * M
lags = x2[["meta"]]["lags"][["lags"]] # number of lags
K = x2[["meta"]]["K"][["K"]] # M * lags + 1

beta = x2[["beta"]] # posterior mean matrix
beta_comp <- BVAR:::get_beta_comp(beta,K,M,lags) # Numeric matrix. Posterior VAR coefficients of the model 
                                                 # M * lags , no constant
# compare to beta_comp of x1
# BVAR:::get_beta_comp(x1[["beta"]][1,,],K,M,lags)

beta_const = x2[["beta"]][1,] # beta_const Numeric vector. Posterior draw of the VAR coefficients corresponding to the constant of the model.

forecast <- BVAR:::compute_fcast(Y = Y,K = K, M = M , N = N, lags= lags, horizon = 5, 
                     beta_comp = beta_comp , beta_const = beta_const)
forecast

y2_pred <- forecast[1,]
y2_pred

data.frame(Y1 = y1_pred, Y2 = y2_pred, row.names = colnames(data))

#### scrap yard ####

# excerpt from compute_fcast() 
Y_f <- matrix(NA, horizon + 1, K - 1)
Y_f[1, ] <- vapply(t(Y[N:(N - lags + 1), ]), c, numeric(1L))
for(i in seq.int(2, 1 + horizon)) {
  Y_f[i, ] <- tcrossprod(Y_f[i - 1, ], beta_comp) +
    c(beta_const, rep(0, M * (lags - 1))) # Maybe go back to normal beta
}

Y_f[-1, 1:M]

# View(get_beta_comp())
# function (beta, K, M, lags) 
# {
  beta_comp <- matrix(0, K - 1, K - 1)
  beta_comp[1:M, ] <- t(beta[2:K, ])
  if (lags > 1) {
    beta_comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * 
                                                        (lags - 1))
  }
#   return(beta_comp)
# }

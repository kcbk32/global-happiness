# R/01_fit_model.R
# ----------------
# Fits the hierarchical Bayesian happiness model in Stan
# and saves the fitted object for downstream analysis.

suppressPackageStartupMessages({
  library(rstan)
})

# ---- Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ---- Load prepared data
source("R/00_prepare_data.R")

# ---- Fit model
fit <- stan(
  file    = "stan/happiness_hierarchical.stan",
  data    = stan_data,
  iter    = 5000,
  chains  = 4,
  cores   = 4,
  control = list(adapt_delta = 0.95)
)

# ---- Save fitted model
dir.create("output/fits", recursive = TRUE, showWarnings = FALSE)
saveRDS(fit, file = "output/fits/fit_happiness.rds")

# ---- Quick sanity print (global parameters)
print(
  fit,
  pars = c(
    "alpha_global",
    "beta_year_global",
    "beta_gdp_global",
    "beta_life_global",
    "sigma_y"
  )
)

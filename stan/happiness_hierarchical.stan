data {
  int<lower=1> N;              // number of observations
  int<lower=1> C;              // number of countries
  int<lower=1> K;              // number of continents
  int<lower=1,upper=C> country[N];
  int<lower=1,upper=K> continent_of_country[C];

  vector[N] year_std;
  vector[N] log_gdp_std;
  vector[N] life_exp_std;
  vector[N] y;
}

parameters {
  // global
  real alpha_global;
  real beta_year_global;
  real beta_gdp_global;
  real beta_life_global;

  // continent-level
  vector[K] alpha_cont;
  vector[K] beta_year_cont;
  vector[K] beta_gdp_cont;
  vector[K] beta_life_cont;

  // country-level
  vector[C] alpha_country;

  // standard deviations
  real<lower=0> sigma_y;
  real<lower=0> sigma_alpha_cont;
  real<lower=0> sigma_alpha_country;
  real<lower=0> sigma_beta_year;
  real<lower=0> sigma_beta_gdp;
  real<lower=0> sigma_beta_life;
}

model {
  // hyperpriors
  alpha_global      ~ normal(5, 3);
  beta_year_global  ~ normal(0, 1);
  beta_gdp_global   ~ normal(0, 1);
  beta_life_global  ~ normal(0, 1);

  sigma_y              ~ exponential(1);
  sigma_alpha_cont     ~ exponential(1);
  sigma_alpha_country  ~ exponential(1);
  sigma_beta_year      ~ exponential(1);
  sigma_beta_gdp       ~ exponential(1);
  sigma_beta_life      ~ exponential(1);

  // continent-level
  alpha_cont     ~ normal(alpha_global,          sigma_alpha_cont);
  beta_year_cont ~ normal(beta_year_global,      sigma_beta_year);
  beta_gdp_cont  ~ normal(beta_gdp_global,       sigma_beta_gdp);
  beta_life_cont ~ normal(beta_life_global,      sigma_beta_life);

  // country-level intercepts (nested in continents)
  for (c in 1:C) {
    int k = continent_of_country[c];
    alpha_country[c] ~ normal(alpha_cont[k], sigma_alpha_country);
  }

  // likelihood
  for (n in 1:N) {
    int c = country[n];
    int k = continent_of_country[c];
    real mu = alpha_country[c]
            + beta_year_cont[k] * year_std[n]
            + beta_gdp_cont[k]  * log_gdp_std[n]
            + beta_life_cont[k] * life_exp_std[n];
    y[n] ~ normal(mu, sigma_y);
  }
}

# R/02_analyze_posterior.R
# ------------------------
# Loads the fitted Stan model and answers the four questions via posterior summaries.

suppressPackageStartupMessages({
  library(rstan)
  library(dplyr)
})

# ---- Load fit
fit <- readRDS("output/fits/fit_happiness.rds")
post <- rstan::extract(fit)

# ---- Load prep objects (country_levels, country_df, etc.)
source("R/00_prepare_data.R")

# -----------------------
# Q1: Global year trend
# -----------------------
prob_increasing <- mean(post$beta_year_global > 0)
ci_year <- quantile(post$beta_year_global, c(0.025, 0.5, 0.975))

cat("\nQ1: P(global year slope > 0) =", prob_increasing, "\n")
print(ci_year)

# -----------------------
# Q2: Global happiness > 5
# -----------------------
prob_happy_global <- mean(post$alpha_global > 5)
ci_alpha <- quantile(post$alpha_global, c(0.025, 0.5, 0.975))

cat("\nQ2: P(global mean happiness > 5) =", prob_happy_global, "\n")
print(ci_alpha)

# -----------------------
# Q3: Slovenia top-20 probability
# -----------------------
slovenia_name <- "Slovenia"
slovenia_id <- which(country_levels == slovenia_name)

if (length(slovenia_id) != 1) {
  stop("Could not uniquely identify Slovenia in `country_levels`.")
}

S <- length(post$alpha_global)
is_top20 <- logical(S)
slv_rank <- numeric(S)

for (s in 1:S) {
  scores <- post$alpha_country[s, ]
  ranks <- rank(-scores, ties.method = "min")  # 1 = happiest
  slv_rank[s] <- ranks[slovenia_id]
  is_top20[s] <- ranks[slovenia_id] <= 20
}

prob_top20 <- mean(is_top20)
rank_ci <- quantile(slv_rank, c(0.025, 0.5, 0.975))

cat("\nQ3: P(Slovenia in top 20) =", prob_top20, "\n")
cat("Slovenia rank 95% CI:\n")
print(rank_ci)

# -----------------------
# Q4: Continent differences (GDP & life expectancy effects)
# -----------------------
continent_levels <- levels(factor(country_df$continent))

beta_gdp_means  <- apply(post$beta_gdp_cont, 2, mean)
beta_life_means <- apply(post$beta_life_cont, 2, mean)

effects_df <- data.frame(
  continent = continent_levels,
  beta_gdp  = beta_gdp_means,
  beta_life = beta_life_means
)

cat("\nQ4: Continent-specific GDP and life-expectancy slopes (posterior means):\n")
print(effects_df)

# Example pairwise comparison: Europe vs Africa GDP effect
EU <- which(continent_levels == "Europe")
AF <- which(continent_levels == "Africa")

if (length(EU) == 1 && length(AF) == 1) {
  diff_gdp_EU_AF <- post$beta_gdp_cont[, EU] - post$beta_gdp_cont[, AF]
  cat("\nDifference in GDP slope (Europe - Africa):\n")
  print(quantile(diff_gdp_EU_AF, c(0.025, 0.5, 0.975)))
  cat("P(GDP effect Europe > Africa) =",
      mean(diff_gdp_EU_AF > 0), "\n")
} else {
  cat("\nSkipping Europe vs Africa comparison: continent labels not found as expected.\n")
}

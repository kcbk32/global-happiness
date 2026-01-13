# R/01_prepare_data.R
# -------------------
# Loads raw CSVs, merges, cleans, builds integer indices for hierarchical Stan model,
# standardizes predictors, and constructs `stan_data`.
#
# Objects created (used by other scripts):
# - cantril, countries
# - dat                 (merged + cleaned + standardized)
# - country_levels      (country names in ID order)
# - continent_levels    (continent names in ID order)
# - country_df          (country_id -> continent -> cont_id mapping)
# - stan_data           (list passed to Stan)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

# ---- Paths
CANTRIL_PATH  <- "data/cantril_ladder.csv"
COUNTRIES_PATH <- "data/countries.csv"

# ---- Load data
cantril   <- read_csv(CANTRIL_PATH, show_col_types = FALSE)
countries <- read_csv(COUNTRIES_PATH, show_col_types = FALSE)

# ---- Basic checks
required_cantril <- c("country", "year", "score")
required_countries <- c("country", "log_gdp", "life_expectancy")

missing_cantril <- setdiff(required_cantril, names(cantril))
missing_countries <- setdiff(required_countries, names(countries))

if (length(missing_cantril) > 0) {
  stop("cantril_ladder.csv is missing columns: ", paste(missing_cantril, collapse = ", "))
}
if (length(missing_countries) > 0) {
  stop("countries.csv is missing columns: ", paste(missing_countries, collapse = ", "))
}

# ---- Merge + clean
dat <- cantril %>%
  left_join(countries, by = "country") %>%
  drop_na(score, log_gdp, life_expectancy)

# Ensure expected types
dat <- dat %>%
  mutate(
    year            = as.integer(year),
    score           = as.numeric(score),
    log_gdp         = as.numeric(log_gdp),
    life_expectancy = as.numeric(life_expectancy)
  )

# basic sanity filtering
dat <- dat %>%
  filter(
    is.finite(score),
    is.finite(log_gdp),
    is.finite(life_expectancy)
  )

# ---- Country IDs (stable ordering)
dat <- dat %>%
  mutate(
    country = as.character(country),
    country_id = as.integer(factor(country))
  )

country_levels <- levels(factor(dat$country))

# ---- Continent mapping
# Prefer continent provided by the dataset if present; otherwise fail fast.
if (!("continent" %in% names(dat))) {
  stop("After merging, `continent` column is missing. Ensure the data provides continent labels.")
}

dat <- dat %>%
  mutate(continent = as.character(continent))

# Build a unique country -> continent map (and validate uniqueness)
country_df <- dat %>%
  select(country, country_id, continent) %>%
  distinct() %>%
  arrange(country_id)

# Check: each country should map to exactly one continent
continent_counts <- country_df %>%
  count(country_id, name = "n_continents") %>%
  filter(n_continents != 1)

if (nrow(continent_counts) > 0) {
  bad_ids <- continent_counts$country_id
  bad_names <- country_df %>%
    filter(country_id %in% bad_ids) %>%
    arrange(country_id) %>%
    pull(country) %>%
    unique()
  
  stop(
    "Some countries map to multiple continents (data inconsistency). ",
    "Example countries: ", paste(head(bad_names, 10), collapse = ", ")
  )
}

# Continent IDs (stable ordering)
country_df <- country_df %>%
  mutate(
    cont_id = as.integer(factor(continent))
  ) %>%
  arrange(country_id)

continent_levels <- levels(factor(country_df$continent))

# Attach cont_id back onto each row
dat <- dat %>%
  left_join(country_df %>% select(country_id, cont_id), by = "country_id")

# ---- Standardize predictors
# (as.numeric(scale(x)) avoids matrix columns)
dat <- dat %>%
  mutate(
    year_std      = as.numeric(scale(year)),
    log_gdp_std   = as.numeric(scale(log_gdp)),
    life_exp_std  = as.numeric(scale(life_expectancy))
  )

# ---- Stan data list
stan_data <- list(
  N = nrow(dat),
  C = max(dat$country_id),
  K = max(country_df$cont_id),
  country = dat$country_id,
  continent_of_country = country_df$cont_id,
  year_std     = dat$year_std,
  log_gdp_std  = dat$log_gdp_std,
  life_exp_std = dat$life_exp_std,
  y = dat$score
)

# ---- quick info
message("Prepared data: N=", stan_data$N, ", C=", stan_data$C, ", K=", stan_data$K)
message("Example countries: ", paste(head(country_levels, 5), collapse = ", "))
message("Continents: ", paste(continent_levels, collapse = ", "))

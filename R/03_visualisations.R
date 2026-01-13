# R/03_visualisations.R
# -------------------
# consistent spacing,
# consistent theme/palette,
# ensure output directory exists.

suppressPackageStartupMessages({
  library(rstan)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(stringr)
})

# ---- Palette
COL_BLUE   <- "#1f77b4"
COL_ORANGE <- "#ff7f0e"

# ---- Theme (shared across figures)
theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.margin = margin(10, 12, 10, 12),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    legend.position = "bottom"
  )

# ---- Ensure output dir exists
dir.create("output/figures1", recursive = TRUE, showWarnings = FALSE)

# ---- Load posterior + data objects
fit  <- readRDS("output/fits/fit_happiness.rds")
post <- rstan::extract(fit)

source("R/00_prepare_data.R")

# ============================================================
# Figure 1: Global posteriors
# ============================================================
global_df <- tibble(
  alpha     = post$alpha_global,
  beta_year = post$beta_year_global,
  beta_gdp  = post$beta_gdp_global,
  beta_life = post$beta_life_global
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  )

p_global <- ggplot(global_df, aes(x = value)) +
  geom_density(
    fill = COL_BLUE,
    alpha = 0.35,
    color = COL_BLUE,
    linewidth = 0.6
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.5
  ) +
  facet_wrap(~ parameter, scales = "free", ncol = 2) +
  labs(
    title = str_wrap("Posterior distributions of global model parameters", 45),
    x = "Value",
    y = "Density"
  ) +
  theme_report +
  theme(legend.position = "none")

ggsave(
  filename = "output/figures/global_posteriors.pdf",
  plot = p_global,
  width = 4.2,
  height = 3.2
)

# ============================================================
# Figure 2: Slovenia rank posterior (based on alpha_country)
# ============================================================
slovenia_id <- which(country_levels == "Slovenia")
if (length(slovenia_id) != 1) stop("Could not uniquely identify Slovenia in `country_levels`.")

S <- nrow(post$alpha_country)
slv_rank <- numeric(S)

for (s in 1:S) {
  scores <- post$alpha_country[s, ]
  ranks  <- rank(-scores, ties.method = "min")  # 1 = happiest
  slv_rank[s] <- ranks[slovenia_id]
}

rank_df <- tibble(rank = slv_rank)

p_slv <- ggplot(rank_df, aes(x = rank)) +
  geom_histogram(bins = 30, fill = COL_BLUE, color = "white") +
  labs(
    title = str_wrap("Posterior distribution of Slovenia’s happiness rank", 45),
    x = "Rank (1 = happiest)",
    y = "Posterior draws"
  ) +
  theme_report +
  theme(legend.position = "none")

ggsave(
  filename = "output/figures/slovenia_rank.pdf",
  plot = p_slv,
  width = 4.0,
  height = 3.0
)

# ============================================================
# Figure 3: Continent effects (GDP + Life expectancy)
# ============================================================
continent_levels <- levels(factor(country_df$continent))

summ_95 <- function(x) {
  c(
    mean = mean(x),
    lo   = as.numeric(quantile(x, 0.025)),
    hi   = as.numeric(quantile(x, 0.975))
  )
}

gdp_sum  <- t(apply(post$beta_gdp_cont,  2, summ_95))
life_sum <- t(apply(post$beta_life_cont, 2, summ_95))

colnames(gdp_sum)  <- c("mean", "lo", "hi")
colnames(life_sum) <- c("mean", "lo", "hi")

effects_ci <- tibble(
  continent = continent_levels,
  GDP_mean  = gdp_sum[, "mean"],
  GDP_lo    = gdp_sum[, "lo"],
  GDP_hi    = gdp_sum[, "hi"],
  Life_mean = life_sum[, "mean"],
  Life_lo   = life_sum[, "lo"],
  Life_hi   = life_sum[, "hi"]
)

effects_long <- effects_ci %>%
  pivot_longer(
    cols = -continent,
    names_to = c("predictor", ".value"),
    names_pattern = "(GDP|Life)_(mean|lo|hi)"
  ) %>%
  mutate(
    predictor = recode(
      predictor,
      GDP  = "Log GDP effect",
      Life = "Life expectancy effect"
    ),
    continent = fct_reorder(continent, mean)
  )

ymin <- min(effects_long$lo, na.rm = TRUE)
ymax <- max(effects_long$hi, na.rm = TRUE)

ymin <- min(ymin, 0)
ymax <- max(ymax, 1.05)

pad <- 0.05 * (ymax - ymin)
y_limits <- c(ymin, ymax + pad)

base_breaks <- c(-1, -0.5, 0, 0.5, 0.75, 1.0, 1.25, 1.5, 2)
y_breaks <- base_breaks[base_breaks >= y_limits[1] & base_breaks <= y_limits[2]]

p_cont <- ggplot(effects_long, aes(x = continent, y = mean, color = predictor)) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position = position_dodge(width = 0.55),
    width = 0,
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 2.2
  ) +
  coord_flip() +
  scale_color_manual(values = c(
    "Log GDP effect" = COL_BLUE,
    "Life expectancy effect" = COL_ORANGE
  )) +
  scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = str_wrap("Continent-level effects on happiness", 55),
    x = NULL,
    y = "Effect on happiness (standardized scale)",
    color = NULL
  ) +
  theme_report

ggsave(
  filename = "output/figures/continent_effects.pdf",
  plot = p_cont,
  width = 4.2,
  height = 3.2
)

# Hierarchical Bayesian Analysis of Global Happiness

This project analyzes global happiness using survey-based Cantril Ladder scores,
combined with country-level indicators of economic wealth (log GDP) and health
(life expectancy). Happiness is measured on a scale from 0 (maximum sorrow)
to 10 (maximum bliss), with 5 representing a neutral midpoint.

The goal of the analysis is to answer concrete, interpretable questions about
global trends, country rankings, and regional differences, while properly
accounting for hierarchical structure and uncertainty.

---

## What was done

Yearly happiness observations (2011–2024) were modeled using a **single hierarchical
Bayesian generalized linear model** with three nested levels:

- **Global level**: overall average happiness and global time trend  
- **Continent level**: continent-specific effects of time, wealth, and health  
- **Country level**: partially pooled country-specific baseline happiness  

The model includes:
- A linear effect of time (year)
- Economic effects via log GDP
- Health effects via life expectancy
- Partial pooling to stabilize estimates for countries with limited data

All predictors were standardized, and the model was fitted using Hamiltonian
Monte Carlo in Stan.

---

## Questions answered

The model was used to answer four concrete questions:

1. **Is global happiness increasing over time?**  
2. **Is average global happiness above the neutral midpoint (5)?**  
3. **What is the probability that Slovenia ranks among the top 20 happiest countries?**  
4. **Do the effects of wealth and health on happiness differ by continent?**

All answers are expressed probabilistically using posterior distributions rather
than point estimates.

---

## Key findings

- **No strong evidence for a global upward trend in happiness.**  
  The posterior for the global time effect is centered near zero, indicating
  relatively stable global happiness over the observed period.

- **Average global happiness is slightly positive.**  
  Posterior mass lies mostly above the neutral midpoint, suggesting that global
  happiness is modestly positive on average.

- **Slovenia is very unlikely to rank in the global top 20.**  
  Posterior country-level rankings place Slovenia well outside the top tier,
  with essentially zero probability of being among the happiest countries.

- **Wealth and health effects vary substantially by continent.**  
  GDP effects are strongest in Europe and weakest in Africa, while health effects
  differ markedly across regions, highlighting structural heterogeneity.

---

## Why this matters

This project demonstrates how hierarchical Bayesian models can:
- Handle multi-level data (years within countries, countries within continents)
- Produce stable country rankings through partial pooling
- Quantify uncertainty in rankings and comparisons
- Reveal regional heterogeneity hidden by global averages

The same modeling strategy applies broadly to social science, economics,
public policy, and applied data science problems involving grouped data.

---

## Repository contents

- `report/` – Full written analysis (PDF)
- `R/` – Data preparation, model fitting, posterior analysis, and

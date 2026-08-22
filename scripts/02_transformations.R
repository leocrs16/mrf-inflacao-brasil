rm(list = ls())

library(dplyr)
library(urca)
library(tseries)
library(ggplot2)
library(tidyr)

load("data/01_data.rda")

# =============================================================
# Transformation codes (tcode) based on Araujo & Gaglianone (2023)
# 1 = level (no transformation)
# 2 = first difference
# 5 = log-difference
# 6 = second log-difference
# =============================================================

# --- tcode = 1: level ---
# Inflation rates already in % per month
vars_level <- c(
  "ipca_headline", "ipca_free", "ipca_admin", "ipca_tradables",
  "ipca_durables", "ipca_nondurables", "ipca_diffusion",
  "ipca15", "core_dw", "core_tm", "inpc",
  "ipca_food_bev", "ipca_housing", "ipca_household",
  "ipca_transport", "ipca_communic", "ipca_personal",
  "igp_di", "igp_m", "igp_10", "incc", "ipc_br", "ipc_fipe",
  "ipa_di", "ipa_ind", "ipa_agro", "ipa_m"
)

# --- tcode = 2: first difference ---
# Interest rates, confidence indexes, % GDP ratios,
# series with negative values (flows, anomalies),
# and highly persistent IPCA components (services inertia)
vars_diff <- c(
  "selic", "tjlp", "savings_yield",
  "us_treas_3m", "us_treas_2y", "us_treas_10y", "us_tips_5y",
  "us_corp_baa", "fed_funds", "us_breakeven",
  "credit_spread", "uci", "cons_conf", "vix", "oni",
  "ipca_acc_12m", "ipca_exp_12m",
  "primary_result_pct", "primary_result", "net_debt_gdp",
  "current_account", "fdi",
  "mon_agg1", "mon_agg2", "mon_agg3",
  # Persistent IPCA components — non-stationary in level
  "ipca_nontradables", "ipca_services", "ipca_semidurables",
  "core_exc", "ipca_apparel", "ipca_health", "ipca_education"
)

# --- tcode = 5: log-difference ---
# Price indexes in levels, quantities, asset prices
vars_logdiff <- c(
  "icbr_total", "icbr_agro", "icbr_metal", "icbr_energy",
  "monetary_base", "monetary_base_broad",
  "exchange_rate", "reer", "dxy", "ibovespa", "sp500",
  "ibc_br", "cni_hours", "cni_employment", "cni_revenue",
  "ind_general", "ind_manuf", "ind_mining", "ind_capital",
  "ind_durable", "ind_consumer", "ind_interm",
  "vehicles_prod", "cars_prod", "trucks_prod", "buses_prod",
  "vehicle_sales", "vehicle_sales_dom", "steel_prod",
  "retail_real", "retail_ext",
  "electricity_com", "electricity_ind", "electricity_res",
  "electricity_tot",
  "imp_total", "imp_cap", "imp_cons", "imp_inter",
  "exp_total", "exp_cap", "exp_cons", "exp_inter", "exp_quantum",
  "oil_brent", "oil_wti", "oil_ipea", "glob_food", "glob_comm",
  "beef", "corn", "soybean",
  "min_wage_real", "min_wage_nom",
  "epu_brazil", "epu_canada", "epu_chile", "epu_china",
  "epu_france", "epu_germany", "epu_greece", "epu_india",
  "epu_ireland", "epu_italy", "epu_japan", "epu_korea",
  "epu_russia", "epu_spain", "epu_singapore", "epu_uk",
  "epu_us", "epu_australia", "epu_global"
)

# --- tcode = 6: second log-difference ---
# Strongly trending nominal stocks
vars_2logdiff <- c(
  "m1", "m2", "m3", "m4",
  "currency_circ", "savings_deposits",
  "int_reserves", "exp_price", "gdp_12m"
)

# Sanity check: every non-date column must be classified exactly once
all_vars   <- names(df_clean)[-1]
classified <- c(vars_level, vars_diff, vars_logdiff, vars_2logdiff)

missing_class <- setdiff(all_vars, classified)
extra_class   <- setdiff(classified, all_vars)
if (length(missing_class) > 0) {
  cat("NOT CLASSIFIED:\n"); print(missing_class)
}
if (length(extra_class) > 0) {
  cat("CLASSIFIED BUT NOT IN DATA:\n"); print(extra_class)
}
stopifnot(length(missing_class) == 0, length(extra_class) == 0)

# =============================================================
# 2.1 - Apply transformations
# =============================================================

df_transf <- df_clean %>%
  mutate(
    across(all_of(vars_diff),     ~ c(NA, diff(.x))),
    across(all_of(vars_logdiff),  ~ c(NA, diff(log(.x)))),
    across(all_of(vars_2logdiff), ~ c(NA, NA, diff(diff(log(.x)))))
  ) %>%
  filter(!is.na(selic))  # removes first row lost to differencing

# =============================================================
# 2.2 - ADF stationarity tests
# =============================================================
# H0: unit root (non-stationary)
# Reject H0 if test statistic < critical value at 5%

series_to_test <- names(df_transf)[-1]

cat("=== ADF Stationarity Tests ===\n")

adf_results <- data.frame(
  variable  = character(),
  statistic = numeric(),
  critical  = numeric(),
  result    = character(),
  stringsAsFactors = FALSE
)

for (s in series_to_test) {
  x <- na.omit(df_transf[[s]])
  if (length(x) < 20) {
    cat(s, "→ insufficient observations\n")
    next
  }
  adf  <- ur.df(x, type = "drift", lags = 12, selectlags = "BIC")
  stat <- round(adf@teststat[1], 2)
  crit <- adf@cval[1, 2]
  res  <- ifelse(stat < crit, "Stationary", "Non-stationary")
  
  adf_results <- rbind(adf_results, data.frame(
    variable  = s,
    statistic = stat,
    critical  = crit,
    result    = res
  ))
}

cat("\nStationary:", sum(adf_results$result == "Stationary"), "/",
    nrow(adf_results), "\n")

non_stationary <- adf_results[adf_results$result == "Non-stationary", ]
if (nrow(non_stationary) > 0) {
  cat("\nNon-stationary series:\n")
  print(non_stationary)
}

# =============================================================
# 2.3 - Visualization
# =============================================================
df_transf %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 0.2) +
  facet_wrap(~ series, ncol = 8, scales = "free_y") +
  labs(title = "Transformed series", x = NULL, y = NULL) +
  theme_minimal(base_size = 5)

# =============================================================
# 2.4 - Save
# =============================================================
save(df_transf,   file = "data/02_transformations.rda")
save(adf_results, file = "data/02_adf_results.rda")

cat("\nSaved: data/02_transformations.rda\n")
cat("Saved: data/02_adf_results.rda\n")
cat("Dimensions:", dim(df_transf), "\n")
cat("NAs:", sum(is.na(df_transf)), "\n")
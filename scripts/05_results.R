rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)

load("data/02_transformations.rda")
load("data/03_arima.rda")        # results_arima, arima_errors
load("data/04_forecasts_fxd.rda")
load("data/04_forecasts_exp.rda")
load("data/04_results_mrf_fxd.rda")
load("data/04_results_mrf_exp.rda")

if (!dir.exists("output")) dir.create("output")

n_oos    <- 120
horizons <- c(1, 3, 6, 12)

# =============================================================
# 5.1 - Realized IPCA and dates for the test period
# =============================================================

df_base <- df_transf %>%
  select(date, ipca_headline) %>%
  na.omit()

ipca_actual <- tail(df_base$ipca_headline, n_oos)
oos_dates   <- tail(df_base$date, n_oos)

# =============================================================
# 5.2 - Comparison table
# =============================================================
# Ratios below 1 mean the model beats the ARIMA benchmark

comparison <- data.frame(
  horizon    = horizons,
  arima_rmse = results_arima$RMSE,
  fxd_rmse   = results_mrf_fxd$RMSE,
  exp_rmse   = results_mrf_exp$RMSE,
  arima_mae  = results_arima$MAE,
  fxd_mae    = results_mrf_fxd$MAE,
  exp_mae    = results_mrf_exp$MAE
) %>%
  mutate(
    fxd_rmse_ratio = fxd_rmse / arima_rmse,
    exp_rmse_ratio = exp_rmse / arima_rmse,
    fxd_mae_ratio  = fxd_mae  / arima_mae,
    exp_mae_ratio  = exp_mae  / arima_mae
  )

cat("=== Model Comparison (ratios < 1 favor MRF) ===\n")
print(round(comparison, 4))

write.csv(comparison, "output/table_comparison.csv", row.names = FALSE)

# =============================================================
# 5.3 - Diebold-Mariano tests
# =============================================================
# H0: both models have equal predictive accuracy
# e1 = MRF errors, e2 = ARIMA errors
# Negative statistic + low p-value → MRF significantly better

dm_results <- data.frame(
  horizon    = integer(),
  model      = character(),
  dm_stat    = numeric(),
  p_value    = numeric(),
  conclusion = character(),
  stringsAsFactors = FALSE
)

for (h in horizons) {
  
  e_arima <- arima_errors[[as.character(h)]]
  
  for (m in c("fixed", "expanding")) {
    
    fc    <- if (m == "fixed") forecasts_fxd else forecasts_exp
    e_mrf <- ipca_actual - fc[, paste0("h", h)]
    
    # Align both error series on the same target months
    n_common <- min(length(e_arima), length(e_mrf))
    e1 <- tail(e_mrf,   n_common)
    e2 <- tail(e_arima, n_common)
    
    test <- dm.test(e1, e2, alternative = "two.sided", h = h, power = 2)
    
    concl <- if (test$p.value >= 0.10) {
      "no significant difference"
    } else if (as.numeric(test$statistic) < 0) {
      "MRF better"
    } else {
      "ARIMA better"
    }
    
    dm_results <- rbind(dm_results, data.frame(
      horizon    = h,
      model      = m,
      dm_stat    = round(as.numeric(test$statistic), 3),
      p_value    = round(test$p.value, 4),
      conclusion = concl,
      stringsAsFactors = FALSE
    ))
  }
}

cat("\n=== Diebold-Mariano Tests (MRF vs ARIMA) ===\n")
print(dm_results)

write.csv(dm_results, "output/table_dm_tests.csv", row.names = FALSE)

# =============================================================
# 5.4 - Chart: actual vs predicted
# =============================================================

df_plot <- lapply(horizons, function(h) {
  data.frame(
    date     = oos_dates,
    horizon  = paste0("h = ", h),
    Actual   = ipca_actual,
    ARIMA    = NA,
    MRF      = forecasts_exp[, paste0("h", h)]
  )
}) %>%
  bind_rows() %>%
  mutate(horizon = factor(horizon, levels = paste0("h = ", horizons))) %>%
  select(-ARIMA) %>%
  pivot_longer(c(Actual, MRF), names_to = "series", values_to = "value")

p1 <- ggplot(df_plot, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ horizon, ncol = 2) +
  scale_color_manual(values = c(Actual = "grey30", MRF = "steelblue")) +
  labs(title = "IPCA: actual vs MRF forecast (expanding window)",
       x = NULL, y = "% per month", color = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("output/fig_actual_vs_forecast.png", p1,
       width = 9, height = 6, dpi = 300)

# =============================================================
# 5.5 - Chart: RMSE by horizon
# =============================================================

df_rmse <- comparison %>%
  select(horizon, ARIMA = arima_rmse,
         `MRF fixed` = fxd_rmse, `MRF expanding` = exp_rmse) %>%
  pivot_longer(-horizon, names_to = "model", values_to = "rmse")

p2 <- ggplot(df_rmse, aes(x = factor(horizon), y = rmse, fill = model)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  labs(title = "Out-of-sample RMSE by forecast horizon",
       subtitle = "Test period: Jan/2016 - Dec/2025",
       x = "Horizon (months)", y = "RMSE (p.p. per month)", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("output/fig_rmse_by_horizon.png", p2,
       width = 8, height = 5, dpi = 300)

cat("\nSaved to output/:\n")
cat(" - table_comparison.csv\n")
cat(" - table_dm_tests.csv\n")
cat(" - fig_actual_vs_forecast.png\n")
cat(" - fig_rmse_by_horizon.png\n")

# =============================================================
# 5.6 - CSFE: Cumulative Squared Forecast Error
# =============================================================
# Cumulative sum of (ARIMA squared error - MRF squared error).
# Upward slope = MRF outperforming the benchmark in that period.
# Downward slope = benchmark winning. Reveals WHEN gains occur.

csfe_list <- list()

for (h in horizons) {
  
  e_arima <- arima_errors[[as.character(h)]]
  
  for (m in c("fixed", "expanding")) {
    
    fc    <- if (m == "fixed") forecasts_fxd else forecasts_exp
    e_mrf <- ipca_actual - fc[, paste0("h", h)]
    
    # Align both error series on the same target months
    n_common <- min(length(e_arima), length(e_mrf))
    e1 <- tail(e_mrf,     n_common)
    e2 <- tail(e_arima,   n_common)
    dt <- tail(oos_dates, n_common)
    
    csfe_list[[paste(h, m)]] <- data.frame(
      date    = dt,
      horizon = paste0("h = ", h),
      model   = paste("MRF", m),
      csfe    = cumsum(e2^2 - e1^2)
    )
  }
}

df_csfe <- bind_rows(csfe_list) %>%
  mutate(horizon = factor(horizon, levels = paste0("h = ", horizons)))

p3 <- ggplot(df_csfe, aes(x = date, y = csfe, color = model)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ horizon, ncol = 2, scales = "free_y") +
  scale_color_manual(values = c("MRF fixed"     = "#4C8DD9",
                                "MRF expanding" = "#1B9E4B")) +
  labs(title    = "Cumulative Squared Forecast Error (CSFE)",
       subtitle = "MRF vs ARIMA — upward slope favors MRF",
       x = NULL, y = "Cumulative SE difference", color = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("output/fig_csfe.png", p3, width = 9, height = 6, dpi = 300)

write.csv(df_csfe, "output/table_csfe.csv", row.names = FALSE)
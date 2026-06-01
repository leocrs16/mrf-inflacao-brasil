rm(list = ls())

library(MacroRF)
library(dplyr)

source("functions/build_data.R")
source("functions/fixed_window.R")
source("functions/expanding_window.R")

load("data/02_transformations.rda")

# =============================================================
# 4 - Macroeconomic Random Forest (MRF)
# =============================================================
# MRF generalizes ARIMA by allowing coefficients to vary
# over time (GTVPs) according to the state of the economy
# Reference: Goulet Coulombe (2024)

# =============================================================
# 4.1 - Prepare dataset
# =============================================================
# IPCA must be the first column
# Dates as rownames — required by auxiliary functions

df_mrf <- df_transf %>%
  select(date, ipca_headline, everything()) %>%
  na.omit()

rownames_dates <- format(df_mrf$date, "%Y-%m")
df_mrf <- df_mrf %>% select(-date)
rownames(df_mrf) <- rownames_dates

cat("Dataset dimensions:", dim(df_mrf), "\n")
cat("Period:", rownames(df_mrf)[1], "to", rownames(df_mrf)[nrow(df_mrf)], "\n")

# =============================================================
# 4.2 - Define out-of-sample period
# =============================================================
# Jan/2016 to Dec/2025 = 120 months = 10 years

n_oos <- 120

cat("Total observations:", nrow(df_mrf), "\n")
cat("Training:", nrow(df_mrf) - n_oos, "| Test:", n_oos, "\n")

# =============================================================
# 4.3 - Fixed Window
# =============================================================
# Estimates MRF once with full training sample
# Faster — good for initial exploration

cat("\n=== Running Fixed Window ===\n")

models_fxd <- list()

for (h in 1:12) {
  cat("Horizon", h, "\n")
  models_fxd[[h]] <- fixed_window(
    data             = df_mrf,
    specification    = "FAARRF",
    n_lags_y         = 4,   # lags of IPCA in S_t
    n_lags_vars      = 2,   # lags of each variable in S_t
    n_lags_factors   = 4,   # lags of 5 PCA factors
    n_lags_maf       = 4,   # lags to build MAFs
    n_maf_components = 2,   # MAF components per variable
    horizon          = h,
    n_oos            = n_oos
  )
}

# 4.4 - Extract and accumulate forecasts (fixed window)
forecasts_fxd <- Reduce(cbind, lapply(models_fxd, function(x) x$pred))
forecasts_fxd <- accumulate_forecasts(forecasts_fxd)

cat("\n=== Fixed Window — First forecasts ===\n")
print(head(forecasts_fxd))

save(models_fxd,    file = "data/04_models_fxd.rda")
save(forecasts_fxd, file = "data/04_forecasts_fxd.rda")

# =============================================================
# 4.5 - Expanding Window
# =============================================================
# Re-estimates MRF every 12 months with growing window
# More robust — comparable to ARIMA expanding window

cat("\n=== Running Expanding Window ===\n")
cat("This may take a while — re-estimates every 12 months\n")

models_exp <- list()

for (h in 1:12) {
  cat("Horizon", h, "\n")
  models_exp[[h]] <- expanding_window(
    data             = df_mrf,
    specification    = "FAARRF",
    n_lags_y         = 4,
    n_lags_vars      = 2,
    n_lags_factors   = 4,
    n_lags_maf       = 4,
    n_maf_components = 2,
    horizon          = h,
    n_oos            = n_oos
  )
}

# 4.6 - Extract and accumulate forecasts (expanding window)
horizon_forecasts <- list()
for (h in 1:12) {
  horizon_forecasts[[h]] <- lapply(models_exp[[h]], function(x) x$pred)
}

forecast_series <- lapply(horizon_forecasts, function(h) unlist(h))
forecasts_exp   <- do.call(cbind, forecast_series)
forecasts_exp   <- accumulate_forecasts(forecasts_exp)

cat("\n=== Expanding Window — First forecasts ===\n")
print(head(forecasts_exp))

save(models_exp,    file = "data/04_models_exp.rda")
save(forecasts_exp, file = "data/04_forecasts_exp.rda")

# =============================================================
# 4.7 - Compute accuracy metrics
# =============================================================

load("data/03_arima.rda")

# Realized IPCA in test period
ipca_actual <- tail(df_mrf$ipca_headline, n_oos)

# Auxiliary function to compute RMSE and MAE
compute_metrics <- function(forecasts, actual, horizons = c(1, 3, 6, 12)) {
  result <- data.frame(horizon = horizons, RMSE = NA, MAE = NA)
  
  for (h in horizons) {
    col <- switch(as.character(h),
                  "1"  = "h1",
                  "3"  = "acc3",
                  "6"  = "acc6",
                  "12" = "acc12")
    
    # Compute accumulated actual for h > 1
    if (h == 1) {
      actual_h <- actual
    } else {
      n <- length(actual)
      actual_h <- c(rep(NA, h - 1), sapply(seq_len(n - h + 1), function(t) {
        prod(1 + actual[t:(t + h - 1)]) - 1
      }))
    }
    
    prev  <- forecasts[, col]
    errors <- actual_h - prev
    errors <- errors[!is.na(errors)]
    
    result[result$horizon == h, "RMSE"] <- sqrt(mean(errors^2))
    result[result$horizon == h, "MAE"]  <- mean(abs(errors))
  }
  
  return(result)
}

results_mrf_fxd <- compute_metrics(forecasts_fxd, ipca_actual)
results_mrf_exp <- compute_metrics(forecasts_exp, ipca_actual)

# 4.8 - Final comparison table
cat("\n=== Final Comparison: ARIMA vs MRF ===\n")
cat("\nARIMA:\n");               print(results_arima)
cat("\nMRF Fixed Window:\n");    print(results_mrf_fxd)
cat("\nMRF Expanding Window:\n"); print(results_mrf_exp)

# 4.9 - Save results
save(results_mrf_fxd, file = "data/04_results_mrf_fxd.rda")
save(results_mrf_exp, file = "data/04_results_mrf_exp.rda")

cat("\nSaved all MRF results to data/\n")
rm(list = ls())

library(dplyr)
library(forecast)

load("data/02_transformations.rda")

# =============================================================
# 3 - ARIMA Benchmark
# =============================================================
# The ARIMA serves as the reference model (benchmark)
# If MRF does not outperform ARIMA, something is wrong
# Evaluation is done out-of-sample using expanding window

# =============================================================
# 3.1 - Convert IPCA to time series object
# =============================================================
# Starts in Feb/1996 because Jan/1996 was lost to differencing
# ipca_headline is already in % p.m. (tcode = 1, no transformation)

ipca_ts <- ts(df_transf$ipca_headline,
              start     = c(1996, 2),
              frequency = 12)

# =============================================================
# 3.2 - Define training and test windows
# =============================================================
# Training: Feb/1996 to Dec/2015 (~70% of sample)
# Test:     Jan/2016 to Dec/2025 (~30% of sample)

train_end <- c(2015, 12)

n_total <- length(ipca_ts)
n_train <- length(window(ipca_ts, end = train_end))
n_test  <- n_total - n_train

cat("Total observations:", n_total, "\n")
cat("Training:", n_train, "| Test:", n_test, "\n")
cat("Training period: Feb/1996 to Dec/2015\n")
cat("Test period: Jan/2016 to Dec/2025\n")

# =============================================================
# 3.3 - Expanding window estimation
# =============================================================
# At each step the training window grows by one month
# Simulates real-time forecasting — model never sees the future
# Horizons: 1, 3, 6 and 12 months ahead

horizons <- c(1, 3, 6, 12)

results_arima <- data.frame(
  horizon = horizons,
  RMSE    = NA,  # Root Mean Squared Error — penalizes large errors
  MAE     = NA   # Mean Absolute Error — direct interpretation
)

for (h in horizons) {
  cat("\nEstimating ARIMA for horizon h =", h, "...\n")
  errors <- c()
  
  for (i in 0:(n_test - h)) {
    
    # Training window grows by one observation each iteration
    train <- window(ipca_ts, end = time(ipca_ts)[n_train + i])
    
    # Actual value we want to forecast
    actual <- window(ipca_ts,
                     start = time(ipca_ts)[n_train + i + 1],
                     end   = time(ipca_ts)[n_train + i + h])
    
    # Automatically selects best ARIMA via AIC criterion
    model <- auto.arima(train, seasonal = TRUE)
    
    # Generate h-step ahead forecast
    pred <- forecast(model, h = h)$mean
    
    # Store h-step ahead error
    errors <- c(errors, actual[h] - pred[h])
  }
  
  # 3.4 - Compute accuracy metrics for this horizon
  results_arima[results_arima$horizon == h, "RMSE"] <- sqrt(mean(errors^2))
  results_arima[results_arima$horizon == h, "MAE"]  <- mean(abs(errors))
  
  cat("h =", h,
      "→ RMSE:", round(results_arima[results_arima$horizon == h, "RMSE"], 6),
      "| MAE:", round(results_arima[results_arima$horizon == h, "MAE"], 6), "\n")
}

# =============================================================
# 3.5 - Results
# =============================================================
cat("\n=== ARIMA Benchmark — Out-of-Sample Results ===\n")
print(results_arima)

# =============================================================
# 3.6 - Save
# =============================================================
save(results_arima, file = "data/03_arima.rda")
cat("\nSaved: data/03_arima.rda\n")
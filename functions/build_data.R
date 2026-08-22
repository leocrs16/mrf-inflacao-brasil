# =============================================================
# MRF auxiliary functions
# Based on Goulet Coulombe (2024) methodology
# =============================================================

# =============================================================
# F1 - Build state variable set S_t
# =============================================================
# S_t is the information set the RF uses to determine
# how the beta_t coefficients vary over time
# Composed of: y lags, variable lags, PCA factors and MAFs

build_st <- function(data, n_lags_y, n_lags_vars,
                     n_lags_factors, n_lags_maf,
                     n_maf_components) {
  
  data  <- as.data.frame(data)
  y     <- as.numeric(data[, 1])
  x     <- as.data.frame(data[, 2:ncol(data)])
  dates <- rownames(data)
  
  cat("DEBUG — dim(data):", dim(data), "\n")
  cat("DEBUG — class(x):", class(x), "\n")
  cat("DEBUG — ncol(x):", ncol(x), "\n")
  cat("DEBUG — class(x[[1]]):", class(x[[1]]), "\n")
  cat("DEBUG — n_lags_vars:", n_lags_vars, "\n")

  
  # --- F1.1 Lags of y ---
  # embed() creates a matrix where each column is a lag of the series
  lags_y_matrix <- embed(y, n_lags_y + 1)[, -1]
  colnames(lags_y_matrix) <- paste0("y_L", seq_len(n_lags_y))
  rownames(lags_y_matrix) <- dates[-seq_len(n_lags_y)]
  lags_y_matrix <- as.data.frame(lags_y_matrix)
  
  # --- F1.2 Lags of each predictor variable ---
  # For each variable, creates n_lags_vars lags
  predictor_lags <- lapply(colnames(x), function(var) {
    serie <- as.numeric(x[[var]])
    m <- embed(serie, n_lags_vars + 1)[, -1, drop = FALSE]
    colnames(m) <- paste0(var, "_L", seq_len(n_lags_vars))
    m
  })
  df_predictor_lags <- as.data.frame(do.call(cbind, predictor_lags))
  rownames(df_predictor_lags) <- dates[-seq_len(n_lags_vars)]
  
  # --- F1.3 PCA factors (cross-sectional compression) ---
  # First 5 principal components compress the cross-sectional
  # information from all predictors
  pca_vars   <- prcomp(x, center = TRUE, scale. = TRUE)
  factors5   <- pca_vars$x[, 1:5]
  
  factors_matrix <- embed(factors5, n_lags_factors + 1)
  factors_matrix <- factors_matrix[, -(1:5)]
  factors_matrix <- as.data.frame(factors_matrix)
  rownames(factors_matrix) <- dates[-seq_len(n_lags_factors)]
  
  factor_names <- c()
  for (lag in seq_len(n_lags_factors)) {
    for (f in 1:5) {
      factor_names <- c(factor_names, paste0("F", f, "_L", lag))
    }
  }
  colnames(factors_matrix) <- factor_names
  
  # --- F1.4 MAFs - Moving Average Factors ---
  # Compress temporal information (across lags) of each variable
  # via PCA applied to the variable's own lags
  # Analogous to cross-sectional PCA but in the temporal dimension
  maf_list <- lapply(colnames(data), function(var) {
    series   <- as.numeric(data[[var]])
    var_lags <- embed(series, n_lags_maf + 1)[, -1]
    rownames(var_lags) <- dates[-seq_len(n_lags_maf)]
    
    pca_var  <- prcomp(var_lags, center = TRUE, scale. = TRUE)
    mafs_var <- pca_var$x[, seq_len(n_maf_components), drop = FALSE]
    colnames(mafs_var) <- paste0(var, "_MAF", seq_len(n_maf_components))
    mafs_var
  })
  
  df_mafs <- as.data.frame(do.call(cbind, maf_list))
  
  # --- F1.5 Align and merge all blocks ---
  # Each block has different sizes due to lags,
  # so we align by taking the N last rows of each
  blocks <- list(lags_y_matrix, df_predictor_lags, factors_matrix, df_mafs)
  N      <- min(sapply(blocks, nrow))
  blocks_aligned <- lapply(blocks, tail, N)
  
  st_final <- do.call(cbind, blocks_aligned)
  st_final$trend <- seq_len(nrow(st_final))
  
  return(st_final)
}

# =============================================================
# F2 - Build linear part X_t (FAARRF specification)
# =============================================================
# X_t determines which coefficients will have time variation
# Specification: 2 lags of y + 1 lag of first 2 PCA factors
# Equation: y_t = mu_t + phi1_t*y_{t-1} + phi2_t*y_{t-2}
#               + gamma1_t*F1_{t-1} + gamma2_t*F2_{t-1} + error

build_xt_faarrf <- function(data) {
  
  data  <- as.data.frame(data)
  y     <- as.numeric(data[, 1])
  dates <- rownames(data)
  
  # 2 lags of y
  lags_y <- embed(y, 3)[, -1]
  rownames(lags_y) <- dates[-(1:2)]
  colnames(lags_y) <- c("y_t1", "y_t2")
  
  # 1 lag of first 2 PCA factors
  pca_data <- prcomp(as.data.frame(data), center = TRUE, scale. = TRUE)
  factors2 <- pca_data$x[, 1:2]
  
  lags_f <- embed(factors2, 2)[, -(1:2)]
  rownames(lags_f) <- dates[-1]
  colnames(lags_f) <- c("F1_t1", "F2_t1")
  
  # Align and merge
  blocks <- list(as.data.frame(lags_y), as.data.frame(lags_f))
  N      <- min(sapply(blocks, nrow))
  blocks_aligned <- lapply(blocks, tail, N)
  
  xt_final <- do.call(cbind, blocks_aligned)
  
  return(xt_final)
}

# =============================================================
# F3 - Build linear part X_t (4-lag specification)
# =============================================================
# Extended version: 4 lags of y + 2 lags of first 2 PCA factors

build_xt_4lags <- function(data) {
  
  data  <- as.data.frame(data)
  y     <- as.numeric(data[, 1])
  dates <- rownames(data)
  
  # 4 lags of y
  lags_y <- embed(y, 5)[, -1]
  rownames(lags_y) <- dates[-(1:4)]
  colnames(lags_y) <- c("y_t1", "y_t2", "y_t3", "y_t4")
  
  # 2 lags of first 2 PCA factors
  pca_data <- prcomp(as.data.frame(data), center = TRUE, scale. = TRUE)
  factors2 <- pca_data$x[, 1:2]
  
  lags_f <- embed(factors2, 3)[, -(1:2)]
  rownames(lags_f) <- dates[-(1:2)]
  colnames(lags_f) <- c("F1_t1", "F2_t1", "F1_t2", "F2_t2")
  
  # Align and merge
  blocks <- list(as.data.frame(lags_y), as.data.frame(lags_f))
  N      <- min(sapply(blocks, nrow))
  blocks_aligned <- lapply(blocks, tail, N)
  
  xt_final <- do.call(cbind, blocks_aligned)
  
  return(xt_final)
}

# =============================================================
# F4 - Build complete MRF dataset
# =============================================================
# Merges X_t (linear part) with S_t (forest part)
# and adjusts the forecast horizon by shifting y_{t+h}

build_mrf_dataset <- function(data, specification, n_lags_y,
                              n_lags_vars, n_lags_factors,
                              n_lags_maf, n_maf_components,
                              horizon) {
  
  # Build X_t according to specification
  if (specification == "FAARRF") {
    xt <- build_xt_faarrf(data)
  }
  if (specification == "FAARRF_4lags") {
    xt <- build_xt_4lags(data)
  }
  
  # Build S_t
  st <- build_st(data,
                 n_lags_y         = n_lags_y,
                 n_lags_vars      = n_lags_vars,
                 n_lags_factors   = n_lags_factors,
                 n_lags_maf       = n_lags_maf,
                 n_maf_components = n_maf_components)
  
  # Align X_t and S_t
  blocks <- list(xt, st)
  N      <- min(sapply(blocks, nrow))
  blocks_aligned <- lapply(blocks, tail, N)
  xt_st  <- do.call(cbind, blocks_aligned)
  
  # Adjust target for horizon h
  # For h=1: use y_{t+1} (next period)
  # For h>1: shift y h periods forward
  y_series <- as.data.frame(as.numeric(data[, 1]))
  colnames(y_series) <- "y"
  y_series <- tail(y_series, N)
  
  if (horizon == 1) {
    final_dataset <- cbind(y_series, xt_st)
  } else {
    h_adj    <- horizon - 1
    xt_st    <- head(xt_st, -h_adj)
    y_series <- tail(y_series, -h_adj)
    colnames(y_series) <- paste0("y_h", horizon)
    final_dataset <- cbind(y_series, xt_st)
  }
  
  return(final_dataset)
}

# =============================================================
# F5 - Accumulate forecasts for 3, 6 and 12-month horizons
# =============================================================
# Computes accumulated forecasts using diagonals of the
# direct forecast matrix (one horizon per column)
# Logic: h-month accumulated forecast = product of returns
# along the h corresponding diagonals of the forecast matrix

accumulate_forecasts <- function(forecast_matrix) {
  
  n <- nrow(forecast_matrix)
  
  # 3-month accumulated
  acc3 <- c(rep(NA, 2), sapply(seq_len(n - 2), function(t) {
    prod(1 + diag(forecast_matrix[t:(t + 2), 1:3])) - 1
  }))
  
  # 6-month accumulated
  acc6 <- c(rep(NA, 5), sapply(seq_len(n - 5), function(t) {
    prod(1 + diag(forecast_matrix[t:(t + 5), 1:6])) - 1
  }))
  
  # 12-month accumulated
  acc12 <- c(rep(NA, 11), sapply(seq_len(n - 11), function(t) {
    prod(1 + diag(forecast_matrix[t:(t + 11), 1:12])) - 1
  }))
  
  result <- cbind(forecast_matrix, acc3, acc6, acc12)
  colnames(result) <- c(paste0("h", 1:12), "acc3", "acc6", "acc12")
  
  return(result)
}
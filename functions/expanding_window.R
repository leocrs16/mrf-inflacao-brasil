# Expanding window estimation
# Re-estimates MRF every 12 months with growing window
# More robust for realistic out-of-sample evaluation

expanding_window <- function(data, specification, n_lags_y,
                             n_lags_vars, n_lags_factors,
                             n_lags_maf, n_maf_components,
                             horizon, n_oos) {
  
  if (n_oos %% 12 != 0) {
    stop("n_oos must be a multiple of 12 (integer number of years)")
  }
  
  dataset <- build_mrf_dataset(
    data             = data,
    specification    = specification,
    n_lags_y         = n_lags_y,
    n_lags_vars      = n_lags_vars,
    n_lags_factors   = n_lags_factors,
    n_lags_maf       = n_lags_maf,
    n_maf_components = n_maf_components,
    horizon          = horizon
  )
  
  if (specification == "FAARRF") {
    y_pos <- 1; x_pos <- 2:5
  }
  if (specification == "FAARRF_4lags") {
    y_pos <- 1; x_pos <- 2:8
  }
  
  s_pos    <- (max(x_pos) + 1):ncol(dataset)
  n_windows <- n_oos / 12
  results  <- list()
  
  for (j in n_windows:1) {
    
    # Training window grows by 12 months per iteration
    train_end    <- nrow(dataset) - 12 * j + 12
    window_data  <- dataset[1:train_end, ]
    oos_window   <- (nrow(window_data) - 12 + 1):nrow(window_data)
    
    result <- MRF(
      data           = as.matrix(window_data),
      y.pos          = y_pos,
      x.pos          = x_pos,
      S.pos          = s_pos,
      oos.pos        = oos_window,
      ridge.lambda   = 0.01,
      B              = 50,
      VI             = FALSE,
      printb         = FALSE,
      fast.rw        = TRUE,
      keep.forest    = FALSE
    )
    
    results[[n_windows - j + 1]] <- result
  }
  
  return(results)
}
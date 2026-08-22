# Fixed window estimation
# Estimates MRF once with full training sample
# and generates forecasts for the out-of-sample period

fixed_window <- function(data, specification, n_lags_y,
                         n_lags_vars, n_lags_factors,
                         n_lags_maf, n_maf_components,
                         horizon, n_oos) {
  
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
  
  s_pos   <- (max(x_pos) + 1):ncol(dataset)
  oos_pos <- (nrow(dataset) - n_oos + 1):nrow(dataset)
  
  result <- MRF(
    data           = as.matrix(dataset),
    y.pos          = y_pos,
    x.pos          = x_pos,
    S.pos          = s_pos,
    oos.pos        = oos_pos,
    ridge.lambda   = 0.01,
    B              = 50,
    VI             = FALSE,
    printb         = FALSE,
    fast.rw        = TRUE,
    keep.forest    = FALSE
  )
  
  return(result)
}
# =============================================================
# Janela Fixa (Fixed Window)
# =============================================================
# Estima o MRF uma única vez com toda a amostra de treino
# e gera previsões para o período out-of-sample

janela_fixa <- function(dados, especificacao, n_lags_y,
                        n_lags_vars, n_lags_fatores,
                        n_lags_maf, n_componentes_maf,
                        horizonte, n_oos) {
  
  dataset <- montar_dataset_mrf(
    dados            = dados,
    especificacao    = especificacao,
    n_lags_y         = n_lags_y,
    n_lags_vars      = n_lags_vars,
    n_lags_fatores   = n_lags_fatores,
    n_lags_maf       = n_lags_maf,
    n_componentes_maf = n_componentes_maf,
    horizonte        = horizonte
  )
  
  if (especificacao == "FAARRF") {
    y_pos <- 1; x_pos <- 2:5
  }
  if (especificacao == "FAARRF_4lags") {
    y_pos <- 1; x_pos <- 2:8
  }
  
  s_pos   <- (max(x_pos) + 1):ncol(dataset)
  oos_pos <- (nrow(dataset) - n_oos + 1):nrow(dataset)
  
  resultado <- MRF(
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
  
  return(resultado)
}
# =============================================================
# Janela Expansível (Expanding Window)
# =============================================================
# Re-estima o MRF a cada 12 meses com janela crescente
# Mais robusto para avaliação out-of-sample realista

janela_expansivel <- function(dados, especificacao, n_lags_y,
                              n_lags_vars, n_lags_fatores,
                              n_lags_maf, n_componentes_maf,
                              horizonte, n_oos) {
  
  if (n_oos %% 12 != 0) {
    stop("n_oos deve ser múltiplo de 12 (número inteiro de anos)")
  }
  
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
  
  s_pos    <- (max(x_pos) + 1):ncol(dataset)
  n_janelas <- n_oos / 12
  resultados <- list()
  
  for (j in n_janelas:1) {
    
    # Cresce a janela de treino 12 meses por iteração
    fim_treino <- nrow(dataset) - 12 * j + 12
    dados_janela <- dataset[1:fim_treino, ]
    oos_janela   <- (nrow(dados_janela) - 12 + 1):nrow(dados_janela)
    
    resultado <- MRF(
      data           = as.matrix(dados_janela),
      y.pos          = y_pos,
      x.pos          = x_pos,
      S.pos          = s_pos,
      oos.pos        = oos_janela,
      ridge.lambda   = 0.01,
      B              = 50,
      VI             = FALSE,
      printb         = FALSE,
      fast.rw        = TRUE,
      keep.forest    = FALSE
    )
    
    resultados[[n_janelas - j + 1]] <- resultado
  }
  
  return(resultados)
}
# =============================================================
# Funções auxiliares para o MRF
# Baseadas na metodologia de Goulet Coulombe (2024)
# =============================================================

# =============================================================
# F1 - Constrói o conjunto de variáveis de estado S_t
# =============================================================
# S_t é o conjunto de informações que o RF usa para determinar
# como os coeficientes beta_t variam ao longo do tempo
# Composto por: lags do y, lags das variáveis, fatores PCA e MAFs

construir_st <- function(dados, n_lags_y, n_lags_vars,
                         n_lags_fatores, n_lags_maf,
                         n_componentes_maf) {
  
  dados  <- as.data.frame(dados)
  y      <- dados[, 1]
  x      <- dados[, 2:ncol(dados)]
  datas  <- rownames(dados)
  
  # --- F1.1 Lags do y ---
  # embed() cria uma matriz onde cada coluna é um lag da série
  matriz_lags_y <- embed(y, n_lags_y + 1)[, -1]
  colnames(matriz_lags_y) <- paste0("y_L", seq_len(n_lags_y))
  rownames(matriz_lags_y) <- datas[-seq_len(n_lags_y)]
  matriz_lags_y <- as.data.frame(matriz_lags_y)
  
  # --- F1.2 Lags de cada variável preditora ---
  # Para cada variável, cria n_lags_vars defasagens
  lags_preditores <- lapply(colnames(x), function(var) {
    m <- embed(x[[var]], n_lags_vars + 1)[, -1, drop = FALSE]
    colnames(m) <- paste0(var, "_L", seq_len(n_lags_vars))
    m
  })
  df_lags_preditores <- as.data.frame(do.call(cbind, lags_preditores))
  rownames(df_lags_preditores) <- datas[-seq_len(n_lags_vars)]
  
  # --- F1.3 Fatores PCA das variáveis (compressão cross-seccional) ---
  # Os 5 primeiros componentes principais comprimem a informação
  # transversal do conjunto de preditores
  pca_vars  <- prcomp(x, center = TRUE, scale. = TRUE)
  fatores5  <- pca_vars$x[, 1:5]
  
  matriz_fat <- embed(fatores5, n_lags_fatores + 1)
  matriz_fat <- matriz_fat[, -(1:5)]
  matriz_fat <- as.data.frame(matriz_fat)
  rownames(matriz_fat) <- datas[-seq_len(n_lags_fatores)]
  
  nomes_fat <- c()
  for (lag in seq_len(n_lags_fatores)) {
    for (fator in 1:5) {
      nomes_fat <- c(nomes_fat, paste0("F", fator, "_L", lag))
    }
  }
  colnames(matriz_fat) <- nomes_fat
  
  # --- F1.4 MAFs - Moving Average Factors ---
  # Comprimem a informação temporal (ao longo dos lags) de cada variável
  # via PCA aplicado sobre os lags da própria variável
  # Análogo ao PCA cross-seccional, mas na dimensão temporal
  lista_mafs <- lapply(colnames(dados), function(var) {
    serie     <- dados[[var]]
    lags_var  <- embed(serie, n_lags_maf + 1)[, -1]
    rownames(lags_var) <- datas[-seq_len(n_lags_maf)]
    
    pca_var  <- prcomp(lags_var, center = TRUE, scale. = TRUE)
    mafs_var <- pca_var$x[, seq_len(n_componentes_maf), drop = FALSE]
    colnames(mafs_var) <- paste0(var, "_MAF", seq_len(n_componentes_maf))
    mafs_var
  })
  
  df_mafs <- as.data.frame(do.call(cbind, lista_mafs))
  
  # --- F1.5 Alinha e une todos os blocos ---
  # Como cada bloco tem tamanhos diferentes por causa dos lags,
  # alinhamos pegando as N últimas linhas de cada um
  blocos <- list(matriz_lags_y, df_lags_preditores, matriz_fat, df_mafs)
  N      <- min(sapply(blocos, nrow))
  blocos_alinhados <- lapply(blocos, tail, N)
  
  st_final <- do.call(cbind, blocos_alinhados)
  st_final$tendencia <- seq_len(nrow(st_final))
  
  return(st_final)
}

# =============================================================
# F2 - Constrói a parte linear X_t (especificação FAARRF)
# =============================================================
# X_t determina quais coeficientes terão variação temporal
# Especificação: 2 lags do y + 1 lag dos 2 primeiros fatores PCA
# Equação: y_t = mu_t + phi1_t*y_{t-1} + phi2_t*y_{t-2}
#              + gamma1_t*F1_{t-1} + gamma2_t*F2_{t-1} + erro

construir_xt_faarrf <- function(dados) {
  
  dados  <- as.data.frame(dados)
  y      <- dados[, 1]
  datas  <- rownames(dados)
  
  # 2 lags do y
  lags_y <- embed(y, 3)[, -1]
  rownames(lags_y) <- datas[-(1:2)]
  colnames(lags_y) <- c("y_t1", "y_t2")
  
  # 1 lag dos 2 primeiros fatores PCA
  pca_dados <- prcomp(dados, center = TRUE, scale. = TRUE)
  fatores2  <- pca_dados$x[, 1:2]
  
  lags_f <- embed(fatores2, 2)[, -(1:2)]
  rownames(lags_f) <- datas[-1]
  colnames(lags_f) <- c("F1_t1", "F2_t1")
  
  # Alinha e une
  blocos <- list(as.data.frame(lags_y), as.data.frame(lags_f))
  N      <- min(sapply(blocos, nrow))
  blocos_alinhados <- lapply(blocos, tail, N)
  
  xt_final <- do.call(cbind, blocos_alinhados)
  
  return(xt_final)
}

# =============================================================
# F3 - Constrói a parte linear X_t (especificação com 4 lags)
# =============================================================
# Versão estendida: 4 lags do y + 2 lags dos 2 primeiros fatores PCA

construir_xt_4lags <- function(dados) {
  
  dados  <- as.data.frame(dados)
  y      <- dados[, 1]
  datas  <- rownames(dados)
  
  # 4 lags do y
  lags_y <- embed(y, 5)[, -1]
  rownames(lags_y) <- datas[-(1:4)]
  colnames(lags_y) <- c("y_t1", "y_t2", "y_t3", "y_t4")
  
  # 2 lags dos 2 primeiros fatores PCA
  pca_dados <- prcomp(dados, center = TRUE, scale. = TRUE)
  fatores2  <- pca_dados$x[, 1:2]
  
  lags_f <- embed(fatores2, 3)[, -(1:2)]
  rownames(lags_f) <- datas[-(1:2)]
  colnames(lags_f) <- c("F1_t1", "F2_t1", "F1_t2", "F2_t2")
  
  # Alinha e une
  blocos <- list(as.data.frame(lags_y), as.data.frame(lags_f))
  N      <- min(sapply(blocos, nrow))
  blocos_alinhados <- lapply(blocos, tail, N)
  
  xt_final <- do.call(cbind, blocos_alinhados)
  
  return(xt_final)
}

# =============================================================
# F4 - Monta o dataset completo para o MRF
# =============================================================
# Une X_t (parte linear) com S_t (parte não linear/floresta)
# e ajusta o horizonte de previsão deslocando y_{t+h}

montar_dataset_mrf <- function(dados, especificacao, n_lags_y,
                               n_lags_vars, n_lags_fatores,
                               n_lags_maf, n_componentes_maf,
                               horizonte) {
  
  # Constrói X_t conforme especificação
  if (especificacao == "FAARRF") {
    xt <- construir_xt_faarrf(dados)
  }
  if (especificacao == "FAARRF_4lags") {
    xt <- construir_xt_4lags(dados)
  }
  
  # Constrói S_t
  st <- construir_st(dados,
                     n_lags_y         = n_lags_y,
                     n_lags_vars      = n_lags_vars,
                     n_lags_fatores   = n_lags_fatores,
                     n_lags_maf       = n_lags_maf,
                     n_componentes_maf = n_componentes_maf)
  
  # Alinha X_t e S_t
  blocos <- list(xt, st)
  N      <- min(sapply(blocos, nrow))
  blocos_alinhados <- lapply(blocos, tail, N)
  xt_st  <- do.call(cbind, blocos_alinhados)
  
  # Ajusta o target para o horizonte h
  # Para h=1: usa y_t+1 (próximo período)
  # Para h>1: desloca y h períodos à frente
  y_serie <- as.data.frame(dados[, 1])
  colnames(y_serie) <- "y"
  y_serie <- tail(y_serie, N)
  
  if (horizonte == 1) {
    dataset_final <- cbind(y_serie, xt_st)
  } else {
    h_adj  <- horizonte - 1
    xt_st  <- head(xt_st, -h_adj)
    y_serie <- tail(y_serie, -h_adj)
    colnames(y_serie) <- paste0("y_h", horizonte)
    dataset_final <- cbind(y_serie, xt_st)
  }
  
  return(dataset_final)
}

# =============================================================
# F5 - Acumula previsões para horizontes de 3, 6 e 12 meses
# =============================================================
# Calcula previsões acumuladas usando as diagonais da matriz
# de previsões diretas (um horizonte por coluna)
# Lógica: previsão acumulada de h meses = produto dos retornos
# nas h diagonais correspondentes da matriz de previsões

acumular_previsoes <- function(matriz_prev) {
  
  n <- nrow(matriz_prev)
  
  # Acumulado 3 meses
  acc3 <- c(rep(NA, 2), sapply(seq_len(n - 2), function(t) {
    prod(1 + diag(matriz_prev[t:(t + 2), 1:3])) - 1
  }))
  
  # Acumulado 6 meses
  acc6 <- c(rep(NA, 5), sapply(seq_len(n - 5), function(t) {
    prod(1 + diag(matriz_prev[t:(t + 5), 1:6])) - 1
  }))
  
  # Acumulado 12 meses
  acc12 <- c(rep(NA, 11), sapply(seq_len(n - 11), function(t) {
    prod(1 + diag(matriz_prev[t:(t + 11), 1:12])) - 1
  }))
  
  resultado <- cbind(matriz_prev, acc3, acc6, acc12)
  colnames(resultado) <- c(paste0("h", 1:12), "acc3", "acc6", "acc12")
  
  return(resultado)
}
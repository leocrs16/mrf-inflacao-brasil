# =============================================================
# Previsão da Inflação Brasileira com MRF
# SINAPE 2026
# Autores: Leonardo Carvalho Ribeiro da Silva
#          Hudson da Silva Torrent
# =============================================================

# =============================================================
# 0 - Configuração
# =============================================================
rm(list = ls())

library(MacroRF)
library(dplyr)

source("functions/construir_dados.R")
source("functions/janela_fixa.R")
source("functions/janela_expansivel.R")

load("data/df_transf.rda")

# =============================================================
# 4 - Macroeconomic Random Forest (MRF)
# =============================================================
# O MRF generaliza o ARIMA permitindo que os coeficientes
# variem no tempo (GTVPs) conforme o estado da economia
# Referência: Goulet Coulombe (2024)

# 4.1 - Prepara o dataframe no formato esperado
# IPCA deve ser a primeira coluna
# Datas como rownames — exigido pelas funções auxiliares

df_mrf <- df_transf %>%
  select(date, ipca, selic, cambio, uci, carne,
         petroleo, soja, sal_min, exp_ipca, result_prim, agr_mon) %>%
  na.omit()

rownames_datas <- format(df_mrf$date, "%Y-%m")
df_mrf <- df_mrf %>% select(-date)
rownames(df_mrf) <- rownames_datas
df_mrf <- df_mrf %>% select(ipca, everything())

cat("Dimensões do dataset:", dim(df_mrf), "\n")
cat("Período:", rownames(df_mrf)[1], "a", rownames(df_mrf)[nrow(df_mrf)], "\n")

# 4.2 - Define número de observações out-of-sample
# jan/2016 a dez/2025 = 120 meses = 10 anos
n_oos <- 120

cat("Observações totais:", nrow(df_mrf), "\n")
cat("Treino:", nrow(df_mrf) - n_oos, "| Teste:", n_oos, "\n")

# =============================================================
# 4.3 - Fixed Window
# =============================================================
# Estima o MRF uma única vez com toda a amostra de treino
# Mais rápido — bom para exploração inicial e resultados rápidos

cat("\n=== Rodando Fixed Window ===\n")

lista_modelos_fxd <- list()

for (h in 1:12) {
  cat("Horizonte", h, "\n")
  lista_modelos_fxd[[h]] <- janela_fixa(
    dados             = df_mrf,
    especificacao     = "FAARRF",
    n_lags_y          = 4,  # lags do IPCA no S_t
    n_lags_vars       = 2,  # lags de cada variável no S_t
    n_lags_fatores    = 4,  # lags dos 5 fatores PCA
    n_lags_maf        = 4,  # lags para construir MAFs
    n_componentes_maf = 2,  # componentes MAF por variável
    horizonte         = h,
    n_oos             = n_oos
  )
}

# 4.4 - Extrai e acumula previsões (fixed window)
# Cada modelo gera previsão de 1 passo para seu horizonte
# acumular_previsoes() calcula as diagonais para h=3,6,12
previsoes_fxd <- Reduce(cbind, lapply(lista_modelos_fxd, function(x) x$pred))
previsoes_fxd <- acumular_previsoes(previsoes_fxd)

cat("\n=== Fixed Window - Primeiras previsões ===\n")
print(head(previsoes_fxd))

save(lista_modelos_fxd, file = "data/lista_modelos_fxd.rda")
save(previsoes_fxd,     file = "data/previsoes_fxd.rda")

# =============================================================
# 4.5 - Expanding Window
# =============================================================
# Re-estima o MRF a cada 12 meses com janela crescente
# Mais robusto — comparável à expanding window do ARIMA

cat("\n=== Rodando Expanding Window ===\n")
cat("Isso pode demorar bastante — re-estima a cada 12 meses\n")

lista_modelos_exp <- list()

for (h in 1:12) {
  cat("Horizonte", h, "\n")
  lista_modelos_exp[[h]] <- janela_expansivel(
    dados             = df_mrf,
    especificacao     = "FAARRF",
    n_lags_y          = 4,
    n_lags_vars       = 2,
    n_lags_fatores    = 4,
    n_lags_maf        = 4,
    n_componentes_maf = 2,
    horizonte         = h,
    n_oos             = n_oos
  )
}

# 4.6 - Extrai e acumula previsões (expanding window)
# Para cada horizonte, coleta as previsões de cada janela anual
horizonte_prevs <- list()
for (h in 1:12) {
  horizonte_prevs[[h]] <- lapply(lista_modelos_exp[[h]], function(x) x$pred)
}

series_prevs  <- lapply(horizonte_prevs, function(h) unlist(h))
previsoes_exp <- do.call(cbind, series_prevs)
previsoes_exp <- acumular_previsoes(previsoes_exp)

cat("\n=== Expanding Window - Primeiras previsões ===\n")
print(head(previsoes_exp))

save(lista_modelos_exp, file = "data/lista_modelos_exp.rda")
save(previsoes_exp,     file = "data/previsoes_exp.rda")

# =============================================================
# 4.7 - Calcula métricas de acurácia
# =============================================================

load("data/resultados_arima.rda")

# IPCA realizado no período de teste
ipca_real <- tail(df_mrf$ipca, n_oos)

# Função auxiliar para calcular RMSE e MAE
calcular_metricas <- function(previsoes, real, horizontes = c(1, 3, 6, 12)) {
  resultado <- data.frame(horizonte = horizontes, RMSE = NA, MAE = NA)
  
  for (h in horizontes) {
    
    if (h == 1) {
      # Previsão direta — compara com IPCA mensal real
      prev  <- previsoes[, "h1"]
      erros <- real - prev
      erros <- erros[!is.na(erros)]
      
    } else {
      # Previsão acumulada — compara com IPCA acumulado real
      col  <- switch(as.character(h),
                     "3"  = "acc3",
                     "6"  = "acc6",
                     "12" = "acc12")
      prev <- previsoes[, col]
      
      # Calcula IPCA acumulado real com a mesma lógica diagonal
      n <- length(real)
      real_acc <- c(rep(NA, h - 1), sapply(seq_len(n - h + 1), function(t) {
        prod(1 + real[t:(t + h - 1)]) - 1
      }))
      
      erros <- real_acc - prev
      erros <- erros[!is.na(erros)]
    }
    
    resultado[resultado$horizonte == h, "RMSE"] <- sqrt(mean(erros^2))
    resultado[resultado$horizonte == h, "MAE"]  <- mean(abs(erros))
  }
  
  return(resultado)
}

resultados_mrf_fxd <- calcular_metricas(previsoes_fxd, ipca_real)
resultados_mrf_exp <- calcular_metricas(previsoes_exp, ipca_real)

# 4.8 - Tabela comparativa final
cat("\n=== Comparação Final: ARIMA vs MRF ===\n")
cat("\nARIMA:\n")
print(resultados_arima)
cat("\nMRF Fixed Window:\n")
print(resultados_mrf_fxd)
cat("\nMRF Expanding Window:\n")
print(resultados_mrf_exp)

# 4.9 - Salva resultados
save(resultados_mrf_fxd, file = "data/resultados_mrf_fxd.rda")
save(resultados_mrf_exp, file = "data/resultados_mrf_exp.rda")
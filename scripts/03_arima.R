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

library(forecast)

load("data/df_transf.rda")

# =============================================================
# 3 - Benchmark ARIMA
# =============================================================
# O ARIMA serve como modelo de referência (benchmark)
# Se o MRF não superar o ARIMA, algo está errado
# Comparação feita fora da amostra (out-of-sample)

# 3.1 - Converte IPCA para série temporal
# Começa em fev/1996 pois jan/1996 foi perdido pela diferenciação
ipca_ts <- ts(df_transf$ipca,
              start     = c(1996, 2),
              frequency = 12)

# 3.2 - Define janelas de treino e teste
# Treino: jan/1996 a dez/2015 (~70% da amostra)
# Teste:  jan/2016 a dez/2025 (~30% da amostra)
train_end <- c(2015, 12)

n_total <- length(ipca_ts)
n_train <- length(window(ipca_ts, end = train_end))
n_test  <- n_total - n_train

# 3.3 - Expanding window com ARIMA
# A cada passo o treino cresce um mês
# Simula o que aconteceria na prática: modelo re-estimado com novos dados
# Nunca usa informação do futuro para prever o passado

horizontes <- c(1, 3, 6, 12) # Horizontes de previsão em meses

resultados_arima <- data.frame(
  horizonte = horizontes,
  RMSE      = NA, # Raiz do Erro Quadrático Médio - penaliza erros grandes
  MAE       = NA  # Erro Absoluto Médio - interpretação direta
)

for (h in horizontes) {
  erros <- c()
  
  for (i in 0:(n_test - h)) {
    
    # Treino cresce a cada iteração (expanding window)
    train <- window(ipca_ts, end = time(ipca_ts)[n_train + i])
    
    # Valor real que queremos prever
    real  <- window(ipca_ts,
                    start = time(ipca_ts)[n_train + i + 1],
                    end   = time(ipca_ts)[n_train + i + h])
    
    # Estima ARIMA automaticamente via critério AIC
    modelo <- auto.arima(train, seasonal = TRUE)
    
    # Gera previsão h passos à frente
    prev <- forecast(modelo, h = h)$mean
    
    # Guarda erro do h-ésimo passo
    erros <- c(erros, real[h] - prev[h])
  }
  
  # 3.4 - Calcula métricas para esse horizonte
  resultados_arima[resultados_arima$horizonte == h, "RMSE"] <- sqrt(mean(erros^2))
  resultados_arima[resultados_arima$horizonte == h, "MAE"]  <- mean(abs(erros))
}

# 3.5 - Resultados
cat("\n=== Benchmark ARIMA - Resultados Out-of-Sample ===\n")
print(resultados_arima)

# Resultado obtido:
# horizonte    RMSE         MAE
#         1  0.00348      0.00261
#         3  0.00391      0.00291
#         6  0.00399      0.00306
#        12  0.00405      0.00312

# 3.6 - Salva resultados
save(resultados_arima, file = "data/resultados_arima.rda")
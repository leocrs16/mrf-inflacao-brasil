# =============================================================
# Previsão da Inflação Brasileira com MRF
# SINAPE 2026
# Autores: Leonardo Carvalho Ribeiro da Silva
#          Hudson da Silva Torrent
# =============================================================

# =============================================================
# 0. Configuração
# =============================================================
rm(list = ls())

library(dplyr)
library(ipeadatar)
library(lubridate)
library(tidyr)
library(rbcb)
library(urca)
library(tseries)
library(forecast)
library(purrr)

# =============================================================
# 1. Coleta de Dados
# =============================================================

# 1.1 - Séries mensais do Ipeadata
codigos_mensais <- c(
  "PRECOS12_IPCA12",   # IPCA
  "BM12_TJOVER12",     # Selic
  "IFS12_BEEFB12",     # Carne
  "IFS12_PETROLEUM12", # Petróleo
  "IFS12_SOJAGP12",    # Soja
  "GAC12_SALMINRE12"   # Salário mínimo real
)

metadados <- metadata(codigos_mensais)
data      <- ipeadata(metadados$code)

df <- data %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode))

df <- df[order(df$date), ]

# 1.2 - Séries diárias → mensais do Ipeadata
# Câmbio é diário — converte para mensal via média
diario_para_mensal <- function(codigo) {
  meta <- metadata(codigo)
  d    <- ipeadata(meta$code)
  d %>%
    pivot_wider(names_from = "code") %>%
    select(-c(uname, tcode)) %>%
    mutate(year_month = format(date, "%Y-%m")) %>%
    group_by(year_month) %>%
    summarise(across(-date, ~ mean(.x, na.rm = TRUE))) %>%
    mutate(date = as.Date(paste(year_month, "-01", sep = ""))) %>%
    select(-year_month)
}

df_cambio <- diario_para_mensal("GM366_ERV366") # Câmbio R$/US$

# 1.3 - Séries do Banco Central (SGS/BCB)
# Puxamos as três séries e juntamos em um único dataframe

df_bcb <- rbcb::get_series(
  c(13522, 7384, 27838, 1396),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>%
  reduce(full_join, by = "date") %>%
  rename(
    exp_ipca        = `13522`, # Expectativa IPCA 12 meses (Focus)
    result_primario = `7384`,  # Resultado primário do governo
    agr_monetario   = `27838`, # Agregado monetário (variação %)
    uci             = `1396`   # Utilização da capacidade instalada
  )

# 1.4 - Junta todas as fontes
df <- df %>%
  left_join(df_cambio, by = "date") %>%
  left_join(df_bcb,    by = "date")

# 1.5 - Filtro de período
df_final <- df %>%
  filter(date >= as.Date("1996-01-01") & date <= as.Date("2025-12-01"))

# 1.6 - Diagnóstico
dim(df_final)
names(df_final)
colSums(is.na(df_final))

# =============================================================
# 2. Transformações para Estacionariedade
# =============================================================
# Log-diferença para séries de nível (índices e preços)
# Diferença simples para séries que já são taxas

df_transf <- df_final %>%
  mutate(
    ipca        = c(NA, diff(log(PRECOS12_IPCA12))),  # Variação mensal do IPCA
    selic       = c(NA, diff(BM12_TJOVER12)),          # Variação da Selic
    cambio      = c(NA, diff(log(GM366_ERV366))),      # Variação % do câmbio
    uci         = c(NA, diff(uci)),                    # Variação da cap. instalada
    carne       = c(NA, diff(log(IFS12_BEEFB12))),     # Variação % da carne
    petroleo    = c(NA, diff(log(IFS12_PETROLEUM12))), # Variação % do petróleo
    soja        = c(NA, diff(log(IFS12_SOJAGP12))),    # Variação % da soja
    sal_min     = c(NA, diff(log(GAC12_SALMINRE12))),  # Variação % do salário
    exp_ipca    = c(NA, diff(exp_ipca)),               # Variação da expectativa
    result_prim = c(NA, diff(result_primario)),        # Variação do resultado
    agr_mon     = agr_monetario                        # Já é variação %
  ) %>%
  select(date, ipca, selic, cambio, uci, carne,
         petroleo, soja, sal_min, exp_ipca, result_prim, agr_mon) %>%
  filter(!is.na(ipca))

# 2.1 - Teste ADF para todas as séries
series_testar <- names(df_transf)[-1] # Remove coluna date

cat("== Teste ADF de Estacionariedade ==\n")
for(s in series_testar) {
  x <- na.omit(df_transf[[s]])
  adf <- ur.df(x, type = "drift", lags = 12, selectlags = "BIC")
  stat <- adf@teststat[1]
  crit <- adf@cval[1, 2] # Valor crítico 5%
  resultado <- ifelse(stat < crit, "Estacionária", "Não estacionária")
  cat(s, "estatistica:", round(stat, 2),
      "| critico 5%:", crit, "|", resultado, "\n")
}

# =============================================================
# 3. Benchmark ARIMA
# =============================================================

# 3.1 - Converte IPCA para série temporal
ipca_ts <- ts(df_transf$ipca,
              start = c(1996, 2),
              frequency = 12)

# 3.2 - Define janelas de treino e teste
# Treino: jan/1996 a dez/2015
# Teste:  jan/2016 a dez/2025
train_end <- c(2015, 12)

n_total <- length(ipca_ts)
n_train <- length(window(ipca_ts, end = train_end))
n_test <- n_total - n_train

# 3.3 - Expanding window com ARIMA
horizontes <- c(1, 3, 6, 12)

resultados_arima <- data.frame(
  horizonte = horizontes,
  RMSE = NA,
  MAE = NA
)

for (h in horizontes) {
  erros <- c()
  
  for (i in 0:(n_test - h)) {
    train  <- window(ipca_ts, end = time(ipca_ts)[n_train + i])
    real   <- window(ipca_ts,
                     start = time(ipca_ts)[n_train + i + 1],
                     end   = time(ipca_ts)[n_train + i + h])
    modelo <- auto.arima(train, seasonal = TRUE)
    prev   <- forecast(modelo, h = h)$mean
    erros  <- c(erros, real[h] - prev[h])
  }
  
  resultados_arima[resultados_arima$horizonte == h, "RMSE"] <- sqrt(mean(erros^2))
  resultados_arima[resultados_arima$horizonte == h, "MAE"]  <- mean(abs(erros))
}

# 3.4 - Resultados
cat("\n== Benchmark ARIMA - Resultados Out-of-Sample ==\n")
print(resultados_arima)

colSums(is.na(df_final))  # Checa NAs

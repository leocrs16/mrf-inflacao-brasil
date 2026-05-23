rm(list = ls())

# Pacotes necessários

library(dplyr)
library(ipeadatar)
library(lubridate)
library(tidyr)

##########################
# Desemprego: PME (até 2015) + PNAD Contínua (2016 em diante)
##########################

# PME antiga (1996-2002)
meta_pme1 <- metadata("PME12_TDA12")
data_pme1 <- ipeadata(meta_pme1$code)
df_pme1 <- data_pme1 %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(desemprego = PME12_TDA12) %>%
  filter(date <= as.Date("2002-12-01"))

# PAN (2003-2015)
meta_pme2 <- metadata("PAN12_TD12")
data_pme2 <- ipeadata(meta_pme2$code)
df_pme2 <- data_pme2 %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(desemprego = PAN12_TD12) %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2015-12-01"))

# PNAD Contínua (2016-2025)
meta_pnad <- metadata("PNADC12_TDESOCMD12")
data_pnad <- ipeadata(meta_pnad$code)
df_pnad <- data_pnad %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(desemprego = PNADC12_TDESOCMD12) %>%
  filter(date >= as.Date("2016-01-01"))

# Combina as três
df_desemprego <- bind_rows(df_pme1, df_pme2, df_pnad)


# Séries MENSAIS

codigos_mensais <- c(
  "PRECOS12_IPCA12",  # IPCA
  "BM12_TJOVER12"     # Selic
)

metadados <- metadata(codigos_mensais)
data      <- ipeadata(metadados$code)

df <- data %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode))

df <- df[order(df$date), ]

##########################
# Séries DIÁRIAS → MENSAL
##########################

# Função para converter série diária em mensal (média)
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

##########################
# Junta tudo
##########################
df <- df %>%
  left_join(df_cambio,     by = "date") %>%
  left_join(df_desemprego, by = "date")

##########################
# Filtro de período
##########################
df_final <- df %>%
  filter(date >= as.Date("1996-01-01") & date <= as.Date("2025-12-01"))

##########################
# Diagnóstico geral
##########################
dim(df_final)
names(df_final)
head(df_final)
colSums(is.na(df_final))

# 1. Estrutura geral
dim(df_final)    
summary(df_final) # Checa mínimos e máximos - valores absurdos indicam problema

# 2. Checa as emendas do desemprego
# Emenda 1: 2002 → 2003 (PME antiga para PAN)
df_final %>%
  filter(date >= as.Date("2001-06-01") & date <= as.Date("2004-06-01")) %>%
  select(date, desemprego) %>%
  print(n = 36)

# Emenda 2: 2015 → 2016 (PAN para PNAD)
df_final %>%
  filter(date >= as.Date("2014-06-01") & date <= as.Date("2017-06-01")) %>%
  select(date, desemprego) %>%
  print(n = 36)

# 3. Checa valores extremos em cada série
df_final %>%
  summarise(
    ipca_min  = min(PRECOS12_IPCA12),  ipca_max  = max(PRECOS12_IPCA12),
    selic_min = min(BM12_TJOVER12),    selic_max = max(BM12_TJOVER12),
    cambio_min= min(GM366_ERV366),     cambio_max= max(GM366_ERV366),
    desemp_min= min(desemprego),       desemp_max= max(desemprego)
  )

df_final <- df_final %>% select(-desemprego)
View(df_final)

##########################
# Transformações
##########################
library(urca)
library(tseries)

# Visualiza as séries brutas primeiro
par(mfrow = c(3, 1))
plot(df_final$date, df_final$PRECOS12_IPCA12, type = "l", main = "IPCA (nível)")
plot(df_final$date, df_final$BM12_TJOVER12,   type = "l", main = "Selic (nível)")
plot(df_final$date, df_final$GM366_ERV366,    type = "l", main = "Câmbio (nível)")
par(mfrow = c(1, 1))

# Aplica transformações
df_transf <- df_final %>%
  mutate(
    # IPCA: log-diferença (retorno mensal da inflação)
    ipca  = c(NA, diff(log(PRECOS12_IPCA12))),
    
    # Selic: já é taxa, usa só diferença
    selic = c(NA, diff(BM12_TJOVER12)),
    
    # Câmbio: log-diferença (variação percentual)
    cambio = c(NA, diff(log(GM366_ERV366)))
  ) %>%
  select(date, ipca, selic, cambio) %>%
  filter(!is.na(ipca))  # Remove o NA gerado pela diferença

# Visualiza após transformação
par(mfrow = c(3, 1))
plot(df_transf$date, df_transf$ipca,   type = "l", main = "IPCA (log-diff)")
plot(df_transf$date, df_transf$selic,  type = "l", main = "Selic (diff)")
plot(df_transf$date, df_transf$cambio, type = "l", main = "Câmbio (log-diff)")
par(mfrow = c(1, 1))

# Testa estacionariedade
adf_ipca   <- ur.df(df_transf$ipca,   type = "drift", lags = 12, selectlags = "BIC")
adf_selic  <- ur.df(df_transf$selic,  type = "drift", lags = 12, selectlags = "BIC")
adf_cambio <- ur.df(df_transf$cambio, type = "drift", lags = 12, selectlags = "BIC")

cat("=== ADF - IPCA ===\n");   print(summary(adf_ipca))
cat("=== ADF - Selic ===\n");  print(summary(adf_selic))
cat("=== ADF - Câmbio ===\n"); print(summary(adf_cambio))

##########################
# ARIMA Benchmark
##########################
library(forecast)

# Série alvo: IPCA transformado
ipca_ts <- ts(df_transf$ipca, 
              start = c(1996, 2),  # começa em fev/1996 por causa do diff
              frequency = 12)

# Janela inicial de estimação: até dez/2015 (~70% da amostra)
# Janela de avaliação: jan/2016 a dez/2025
train_end   <- c(2015, 12)
test_start  <- c(2016,  1)

# Índices
n_total <- length(ipca_ts)
n_train <- length(window(ipca_ts, end = train_end))
n_test  <- n_total - n_train

# Horizontes de previsão
horizontes <- c(1, 3, 6, 12)

# Armazena resultados
resultados <- data.frame(
  horizonte = horizontes,
  RMSE = NA,
  MAE  = NA
)

for (h in horizontes) {
  erros <- c()
  
  for (i in 0:(n_test - h)) {
    # Expanding window
    train <- window(ipca_ts, end = time(ipca_ts)[n_train + i])
    real  <- window(ipca_ts, 
                    start = time(ipca_ts)[n_train + i + 1],
                    end   = time(ipca_ts)[n_train + i + h])
    
    # Estima ARIMA automaticamente
    modelo <- auto.arima(train, seasonal = TRUE)
    
    # Previsão h passos à frente
    prev <- forecast(modelo, h = h)$mean
    
    # Erro apenas no último passo (h-step ahead)
    erros <- c(erros, real[h] - prev[h])
  }
  
  resultados[resultados$horizonte == h, "RMSE"] <- sqrt(mean(erros^2))
  resultados[resultados$horizonte == h, "MAE"]  <- mean(abs(erros))
}

print(resultados)
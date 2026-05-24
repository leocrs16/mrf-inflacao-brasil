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

library(dplyr)
library(ipeadatar)
library(lubridate)
library(tidyr)
library(rbcb)
library(purrr)

# =============================================================
# 1 - Coleta de Dados
# =============================================================

# 1.1 - Séries mensais do Ipeadata
codigos_mensais <- c(
  "PRECOS12_IPCA12",   # IPCA - índice de preços ao consumidor
  "BM12_TJOVER12",     # Selic - taxa básica de juros
  "IFS12_BEEFB12",     # Carne - cotação internacional
  "IFS12_PETROLEUM12", # Petróleo - cotação internacional
  "IFS12_SOJAGP12",    # Soja - cotação internacional
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

# 1.7 - Salva dataset
save(df_final, file = "data/df_final.rda")
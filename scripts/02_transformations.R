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
library(urca)
library(tseries)

load("data/df_final.rda")

# =============================================================
# 2 - Transformações para Estacionariedade
# =============================================================
# Séries econômicas em nível geralmente não são estacionárias
# (média e variância mudam ao longo do tempo)
# O ADF testa isso formalmente — precisamos rejeitar raiz unitária

# 2.1 - Aplica transformações
df_transf <- df_final %>%
  mutate(
    # Log-diferença → converte índices em variação percentual mensal
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
  filter(!is.na(ipca)) # Remove o NA gerado pela diferença

# 2.2 - Teste ADF de estacionariedade
# H0: série tem raiz unitária (não estacionária)
# Rejeitamos H0 se estatística < valor crítico
# Quanto mais negativo, mais forte a evidência de estacionariedade

series_testar <- names(df_transf)[-1]

cat("=== Testes ADF de Estacionariedade ===\n")
for (s in series_testar) {
  x    <- na.omit(df_transf[[s]])
  adf  <- ur.df(x, type = "drift", lags = 12, selectlags = "BIC")
  stat <- adf@teststat[1]
  crit <- adf@cval[1, 2] # Valor crítico 5%
  resultado <- ifelse(stat < crit, "Estacionária", "Não estacionária")
  cat(s, "→ estatística:", round(stat, 2),
      "| crítico 5%:", crit, "|", resultado, "\n")
}

# Resultado esperado: todas as 11 séries estacionárias
# ipca:        -8.15  < -2.87 
# selic:       -15.43 < -2.87 
# cambio:      -12.05 < -2.87 
# uci:         -6.98  < -2.87 
# carne:       -13.50 < -2.87 
# petroleo:    -11.86 < -2.87 
# soja:        -10.86 < -2.87 
# sal_min:     -4.20  < -2.87 
# exp_ipca:    -5.50  < -2.87 
# result_prim: -7.21  < -2.87 
# agr_mon:     -5.43  < -2.87 

# 2.3 - Visualização das séries transformadas
library(ggplot2)
library(tidyr)

df_transf %>%
  pivot_longer(-date, names_to = "serie", values_to = "valor") %>%
  ggplot(aes(x = date, y = valor)) +
  geom_line() +
  facet_wrap(~ serie, ncol = 3, scales = "free_y") +
  labs(title = "Séries transformadas", x = NULL, y = NULL) +
  theme_minimal()

# 2.4 - Salva dataset transformado
save(df_transf, file = "data/df_transf.rda")
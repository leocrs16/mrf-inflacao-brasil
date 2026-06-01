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
library(tidyr)
library(lubridate)
library(ipeadatar)
library(rbcb)
library(purrr)
library(quantmod)
library(fredr)
library(readxl)

# Configura chave FRED — substitui pela sua chave de 32 caracteres
fredr_set_key("ee6f5eabf146cdb8686aaf000e28d723")

# =============================================================
# Funções auxiliares
# =============================================================

# Converte série diária Ipeadata para mensal via média
diario_para_mensal_ipea <- function(codigo) {
  meta <- metadata(codigo)
  d    <- ipeadata(meta$code)
  d %>%
    pivot_wider(names_from = "code") %>%
    select(-c(uname, tcode)) %>%
    mutate(ano_mes = format(date, "%Y-%m")) %>%
    group_by(ano_mes) %>%
    summarise(across(-date, ~ mean(.x, na.rm = TRUE))) %>%
    mutate(date = as.Date(paste(ano_mes, "-01", sep = ""))) %>%
    select(-ano_mes)
}

# Converte série Yahoo Finance (diária) para mensal
yahoo_para_mensal <- function(ticker) {
  d <- tryCatch(
    getSymbols(ticker, src = "yahoo",
               from = "1996-01-01", to = "2025-12-31",
               auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)
  d_mensal <- to.monthly(Ad(d), indexAt = "firstof", OHLC = FALSE)
  data.frame(
    date  = as.Date(index(d_mensal)),
    valor = as.numeric(d_mensal)
  )
}

# Coleta série FRED com tratamento de erro
fred_serie <- function(codigo, nome, frequencia = "m") {
  tryCatch({
    fredr(codigo,
          observation_start = as.Date("1996-01-01"),
          observation_end   = as.Date("2025-12-31"),
          frequency = frequencia) %>%
      select(date, value) %>%
      rename(!!nome := value)
  }, error = function(e) {
    cat(codigo, "→ ERRO FRED\n")
    NULL
  })
}

# =============================================================
# 1.1 - Inflação BCB
# =============================================================
cat("Puxando inflação BCB...\n")

codigos_inflacao_bcb <- c(
  433,    # IPCA headline
  4448,   # IPCA mercado
  4449,   # IPCA administrados
  4452,   # IPCA tradables
  4453,   # IPCA non-tradables
  10844,  # IPCA serviços
  11427,  # IPCA bens industriais
  1635,   # IPCA alimentação
  21379,  # Difusão IPCA
  16121,  # IPCA-15
  4466,   # Núcleo EX0
  4465,   # Núcleo EX1
  4463,   # Núcleo trimmed means
  11426   # Núcleo double weight
)

df_inflacao_bcb <- rbcb::get_series(
  codigos_inflacao_bcb,
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(
    ipca_headline  = `433`,
    ipca_mercado   = `4448`,
    ipca_admin     = `4449`,
    ipca_tradables = `4452`,
    ipca_nontrad   = `4453`,
    ipca_servicos  = `10844`,
    ipca_ind_goods = `11427`,
    ipca_alim      = `1635`,
    ipca_difusao   = `21379`,
    ipca15         = `16121`,
    nucleo_ex0     = `4466`,
    nucleo_ex1     = `4465`,
    nucleo_tm      = `4463`,
    nucleo_dw      = `11426`
  )

# =============================================================
# 1.2 - Inflação Ipeadata (IGP, INCC, IPC-BR)
# =============================================================
cat("Puxando inflação Ipeadata...\n")

codigos_inflacao_ipea <- c(
  "IGP12_IGPDIG12",   # IGP-DI variação
  "IGP12_IGPMG12",    # IGP-M variação
  "IGP12_IGP1012",    # IGP-10
  "IGP12_INCCG12",    # INCC
  "IGP12_IPCSNUCL12"  # Núcleo IPC-BR
)

meta_inf <- metadata(codigos_inflacao_ipea)
data_inf <- ipeadata(meta_inf$code)

df_inflacao_ipea <- data_inf %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    igp_di       = IGP12_IGPDIG12,
    igp_m        = IGP12_IGPMG12,
    igp_10       = IGP12_IGP1012,
    incc         = IGP12_INCCG12,
    nucleo_ipcbr = IGP12_IPCSNUCL12
  )

# =============================================================
# 1.3 - Juros BCB
# =============================================================
cat("Puxando juros BCB...\n")

df_juros_bcb <- rbcb::get_series(
  c(7454, 4382, 7385),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(
    tjlp          = `7454`,
    result_pib    = `4382`,
    result_mensal = `7385`
  )

# Selic (Ipeadata — converte diário para mensal)
df_selic <- diario_para_mensal_ipea("BM12_TJOVER12") %>%
  rename(selic = BM12_TJOVER12)

# =============================================================
# 1.4 - Juros Americanos FRED
# =============================================================
cat("Puxando juros americanos FRED...\n")

lista_fred_juros <- list(
  fred_serie("DTB3",  "us_treas_3m"),
  fred_serie("DGS2",  "us_treas_2y"),
  fred_serie("DGS10", "us_treas_10y"),
  fred_serie("DFII5", "us_tips_5y"),
  fred_serie("BAA",   "us_corp_baa")
) %>% compact()

if (length(lista_fred_juros) > 0) {
  df_juros_fred <- lista_fred_juros %>% reduce(full_join, by = "date")
} else {
  df_juros_fred <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
  cat("Nenhuma série FRED de juros carregada\n")
}

# =============================================================
# 1.5 - Moeda
# =============================================================
cat("Puxando moeda...\n")

codigos_moeda_ipea <- c(
  "BM12_M1N12",   # M1
  "BM12_M2NCN12", # M2
  "BM12_M3NCN12", # M3
  "BM12_M4NCN12"  # M4
)

meta_moeda <- metadata(codigos_moeda_ipea)
data_moeda <- ipeadata(meta_moeda$code)

df_moeda <- data_moeda %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    m1 = BM12_M1N12,
    m2 = BM12_M2NCN12,
    m3 = BM12_M3NCN12,
    m4 = BM12_M4NCN12
  )

# Agregados monetários BCB
df_agr_bcb <- rbcb::get_series(
  c(27838, 28750, 28751),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(
    agr_mon1 = `27838`,
    agr_mon2 = `28750`,
    agr_mon3 = `28751`
  )

# =============================================================
# 1.6 - Crédito e Bancos BCB
# =============================================================
cat("Puxando crédito...\n")

df_credito_bcb <- rbcb::get_series(
  c(1799, 21082),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(
    spread_credito = `1799`,
    npl            = `21082`
  )

# =============================================================
# 1.7 - Câmbio e Risco
# =============================================================
cat("Puxando câmbio e risco...\n")

# Câmbio R$/US$ BCB
df_cambio_bcb <- rbcb::get_series(
  4192,
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% rename(cambio = `4192`)

# REER BCB
df_reer <- rbcb::get_series(
  7448,
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% rename(reer = `7448`)

# EMBI+Br Ipeadata (diário → mensal)
df_embi <- diario_para_mensal_ipea("JPM366_EMBI366") %>%
  rename(embi = JPM366_EMBI366)

# VIX, DXY e Ibovespa Yahoo Finance (diário → mensal)
df_vix  <- yahoo_para_mensal("^VIX")   %>% rename(vix     = valor)
df_dxy  <- yahoo_para_mensal("DX-Y.NYB") %>% rename(dxy   = valor)
df_ibov <- yahoo_para_mensal("^BVSP")  %>% rename(ibovespa = valor)

# =============================================================
# 1.8 - Atividade Econômica BCB
# =============================================================
cat("Puxando atividade econômica...\n")

df_atividade_bcb <- rbcb::get_series(
  c(7478, 1396),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(ibc_br = `7478`, uci = `1396`)

# Confiança do consumidor BCB (começa 1999)
df_confianca <- rbcb::get_series(
  4393,
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% rename(confianca_cons = `4393`)

# =============================================================
# 1.9 - Trabalho Ipeadata
# =============================================================
cat("Puxando trabalho...\n")

codigos_trabalho_ipea <- c(
  "PNADC12_TDESOCMD12", # Desemprego dessaz
  "GAC12_SALMINRE12",   # Salário mínimo real
  "MTE12_SALMIN12",     # Salário mínimo vigente
  "CAGED12_SALDON12",   # Saldo CAGED
  "CAGED12_ADMISN12",   # Admissões
  "CAGED12_DESLIGN12"   # Demissões
)

meta_trab <- metadata(codigos_trabalho_ipea)
data_trab <- ipeadata(meta_trab$code)

df_trabalho <- data_trab %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    desemprego    = PNADC12_TDESOCMD12,
    sal_min_real  = GAC12_SALMINRE12,
    sal_min_nom   = MTE12_SALMIN12,
    caged_saldo   = CAGED12_SALDON12,
    caged_admis   = CAGED12_ADMISN12,
    caged_deslig  = CAGED12_DESLIGN12
  )

# =============================================================
# 1.10 - Indústria Ipeadata (nacional, começa 2002)
# =============================================================
cat("Puxando indústria...\n")

codigos_industria_ipea <- c(
  "PIMPFN12_QIIGNN12",    # Indústria geral
  "PIMPFN12_QIITNN12",    # Transformação
  "PIMPFN12_QIEMNNAS12",  # Extrativa dessaz
  "PIMPFN12_QIBKNN12",    # Bens de capital
  "PIMPFN12_QIBCDNN12",   # Bens de consumo duráveis
  "PIMPFN12_QIBCTNNAS12", # Bens de consumo total dessaz
  "PIMPFN12_QIBINNAS12"   # Bens intermediários dessaz
)

meta_ind <- metadata(codigos_industria_ipea)
data_ind  <- ipeadata(meta_ind$code)

df_industria <- data_ind %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    ind_geral  = PIMPFN12_QIIGNN12,
    ind_transf = PIMPFN12_QIITNN12,
    ind_extrat = PIMPFN12_QIEMNNAS12,
    ind_cap    = PIMPFN12_QIBKNN12,
    ind_dur    = PIMPFN12_QIBCDNN12,
    ind_cons   = PIMPFN12_QIBCTNNAS12,
    ind_inter  = PIMPFN12_QIBINNAS12
  )

# =============================================================
# 1.11 - Vendas e Energia Ipeadata
# =============================================================
cat("Puxando vendas e energia...\n")

codigos_ve_ipea <- c(
  "PMC12_IVVRN12",      # Varejo real
  "PMC12_IVVRNAMP12",   # Varejo ampliado
  "ELETRO12_CEETCOM12", # Energia comercial
  "ELETRO12_CEETIND12", # Energia industrial
  "ELETRO12_CEETRES12", # Energia residencial
  "ELETRO12_CEETT12"    # Energia total
)

meta_ve <- metadata(codigos_ve_ipea)
data_ve  <- ipeadata(meta_ve$code)

df_vendas_energia <- data_ve %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    varejo_real  = PMC12_IVVRN12,
    varejo_amp   = PMC12_IVVRNAMP12,
    energia_com  = ELETRO12_CEETCOM12,
    energia_ind  = ELETRO12_CEETIND12,
    energia_res  = ELETRO12_CEETRES12,
    energia_tot  = ELETRO12_CEETT12
  )

# =============================================================
# 1.12 - Setor Externo Ipeadata + BCB
# =============================================================
cat("Puxando setor externo...\n")

codigos_ext_ipea <- c(
  "SECEX12_MVTOT12",       # Importações total
  "SECEX12_XVTOT12",       # Exportações total
  "SECEX12_MBENCAPGCE12",  # Imp bens de capital
  "SECEX12_MBENCONGCE12",  # Imp bens de consumo
  "SECEX12_MINTGCE12",     # Imp intermediários
  "SECEX12_XBENCAPGCE12",  # Exp bens de capital
  "SECEX12_XBENCONGCE12",  # Exp bens de consumo
  "SECEX12_XINTGCE12"      # Exp intermediários
)

meta_ext <- metadata(codigos_ext_ipea)
data_ext  <- ipeadata(meta_ext$code)

df_externo_ipea <- data_ext %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    imp_total = SECEX12_MVTOT12,
    exp_total = SECEX12_XVTOT12,
    imp_cap   = SECEX12_MBENCAPGCE12,
    imp_cons  = SECEX12_MBENCONGCE12,
    imp_inter = SECEX12_MINTGCE12,
    exp_cap   = SECEX12_XBENCAPGCE12,
    exp_cons  = SECEX12_XBENCONGCE12,
    exp_inter = SECEX12_XINTGCE12
  )

# Reservas e FDI BCB
df_externo_bcb <- rbcb::get_series(
  c(11753, 13067),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(reservas = `11753`, fdi = `13067`)

# =============================================================
# 1.13 - Setor Público BCB
# =============================================================
cat("Puxando setor público...\n")

df_fiscal_bcb <- rbcb::get_series(
  c(7384, 7385, 4382, 4503),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(
    result_prim    = `7384`,
    result_prim2   = `7385`,
    result_pib     = `4382`,
    divida_liq_pib = `4503`
  )

# =============================================================
# 1.14 - Commodities FRED + Ipeadata
# =============================================================
cat("Puxando commodities...\n")

# FRED
lista_comm_fred <- list(
  fred_serie("DCOILBRENTEU", "oil_brent"),
  fred_serie("DCOILWTICO",   "oil_wti")
) %>% compact()

if (length(lista_comm_fred) > 0) {
  df_commodities_fred <- lista_comm_fred %>%
    reduce(full_join, by = "date")
} else {
  df_commodities_fred <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
  cat("Commodities FRED não carregadas — usando Ipeadata\n")
}

# Ipeadata
codigos_comm_ipea <- c(
  "IFS12_BEEFB12",
  "IFS12_SOJAGP12",
  "IFS12_PETROLEUM12",
  "IFS12_MAIZE12"
)

meta_comm <- metadata(codigos_comm_ipea)
data_comm  <- ipeadata(meta_comm$code)

df_commodities_ipea <- data_comm %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    carne    = IFS12_BEEFB12,
    soja     = IFS12_SOJAGP12,
    petroleo = IFS12_PETROLEUM12,
    milho    = IFS12_MAIZE12
  )

# =============================================================
# 1.15 - Expectativas BCB
# =============================================================
cat("Puxando expectativas...\n")

df_expect_bcb <- rbcb::get_series(
  c(13522, 7454),
  start_date = "1996-01-01",
  end_date   = "2025-12-01"
) %>% reduce(full_join, by = "date") %>%
  rename(
    exp_ipca = `13522`,
    tjlp2    = `7454`
  )

# =============================================================
# 1.16 - EPU (Incerteza Global)
# =============================================================
cat("Puxando EPU...\n")

tryCatch({
  epu_url <- "https://www.policyuncertainty.com/media/All_Country_Data.xlsx"
  temp    <- tempfile(fileext = ".xlsx")
  download.file(epu_url, temp, mode = "wb", quiet = TRUE)
  epu_raw <- read_excel(temp)
  
  df_epu <- epu_raw %>%
    mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    filter(date >= as.Date("1996-01-01"),
           date <= as.Date("2025-12-01")) %>%
    select(date, Brazil, Canada, Chile, China, France,
           Germany, Greece, India, Ireland, Italy,
           Japan, Korea, Russia, Spain, Singapore,
           UK, US, Australia, GEPU_current) %>%
    rename(
      epu_brazil    = Brazil,
      epu_canada    = Canada,
      epu_chile     = Chile,
      epu_china     = China,
      epu_france    = France,
      epu_germany   = Germany,
      epu_greece    = Greece,
      epu_india     = India,
      epu_ireland   = Ireland,
      epu_italy     = Italy,
      epu_japan     = Japan,
      epu_korea     = Korea,
      epu_russia    = Russia,
      epu_spain     = Spain,
      epu_singapore = Singapore,
      epu_uk        = UK,
      epu_usa       = US,
      epu_australia = Australia,
      epu_global    = GEPU_current
    )
  cat("EPU carregado:", nrow(df_epu), "linhas\n")
}, error = function(e) {
  cat("EPU → ERRO:", e$message, "\n")
  df_epu <<- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
})

# =============================================================
# 1.17 - Junta Tudo
# =============================================================
cat("\nJuntando todos os datasets...\n")

lista_dfs <- list(
  df_inflacao_bcb,
  df_inflacao_ipea,
  df_selic,
  df_juros_bcb,
  df_juros_fred,
  df_moeda,
  df_agr_bcb,
  df_credito_bcb,
  df_cambio_bcb,
  df_reer,
  df_embi,
  df_vix,
  df_dxy,
  df_ibov,
  df_atividade_bcb,
  df_confianca,
  df_trabalho,
  df_industria,
  df_vendas_energia,
  df_externo_ipea,
  df_externo_bcb,
  df_fiscal_bcb,
  df_commodities_fred,
  df_commodities_ipea,
  df_expect_bcb,
  df_epu
)

df_completo <- lista_dfs %>%
  reduce(full_join, by = "date")

# =============================================================
# 1.18 - Filtro de período e diagnóstico
# =============================================================
df_final_completo <- df_completo %>%
  filter(date >= as.Date("1996-01-01") &
           date <= as.Date("2025-12-01")) %>%
  arrange(date)

cat("\n=== Diagnóstico do Dataset Completo ===\n")
cat("Dimensões:", dim(df_final_completo), "\n")
cat("Período:", as.character(min(df_final_completo$date)),
    "a", as.character(max(df_final_completo$date)), "\n")
cat("Datas duplicadas:",
    sum(duplicated(df_final_completo$date)), "\n")

cat("\nNAs por coluna (só as que têm NA):\n")
nas <- colSums(is.na(df_final_completo))
print(nas[nas > 0])

# =============================================================
# 1.19 - Salva
# =============================================================
save(df_final_completo, file = "data/df_final_completo.rda")
cat("\nDataset salvo em data/df_final_completo.rda\n")
cat("Total de variáveis:", ncol(df_final_completo) - 1, "\n")
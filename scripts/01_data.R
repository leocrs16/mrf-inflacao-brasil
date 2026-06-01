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

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# =============================================================
# Helper functions
# =============================================================

# Converts daily Ipeadata series to monthly via mean
daily_to_monthly_ipea <- function(code) {
  meta <- metadata(code)
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

# Converts Yahoo Finance daily series to monthly via closing price
yahoo_to_monthly <- function(ticker) {
  d <- tryCatch(
    getSymbols(ticker, src = "yahoo",
               from = "1996-01-01", to = "2025-12-31",
               auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)
  d_monthly <- to.monthly(Ad(d), indexAt = "firstof", OHLC = FALSE)
  data.frame(
    date  = as.Date(index(d_monthly)),
    value = as.numeric(d_monthly)
  )
}

# Fetches a single FRED series with error handling
fred_series <- function(code, name, frequency = "m") {
  tryCatch({
    fredr(code,
          observation_start = as.Date("1996-01-01"),
          observation_end   = as.Date("2025-12-31"),
          frequency = frequency) %>%
      select(date, value) %>%
      rename(!!name := value)
  }, error = function(e) {
    cat(code, "→ FRED ERROR\n")
    NULL
  })
}

# Fetches a single BCB series with error handling
bcb_series <- function(code, name) {
  tryCatch({
    rbcb::get_series(code,
                     start_date = "1996-01-01",
                     end_date   = "2025-12-01") %>%
      rename(!!name := as.character(code))
  }, error = function(e) {
    cat("BCB", code, "→ ERROR\n")
    NULL
  })
}

# =============================================================
# 1.1 - Inflation (BCB)
# =============================================================
cat("Fetching BCB inflation series...\n")

codigos_inflacao_bcb <- c(
  433,    # IPCA headline
  4448,   # IPCA market prices
  4449,   # IPCA administered prices
  4452,   # IPCA tradables
  4453,   # IPCA non-tradables
  10844,  # IPCA services
  11427,  # IPCA industrial goods
  1635,   # IPCA food at home
  21379,  # IPCA diffusion index
  16121,  # IPCA-15
  4466,   # Core IPCA - exclusion EX0
  4465,   # Core IPCA - exclusion EX1
  4463,   # Core IPCA - trimmed means
  11426   # Core IPCA - double weight
)

df_inflation_bcb <- lapply(codigos_inflacao_bcb, function(cod) {
  tryCatch(
    rbcb::get_series(cod, start_date = "1996-01-01", end_date = "2025-12-01"),
    error = function(e) { cat("BCB", cod, "→ ERROR\n"); NULL }
  )
}) %>%
  compact() %>%
  reduce(full_join, by = "date") %>%
  rename(
    ipca_headline  = `433`,
    ipca_market    = `4448`,
    ipca_admin     = `4449`,
    ipca_tradables = `4452`,
    ipca_nontrad   = `4453`,
    ipca_services  = `10844`,
    ipca_ind_goods = `11427`,
    ipca_food      = `1635`,
    ipca_diffusion = `21379`,
    ipca15         = `16121`,
    core_ex0       = `4466`,
    core_ex1       = `4465`,
    core_tm        = `4463`,
    core_dw        = `11426`
  )

# =============================================================
# 1.2 - Inflation (Ipeadata — IGP, INCC, IPC-BR core)
# =============================================================
cat("Fetching Ipeadata inflation series...\n")

codes_inflation_ipea <- c(
  "IGP12_IGPDIG12",   # IGP-DI monthly change
  "IGP12_IGPMG12",    # IGP-M monthly change
  "IGP12_IGP1012",    # IGP-10
  "IGP12_INCCG12",    # INCC
  "IGP12_IPCSNUCL12"  # IPC-BR core
)

meta_inf <- metadata(codes_inflation_ipea)
data_inf <- ipeadata(meta_inf$code)

df_inflation_ipea <- data_inf %>%
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
    core_ipcbr   = IGP12_IPCSNUCL12
  )

# =============================================================
# 1.3 - Interest rates (BCB)
# =============================================================
cat("Fetching BCB interest rate series...\n")

# Selic — daily series converted to monthly
df_selic <- daily_to_monthly_ipea("BM12_TJOVER12") %>%
  rename(selic = BM12_TJOVER12)

# Other BCB interest/fiscal rates
lista_juros_bcb <- list(
  bcb_series(7454, "tjlp"),
  bcb_series(7385, "result_primary")
) %>% compact()

if (length(lista_juros_bcb) > 0) {
  df_rates_bcb <- lista_juros_bcb %>% reduce(full_join, by = "date")
} else {
  df_rates_bcb <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.4 - US interest rates (FRED)
# =============================================================
cat("Fetching FRED US interest rates...\n")

lista_fred_rates <- list(
  fred_series("DTB3",  "us_treas_3m"),
  fred_series("DGS2",  "us_treas_2y"),
  fred_series("DGS10", "us_treas_10y"),
  fred_series("DFII5", "us_tips_5y"),
  fred_series("BAA",   "us_corp_baa")
) %>% compact()

if (length(lista_fred_rates) > 0) {
  df_rates_fred <- lista_fred_rates %>% reduce(full_join, by = "date")
} else {
  df_rates_fred <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
  cat("No FRED rate series loaded\n")
}

# =============================================================
# 1.5 - Money aggregates
# =============================================================
cat("Fetching money aggregates...\n")

codes_money_ipea <- c(
  "BM12_M1N12",   # M1
  "BM12_M2NCN12", # M2
  "BM12_M3NCN12", # M3
  "BM12_M4NCN12"  # M4
)

meta_money <- metadata(codes_money_ipea)
data_money <- ipeadata(meta_money$code)

df_money_ipea <- data_money %>%
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

# BCB monetary aggregates (already in % change)
lista_money_bcb <- list(
  bcb_series(27838, "mon_agg1"),
  bcb_series(28750, "mon_agg2"),
  bcb_series(28751, "mon_agg3")
) %>% compact()

if (length(lista_money_bcb) > 0) {
  df_money_bcb <- lista_money_bcb %>% reduce(full_join, by = "date")
} else {
  df_money_bcb <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.6 - Banking and credit (BCB)
# =============================================================
cat("Fetching credit series...\n")

lista_credit <- list(
  bcb_series(1799,  "credit_spread"),
  bcb_series(21082, "npl")
) %>% compact()

if (length(lista_credit) > 0) {
  df_credit <- lista_credit %>% reduce(full_join, by = "date")
} else {
  df_credit <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.7 - FX and risk
# =============================================================
cat("Fetching FX and risk series...\n")

# Nominal exchange rate (BCB)
df_fx <- bcb_series(4192, "exchange_rate")

# REER (BCB)
df_reer <- bcb_series(7448, "reer")

# EMBI+Br (Ipeadata — daily to monthly)
df_embi <- daily_to_monthly_ipea("JPM366_EMBI366") %>%
  rename(embi = JPM366_EMBI366)

# VIX, DXY, Ibovespa (Yahoo Finance — daily to monthly)
df_vix  <- yahoo_to_monthly("^VIX")     %>% rename(vix      = value)
df_dxy  <- yahoo_to_monthly("DX-Y.NYB") %>% rename(dxy      = value)
df_ibov <- yahoo_to_monthly("^BVSP")    %>% rename(ibovespa = value)

# =============================================================
# 1.8 - Economic activity (BCB)
# =============================================================
cat("Fetching economic activity series...\n")

lista_activity <- list(
  bcb_series(7478, "ibc_br"),    # IBC-Br (starts 2000)
  bcb_series(1396, "uci"),       # Capacity utilization
  bcb_series(4393, "cons_conf")  # Consumer confidence (starts 1999)
) %>% compact()

if (length(lista_activity) > 0) {
  df_activity <- lista_activity %>% reduce(full_join, by = "date")
} else {
  df_activity <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.9 - Labor market (Ipeadata)
# =============================================================
cat("Fetching labor market series...\n")

codes_labor_ipea <- c(
  "PNADC12_TDESOCMD12", # Unemployment rate - seasonally adjusted (starts 2012)
  "GAC12_SALMINRE12",   # Real minimum wage
  "MTE12_SALMIN12",     # Nominal minimum wage
  "CAGED12_SALDON12",   # Net formal employment - CAGED (starts 2020)
  "CAGED12_ADMISN12",   # Hires - CAGED
  "CAGED12_DESLIGN12"   # Separations - CAGED
)

meta_labor <- metadata(codes_labor_ipea)
data_labor <- ipeadata(meta_labor$code)

df_labor <- data_labor %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    unemployment  = PNADC12_TDESOCMD12,
    min_wage_real = GAC12_SALMINRE12,
    min_wage_nom  = MTE12_SALMIN12,
    caged_net     = CAGED12_SALDON12,
    caged_hires   = CAGED12_ADMISN12,
    caged_sep     = CAGED12_DESLIGN12
  )

# =============================================================
# 1.10 - Industrial production (Ipeadata — national, starts 2002)
# =============================================================
cat("Fetching industrial production series...\n")

codes_industry_ipea <- c(
  "PIMPFN12_QIIGNN12",    # General industry
  "PIMPFN12_QIITNN12",    # Manufacturing
  "PIMPFN12_QIEMNNAS12",  # Mining - seasonally adjusted
  "PIMPFN12_QIBKNN12",    # Capital goods
  "PIMPFN12_QIBCDNN12",   # Durable consumer goods
  "PIMPFN12_QIBCTNNAS12", # Total consumer goods - seasonally adjusted
  "PIMPFN12_QIBINNAS12"   # Intermediate goods - seasonally adjusted
)

meta_ind <- metadata(codes_industry_ipea)
data_ind  <- ipeadata(meta_ind$code)

df_industry <- data_ind %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    ind_general  = PIMPFN12_QIIGNN12,
    ind_manuf    = PIMPFN12_QIITNN12,
    ind_mining   = PIMPFN12_QIEMNNAS12,
    ind_capital  = PIMPFN12_QIBKNN12,
    ind_durable  = PIMPFN12_QIBCDNN12,
    ind_consumer = PIMPFN12_QIBCTNNAS12,
    ind_interm   = PIMPFN12_QIBINNAS12
  )

# =============================================================
# 1.11 - Retail sales and energy (Ipeadata)
# =============================================================
cat("Fetching retail sales and energy series...\n")

codes_sales_energy_ipea <- c(
  "PMC12_IVVRN12",      # Real retail sales
  "PMC12_IVVRNAMP12",   # Real retail sales - extended
  "ELETRO12_CEETCOM12", # Electricity consumption - commercial
  "ELETRO12_CEETIND12", # Electricity consumption - industrial
  "ELETRO12_CEETRES12", # Electricity consumption - residential
  "ELETRO12_CEETT12"    # Electricity consumption - total
)

meta_se <- metadata(codes_sales_energy_ipea)
data_se  <- ipeadata(meta_se$code)

df_sales_energy <- data_se %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    retail_real    = PMC12_IVVRN12,
    retail_ext     = PMC12_IVVRNAMP12,
    electricity_com = ELETRO12_CEETCOM12,
    electricity_ind = ELETRO12_CEETIND12,
    electricity_res = ELETRO12_CEETRES12,
    electricity_tot = ELETRO12_CEETT12
  )

# =============================================================
# 1.12 - External sector (Ipeadata + BCB)
# =============================================================
cat("Fetching external sector series...\n")

codes_external_ipea <- c(
  "SECEX12_MVTOT12",       # Total imports
  "SECEX12_XVTOT12",       # Total exports
  "SECEX12_MBENCAPGCE12",  # Imports - capital goods
  "SECEX12_MBENCONGCE12",  # Imports - consumer goods
  "SECEX12_MINTGCE12",     # Imports - intermediate goods
  "SECEX12_XBENCAPGCE12",  # Exports - capital goods
  "SECEX12_XBENCONGCE12",  # Exports - consumer goods
  "SECEX12_XINTGCE12"      # Exports - intermediate goods
)

meta_ext <- metadata(codes_external_ipea)
data_ext  <- ipeadata(meta_ext$code)

df_external_ipea <- data_ext %>%
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

# International reserves and FDI (BCB)
lista_external_bcb <- list(
  bcb_series(11753, "int_reserves"),
  bcb_series(13067, "fdi")
) %>% compact()

if (length(lista_external_bcb) > 0) {
  df_external_bcb <- lista_external_bcb %>% reduce(full_join, by = "date")
} else {
  df_external_bcb <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.13 - Public sector (BCB)
# =============================================================
cat("Fetching public sector series...\n")

lista_fiscal <- list(
  bcb_series(7384, "primary_result"),
  bcb_series(7385, "primary_result2"),
  bcb_series(4382, "primary_result_gdp"),
  bcb_series(4503, "net_debt_gdp")
) %>% compact()

if (length(lista_fiscal) > 0) {
  df_fiscal <- lista_fiscal %>% reduce(full_join, by = "date")
} else {
  df_fiscal <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.14 - Commodities (FRED + Ipeadata)
# =============================================================
cat("Fetching commodities series...\n")

# FRED
lista_comm_fred <- list(
  fred_series("DCOILBRENTEU", "oil_brent"),
  fred_series("DCOILWTICO",   "oil_wti")
) %>% compact()

if (length(lista_comm_fred) > 0) {
  df_comm_fred <- lista_comm_fred %>% reduce(full_join, by = "date")
} else {
  df_comm_fred <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
  cat("FRED commodity series not loaded\n")
}

# Ipeadata
codes_comm_ipea <- c(
  "IFS12_BEEFB12",     # Beef international price
  "IFS12_SOJAGP12",    # Soybean international price
  "IFS12_PETROLEUM12", # Oil international price
  "IFS12_MAIZE12"      # Corn international price
)

meta_comm <- metadata(codes_comm_ipea)
data_comm  <- ipeadata(meta_comm$code)

df_comm_ipea <- data_comm %>%
  group_by(code, date) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(names_from = "code") %>%
  select(-c(uname, tcode)) %>%
  rename(
    beef      = IFS12_BEEFB12,
    soybean   = IFS12_SOJAGP12,
    oil_ipea  = IFS12_PETROLEUM12,
    corn      = IFS12_MAIZE12
  )

# =============================================================
# 1.15 - Inflation expectations (BCB)
# =============================================================
cat("Fetching expectations series...\n")

lista_expect <- list(
  bcb_series(13522, "ipca_exp_focus") # Focus survey - IPCA 12m expectations
) %>% compact()

if (length(lista_expect) > 0) {
  df_expectations <- lista_expect %>% reduce(full_join, by = "date")
} else {
  df_expectations <- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
}

# =============================================================
# 1.16 - Economic Policy Uncertainty (EPU)
# =============================================================
cat("Fetching EPU series...\n")

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
  cat("EPU loaded:", nrow(df_epu), "rows\n")
}, error = function(e) {
  cat("EPU → ERROR:", e$message, "\n")
  df_epu <<- data.frame(
    date = seq(as.Date("1996-01-01"), as.Date("2025-12-01"), by = "month")
  )
})

# =============================================================
# 1.17 - Merge all datasets
# =============================================================
cat("\nMerging all datasets...\n")

lista_dfs <- list(
  df_inflation_bcb,
  df_inflation_ipea,
  df_selic,
  df_rates_bcb,
  df_rates_fred,
  df_money_ipea,
  df_money_bcb,
  df_credit,
  df_fx,
  df_reer,
  df_embi,
  df_vix,
  df_dxy,
  df_ibov,
  df_activity,
  df_labor,
  df_industry,
  df_sales_energy,
  df_external_ipea,
  df_external_bcb,
  df_fiscal,
  df_comm_fred,
  df_comm_ipea,
  df_expectations,
  df_epu
)

df_complete <- lista_dfs %>%
  reduce(full_join, by = "date")

# =============================================================
# 1.18 - Diagnostics
# =============================================================
cat("\n=== Dataset Diagnostics ===\n")
cat("Dimensions:", dim(df_clean), "\n")
cat("Period:", as.character(min(df_clean$date)),
    "to", as.character(max(df_clean$date)), "\n")
cat("Duplicate dates:", sum(duplicated(df_clean$date)), "\n")
cat("Remaining NAs:", sum(is.na(df_clean)), "\n")
cat("Variables kept:", ncol(df_clean) - 1, "\n")
cat("\nVariable names:\n")
print(names(df_clean))

# =============================================================
# 1.19 - Save
# =============================================================
save(df_clean, file = "data/01_data.rda")
cat("Saved: data/01_data.rda\n")
cat("Variables:", ncol(df_clean) - 1, "\n")
cat("Dimensions:", dim(df_clean), "\n")
cat("Duplicate dates:", sum(duplicated(df_clean$date)), "\n")
cat("NAs:", sum(is.na(df_clean)), "\n")
names(df_clean)
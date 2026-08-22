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

# Sample period: Jan/2003 - Dec/2025 (276 monthly observations)
START <- "2003-01-01"
END   <- "2025-12-01"

# =============================================================
# Helper functions
# =============================================================

# Converts Yahoo Finance daily series to monthly closing price
yahoo_to_monthly <- function(ticker) {
  d <- tryCatch(
    getSymbols(ticker, src = "yahoo",
               from = START, to = "2025-12-31",
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
          observation_start = as.Date(START),
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
    rbcb::get_series(code, start_date = START, end_date = END) %>%
      rename(!!name := as.character(code))
  }, error = function(e) {
    cat("BCB", code, "→ ERROR\n")
    NULL
  })
}

# Fetches Ipeadata series (monthly) with dedup safeguard
ipea_block <- function(codes) {
  meta <- metadata(codes)
  d    <- ipeadata(meta$code)
  d %>%
    group_by(code, date) %>%
    slice(1) %>%
    ungroup() %>%
    pivot_wider(names_from = "code") %>%
    select(-any_of(c("uname", "tcode")))
}

# Safely merges a list of BCB series
merge_bcb <- function(lst) {
  lst <- compact(lst)
  if (length(lst) > 0) {
    reduce(lst, full_join, by = "date")
  } else {
    data.frame(date = seq(as.Date(START), as.Date(END), by = "month"))
  }
}

# =============================================================
# 1.1 - Consumer inflation (BCB)
# =============================================================
cat("Fetching BCB consumer inflation series...\n")

df_inflation_bcb <- merge_bcb(list(
  bcb_series(433,   "ipca_headline"),      # IPCA monthly change
  bcb_series(11428, "ipca_free"),          # IPCA free prices
  bcb_series(4449,  "ipca_admin"),         # IPCA administered prices
  bcb_series(4447,  "ipca_tradables"),     # IPCA tradables
  bcb_series(4448,  "ipca_nontradables"),  # IPCA non-tradables
  bcb_series(10844, "ipca_services"),      # IPCA services
  bcb_series(10843, "ipca_durables"),      # IPCA durable goods
  bcb_series(10842, "ipca_semidurables"),  # IPCA semi-durable goods
  bcb_series(10841, "ipca_nondurables"),   # IPCA non-durable goods
  bcb_series(21379, "ipca_diffusion"),     # IPCA diffusion index
  bcb_series(7478,  "ipca15"),             # IPCA-15 preview
  bcb_series(16122, "core_dw"),            # Core IPCA double weight
  bcb_series(11427, "core_exc"),           # Core IPCA exclusion
  bcb_series(4466,  "core_tm"),            # Core IPCA trimmed means smoothed
  bcb_series(188,   "inpc"),               # INPC monthly change
  # IPCA expenditure groups
  bcb_series(1635,  "ipca_food_bev"),      # Food and beverages
  bcb_series(1636,  "ipca_housing"),       # Housing
  bcb_series(1637,  "ipca_household"),     # Household articles
  bcb_series(1638,  "ipca_apparel"),       # Apparel
  bcb_series(1639,  "ipca_transport"),     # Transportation
  bcb_series(1640,  "ipca_communic"),      # Communication
  bcb_series(1641,  "ipca_health"),        # Health and personal care
  bcb_series(1642,  "ipca_personal"),      # Personal expenses
  bcb_series(1643,  "ipca_education")      # Education
))

# =============================================================
# 1.2 - Producer and general price indexes (BCB)
# =============================================================
cat("Fetching producer/general price indexes...\n")

df_prices_bcb <- merge_bcb(list(
  bcb_series(190,  "igp_di"),      # IGP-DI monthly change
  bcb_series(189,  "igp_m"),       # IGP-M monthly change
  bcb_series(7447, "igp_10"),      # IGP-10 monthly change
  bcb_series(192,  "incc"),        # INCC construction cost index
  bcb_series(191,  "ipc_br"),      # IPC-BR (FGV consumer prices)
  bcb_series(193,  "ipc_fipe"),    # IPC-FIPE (Sao Paulo consumer prices)
  bcb_series(225,  "ipa_di"),      # IPA-DI wholesale prices - general
  bcb_series(7459, "ipa_ind"),     # IPA-DI wholesale - industrial
  bcb_series(7460, "ipa_agro"),    # IPA-DI wholesale - agricultural
  bcb_series(7450, "ipa_m")        # IPA-M wholesale prices
))

# =============================================================
# 1.3 - Commodity prices in BRL — IC-Br (BCB)
# =============================================================
cat("Fetching IC-Br commodity indexes...\n")

df_icbr <- merge_bcb(list(
  bcb_series(27574, "icbr_total"),   # IC-Br composite
  bcb_series(27575, "icbr_agro"),    # IC-Br agriculture
  bcb_series(27576, "icbr_metal"),   # IC-Br metals
  bcb_series(27577, "icbr_energy")   # IC-Br energy
))

# =============================================================
# 1.4 - Interest rates (BCB + FRED)
# =============================================================
cat("Fetching interest rates...\n")

df_rates_bcb <- merge_bcb(list(
  bcb_series(4390, "selic"),          # Selic accumulated in month (% p.m.)
  bcb_series(256,  "tjlp"),           # Long-term interest rate
  bcb_series(7828, "savings_yield")   # Savings deposits monthly yield
))

df_rates_fred <- {
  lst <- compact(list(
    fred_series("TB3MS",    "us_treas_3m"),   # US Treasury 3-month
    fred_series("GS2",      "us_treas_2y"),   # US Treasury 2-year
    fred_series("GS10",     "us_treas_10y"),  # US Treasury 10-year
    fred_series("DFII5",    "us_tips_5y"),    # US TIPS 5-year
    fred_series("BAA",      "us_corp_baa"),   # US corporate bonds BAA
    fred_series("FEDFUNDS", "fed_funds"),     # Fed Funds effective rate
    fred_series("T10YIE",   "us_breakeven")   # US 10y breakeven inflation
  ))
  if (length(lst) > 0) reduce(lst, full_join, by = "date")
  else data.frame(date = seq(as.Date(START), as.Date(END), by = "month"))
}

# =============================================================
# 1.5 - Money aggregates (Ipeadata + BCB)
# =============================================================
cat("Fetching money aggregates...\n")

df_money_ipea <- ipea_block(c(
  "BM12_M1N12",   # M1
  "BM12_M2NCN12", # M2
  "BM12_M3NCN12", # M3
  "BM12_M4NCN12"  # M4
)) %>%
  rename(m1 = BM12_M1N12, m2 = BM12_M2NCN12,
         m3 = BM12_M3NCN12, m4 = BM12_M4NCN12)

df_money_bcb <- merge_bcb(list(
  bcb_series(27838, "mon_agg1"),           # Monetary aggregate (% change)
  bcb_series(28750, "mon_agg2"),
  bcb_series(28751, "mon_agg3"),
  bcb_series(1788,  "monetary_base"),       # Monetary base
  bcb_series(1833,  "monetary_base_broad"), # Broad monetary base
  bcb_series(1786,  "currency_circ"),       # Currency in circulation
  bcb_series(1835,  "savings_deposits")     # Savings deposits stock
))

# =============================================================
# 1.6 - Credit (BCB)
# =============================================================
cat("Fetching credit series...\n")

df_credit <- merge_bcb(list(
  bcb_series(1799, "credit_spread")   # Credit spread
))

# =============================================================
# 1.7 - FX and financial markets (BCB + Yahoo)
# =============================================================
cat("Fetching FX and financial markets...\n")

df_fx_bcb <- merge_bcb(list(
  bcb_series(3698,  "exchange_rate"),  # BRL/USD end of period
  bcb_series(11752, "reer")            # Real effective exchange rate (IPCA)
))

df_vix   <- yahoo_to_monthly("^VIX")     %>% rename(vix      = value)
df_dxy   <- yahoo_to_monthly("DX-Y.NYB") %>% rename(dxy      = value)
df_ibov  <- yahoo_to_monthly("^BVSP")    %>% rename(ibovespa = value)
df_sp500 <- yahoo_to_monthly("^GSPC")    %>% rename(sp500    = value)

# =============================================================
# 1.8 - Economic activity (BCB + Ipeadata/CNI)
# =============================================================
cat("Fetching economic activity...\n")

df_activity_bcb <- merge_bcb(list(
  bcb_series(24363, "ibc_br"),      # IBC-Br activity index (starts 2003)
  bcb_series(1396,  "uci"),         # Capacity utilization
  bcb_series(4393,  "cons_conf")    # Consumer confidence
))

# CNI industrial indicators via Ipeadata (all start 1992)
df_cni <- ipea_block(c(
  "CNI12_HTRABD12",  # Hours worked in production - seasonally adjusted
  "CNI12_PEEMPD12",  # Industrial employment - seasonally adjusted
  "CNI12_VENRED12"   # Real industrial revenue - seasonally adjusted
)) %>%
  rename(cni_hours      = CNI12_HTRABD12,
         cni_employment = CNI12_PEEMPD12,
         cni_revenue    = CNI12_VENRED12)

# =============================================================
# 1.9 - Industrial production (Ipeadata PIM-PF + BCB)
# =============================================================
cat("Fetching industrial production...\n")

df_industry <- ipea_block(c(
  "PIMPFN12_QIIGNN12",    # General industry
  "PIMPFN12_QIITNN12",    # Manufacturing
  "PIMPFN12_QIEMNNAS12",  # Mining - seasonally adjusted
  "PIMPFN12_QIBKNN12",    # Capital goods
  "PIMPFN12_QIBCDNN12",   # Durable consumer goods
  "PIMPFN12_QIBCTNNAS12", # Consumer goods - seasonally adjusted
  "PIMPFN12_QIBINNAS12"   # Intermediate goods - seasonally adjusted
)) %>%
  rename(ind_general  = PIMPFN12_QIIGNN12,
         ind_manuf    = PIMPFN12_QIITNN12,
         ind_mining   = PIMPFN12_QIEMNNAS12,
         ind_capital  = PIMPFN12_QIBKNN12,
         ind_durable  = PIMPFN12_QIBCDNN12,
         ind_consumer = PIMPFN12_QIBCTNNAS12,
         ind_interm   = PIMPFN12_QIBINNAS12)

# Vehicle and steel production (BCB)
df_vehicles <- merge_bcb(list(
  bcb_series(1373, "vehicles_prod"),      # Total vehicles production
  bcb_series(1374, "cars_prod"),          # Cars production
  bcb_series(1375, "trucks_prod"),        # Trucks production
  bcb_series(1376, "buses_prod"),         # Buses production
  bcb_series(1378, "vehicle_sales"),      # Vehicle licensing - total
  bcb_series(1379, "vehicle_sales_dom"),  # Vehicle licensing - domestic
  bcb_series(7357, "steel_prod")          # Crude steel production
))

# =============================================================
# 1.10 - Retail sales and energy (Ipeadata)
# =============================================================
cat("Fetching retail and energy...\n")

df_retail_energy <- ipea_block(c(
  "PMC12_IVVRN12",      # Real retail sales
  "PMC12_IVVRNAMP12",   # Real retail sales extended
  "ELETRO12_CEETCOM12", # Electricity - commercial
  "ELETRO12_CEETIND12", # Electricity - industrial
  "ELETRO12_CEETRES12", # Electricity - residential
  "ELETRO12_CEETT12"    # Electricity - total
)) %>%
  rename(retail_real     = PMC12_IVVRN12,
         retail_ext      = PMC12_IVVRNAMP12,
         electricity_com = ELETRO12_CEETCOM12,
         electricity_ind = ELETRO12_CEETIND12,
         electricity_res = ELETRO12_CEETRES12,
         electricity_tot = ELETRO12_CEETT12)

# =============================================================
# 1.11 - External sector (Ipeadata + BCB)
# =============================================================
cat("Fetching external sector...\n")

df_external_ipea <- ipea_block(c(
  "SECEX12_MVTOT12",       # Total imports
  "SECEX12_XVTOT12",       # Total exports
  "SECEX12_MBENCAPGCE12",  # Imports - capital goods
  "SECEX12_MBENCONGCE12",  # Imports - consumer goods
  "SECEX12_MINTGCE12",     # Imports - intermediate goods
  "SECEX12_XBENCAPGCE12",  # Exports - capital goods
  "SECEX12_XBENCONGCE12",  # Exports - consumer goods
  "SECEX12_XINTGCE12",     # Exports - intermediate goods
  "FUNCEX12_XPT12",        # Export price index (Funcex)
  "FUNCEX12_XQT12"         # Export quantum index (Funcex)
)) %>%
  rename(imp_total   = SECEX12_MVTOT12,
         exp_total   = SECEX12_XVTOT12,
         imp_cap     = SECEX12_MBENCAPGCE12,
         imp_cons    = SECEX12_MBENCONGCE12,
         imp_inter   = SECEX12_MINTGCE12,
         exp_cap     = SECEX12_XBENCAPGCE12,
         exp_cons    = SECEX12_XBENCONGCE12,
         exp_inter   = SECEX12_XINTGCE12,
         exp_price   = FUNCEX12_XPT12,
         exp_quantum = FUNCEX12_XQT12)

df_external_bcb <- merge_bcb(list(
  bcb_series(11753, "int_reserves"),     # International reserves
  bcb_series(13067, "fdi"),              # Foreign direct investment
  bcb_series(22701, "current_account")   # Current account balance
))

# =============================================================
# 1.12 - Public sector (BCB)
# =============================================================
cat("Fetching public sector...\n")

df_fiscal <- merge_bcb(list(
  bcb_series(7384, "primary_result_pct"),  # Primary result % GDP - monthly
  bcb_series(7385, "primary_result"),      # Primary result - monthly flow
  bcb_series(4503, "net_debt_gdp"),        # Net public debt % GDP
  bcb_series(4382, "gdp_12m")              # Nominal GDP accumulated 12m
))

# =============================================================
# 1.13 - Commodities (FRED + Ipeadata + NOAA)
# =============================================================
cat("Fetching commodities...\n")

df_comm_fred <- {
  lst <- compact(list(
    fred_series("DCOILBRENTEU",  "oil_brent"),    # Brent oil
    fred_series("DCOILWTICO",    "oil_wti"),      # WTI oil
    fred_series("PFOODINDEXM",   "glob_food"),    # IMF global food index
    fred_series("PALLFNFINDEXM", "glob_comm")     # IMF global commodities
  ))
  if (length(lst) > 0) reduce(lst, full_join, by = "date")
  else data.frame(date = seq(as.Date(START), as.Date(END), by = "month"))
}

df_comm_ipea <- ipea_block(c(
  "IFS12_BEEFB12",     # Beef international price
  "IFS12_SOJAGP12",    # Soybean international price
  "IFS12_PETROLEUM12", # Oil international price (IMF)
  "IFS12_MAIZE12"      # Corn international price
)) %>%
  rename(beef     = IFS12_BEEFB12,
         soybean  = IFS12_SOJAGP12,
         oil_ipea = IFS12_PETROLEUM12,
         corn     = IFS12_MAIZE12)

# ONI - Oceanic Nino Index (NOAA) — El Nino affects Brazilian food prices
cat("Fetching ONI (NOAA)...\n")
df_oni <- tryCatch({
  oni_url <- "https://psl.noaa.gov/data/correlation/oni.data"
  raw <- readLines(oni_url)
  hdr <- as.numeric(strsplit(trimws(raw[1]), "\\s+")[[1]])
  yr_start <- hdr[1]; yr_end <- hdr[2]
  n_years  <- yr_end - yr_start + 1
  vals <- lapply(raw[2:(1 + n_years)], function(l) {
    as.numeric(strsplit(trimws(l), "\\s+")[[1]])
  })
  tab <- do.call(rbind, vals)
  df  <- expand.grid(month = 1:12, year = tab[, 1]) %>%
    arrange(year, month) %>%
    mutate(oni  = as.vector(t(tab[, 2:13])),
           date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    filter(oni > -90,
           date >= as.Date(START), date <= as.Date(END)) %>%
    select(date, oni)
  cat("ONI loaded:", nrow(df), "rows\n")
  df
}, error = function(e) {
  cat("ONI → ERROR:", e$message, "\n")
  data.frame(date = seq(as.Date(START), as.Date(END), by = "month"))
})

# =============================================================
# 1.14 - Inflation expectations (BCB SGS + Focus via Olinda API)
# =============================================================
cat("Fetching inflation expectations...\n")

# Realized 12-month accumulated IPCA — regime indicator for S_t
df_ipca12m <- merge_bcb(list(
  bcb_series(13522, "ipca_acc_12m")
))

# Focus median expectation - IPCA 12 months ahead (smoothed)
df_focus <- tryCatch({
  exp_raw <- rbcb::get_market_expectations(
    type = "inflation-12-months",
    start_date = START
  )
  exp_raw %>%
    filter(Indicador == "IPCA", Suavizada == "S") %>%
    mutate(date = as.Date(paste0(format(Data, "%Y-%m"), "-01"))) %>%
    group_by(date) %>%
    summarise(ipca_exp_12m = mean(Mediana, na.rm = TRUE)) %>%
    filter(date >= as.Date(START), date <= as.Date(END))
}, error = function(e) {
  cat("Focus expectations → ERROR:", e$message, "\n")
  data.frame(date = seq(as.Date(START), as.Date(END), by = "month"))
})

# =============================================================
# 1.15 - Economic Policy Uncertainty (EPU)
# =============================================================
cat("Fetching EPU...\n")

df_epu <- tryCatch({
  epu_url <- "https://www.policyuncertainty.com/media/All_Country_Data.xlsx"
  temp    <- tempfile(fileext = ".xlsx")
  download.file(epu_url, temp, mode = "wb", quiet = TRUE)
  epu_raw <- read_excel(temp)
  
  epu_raw %>%
    mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    filter(date >= as.Date(START), date <= as.Date(END)) %>%
    select(date, Brazil, Canada, Chile, China, France,
           Germany, Greece, India, Ireland, Italy,
           Japan, Korea, Russia, Spain, Singapore,
           UK, US, Australia, GEPU_current) %>%
    rename_with(~ paste0("epu_", tolower(.x)), -date) %>%
    rename(epu_global = epu_gepu_current)
}, error = function(e) {
  cat("EPU → ERROR:", e$message, "\n")
  data.frame(date = seq(as.Date(START), as.Date(END), by = "month"))
})

# =============================================================
# 1.16 - Labor (Ipeadata)
# =============================================================
cat("Fetching labor series...\n")

df_labor <- ipea_block(c(
  "GAC12_SALMINRE12",  # Real minimum wage
  "MTE12_SALMIN12"     # Nominal minimum wage
)) %>%
  rename(min_wage_real = GAC12_SALMINRE12,
         min_wage_nom  = MTE12_SALMIN12)

# =============================================================
# 1.17 - Merge all datasets
# =============================================================
cat("\nMerging all datasets...\n")

lista_dfs <- list(
  df_inflation_bcb, df_prices_bcb, df_icbr,
  df_rates_bcb, df_rates_fred,
  df_money_ipea, df_money_bcb, df_credit,
  df_fx_bcb, df_vix, df_dxy, df_ibov, df_sp500,
  df_activity_bcb, df_cni,
  df_industry, df_vehicles,
  df_retail_energy,
  df_external_ipea, df_external_bcb,
  df_fiscal,
  df_comm_fred, df_comm_ipea, df_oni,
  df_ipca12m, df_focus,
  df_epu, df_labor
)

df_complete <- lista_dfs %>%
  reduce(full_join, by = "date")

# =============================================================
# 1.18 - Filter period and drop NA columns
# =============================================================
df_complete <- df_complete %>%
  filter(date >= as.Date(START) & date <= as.Date(END)) %>%
  arrange(date)

# Remove all columns with any NA — keeps full 2003-2025 coverage only
cols_with_na <- names(df_complete)[colSums(is.na(df_complete)) > 0]
if (length(cols_with_na) > 0) {
  cat("\nDropped for NAs:\n")
  print(cols_with_na)
}
df_clean <- df_complete %>% select(-all_of(cols_with_na))

# =============================================================
# 1.19 - Diagnostics
# =============================================================
cat("\n=== Dataset Diagnostics ===\n")
cat("Dimensions:", dim(df_clean), "\n")
cat("Period:", as.character(min(df_clean$date)),
    "to", as.character(max(df_clean$date)), "\n")
cat("Duplicate dates:", sum(duplicated(df_clean$date)), "\n")
cat("NAs:", sum(is.na(df_clean)), "\n")
cat("Variables kept:", ncol(df_clean) - 1, "\n")

# =============================================================
# 1.20 - Save
# =============================================================
save(df_clean, file = "data/01_data.rda")
cat("\nSaved: data/01_data.rda\n") 
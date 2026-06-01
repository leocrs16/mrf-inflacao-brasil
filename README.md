# MRF - Brazilian Inflation Forecasting

Application of the Macroeconomic Random Forest (MRF) algorithm to forecast
Brazilian inflation (IPCA) — SINAPE 2026.

## Authors

- Leonardo Carvalho Ribeiro da Silva (UFRGS)
- Hudson da Silva Torrent (UFRGS)

## Overview

This paper applies the Macroeconomic Random Forest (MRF) developed by
Goulet Coulombe (2024) to forecast Brazilian inflation (IPCA) from
January 1996 to December 2025. The MRF captures nonlinearities and
structural breaks common in emerging market economies through generalized
time-varying parameters (GTVPs). Forecasting accuracy is evaluated
out-of-sample against an ARIMA benchmark using RMSE and MAE across
multiple horizons (1, 3, 6, and 12 months ahead).

## Dataset

- **54 macroeconomic variables** covering inflation, interest rates,
  money aggregates, credit, FX, activity, labor, industry, trade,
  commodities, fiscal sector, and global uncertainty (EPU)
- **Sources**: BCB, Ipeadata, FRED, Yahoo Finance, EPU
- **Period**: January 1996 – December 2025 (360 observations)

## Repository Structure

- `scripts/` — R scripts (data collection, transformations, models)
- `functions/` — MRF auxiliary functions
- `data/` — processed datasets
- `output/` — charts and results tables

## Models

- **Benchmark**: ARIMA (expanding window, univariate)
- **Main model**: Macroeconomic Random Forest — FA-ARRF specification

## Requirements

```r
install.packages(c("ipeadatar", "rbcb", "fredr", "quantmod",
                   "forecast", "urca", "dplyr", "tidyr"))
devtools::install_github("philgoucou/macrorf")
```

## Reference

Goulet Coulombe, P. (2024). The macroeconomy as a random forest.
*Journal of Applied Econometrics*, 39(3), 401–421.
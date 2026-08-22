# MRF - Brazilian Inflation Forecasting

Application of the Macroeconomic Random Forest (MRF) algorithm to forecast
Brazilian inflation (IPCA) — SINAPE 2026.

## Authors

- Leonardo Carvalho Ribeiro da Silva (UFRGS)
- Hudson da Silva Torrent (UFRGS)

## Overview

This paper applies the Macroeconomic Random Forest (MRF) developed by
Goulet Coulombe (2024) to forecast Brazilian inflation (IPCA) from
January 2003 to December 2025. The MRF captures nonlinearities and
structural breaks through generalized time-varying parameters (GTVPs).
Forecast accuracy is evaluated out-of-sample against an ARIMA benchmark
using RMSE and MAE at horizons of 1, 3, 6 and 12 months, with
Diebold-Mariano tests for statistical significance and CSFE plots to
track when the gains occur.

## Dataset

- **141 macroeconomic variables**, monthly, no missing values
- **Sources**: BCB (SGS and Olinda/Focus), Ipeadata, FRED, Yahoo Finance,
  NOAA, Economic Policy Uncertainty
- **Period**: January 2003 – December 2025 (276 observations)
- **Test window**: January 2016 – December 2025 (120 months)

## Repository Structure
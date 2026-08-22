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
- **Period**: January 2003 - December 2025 (276 observations)
- **Test window**: January 2016 - December 2025 (120 months)

## Repository Structure

**scripts/**

| File | Description |
|---|---|
| `01_data.R` | Collects all series and builds the panel |
| `02_transformations.R` | Stationarity transformations and ADF tests |
| `03_arima.R` | ARIMA benchmark, expanding window |
| `04_mrf.R` | MRF estimation, fixed and expanding windows |
| `05_results.R` | Model comparison, DM tests, CSFE, figures |

**functions/**

| File | Description |
|---|---|
| `build_data.R` | Builds the X_t and S_t matrices |
| `fixed_window.R` | Fixed window estimation |
| `expanding_window.R` | Expanding window estimation |

**data/** - intermediate `.rda` files

**output/** - result tables (`.csv`) and figures (`.png`)

Note: `data/04_models_*.rda` are not versioned due to file size (93 MB).
Run `scripts/04_mrf.R` to regenerate them. The forecast and result objects
required by `scripts/05_results.R` are included in the repository.

## Results

Out-of-sample RMSE, test period 2016-2025:

| Horizon | ARIMA | MRF fixed | MRF expanding |
|---|---|---|---|
| 1 month | 0.3481 | 0.3399 | 0.3313 |
| 3 months | 0.3932 | 0.3941 | 0.3800 |
| 6 months | 0.4003 | 0.4283 | 0.3822 |
| 12 months | 0.4100 | 0.4256 | 0.4038 |

The expanding window MRF outperforms the ARIMA benchmark at all horizons.
Diebold-Mariano tests do not reject equal predictive accuracy at conventional
levels, which is consistent with low test power in a 120-observation sample.
The CSFE plots show that at h = 6 the expanding window gains accumulate
steadily over the whole decade rather than concentrating in a few episodes.

## Requirements

    install.packages(c("dplyr", "tidyr", "lubridate", "purrr",
                       "ipeadatar", "rbcb", "fredr", "quantmod",
                       "readxl", "forecast", "urca", "ggplot2"))
    devtools::install_github("philgoucou/macrorf")

A FRED API key is required. Store it in `.Renviron` as `FRED_API_KEY`.

## References

Araujo, G. S. & Gaglianone, W. P. (2023). Machine learning methods for
inflation forecasting in Brazil: New contenders versus classical models.
*Latin American Journal of Central Banking*.

Diebold, F. X. & Mariano, R. S. (1995). Comparing predictive accuracy.
*Journal of Business and Economic Statistics*, 13(3), 253-263.

Goulet Coulombe, P. (2024). The macroeconomy as a random forest.
*Journal of Applied Econometrics*, 39(3), 401-421.

Stock, J. H. & Watson, M. W. (2002). Forecasting using principal components
from a large number of predictors. *Journal of the American Statistical
Association*, 97(460), 1167-1179.

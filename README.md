# Turkey Marriage Forecasting — TÜİK Time Series Analysis

This project uses monthly marriage statistics from TÜİK (Turkish Statistical Institute) to build time series forecasting models that predict the number of marriages in Turkey for the next period.

## Data

- **Source:** TÜİK — Marriages by Province and Month (*İl ve aya göre evlenmeler*)
- **Coverage:** January 2001 – December 2025
- **Frequency:** Monthly
- **Variable:** Total number of marriages in Turkey (aggregated from provincial data)

## Models Used

| Model | Description |
|-------|-------------|
| **ARIMA** | Auto-selected via `auto.arima()` using AICc criterion. Best performing model: ARIMA(2,0,1)(0,1,1)[12] |
| **ETS** | Exponential smoothing with automatic error/trend/seasonality selection |
| **Holt-Winters** | Additive seasonal decomposition model |
| **Ensemble** | Weighted average of all three models, weighted inversely by RMSE |

## Results

- **Best model:** ARIMA (RMSE: 3,264)
- **Forecast target:** January 2026
- **Ensemble forecast:** ~24,472 marriages

## Project Structure

```
├── 00_calistir.R          # Main runner — executes all scripts in order
├── 01_data_prep.R         # Data loading, cleaning, EDA, stationarity tests
├── 02_arima_model.R       # ARIMA model, diagnostics, forecast
├── 03_ets_model.R         # ETS & Holt-Winters models, STL decomposition
├── 04_compare_forecast.R  # Model comparison, ensemble, final forecast
└── İl ve aya göre evlenmeler.xls  # Raw data from TÜİK
```

## How to Run

1. Place all `.R` files and the `.xls` file in the same folder
2. Open R Console and set working directory to that folder
3. Run:

```r
options(repos = c(CRAN = "https://cran.r-project.org"))
source("00_calistir.R")
```

## Requirements

Packages are installed automatically on first run: `readxl`, `forecast`, `tseries`, `ggplot2`, `dplyr`, `tidyr`, `lubridate`, `scales`

## Author

Alara Kuru — 138723516

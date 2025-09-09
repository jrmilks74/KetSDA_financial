# Kettering Adventist Church Financial Overview App

A Shiny web application for visualizing and forecasting the financial health of  
Kettering Adventist Church (Kettering, Ohio, USA).  

The app provides up-to-date **month-to-date (MTD)**, **year-to-date (YTD)**,  
and forecasted financial summaries, along with visualizations of long-term  
trends and inflation-adjusted values. It integrates directly with the church’s  
Google Sheets and public CPI data from the Federal Reserve (FRED).

---

## Features

### 1. **Financial Overview**
- MTD summary vs same month last year  
- YTD summary vs last year through the same month  
- Styled tables using **kableExtra**  

### 2. **In-Depth Analysis**
- Interactive category selection (`Tithe`, `Church Budget Income`, `Church Budget Expenses`)  
- Tables:
  - Current + predicted totals for the year
  - Forecast with 80% and 95% confidence intervals  
- Plots:
  - Monthly time series with LOESS smoothing  
  - Annual change (current vs inflation-adjusted)  
  - Seasonal patterns (current and real dollars)  
  - Forecast vs Actual for the current year  
  - 12-month ARIMA forecast  

### 3. **Ministry Current Funds**
- Snapshot of current balances by ministry  
- Automatically formatted as U.S. dollars  

---

## Data Sources

- **Google Sheets**:
  - [Financials](https://docs.google.com/spreadsheets/d/1DPq4lM36_CcjEGN2v8UE1w1pr-t0C8lenYx56jniNRo)  
  - [Ministry Funds](https://docs.google.com/spreadsheets/d/1d2g7NRipbarvq78LUxuEObWsTru4t4w_TY9uQgQOvZQ)  
- **CPI Data**: Midwest CPI-U (CUUR0200SA0) via [FRED](https://fred.stlouisfed.org/)

---

## Requirements

- R (≥ 4.2 recommended)  
- R packages:
  - `shiny`
  - `googlesheets4`
  - `tidyverse`
  - `plotly`
  - `scales`
  - `fpp3` (includes `tsibble`, `fable`, `fabletools`)
  - `urca`
  - `kableExtra`
  - `rvest`
  - `lubridate`
  - `htmltools`

---

## Authentication

This app uses **Google Sheets API** via `googlesheets4`.

1. On first run, you may be prompted to authenticate with Google.  
2. Credentials are cached in `auth/.secrets/` (already configured in `server.R`).  
3. The app is configured to authenticate using `office@ketsda.org`.  
   - If you need a different account, update the `gs4_auth(email = "...")` line.

---

## Running the App

Clone or download this repository, then from R:

```r
library(shiny)
runApp("path/to/app/folder")

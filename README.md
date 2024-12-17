# Crime Rate Time Series Forecasting in Chicago

(Done in a university setting as final exam of a Predictive Analysis class).

## Overview
This project applies **time series analysis** to forecast crime rates in the city of **Chicago** using data from the Chicago Police Department's **CLEAR system**. The analysis focuses on identifying trends, seasonal patterns, and structural breaks in the historical crime data from **2003 to the present**.

The project implements two forecasting models:
- **SARIMA (Seasonal ARIMA):** Captures seasonality and autocorrelations in crime counts.
- **Holt-Winters Model:** Implements both additive and multiplicative seasonality for exponential smoothing.

---
## Key Features
### 1. Exploratory Data Analysis (EDA)
- Visualizations of crime trends over time.
- Seasonality and subseries decomposition.
- Box-Cox transformations for variance stabilization.

### 2. Stationarity Tests
- **KPSS** and **Augmented Dickey-Fuller (ADF)** tests to ensure stationarity.
- Seasonal differencing applied to stabilize data.

### 3. Structural Break Analysis
- Detection of structural breaks using **QLR** and **SIS** tests.
- **CUSUM** and **MOSUM** tests for change point detection.

### 4. Model Fitting and Comparison
- **SARIMA models** (e.g., ARIMA(1,1,1)(2,1,1)[12]).
- **Holt-Winters additive and multiplicative** models.
- Model diagnostics:
  - Residuals analysis.
  - Shapiro-Wilk test for normality.
  - Ljung-Box test for autocorrelations.
- Accuracy comparison using metrics:
  - **AIC, BIC, RMSE.**

### 5. Forecasting
- Crime count forecasts for the **next 2 to 5 years**.
- Visual comparison of actual data vs. predicted values.

---
## Libraries Used
- **dplyr** and **lubridate** for data manipulation.
- **ggplot2** and **gridExtra** for visualization.
- **fpp3, forecast, strucchange,** and **urca** for time series modeling.
- **HoltWinters** for exponential smoothing.

---
## How to Use
### Step 1: Install Necessary R Libraries
Run the following code to install required libraries:
```r
install.packages(c("dplyr", "lubridate", "ggplot2", "fpp3", "gridExtra", "urca", "strucchange", "forecast"))
```

### Step 2: Load and Prepare the Dataset
Place the crime dataset in the appropriate directory and replace the file path:
```r
total_crime <- read.csv("insert_position_of_total_crime.csv")
```

### Step 3: Run the Script
Execute the script step-by-step to:
1. Visualize and preprocess the data.
2. Perform stationarity tests and structural break analysis.
3. Fit SARIMA and Holt-Winters models.
4. Forecast future crime rates and compare model performance.

---
## Results
- **SARIMA(1,1,1)(2,1,1)[12]** demonstrates the best performance based on **AIC/BIC** and accuracy metrics.
- **Holt-Winters Multiplicative** also provides reliable results for seasonal forecasts.
- Forecast comparison plots highlight predictions for the **next 2 to 5 years**, aiding decision-makers in crime prevention resource allocation.

---
## Dependencies
- **R version ≥ 4.0.0**
- Libraries:
  - dplyr
  - lubridate
  - ggplot2
  - fpp3
  - urca
  - strucchange
  - forecast

---
## Acknowledgments
- **Data source:** Chicago Police Department **CLEAR System.**
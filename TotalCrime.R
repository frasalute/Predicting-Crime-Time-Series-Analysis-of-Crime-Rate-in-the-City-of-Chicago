library(dplyr)
library(lubridate)
library(ggplot2)
library(fpp3)
library(gridExtra)
library(urca)
library(strucchange)
library(forecast)

total_crime <- read.csv("insert_position_of_total_crime.csv")

#Quick analysis of the data selected for analysis
head(total_crime)
names(total_crime)
num_rows_crime <- nrow(total_crime)
print(num_rows_crime)
total_crime <- total_crime %>%
  mutate(YearMonth = as.Date(YearMonth))
str(total_crime)
summary (total_crime)

ggplot(total_crime, aes(x = YearMonth, y = Total_Crime_Count, group = 1)) +
  geom_line() +
  labs(title = "Total Crime Counts in Chicago through the Years",
       x = "Date",
       y = "Total Crime Count") +
  theme_minimal()

# Another type of plot based on colors

total_crime <- total_crime %>%
  mutate(Year = as.factor(year(YearMonth)))

# Create the plot with different colors for points based on the Year
p <- ggplot(total_crime, aes(x = YearMonth, y = Total_Crime_Count, group = 1, color = Year)) +
  geom_line() +
  geom_point() +  # Adding markers with different colors
  labs(title = "Total Monthly Crime Counts in Chicago through the Years",
       x = "Month-Year",
       y = "Total Crime Count") +
  theme_minimal() +
  scale_color_manual(values = rainbow(length(unique(total_crime$Year))))  # Customize the color scale if needed

# Print the plot with specific dimensions
grid.arrange(p, top = "Total Monthly Crime Counts", ncol = 1, nrow = 1,
             widths = unit(14, "inches"), heights = unit(8, "inches"))

# The decision is to drop data from 2001/2002 which is quite low because the system was still being set up
# Also we drop crime from 2024 because the year has ended yet 

total_crime <- total_crime %>%
  filter(year(YearMonth) != 2001 & year(YearMonth) != 2002 & year(YearMonth) != 2024)

ggplot(total_crime, aes(x = YearMonth, y = Total_Crime_Count, group = 1)) +
  geom_line() +
  labs(title = "Total Crime Counts in Chicago through the Years",
       x = "Date",
       y = "Total Crime Count") +
  theme_minimal()

p <- ggplot(total_crime, aes(x = YearMonth, y = Total_Crime_Count, group = 1, color = Year)) +
  geom_line() +
  geom_point() +  # Adding markers with different colors
  labs(title = "Total Monthly Crime Counts in Chicago through the Years",
       x = "Month-Year",
       y = "Total Crime Count") +
  theme_minimal() +
  scale_color_manual(values = rainbow(length(unique(total_crime$Year))))  

grid.arrange(p, top = "Total Monthly Crime Counts", ncol = 1, nrow = 1,
             widths = unit(14, "inches"), heights = unit(8, "inches"))

---------------------------------------------------------------------------

# TIME SERIES ANALYSIS

total_crime <- total_crime %>%
  mutate(YearMonth = yearmonth(YearMonth)) # To have the program run the seasonality without gaps

# Convert total_crime dataframe to a tsibble
total_crime_tsibble <- total_crime %>%
  as_tsibble(index = YearMonth)

# Identify and print the gaps
gaps <- total_crime_tsibble %>%
  scan_gaps()

print(gaps)
# no gaps so we proceed

str(total_crime_tsibble)

-------------------------------------------------------------------------------------------------------------------

# BOX - COX 
lambda <- BoxCox.lambda(total_crime$Total_Crime_Count)
print(lambda)

# Apply the Box-Cox transformation with the found lambda
total_crime_tsibble <- total_crime_tsibble %>%
  mutate(Total_Crime_Count_Transformed = box_cox(Total_Crime_Count, lambda))

str(total_crime_tsibble)

ggplot(total_crime_tsibble, aes(x = YearMonth, y = Total_Crime_Count_Transformed)) +
  geom_line() +
  labs(title = "Box-Cox Transformed Total Crime Counts in Chicago through the Years",
       x = "Date",
       y = "Transformed Total Crime Count") +
  theme_minimal()
--------------------------------------------------------------------- 

# SEASONALITY

total_crime_tsibble %>%
  gg_season(Total_Crime_Count) +
  labs(title = "Seasonal Decomposition of Total Crime Counts",
       x = "Month",
       y = "Total Crime Count") +
  theme_minimal()

# Extract the year for labeling
total_crime_graph <- total_crime_tsibble %>%
  mutate(Year = year(YearMonth))

# Extract the data for the labels at the end of each year
label_data <- total_crime_graph %>%
  group_by(Year) %>%
  filter(YearMonth == max(YearMonth)) %>%
  ungroup() %>%
  select(YearMonth, Total_Crime_Count, Year)

# Plot the seasonal decomposition
ggplot(total_crime_graph, aes(x = month(YearMonth, label = TRUE), y = Total_Crime_Count, group = Year, color = as.factor(Year))) +
  geom_line() +
  geom_text(data = label_data, aes(x = month(YearMonth, label = TRUE), y = Total_Crime_Count, label = Year), hjust = -0.2, size = 3) +
  labs(title = "Seasonal Decomposition of Total Crime Counts",
       x = "Month",
       y = "Total Crime Count") +
  theme_minimal() +
  theme(legend.position = "none")

# SUBSERIES

total_crime_tsibble %>%
  gg_subseries(Total_Crime_Count)

# LAG 

total_crime_tsibble %>%
  gg_lag(Total_Crime_Count)

# ACF

total_crime_tsibble %>%
  ACF(Total_Crime_Count) %>%
  autoplot() +
  labs(title = "Autocorrelation Function of Total Crime Counts",
       x = "Lag",
       y = "ACF")

# PACF 

total_crime_tsibble %>%
  PACF(Total_Crime_Count) %>%
  autoplot() +
  labs(title = "Partial Autocorrelation Function of Total Crime Counts",
       x = "Lag",
       y = "PACF")

# DECOMPOSITION

total_crime_decomposed <- total_crime_tsibble %>%
  model(STL(Total_Crime_Count ~ trend(window = 12) + season(window = "periodic"), robust = TRUE)) %>%
  components()

autoplot(total_crime_decomposed) +
  labs(title = "STL Decomposition of Total Crime Counts",
       x = "YearMonth",
       y = "Crime Count")

# There is seasonality so either Arima with Seasonality or SARIMA

-------------------------------------------------------------------

# UNIT ROOT ANALYSIS 

# KPSS and ADF tests

# Extract the time series
total_crime_ts <- total_crime_tsibble %>%
  pull(Total_Crime_Count)

kpss_test <- ur.kpss(total_crime_ts, type = "mu")
summary(kpss_test)

kpss_test <- ur.kpss(total_crime_ts, type = "tau")
summary(kpss_test)

adf_test_drift <- ur.df(total_crime_ts, type = "drift", selectlags = "AIC")
summary(adf_test_drift)

adf_test_trend <- ur.df(total_crime_ts, type = "trend", selectlags = "AIC")
summary(adf_test_trend)

# non stationarity

# multiple lags

max_lags <- 13

adf_test_drift <- ur.df(total_crime_ts, type = "drift", lags = max_lags)
summary(adf_test_drift)

adf_test_trend <- ur.df(total_crime_ts, type = "trend", lags = max_lags)
summary(adf_test_trend)
----------------------------------------------------------------------------------------

# DETRENDING BY DIFFERENTIATION

# Perform differencing
total_crime_vector <- total_crime %>%
  pull(Total_Crime_Count)

total_crime_diff <- diff(total_crime_vector, lag = 12)

time_index <- total_crime %>%
  filter(row_number() > 12) %>%
  pull(YearMonth)

# Create a new tsibble with the differenced data
total_crime_diff_tsibble <- tsibble(
  YearMonth = yearmonth(time_index),
  Total_Crime_Count = as.numeric(total_crime_diff),
  index = YearMonth
)

# Plot the differenced data
ggplot(total_crime_diff_tsibble, aes(x = YearMonth, y = Total_Crime_Count)) +
  geom_line(color = "black") +
  geom_point(color = "red", size = 0.5) +
  labs(title = "loooooool",
       x = "Date",
       y = "Seasonally Differenced Total Crime Count") +
  theme_minimal()

# Perform KPSS test and ADF test on the differenced series

total_crime_diff<- ts(total_crime_diff_tsibble$Total_Crime_Count, 
                      frequency = 12, 
                      start = c(year(min(total_crime_diff_tsibble$YearMonth)), 
                                month(min(total_crime_diff_tsibble$YearMonth))))

kpss_test_diff <- ur.kpss(total_crime_diff, type = "mu")
summary(kpss_test_diff)
  
kpss_test_diff <- ur.kpss(total_crime_diff, type = "tau")
summary(kpss_test_diff)

adf_test_drift_diff <- ur.df(total_crime_diff, type = "drift", selectlags = "AIC")
summary(adf_test_drift_diff)

adf_test_trend_diff <- ur.df(total_crime_diff, type = "trend", selectlags = "AIC")
summary(adf_test_trend_diff)

# still non stationarity

# multiple lags

max_lags <- 13

adf_test_drift_diff <- ur.df(total_crime_diff, type = "drift", lags = max_lags)
summary(adf_test_drift_diff)

adf_test_trend_diff <- ur.df(total_crime_diff, type = "trend", lags = max_lags)
summary(adf_test_trend_diff) 
  
# First differenced data 

total_crime_diff_diff <- diff(total_crime_diff, differences = 1)

# Create a new tsibble with the first differenced data
time_index_diff <- total_crime_diff_tsibble %>%
  filter(row_number() > 1) %>%
  pull(YearMonth)

total_crime_diff_diff_tsibble <- tsibble(
  YearMonth = yearmonth(time_index_diff),
  Total_Crime_Count = as.numeric(total_crime_diff_diff),
  index = YearMonth
)

# Plot the first differenced data
ggplot(total_crime_diff_diff_tsibble, aes(x = YearMonth, y = Total_Crime_Count)) +
  geom_line(color = "black") +
  geom_point(color = "red", size = 0.5) +
  labs(title = "Differenced Total Crime Counts in Chicago",
       x = "Date",
       y = "First Differenced Total Crime Count") +
  theme_minimal()

# Convert to ts object for unit root tests
total_crime_diff_diff_ts <- ts(total_crime_diff_diff_tsibble$Diff_Total_Crime_Count, 
                               frequency = 12, 
                               start = c(year(min(total_crime_diff_diff_tsibble$YearMonth)), 
                                         month(min(total_crime_diff_diff_tsibble$YearMonth))))

# Perform KPSS test on the first differenced series

kpss_test_mu_diff <- ur.kpss(total_crime_diff_diff_ts, type = "mu")
summary(kpss_test_mu_diff) 

kpss_test_diff <- ur.kpss(total_crime_diff_diff, type = "tau")
summary(kpss_test_diff)

adf_test_drift_diff <- ur.df(total_crime_diff_diff, type = "drift", selectlags = "AIC")
summary(adf_test_drift_diff)

adf_test_trend_diff <- ur.df(total_crime_diff_diff, type = "trend", selectlags = "AIC")
summary(adf_test_trend_diff)

# multiple lags

max_lags <- 13

adf_test_drift_diff <- ur.df(total_crime_diff_diff, type = "drift", lags = max_lags)
summary(adf_test_drift_diff)

adf_test_trend_diff <- ur.df(total_crime_diff_diff, type = "trend", lags = max_lags)
summary(adf_test_trend_diff) 

ndiffs(total_crime_vector)

# stationarity reached
------------------------------------------------------------------------ 
  
# CHECK FOR SEASONALITY AGAIN

# Plot the seasonal decomposition
total_crime_diff_diff_tsibble %>%
  gg_season(Total_Crime_Count) +
  labs(title = "Seasonal Decomposition of Total Crime Counts",
       x = "Month",
       y = "Total Crime Count") +
  theme_minimal()

# SUBSERIES

total_crime_diff_diff_tsibble %>%
  gg_subseries(Total_Crime_Count)

# LAG 

total_crime_diff_diff_tsibble %>%
  gg_lag(Total_Crime_Count)

# ACF

total_crime_diff_diff_tsibble %>%
  ACF(Total_Crime_Count) %>%
  autoplot() +
  labs(title = "Autocorrelation Function of Total Crime Counts",
       x = "Lag",
       y = "ACF")

# PACF 

total_crime_diff_diff_tsibble %>%
  PACF(Total_Crime_Count) %>%
  autoplot() +
  labs(title = "Partial Autocorrelation Function of Total Crime Counts",
       x = "Lag",
       y = "PACF")

# DECOMPOSITION

total_crime_diff_decomposed <- total_crime_diff_diff_tsibble %>%
  model(STL(Total_Crime_Count ~ trend(window = 12) + season(window = "periodic"), robust = TRUE)) %>%
  components()

autoplot(total_crime_diff_decomposed) +
  labs(title = "STL Decomposition of Total Crime Counts",
       x = "YearMonth",
       y = "Crime Count")

  
----------------------------------------------------------------------
  
# STRUCTURAL BREAKS 

total_crime_ts <- ts(total_crime_tsibble$Total_Crime_Count, frequency = 12, start = c(year(min(total_crime_tsibble$YearMonth)), month(min(total_crime_tsibble$YearMonth))))

# Perform the structural break test
breakpoints_result <- breakpoints(total_crime_ts ~ 1)

# Summary of breakpoints
summary(breakpoints_result)

# Extract F-statistics
fs <- Fstats(total_crime_ts ~ 1)

# Summary of F-statistics
summary(fs)

# Plot the F-statistics
plot(fs, main = "QLR Test for Structural Breaks")


---------------------------------------------------------
  # Add trend component
  total_crime_tsibble <- total_crime %>%
  mutate(YearMonth = yearmonth(YearMonth),
         trend = row_number()) %>%
  as_tsibble(index = YearMonth)
  
  
  
  
  # Convert total_crime dataframe to a ts object
  total_crime_ts <- ts(total_crime_tsibble$Total_Crime_Count, 
                       frequency = 12, 
                       start = c(year(min(total_crime_tsibble$YearMonth)), 
                                 month(min(total_crime_tsibble$YearMonth))))

# Perform the structural break test with trend
fs <- Fstats(total_crime_ts ~ total_crime_tsibble$trend)
summary(fs)

# Plot the F-statistics
plot(fs, main = "QLR Test for Structural Breaks")

  
# Add a trend term to the dataset
total_crime_tsibble <- total_crime_tsibble %>%
  mutate(trend = row_number())

# Convert to ts object
total_crime_ts <- ts(total_crime_tsibble$Total_Crime_Count, 
                     frequency = 12, 
                     start = c(year(min(total_crime_tsibble$YearMonth)), 
                               month(min(total_crime_tsibble$YearMonth))))

# Perform the structural break test with trend
breakpoints_result <- breakpoints(total_crime_ts ~ trend)
summary(breakpoints_result)

# Extract F-statistics with trend component
fs <- Fstats(total_crime_ts ~ trend)
summary(fs)

# Plot the F-statistics
plot(fs, main = "QLR Test for Structural Breaks")
  
  
  
  
  
  
  








time_index <- 1:length(total_crime_ts)

crime_data <- data.frame(Total_Crime_Count = as.numeric(total_crime_ts), trend = time_index)

# Perform the structural break test
breakpoints_result <- breakpoints(Total_Crime_Count ~ trend, data = crime_data)
summary(breakpoints_result)

# Extract F-statistics
fs <- Fstats(Total_Crime_Count ~ trend, data = crime_data)
summary(fs)

# Plot the F-statistics
plot(fs, main = "QLR Test for Structural Breaks")

break_dates <- breakdates(breakpoints_result)
print(break_dates)

# Convert break_dates to yearmon class
break_yearmonths <- as.yearmon(break_dates, "%Y-%m")

# Convert break_yearmonths to yearmonth class
break_yearmonths <- yearmonth(break_yearmonths)

# Add segments to the original tsibble for visualization
total_crime_tsibble <- total_crime_tsibble %>%
  mutate(segment = case_when(
    YearMonth < break_yearmonths[1] ~ 1,
    YearMonth >= break_yearmonths[1] & YearMonth < break_yearmonths[2] ~ 2,
    YearMonth >= break_yearmonths[2] & YearMonth < break_yearmonths[3] ~ 3,
    YearMonth >= break_yearmonths[3] ~ 4
  ))

# Plotting the segments
ggplot(total_crime_tsibble, aes(x = YearMonth, y = Total_Crime_Count, color = factor(segment))) +
  geom_line() +
  labs(title = "Total Crime Counts with Structural Breaks",
       x = "Date",
       y = "Total Crime Count",
       color = "Segment") +
  theme_minimal()
---------------------------------

# Mark segments based on breakpoints
break_dates <- breakdates(breakpoints_result)
print(break_dates)

break_yearmonths <- as.yearmon(break_dates, "%Y-%m")

# Convert break_yearmonths to yearmonth class
break_yearmonths <- yearmonth(break_yearmonths)

# Add segments to the original tsibble for visualization
total_crime_tsibble <- total_crime_tsibble %>%
  mutate(segment = case_when(
    YearMonth < break_yearmonths[1] ~ 1,
    YearMonth >= break_yearmonths[1] & YearMonth < break_yearmonths[2] ~ 2,
    YearMonth >= break_yearmonths[2] ~ 3
  ))

# Plotting the segments
ggplot(total_crime_tsibble, aes(x = YearMonth, y = Total_Crime_Count, color = factor(segment))) +
  geom_line() +
  labs(title = "Total Crime Counts with Structural Breaks",
       x = "Date",
       y = "Total Crime Count",
       color = "Segment") +
  theme_minimal()
  
# On differentiated data

# Perform the structural break test
breakpoints_result_diff <- breakpoints(total_crime_diff_diff ~ 1)
summary(breakpoints_result_diff)

fs_diff <- Fstats(total_crime_diff_diff ~ 1)
summary(fs_diff)

plot(fs_diff, main = "QLR Test for Structural Breaks on First Differenced Data")

break_dates_diff <- breakdates(breakpoints_result_diff)
print(break_dates_diff)


# SIS

sis_result_original <- breakpoints(total_crime_ts ~ 1)
summary(sis_result_original)

# Plot the SIS result for the original data
plot(sis_result_original, main = "SIS Test for Structural Breaks on Crime")

# Extract break dates for original data
break_dates_original <- breakdates(sis_result_original)
print(break_dates_original)


sis_result_diff <- breakpoints(total_crime_diff_diff ~ 1)
summary(sis_result_diff)

# Plot the SIS result for the doubly differenced data
plot(sis_result_diff, main = "SIS Test for Structural Breaks on Differenced Data")

# Extract break dates for differenced data
break_dates_diff <- breakdates(sis_result_diff)
print(break_dates_diff)


# Custom function to plot fitted values, residuals, and coefficients
plot_sis_results <- function(breakpoints_result, data, title) {
  fitted_values <- fitted(breakpoints_result)
  residuals <- residuals(breakpoints_result)
  coef_path <- coef(breakpoints_result)
  
  par(mfrow = c(3, 1))
  
  # Plot the fitted values
  plot(data, type = "l", main = paste(title, "- Fitted Values"), ylab = "Value", xlab = "Time")
  lines(fitted_values, col = "purple")
  legend("topleft", legend = c("Original", "Fitted"), col = c("black", "purple"), lty = 1)
  
  # Plot the residuals
  plot(residuals, type = "h", main = paste(title, "- Standardized Residuals"), ylab = "Residuals", xlab = "Time")
  abline(h = 0, col = "purple")
  
  # Plot the coefficient path
  plot(coef_path, type = "h", main = paste(title, "- Coefficient Path"), ylab = "Coefficient", xlab = "Time")
  abline(h = 0, col = "purple")
  
  par(mfrow = c(1, 1))
}

# Plot the results for the original data
plot_sis_results(sis_result_original, total_crime_ts, "SIS Test for Structural Breaks on Total Crime")

# Plot the results for the differenced data
plot_sis_results(sis_result_diff, total_crime_diff_diff, "SIS Test for Structural Breaks on Differenced Data")


# CUSUM 

# CUSUM Test for Original Data
cusum_original <- efp(total_crime_ts ~ 1, type = "OLS-CUSUM")
plot(cusum_original, main = "CUSUM Test for Total Crime")

# CUSUM Test for Differenced Data
cusum_diff <- efp(total_crime_diff_diff ~ 1, type = "OLS-CUSUM")
plot(cusum_diff, main = "CUSUM Test for Differenced Data")


# MOSUM 

# MOSUM Test for Original Data
mosum_original <- efp(total_crime_ts ~ 1, type = "OLS-MOSUM", h = 0.2)
plot(mosum_original, main = "MOSUM Test for Total Crime")

# MOSUM Test for Differenced Data
mosum_diff <- efp(total_crime_diff_diff ~ 1, type = "OLS-MOSUM", h = 0.2)
plot(mosum_diff, main = "MOSUM Test for Differenced Data")
  
-----------------------------------------------------------

# SARIMA

# auto.arima to original data
arima_model_original <- auto.arima(total_crime_ts)
summary(arima_model_original)

#Series: total_crime_ts 
#ARIMA(0,1,2)(0,1,1)[12] 
#MATCHES CODE

# Apply auto.arima to the differenced data
arima_model_diff <- auto.arima(total_crime_diff_diff)
summary(arima_model_diff)

#Series: total_crime_diff_diff 
#ARIMA(0,0,2)(0,0,1)[12] with zero mean 

# thorough search in original data
fit_arima_orig <- auto.arima(total_crime_ts, stepwise = FALSE)
summary(fit_arima_orig)
# Series: total_crime_ts 
# ARIMA(1,1,1)(2,1,1)[12] 


# differenced data
fit_arima_diff <- auto.arima(total_crime_diff_diff, stepwise = FALSE)
summary(fit_arima_diff)

#Series: total_crime_diff_diff 
#ARIMA(1,0,1)(2,0,1)[12] with zero mean 


# MY SARIMA

sarima_model <- Arima(total_crime_ts, order = c(1, 1, 1), seasonal = c(1, 1, 1))
summary(sarima_model)

sarima_model_test <- Arima(total_crime_ts, order = c(1, 1, 2), seasonal = c(1, 1, 1))
summary(sarima_model_test)

# residuals 
checkresiduals(arima_model_original)
checkresiduals(fit_arima_orig)
checkresiduals(sarima_model)
checkresiduals(sarima_model_test)
checkresiduals(fit_arima_diff)


residuals <- residuals(arima_model_original)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(fit_arima_orig)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(sarima_model)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)


residuals <- residuals(sarima_model_test)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(arima_model_diff)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(arima_model_original)
ljung_box_tests <- lapply(c(5, 10, 15, 20), function(lag) {
  Box.test(residuals, lag = lag, type = "Ljung-Box")
})
for (i in seq_along(ljung_box_tests)) {
  print(ljung_box_tests[[i]])
}

residuals <- residuals(fit_arima_orig)
ljung_box_tests <- lapply(c(5, 10, 15, 20), function(lag) {
  Box.test(residuals, lag = lag, type = "Ljung-Box")
})
for (i in seq_along(ljung_box_tests)) {
  print(ljung_box_tests[[i]])
}

residuals <- residuals(sarima_model)
ljung_box_tests <- lapply(c(5, 10, 15, 20), function(lag) {
  Box.test(residuals, lag = lag, type = "Ljung-Box")
})
for (i in seq_along(ljung_box_tests)) {
  print(ljung_box_tests[[i]])
}

residuals <- residuals(sarima_model_test)
ljung_box_tests <- lapply(c(5, 10, 15, 20), function(lag) {
  Box.test(residuals, lag = lag, type = "Ljung-Box")
})
for (i in seq_along(ljung_box_tests)) {
  print(ljung_box_tests[[i]])
}


# work on SARIMA(1,1,1)(2,1,1)[12] (fit_arima_orig) - same as diff

-------------------------------------------------------
  
# ETS
ets_ana <- ets(total_crime_ts)
summary(ets_model)

ets_aaa <- ets(total_crime_ts, model = "AAA")
summary(ets_aaa)

ets_mam <- ets(total_crime_ts, model = "MAM")
summary(ets_mam)

ets_maa <- ets(total_crime_ts, model="MAA")
summary(ets_maa)

# Check residuals for each model
checkresiduals(ets_aaa)
checkresiduals(ets_ana)
checkresiduals(ets_mam)
checkresiduals(ets_maa)


residuals <- residuals(ets_ana)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(ets_aaa)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(ets_mam)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

residuals <- residuals(ets_maa)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

--------------------------------------------
  
# Holt Winters 

holt_winters_model_add <- HoltWinters(total_crime_ts, seasonal="additive")
print(holt_winters_model_add)

residuals_hw <- residuals(holt_winters_model_add)

checkresiduals(holt_winters_model_add)

shapiro_test_hw <- shapiro.test(residuals_hw)
print(shapiro_test_hw)


holt_winters_model_mul <- HoltWinters(total_crime_ts, seasonal="multiplicative")
print(holt_winters_model_mul)

residuals_hwmul <- residuals(holt_winters_model_mul)

checkresiduals(holt_winters_model_mul)

shapiro_test_hwmul <- shapiro.test(residuals_hwmul)
print(shapiro_test_hwmul)


# Calculate the log-likelihood
n <- length(residuals_hw)  # Number of observations
sigma2 <- sum(residuals_hw^2) / n  # Variance of the residuals
log_likelihood_hw <- -n / 2 * log(2 * pi * sigma2) - sum(residuals_hw^2) / (2 * sigma2)

# Number of parameters: alpha, beta, gamma, and initial values for level, trend, and seasonality components
k <- length(holt_winters_model_add$coefficients) + 3  # +3 for alpha, beta, and gamma

# Calculate AIC and BIC
aic_hw <- -2 * log_likelihood_hw + 2 * k
bic_hw <- -2 * log_likelihood_hw + log(n) * k

print(paste("AIC: ", aic_hw))
print(paste("BIC: ", bic_hw))

aicc_hw <- aic_hw + (2 * k * (k + 1)) / (n - k - 1)

# Print AIC, BIC, and AICc
print(paste("AIC: ", aic_hw))
print(paste("BIC: ", bic_hw))
print(paste("AICc: ", aicc_hw))


# Calculate the log-likelihood
nmul <- length(residuals_hwmul)  # Number of observations
sigma2mul <- sum(residuals_hwmul^2) / n  # Variance of the residuals
log_likelihood_hwmul <- -nmul / 2 * log(2 * pi * sigma2mul) - sum(residuals_hwmul^2) / (2 * sigma2mul)

# Number of parameters: alpha, beta, gamma, and initial values for level, trend, and seasonality components
kmul <- length(holt_winters_model_mul$coefficients) + 3  # +3 for alpha, beta, and gamma

# Calculate AIC and BIC
aic_hwmul <- -2 * log_likelihood_hwmul + 2 * kmul
bic_hwmul <- -2 * log_likelihood_hwmul + log(nmul) * kmul

print(paste("AIC: ", aic_hwmul))
print(paste("BIC: ", bic_hwmul))



model_comparison <- data.frame(
  Model = c("ETS(A,N,A)", "ETS(A,A,A)", "ETS(M,A,M)", "Holt-Winters-Add", "Holt_Winters-Mult"),
  AIC = c(ets_ana$aic, ets_aaa$aic, ets_mam$aic, aic_hw, aic_hwmul),
  BIC = c(ets_ana$bic, ets_aaa$bic, ets_mam$bic, bic_hw, bic_hwmul)
)

print(model_comparison)

#Model      AIC      BIC
#1        ETS(A,N,A) 4973.097 5026.038
#2        ETS(A,A,A) 4975.905 5035.905
#3       ETS(M,Ad,M) 4972.290 5035.820
#4  Holt-Winters-Add 4087.425 4146.596
#5 Holt_Winters-Mult 4075.587 4134.758
# SARIMA             4031.57  4031.93   BIC=4052.43

------------------------------------------------------------------
  
# FORECASTING

train_size <- floor(0.8 * length(total_crime_ts))
train_ts <- head(total_crime_ts, train_size)
test_ts <- tail(total_crime_ts, length(total_crime_ts) - train_size)

# Fit the Holt-Winters multiplicative model on the training set
holt_winters_model_mul_train <- HoltWinters(train_ts, seasonal = "multiplicative")

# Fit the SARIMA model (1,1,1)(2,1,1)[12] on the training set
sarima_model <- Arima(train_ts, order = c(1, 1, 1), seasonal = c(2, 1, 1))

# Fit the Holt-Winters additive model on the training set
holt_winters_model_add_train <- HoltWinters(train_ts, seasonal = "additive")

# Generate the forecast for the test period
holt_winters_mult_forecast <- forecast(holt_winters_model_mul_train, h = length(test_ts))
sarima_forecast <- forecast(sarima_model, h = length(test_ts))
holt_winters_add_forecast <- forecast(holt_winters_model_add_train, h = length(test_ts))

# Calculate accuracy metrics for the Holt-Winters model (using test data)
hw_accuracy <- accuracy(holt_winters_mult_forecast, test_ts)
hw_accuracy_add <- accuracy(holt_winters_add_forecast, test_ts)

# Calculate accuracy metrics for the SARIMA model (using test data)
sarima_accuracy <- accuracy(sarima_forecast, test_ts)

# Print accuracy metrics
print("Holt-Winters Multiplicative Model Accuracy:")
print(hw_accuracy)

print("Holt-Winters Additive Model Accuracy:")
print(hw_accuracy_add)

print("SARIMA Model Accuracy:")
print(sarima_accuracy)

--------------------------

holt_winters_forecast_12 <- forecast(holt_winters_model_mul, h = 12)

# Plot the forecast for Holt-Winters multiplicative model
autoplot(holt_winters_forecast_12) +
  ggtitle("Holt-Winters Multiplicative Model Forecast") +
  xlab("Date") +
  ylab("Total Crime Count")

# Print the forecasted values for Holt-Winters multiplicative model
print(holt_winters_forecast_12)


holt_winters_forecast_60 <- forecast(holt_winters_model_add_train, h = 24)

# Plot the forecast for Holt-Winters multiplicative model
autoplot(holt_winters_forecast_60) +
  ggtitle("Holt-Winters Additive Model Forecast For Next 2 Years") +
  xlab("Date") +
  ylab("Total Crime Count")

# Print the forecasted values for Holt-Winters multiplicative model
print(holt_winters_forecast_60)




sarima_forecast_12 <- forecast(sarima_model, h = 24)

# Plot the forecast for SARIMA model
autoplot(sarima_forecast_12) +
  ggtitle("SARIMA(1,1,1)(2,1,1)[12] Model Forecast For Next 2 Years") +
  xlab("Date") +
  ylab("Total Crime Count")

# Print the forecasted values for SARIMA model
print(sarima_forecast_12)

sarima_forecast_60 <- forecast(fit_arima_orig, h = 24)

# Plot the forecast for SARIMA model
autoplot(sarima_forecast_60) +
  ggtitle("SARIMA(1,1,1)(2,1,1)[12] Model Forecast For Next 5 Years") +
  xlab("Date") +
  ylab("Total Crime Count")

# Print the forecasted values for SARIMA model
print(sarima_forecast_60)

-----------------------------
  
# COMPARISON GRAPH 

actual_data <- data.frame(
    Date = as.Date(time(total_crime_ts)),
    Total_Crime_Count = as.numeric(total_crime_ts),
    Series = "Actual Data"
  )

forecast_start_date <- as.Date(time(tail(total_crime_ts, 1))) + months(1)

sarima_forecast_data <- data.frame(
  Date = seq(forecast_start_date, by = "month", length.out = 60),
  Total_Crime_Count = as.numeric(sarima_forecast_60$mean),
  Series = "SARIMA"
)

hw_forecast_data <- data.frame(
  Date = seq(forecast_start_date, by = "month", length.out = 60),
  Total_Crime_Count = as.numeric(holt_winters_forecast_60$mean),
  Series = "Holt-Winters Multiplicative"
)

# Combine the data frames
combined_df <- bind_rows(actual_data, sarima_forecast_data, hw_forecast_data)

# Plot the forecast for the test period and compare it with the actual test data
ggplot(combined_df, aes(x = Date)) +
  geom_line(aes(y = Total_Crime_Count, color = Series), size = 1) +
  ggtitle("Forecast Comparison: SARIMA vs Holt-Winters Multiplicative") +
  xlab("Date") +
  ylab("Total Crime Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Actual Data" = "black", "SARIMA" = "red", "Holt-Winters Multiplicative" = "blue")) +
  guides(colour = guide_legend(title = "Series"))

# Define start and end dates for zooming in
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2028-01-01")

# Filter the combined dataframe to only include data within the specified date range
zoomed_df <- combined_df %>%
  filter(Date >= start_date & Date <= end_date)

# Plot the forecast for the test period and compare it with the actual test data
ggplot(zoomed_df, aes(x = Date)) +
  geom_line(aes(y = Total_Crime_Count, color = Series), size = 1) +
  ggtitle("Forecast Comparison: SARIMA vs Holt-Winters Multiplicative") +
  xlab("Date") +
  ylab("Total Crime Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Actual Data" = "black", "SARIMA" = "red", "Holt-Winters Multiplicative" = "blue")) +
  guides(colour = guide_legend(title = "Series"))


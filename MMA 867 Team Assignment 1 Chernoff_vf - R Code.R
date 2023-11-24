#Importing necessary libraries
library(forecast)
library(ggplot2)
library(lubridate)
library(zoo)
library(tseries)
library(urca)
library(dplyr)
library(stats)
library(tseries)
library(car)

##########Q1 and Q2#########################

#Importing MET global annual summary dataset
METdata <-
  read.csv(file.choose(), header = TRUE, sep = ",") #load the data

#Creating a separate dataset for Q1 with necessary formatting updates
METdata_Mon_Q12 <- data.frame(
  Year_Mon = as.yearmon(METdata$Time, format = "%Y-%m"),
  Avg_Temp_Cel = METdata$Anomaly..deg.C. + 14
)

str(METdata_Mon_Q12) #check the structure of the data

# Create a plot of Avg_Tem_Cel vs. Time
ggplot(data = METdata_Mon_Q12, aes(x = Year_Mon, y = Avg_Temp_Cel)) +
  geom_line() +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Average Temperature Over Time")

#Convert this data in to a time series object with yearly frequency from 1850, frequency of 12 for monthly seasonality
METdata_Mon_Q12_ts <- ts(METdata_Mon_Q12$Avg_Temp_Cel, start = 1850)

#Convert this data in to a time series object with yearly frequency from 1850, frequency of 12 for monthly seasonality
METdata_Mon_Q12_ts1 <-
  ts(METdata_Mon_Q12$Avg_Temp_Cel,
     start = 1850,
     frequency = 12)

#Convert this data in to a time series object with yearly frequency from 1850, frequency of 48 for El Nino effect
METdata_Mon_Q12_ts2 <-
  ts(METdata_Mon_Q12$Avg_Temp_Cel,
     start = 1850,
     frequency = 48)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
METdata_Q12_AAN <- ets(METdata_Mon_Q12_ts, model = "AAN")
METdata_Q12_AAZ <-
  ets(METdata_Mon_Q12_ts, model = "AAZ", damped = FALSE)
METdata_Q12_MMN <-
  ets(METdata_Mon_Q12_ts, model = "MMN", damped = FALSE)
METdata_Q12_MMZ <-
  ets(METdata_Mon_Q12_ts, model = "MMZ", damped = FALSE)
METdata_Q12_tbats <- tbats(METdata_Mon_Q12_ts1)
METdata_Q12_arima <- auto.arima(METdata_Mon_Q12_ts, seasonal = FALSE)
METdata_Q12_sarima1 <- auto.arima(METdata_Mon_Q12_ts1, seasonal = TRUE)
METdata_Q12_sarima2 <- auto.arima(METdata_Mon_Q12_ts2, seasonal = TRUE)

#Summary of models run
summary(METdata_Q12_AAZ)
summary(METdata_Q12_MMZ)
summary(METdata_Q12_tbats)
summary(METdata_Q12_arima)
summary(METdata_Q12_sarima1)
summary(METdata_Q12_sarima2)

#plot various decompositions into error/noise, trend and seasonality

fit <-
  decompose(METdata_Mon_Q12_ts2, type = "multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <-
  decompose(METdata_Mon_Q12_ts2, type = "additive") #decompose using "classical" method, additive form
plot(fit)

fit <-
  stl(METdata_Mon_Q12_ts2,
      t.window = 12,
      s.window = "periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

# Create their prediction "cones" for 924 months into the future with quintile confidence intervals
METdata_Q12_AAN_pred <-
  forecast(METdata_Q12_AAN, h = 925, level = c(0.8, 0.90))
METdata_Q12_AAZ_pred <-
  forecast(METdata_Q12_AAZ, h = 925, level = c(0.8, 0.90))
METdata_Q12_MMN_pred <-
  forecast(METdata_Q12_MMN, h = 925, level = c(0.8, 0.90))
METdata_Q12_MMZ_pred <-
  forecast(METdata_Q12_MMZ, h = 925, level = c(0.8, 0.90))
METdata_Q12_tbats_pred <-
  forecast(METdata_Q12_tbats, h = 925, level = c(0.8, 0.90))
METdata_Q12_ARIMA_pred <-
  forecast(METdata_Q12_arima, h = 925, level = c(0.8, 0.90))
METdata_Q12_SARIMA1_pred <-
  forecast(METdata_Q12_sarima1,
           h = 925,
           level = c(0.8, 0.90))
METdata_Q12_SARIMA2_pred <-
  forecast(METdata_Q12_sarima2,
           h = 925,
           level = c(0.8, 0.90))


#####
##### ARIMA on residuals, with a multiseasonality time-series object
#####

METdata_Mon_Q12_msts <-
  msts(METdata_Mon_Q12$Avg_Temp_Cel, seasonal.periods = c(12, 48))

METdata_Mon_Q12lm_msts <-
  tslm(METdata_Mon_Q12_msts ~ trend + season) # Build a linear model for trend and seasonality
summary(METdata_Mon_Q12lm_msts)

residarima1 <-
  auto.arima(METdata_Mon_Q12lm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <-
  forecast(residarima1, h = 925, level = c(0.90)) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <-
  forecast(METdata_Mon_Q12lm_msts, h = 925, level = c(0.80)) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)


#######Visualization
# Compare the prediction "cones" visually
par(mfrow = c(1, 3)) # Lets look at all  models on one graph on the same scale
plot(
  METdata_Q12_AAZ_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  METdata_Q12_MMZ_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  METdata_Q12_tbats_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)

# Compare the prediction "cones" visually
par(mfrow = c(1, 3)) # Lets look at all  models on one graph on the same scale
plot(
  METdata_Q12_ARIMA_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  METdata_Q12_SARIMA1_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  METdata_Q12_SARIMA2_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)


# Plot the combined forecasts with shaded regions
plot(
  combined_df$Month,
  combined_df$Point_Forecast,
  type = "l",
  col = "blue",
  xlab = "Month",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5),
  main = "Combined Forecast"
)
shade <- function(from, to, df) {
  x <- c(from, rev(to))
  y <- c(df$Hi_80, rev(df$Lo_80))
  polygon(x,
          y,
          col = "blue",
          border = NA,
          density = 20)
  y <- c(df$Hi_90, rev(df$Lo_90))
  polygon(x,
          y,
          col = "red",
          border = NA,
          density = 20)
}
shade(combined_df$Month, combined_df$Month, combined_df)
legend(
  "topleft",
  legend = c("Point Forecast", "80% CI", "90% CI"),
  col = c("blue", "blue", "red"),
  lty = 1
)

xSeqFull <-
  seq(as.Date("1850/1/1"), by = "month", length.out = 2005)
plot(xSeqFull, val_ts, type = "l")
lines(xSeq, GlobalMeanPred, col = "blue")
lines(xSeq, niaveForecast, col = "red")
polygon(c(xSeq, rev(xSeq)),
        c(max_a , rev(min_a)),
        col = rgb(0.3, 0.3, 0.7, 0.2),
        border = "NA")


###
### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout), using a window of 90 years as window
###

f_AAN  <- function(y, h)
  forecast(ets(y, model = "AAN"), h = h)
errors_AAN <- tsCV(METdata_Mon_Q12_ts, f_AAN, h = 1, window = 924)

f_MMN  <- function(y, h)
  forecast(ets(y, model = "MMN"), h = h)
errors_MMN <- tsCV(METdata_Mon_Q12_ts, f_MMN, h = 1, window = 924)

f_ARIMA  <-
  function(y, h)
    forecast(auto.arima(y, seasonal = FALSE), h = h)
errors_ARIMA <- tsCV(METdata_Mon_Q12_ts, f_ARIMA, h = 1, window = 924)

f_SARIMA1  <-
  function(y, h)
    forecast(auto.arima(y, seasonal = TRUE), h = h)
errors_SARIMA1 <-
  tsCV(METdata_Mon_Q12_ts1,
       f_SARIMA1,
       h = 1,
       window = 924)

f_SARIMA2  <-
  function(y, h)
    forecast(auto.arima(y, seasonal = TRUE), h = h)
errors_SARIMA2 <-
  tsCV(METdata_Mon_Q12_ts2,
       f_SARIMA2,
       h = 1,
       window = 924)


par(mfrow = c(1, 1))
plot(errors_AAN, ylab = 'tsCV errors')
abline(0, 0)
lines(errors_AAN, col = "red")
lines(errors_MMN, col = "blue")
lines(errors_ARIMA, col = "yellow")
lines(errors_SARIMA, col = "orange")
lines(errors_SARIMA2, col = "green")
legend(
  "left",
  legend = c(
    "CV_error_AAN",
    "CV_error_MMN",
    "errors_ARIMA",
    "errors_SARIMA",
    "errors_SARIMA2"
  ),
  col = c("red", "blue", "yellow", "orange", "green"),
  lty = 1:5
)

mean(abs(errors_AAN / METdata_Mon_Q12_ts), na.rm = TRUE) * 100
mean(abs(errors_MMN / METdata_Mon_Q12_ts), na.rm = TRUE) * 100
mean(abs(errors_ARIMA / METdata_Mon_Q12_ts), na.rm = TRUE) * 100
mean(abs(errors_SARIMA1 / METdata_Mon_Q12_ts1), na.rm = TRUE) * 100
mean(abs(errors_SARIMA2 / METdata_Mon_Q12_ts), na.rm = TRUE) * 100


#####
##### Rolling-horizon holdout: ARIMA on residuals
#####

accuracy.arima = 0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:30)
{
  nTest <- 12 * i
  nTrain <- length(METdata_Mon_Q12_msts) - nTest - 1
  train <-
    window(METdata_Mon_Q12_msts,
           start = 1,
           end = 1 + (nTrain) / (30 * 12))
  test <-
    window(
      METdata_Mon_Q12_msts,
      start = 1 + (nTrain + 1) / (30 * 12),
      end = 1 + (nTrain + 12) / (30 * 12)
    )
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm, h = 12)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto, h = 12)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x + y
  
  cat(
    "----------------------------------

      Data Partition",
    i,
    "

      Training Set includes",
    nTrain,
    " time periods. Observations 1 to",
    nTrain,
    "
      Test Set includes 12 time periods. Observations",
    nTrain + 1,
    "to",
    nTrain + 12,
    "

      "
  )
  
  print(accuracy(sp, test))
  #  print(residauto)
  
  accuracy.arima <- rbind(accuracy.arima, accuracy(sp, test)[1, 5])
  
  #print(sp$model)
}
accuracy.arima <- accuracy.arima[-1]

#compare mean accuracies of the rolling holdout
mean(accuracy.arima)

sd(accuracy.arima)

###Final prediction of model and answers to Q1 and Q2
forecastR <- regressionF + residualsF # Total prediction
residualsLB <- as.numeric(residualsArimaForecast$lower)
residualsUB <- as.numeric(residualsArimaForecast$upper)
ForecastLB <- regressionF + residualsLB
ForecastUB <- regressionF + residualsUB
forecastR_Pred <-
  data.frame(forecastR = forecastR,
             ForecastLB = ForecastLB,
             ForecastUB = ForecastUB)

# Create a sequence of months starting from Jul 2023
start_date <- as.Date("2023-07-01")
num_months <- nrow(forecastR_Pred)
month_sequence <-
  seq(start_date, by = "1 month", length.out = num_months)

# Create a dataframe with the forecast data and month column
forecastR_Pred_with_months <- data.frame(Month = month_sequence,
                                         forecastR_Pred)

# Print the point predictions
subset(forecastR_Pred_with_months, Month == as.Date('2030-01-01'))


#Visualization
# Create a dataframe with the data
forecastR_Pred <- data.frame(
  Year_Mon = month_sequence,
  # Assuming you have this sequence defined
  forecastR = forecastR,
  ForecastLB = ForecastLB,
  ForecastUB = ForecastUB
)

# Convert 'Year_Mon' column in METdata_Mon_Q12 to a Date type
METdata_Mon_Q12$Year_Mon <-
  as.Date(METdata_Mon_Q12$Year_Mon, format = "%b %Y")

# Create the plot
ggplot(forecastR_Pred, aes(x = Year_Mon)) +
  geom_line(aes(y = forecastR), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = ForecastLB, ymax = ForecastUB),
              fill = "blue",
              alpha = 0.3) +
  geom_line(
    data = METdata_Mon_Q12,
    aes(x = Year_Mon, y = Avg_Temp_Cel),
    color = "black",
    size = 0.8
  ) +
  #geom_point(data = METdata_Mon_Q12, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "blue", size = 2) +
  labs(x = "Year_Mon", y = "Predicted Avg Temp") +
  ggtitle("Temperature Prediction with Confidence Intervals") +
  theme_minimal()

#Q1
# Extract data for January and July of 2023 from METdata_Mon_Q12
january_2023 <-
  METdata_Mon_Q12[year(METdata_Mon_Q12$Year_Mon) == 2023 &
                    month(METdata_Mon_Q12$Year_Mon) == 1,]
jun_2023 <-
  METdata_Mon_Q12[year(METdata_Mon_Q12$Year_Mon) == 2023 &
                    month(METdata_Mon_Q12$Year_Mon) == 6,]

# Extract data for January and July of 2100 from forecastR_Pred_with_months
january_2100 <-
  forecastR_Pred_with_months[forecastR_Pred_with_months$Month == as.Date('2100-01-01'),]
jun_2100 <-
  forecastR_Pred_with_months[forecastR_Pred_with_months$Month == as.Date('2100-06-01'),]

# Calculate the temperature differences
temperature_diff_january <-
  january_2100$forecastR - january_2023$Avg_Temp_Cel
temperature_diff_jun <- jun_2100$forecastR - jun_2023$Avg_Temp_Cel

# Print the temperature differences
cat("Temperature difference for January 2100 - 2023:",
    temperature_diff_january,
    "\n")
cat("Temperature difference for Jun 2100 - 2023:",
    temperature_diff_jun,
    "\n")

# Calculate average temperatures for 2100 and 2023
average_temp_2100 <-
  mean(forecastR_Pred_with_months[year(forecastR_Pred_with_months$Month) == 2100, "forecastR"], na.rm = TRUE)
average_temp_2023 <-
  mean(METdata_Mon_Q12[year(METdata_Mon_Q12$Year_Mon) == 2023, "Avg_Temp_Cel"])

# Print the average temperatures
cat(
  "Diff in Average temperature 2100-2023:",
  average_temp_2100 - average_temp_2023,
  "°C\n"
)


#Q2
subset(
  forecastR_Pred_with_months,
  format(Month, "%Y") %in% c("2030", "2050", "2100") &
    format(Month, "%m") %in% c("01", "07")
)


#######NASA Data

#Importing NASA global annual summary dataset
NASAdata <-
  read.csv(file.choose(), header = TRUE, sep = ",") #load the data

# Convert character columns to numeric
NASAdata <- NASAdata %>%
  mutate(across(Jan:Dec, as.numeric))

str(NASAdata)

# Tidy the data into long format and transform Year_Mon
NASAdata_tidy <- NASAdata %>%
  pivot_longer(cols = -Year,
               names_to = "Month",
               values_to = "Temperature_Anomaly") %>%
  mutate(
    Year_Mon = as.yearmon(paste(Month, Year), format = "%b %Y"),
    Year_Mon = as.yearmon(Year_Mon, format = "%Y-%m")
  )

# Transform Avg_Temp_Cel
NASAdata_tidy$Avg_Temp_Cel <- NASAdata_tidy$Temperature_Anomaly + 14

# Extract only the desired columns
NASAdata_tidy_final <- NASAdata_tidy %>%
  select(Year_Mon, Avg_Temp_Cel)

NASAdata_tidy_final <- na.omit(NASAdata_tidy_final)

str(NASAdata_tidy_final)

# Create a plot of Avg_Tem_Cel vs. Time
ggplot(data = NASAdata_tidy_final, aes(x = Year_Mon, y = Avg_Temp_Cel)) +
  geom_line() +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Average Temperature Over Time")

#Convert this data in to a time series object with yearly frequency from 1880
NASAdata_tidy_final_ts <-
  ts(NASAdata_tidy_final$Avg_Temp_Cel, start = 1880)

#Convert this data in to a time series object with yearly frequency from 1880, frequency of 12 for monthly seasonality
NASAdata_tidy_final_ts1 <-
  ts(NASAdata_tidy_final$Avg_Temp_Cel,
     start = 1880,
     frequency = 12)

#Convert this data in to a time series object with yearly frequency from 1880, frequency of 48 for El Nino effect
NASAdata_tidy_final_ts2 <-
  ts(NASAdata_tidy_final$Avg_Temp_Cel,
     start = 1880,
     frequency = 48)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
NASAdata_tidy_final_AAN <- ets(NASAdata_tidy_final_ts, model = "AAN")
NASAdata_tidy_final_AAZ <-
  ets(NASAdata_tidy_final_ts, model = "AAZ", damped = FALSE)
NASAdata_tidy_final_MMN <-
  ets(NASAdata_tidy_final_ts, model = "MMN", damped = FALSE)
NASAdata_tidy_final_MMZ <-
  ets(NASAdata_tidy_final_ts, model = "MMZ", damped = FALSE)
NASAdata_tidy_final_tbats <- tbats(NASAdata_tidy_final_ts)
NASAdata_tidy_final_arima <-
  auto.arima(NASAdata_tidy_final_ts, seasonal = FALSE)
NASAdata_tidy_final_sarima1 <-
  auto.arima(NASAdata_tidy_final_ts1, seasonal = TRUE)
NASAdata_tidy_final_sarima2 <-
  auto.arima(NASAdata_tidy_final_ts2, seasonal = TRUE)

#Summary of models run
summary(NASAdata_tidy_final_AAN)
summary(NASAdata_tidy_final_AAZ)
summary(NASAdata_tidy_final_MMN)
summary(NASAdata_tidy_final_MMZ)
summary(NASAdata_tidy_final_tbats)
summary(NASAdata_tidy_final_arima)
summary(NASAdata_tidy_final_sarima1)
summary(NASAdata_tidy_final_sarima2)

# Create their prediction "cones" for 925 months into the future with quintile confidence intervals
NASAdata_tidy_final_AAN_pred <-
  forecast(NASAdata_tidy_final_AAN,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_AAZ_pred <-
  forecast(NASAdata_tidy_final_AAZ,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_MMN_pred <-
  forecast(NASAdata_tidy_final_MMN,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_MMZ_pred <-
  forecast(NASAdata_tidy_final_MMZ,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_tbats_pred <-
  forecast(NASAdata_tidy_final_tbats,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_ARIMA_pred <-
  forecast(NASAdata_tidy_final_arima,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_SARIMA1_pred <-
  forecast(NASAdata_tidy_final_sarima1,
           h = 925,
           level = c(0.8, 0.90))
NASAdata_tidy_final_SARIMA2_pred <-
  forecast(NASAdata_tidy_final_sarima2,
           h = 925,
           level = c(0.8, 0.90))

# Compare the prediction "cones" visually
par(mfrow = c(1, 3)) # Lets look at all  models on one graph on the same scale
plot(
  NASAdata_tidy_final_AAZ_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  NASAdata_tidy_final_MMZ_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  NASAdata_tidy_final_tbats_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)

par(mfrow = c(1, 3)) # Lets look at all  models on one graph on the same scale
plot(
  NASAdata_tidy_final_ARIMA_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  NASAdata_tidy_final_SARIMA1_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)
plot(
  NASAdata_tidy_final_SARIMA2_pred,
  xlab = "Year",
  ylab = "Predicted Avg Temp",
  ylim = c(13, 16.5)
)


###
### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout), using a window of 90 years as window
###

f_AAN  <- function(y, h)
  forecast(ets(y, model = "AAN"), h = h)
errors_AAN <- tsCV(NASAdata_tidy_final_ts,
                   f_AAN,
                   h = 1,
                   window = 925)

f_MMN  <- function(y, h)
  forecast(ets(y, model = "MMN"), h = h)
errors_MMN <- tsCV(NASAdata_tidy_final_ts,
                   f_MMN,
                   h = 1,
                   window = 925)

f_ARIMA  <-
  function(y, h)
    forecast(auto.arima(y, seasonal = FALSE), h = h)
errors_ARIMA <-
  tsCV(NASAdata_tidy_final_ts,
       f_ARIMA,
       h = 1,
       window = 925)

f_SARIMA1  <-
  function(y, h)
    forecast(auto.arima(y, seasonal = TRUE), h = h)
errors_SARIMA1 <-
  tsCV(NASAdata_tidy_final_ts1,
       f_SARIMA1,
       h = 1,
       window = 925)

f_SARIMA2  <-
  function(y, h)
    forecast(auto.arima(y, seasonal = TRUE), h = h)
errors_SARIMA2 <-
  tsCV(NASAdata_tidy_final_ts2,
       f_SARIMA2,
       h = 12,
       window = 144)


par(mfrow = c(1, 1))
plot(errors_AAN, ylab = 'tsCV errors')
abline(0, 0)
lines(errors_AAN, col = "red")
lines(errors_MMN, col = "blue")
lines(errors_ARIMA, col = "yellow")
lines(errors_SARIMA1, col = "orange")
lines(errors_SARIMA2, col = "green")
legend(
  "left",
  legend = c(
    "CV_error_AAN",
    "CV_error_MMN",
    "errors_ARIMA",
    "errors_SARIMA",
    "errors_SARIMA2"
  ),
  col = c("red", "blue", "yellow", "orange", "green"),
  lty = 1:5
)

mean(abs(errors_AAN / NASAdata_tidy_final_ts), na.rm = TRUE) * 100
mean(abs(errors_MMN / NASAdata_tidy_final_ts), na.rm = TRUE) * 100
mean(abs(errors_ARIMA / NASAdata_tidy_final_ts), na.rm = TRUE) * 100
mean(abs(errors_SARIMA1 / NASAdata_tidy_final_ts1), na.rm = TRUE) * 100
mean(abs(errors_SARIMA2 / NASAdata_tidy_final_ts2), na.rm = TRUE) * 100


#####
##### ARIMA on residuals, checking if a seasonality of 4 years gives a better model
#####

NASAdata_tidy_final_msts <-
  msts(NASAdata_tidy_final$Avg_Temp_Cel, seasonal.periods = c(12, 48))

NASAdata_tidy_finallm_msts <-
  tslm(NASAdata_tidy_final_msts ~ trend + season) # Build a linear model for trend and seasonality
summary(NASAdata_tidy_finallm_msts)

residarima1 <-
  auto.arima(NASAdata_tidy_finallm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <-
  forecast(residarima1, h = 925, level = c(0.90)) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <-
  forecast(NASAdata_tidy_finallm_msts,
           h = 925,
           level = c(0.90)) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)


#####
##### Rolling-horizon holdout: ARIMA on residuals
#####

accuracy.arima = 0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:30)
{
  nTest <- 12 * i
  nTrain <- length(NASAdata_tidy_final_msts) - nTest - 1
  train <-
    window(NASAdata_tidy_final_msts,
           start = 1,
           end = 1 + (nTrain) / (30 * 12))
  test <-
    window(
      NASAdata_tidy_final_msts,
      start = 1 + (nTrain + 1) / (30 * 12),
      end = 1 + (nTrain + 12) / (30 * 12)
    )
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm, h = 12)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto, h = 12)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x + y
  
  cat(
    "----------------------------------

      Data Partition",
    i,
    "

      Training Set includes",
    nTrain,
    " time periods. Observations 1 to",
    nTrain,
    "
      Test Set includes 12 time periods. Observations",
    nTrain + 1,
    "to",
    nTrain + 12,
    "

      "
  )
  
  print(accuracy(sp, test))
  #  print(residauto)
  
  accuracy.arima <- rbind(accuracy.arima, accuracy(sp, test)[1, 5])
  
  #print(sp$model)
}
accuracy.arima <- accuracy.arima[-1]

#compare mean accuracies of the rolling holdout
mean(accuracy.arima)

sd(accuracy.arima)

###Final prediction of model and answers to Q1 and Q2
forecastR <- regressionF + residualsF # Total prediction
residualsLB <- as.numeric(residualsArimaForecast$lower)
residualsUB <- as.numeric(residualsArimaForecast$upper)
ForecastLB <- regressionF + residualsLB
ForecastUB <- regressionF + residualsUB
forecastR_Pred <-
  data.frame(forecastR = forecastR,
             ForecastLB = ForecastLB,
             ForecastUB = ForecastUB)

# Create a sequence of months starting from Jul 2023
start_date <- as.Date("2023-07-01")
num_months <- nrow(forecastR_Pred)
month_sequence <-
  seq(start_date, by = "1 month", length.out = num_months)

# Create a dataframe with the forecast data and month column
forecastR_Pred_with_months <- data.frame(Month = month_sequence,
                                         forecastR_Pred)


#Visualization
# Create a dataframe with the data
forecastR_Pred <- data.frame(
  Year_Mon = month_sequence,
  # Assuming you have this sequence defined
  forecastR = forecastR,
  ForecastLB = ForecastLB,
  ForecastUB = ForecastUB
)

# Convert 'Year_Mon' column in METdata_Mon_Q12 to a Date type
NASAdata_tidy_final$Year_Mon <-
  as.Date(NASAdata_tidy_final$Year_Mon, format = "%b %Y")

# Create the plot
ggplot(forecastR_Pred, aes(x = Year_Mon)) +
  geom_line(aes(y = forecastR), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = ForecastLB, ymax = ForecastUB),
              fill = "blue",
              alpha = 0.3) +
  geom_line(
    data = NASAdata_tidy_final,
    aes(x = Year_Mon, y = Avg_Temp_Cel),
    color = "black",
    size = 0.8
  ) +
  #geom_point(data = METdata_Mon_Q12, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "blue", size = 2) +
  labs(x = "Year_Mon", y = "Predicted Avg Temp") +
  ggtitle("Temperature Prediction with Confidence Intervals") +
  theme_minimal()

#Q1
# Extract data for January and July of 2023 from METdata_Mon_Q12
january_2023 <-
  NASAdata_tidy_final[year(NASAdata_tidy_final$Year_Mon) == 2023 &
                        month(NASAdata_tidy_final$Year_Mon) == 1,]
jun_2023 <-
  NASAdata_tidy_final[year(NASAdata_tidy_final$Year_Mon) == 2023 &
                        month(NASAdata_tidy_final$Year_Mon) == 6,]

# Extract data for January and July of 2100 from forecastR_Pred_with_months
january_2100 <-
  forecastR_Pred_with_months[forecastR_Pred_with_months$Month == as.Date('2100-01-01'),]
jun_2100 <-
  forecastR_Pred_with_months[forecastR_Pred_with_months$Month == as.Date('2100-06-01'),]

# Calculate the temperature differences
temperature_diff_january <-
  january_2100$forecastR - january_2023$Avg_Temp_Cel
temperature_diff_jun <- jun_2100$forecastR - jun_2023$Avg_Temp_Cel

# Print the temperature differences
cat("Temperature difference for January 2100 - 2023:",
    temperature_diff_january,
    "\n")
cat("Temperature difference for Jun 2100 - 2023:",
    temperature_diff_jun,
    "\n")

# Calculate average temperatures for 2100 and 2023
average_temp_2100 <-
  mean(forecastR_Pred_with_months[year(forecastR_Pred_with_months$Month) == 2100, "forecastR"], na.rm = TRUE)
average_temp_2023 <-
  NASAdata_tidy_final[year(NASAdata_tidy_final$Year_Mon) == 2023, "Avg_Temp_Cel"]

# Print the average temperatures
cat(
  "Diff in Average temperature 2100-2023:",
  average_temp_2100 - average_temp_2023,
  "°C\n"
)


#Q2
subset(
  forecastR_Pred_with_months,
  format(Month, "%Y") %in% c("2030", "2050", "2100") &
    format(Month, "%m") %in% c("01", "07")
)


##########Q3 and Q4#########################
#Q3
#Test to check if as of 2013 temperatures have been stable for a decade, ~2003 to 2013

#Importing MET global annual summary dataset
METdata <-
  read.csv(file.choose(), header = TRUE, sep = ",") #load the data

#Creating a separate dataset for Q1 with necessary formatting updates
METdata_Mon_Q12 <- data.frame(
  Year_Mon = as.yearmon(METdata$Time, format = "%Y-%m"),
  Avg_Temp_Cel = METdata$Anomaly..deg.C. + 14
)

METdata_Mon_Q3_check <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 1992") &
           Year_Mon <= as.yearmon("Dec 2012"))

# Create a line graph for the data
ggplot(data = METdata_Mon_Q3_check, aes(x = Year_Mon, y = Avg_Temp_Cel)) +
  geom_line() +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Average Temperature (1992-2012)") +
  theme_minimal()

#MET Data
# Creating a subset of data for years from Jan 2003 to Dec 2012
METdata_Mon_Q3 <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2003") &
           Year_Mon <= as.yearmon("Dec 2012"))

# Create a time series object
METdata_Mon_Q3_ts <-
  ts(
    METdata_Mon_Q3$Avg_Temp_Cel,
    start = c(2003, 1),
    end = c(2012, 12),
    frequency = 12
  )

# Perform the Augmented Dickey-Fuller test
adf_test <- adf.test(METdata_Mon_Q3_ts)

# Print ADF test results
print(adf_test)

# Perform KPSS test
kpss_test <- ur.kpss(METdata_Mon_Q3_ts, type = "tau")

# Print KPSS test results
print(kpss_test)

#Visualization
# Calculate mean and standard deviation
avg_temp_mean <- mean(METdata_Mon_Q3$Avg_Temp_Cel)
avg_temp_sd <- sd(METdata_Mon_Q3$Avg_Temp_Cel)
variance_MET <- var(METdata_Mon_Q3$Avg_Temp_Cel)


# Create a plot of Avg_Temp_Cel vs. Time with mean and variance lines
ggplot(data = METdata_Mon_Q3, aes(x = Year_Mon, y = Avg_Temp_Cel)) +
  geom_line() +
  geom_hline(
    yintercept = avg_temp_mean,
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  geom_ribbon(
    aes(ymin = avg_temp_mean - avg_temp_sd, ymax = avg_temp_mean + avg_temp_sd),
    fill = "blue",
    alpha = 0.2
  ) +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Average Temperature (2003-2012) with Mean and Variance") +
  theme_minimal()

print(avg_temp_mean)
print(variance_MET)
print(avg_temp_sd)

#NASA Data
# Creating a subset of data for years from Jan 2003 to Dec 2012
NASAdata_tidy_final_Q3 <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2003") &
           Year_Mon <= as.yearmon("Dec 2012"))

# Create a time series object
NASAdata_tidy_final_Q3_ts <-
  ts(
    NASAdata_tidy_final_Q3$Avg_Temp_Cel,
    start = c(2003, 1),
    end = c(2012, 12),
    frequency = 12
  )

# Perform the Augmented Dickey-Fuller test
adf_test_NASA <- adf.test(NASAdata_tidy_final_Q3_ts)

# Print ADF test results
print(adf_test_NASA)

# Perform KPSS test
kpss_test <- ur.kpss(NASAdata_tidy_final_Q3_ts, type = "tau")

# Print KPSS test results
print(kpss_test)


#Visualization
# Calculate mean and standard deviation
avg_temp_mean <- mean(NASAdata_tidy_final_Q3$Avg_Temp_Cel)
avg_temp_sd <- sd(NASAdata_tidy_final_Q3$Avg_Temp_Cel)
variance_NASA <- var(NASAdata_tidy_final_Q3$Avg_Temp_Cel)


# Create a plot of Avg_Temp_Cel vs. Time with mean and variance lines
ggplot(data = NASAdata_tidy_final_Q3, aes(x = Year_Mon, y = Avg_Temp_Cel)) +
  geom_line() +
  geom_hline(
    yintercept = avg_temp_mean,
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  geom_ribbon(
    aes(ymin = avg_temp_mean - avg_temp_sd, ymax = avg_temp_mean + avg_temp_sd),
    fill = "blue",
    alpha = 0.2
  ) +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Average Temperature (2003-2012) with Mean and Variance") +
  theme_minimal()

print(avg_temp_mean)
print(variance_NASA)


#Q4
#As you will also see from the case and the articles, many of the claims about global
#warming are not only that the average temperatures raise, but also that the fluctuations
#of the temperatures become larger

#Is that a correct conclusion given the data (both as of 2013 and as of today)?

#MET Data

#Diving data into two 10 year chunks, Jan 1993 to Dec 2002 (before)
# and Jan 2003 to Dec 2012

METdata_Mon_Q42013_before <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 1993") &
           Year_Mon <= as.yearmon("Dec 2002"))

METdata_Mon_Q42013_after <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2003") &
           Year_Mon <= as.yearmon("Dec 2012"))


# Perform Bartlett's test
bartlett_result <-
  bartlett.test(
    list(
      METdata_Mon_Q42013_before$Avg_Temp_Cel,
      METdata_Mon_Q42013_after$Avg_Temp_Cel
    )
  )

# Print the results
print(bartlett_result)


#As per the ADF and KPSS tests, temperatures did not raise as of 2013, it was stationary around the mean for the decade prior to 2013

#Diving data into two 10 year chunks, Jan 2003 to Dec 2012 (before)
# and Jan 2013 to Jun 2023

METdata_Mon_Q42023_before <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2003") &
           Year_Mon <= as.yearmon("Dec 2012"))

METdata_Mon_Q42023_after <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2013"))


# Perform Bartlett's test
bartlett_result <-
  bartlett.test(
    list(
      METdata_Mon_Q42023_before$Avg_Temp_Cel,
      METdata_Mon_Q42023_after$Avg_Temp_Cel
    )
  )

# Print the results
print(bartlett_result)

#Visualization
# Calculate and compare variances
variance_group1_Q42013 <-
  var(METdata_Mon_Q42013_before$Avg_Temp_Cel)
variance_group2_Q42013 <- var(METdata_Mon_Q42013_after$Avg_Temp_Cel)
sd_group1_Q42013 <- sd(METdata_Mon_Q42013_before$Avg_Temp_Cel)
sd_group2_Q42013 <- sd(METdata_Mon_Q42013_after$Avg_Temp_Cel)
mean_group1_Q42013 <- mean(METdata_Mon_Q42013_before$Avg_Temp_Cel)
mean_group2_Q42013 <- mean(METdata_Mon_Q42013_after$Avg_Temp_Cel)


# Calculate and compare variances
variance_group1 <- var(METdata_Mon_Q42023_before$Avg_Temp_Cel)
variance_group2 <- var(METdata_Mon_Q42023_after$Avg_Temp_Cel)
sd_group1 <- sd(METdata_Mon_Q42023_before$Avg_Temp_Cel)
sd_group2 <- sd(METdata_Mon_Q42023_after$Avg_Temp_Cel)
mean_group1 <- mean(METdata_Mon_Q42023_before$Avg_Temp_Cel)
mean_group2 <- mean(METdata_Mon_Q42023_after$Avg_Temp_Cel)


METdata_Mon_Q3_check3 <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 1993"))

# Create a plot for both time periods
combined_plot <- ggplot() +
  #geom_line(data = METdata_Mon_Q42013_before, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "blue") +
  #geom_line(data = METdata_Mon_Q42013_after, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "red") +
  geom_ribbon(
    data = data.frame(
      x = METdata_Mon_Q42013_before$Year_Mon,
      ymin = mean_group1_Q42013 - sd_group1_Q42013,
      ymax = mean_group1_Q42013 + sd_group1_Q42013
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = data.frame(
      x = METdata_Mon_Q42013_after$Year_Mon,
      ymin = mean_group2_Q42013 - sd_group2_Q42013,
      ymax = mean_group2_Q42013 + sd_group2_Q42013
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "red",
    alpha = 0.2
  ) +
  geom_line(
    data = METdata_Mon_Q3_check3,
    aes(x = Year_Mon, y = Avg_Temp_Cel),
    color = "black",
    size = 0.5,
    alpha = 0.5
  ) +
  geom_label(
    data = data.frame(
      x = as.yearmon("Jan 1993"),
      y = max(METdata_Mon_Q42013_before$Avg_Temp_Cel),
      label = sprintf("Variance: %.4f", variance_group1_Q42013)
    ),
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.2,
    color = "blue",
    size = 3
  ) +
  geom_label(
    data = data.frame(
      x = as.yearmon("Jan 2008"),
      y = max(METdata_Mon_Q42013_after$Avg_Temp_Cel),
      label = sprintf("Variance: %.4f", variance_group2_Q42013)
    ),
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.2,
    color = "red",
    size = 3
  ) +
  geom_label(
    data = data.frame(
      x = as.yearmon("Jan 2018"),
      y = max(METdata_Mon_Q42013_after$Avg_Temp_Cel),
      label = sprintf("Variance: %.4f", variance_group2)
    ),
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.2,
    color = "green",
    size = 3
  ) +
  #geom_line(data = METdata_Mon_Q42023_before, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "blue", linetype = "dashed") +
  #geom_line(data = METdata_Mon_Q42023_after, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "red", linetype = "dashed") +
  geom_ribbon(
    data = data.frame(
      x = METdata_Mon_Q42023_before$Year_Mon,
      ymin = mean_group1 - sd_group1,
      ymax = mean_group1 + sd_group1
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = data.frame(
      x = METdata_Mon_Q42023_after$Year_Mon,
      ymin = mean_group2 - sd_group2,
      ymax = mean_group2 + sd_group2
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "red",
    alpha = 0.2
  ) +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Temperature Variation Comparison") +
  theme_minimal()

# Display the combined plot
print(combined_plot)


#Checking if temperatures raise in the 10 year period prior to today
# Create a time series object
NASAdata_tidy_final_Q3_ts <-
  ts(
    NASAdata_tidy_final_Q3$Avg_Temp_Cel,
    start = c(2003, 1),
    end = c(2012, 12),
    frequency = 12
  )

# Perform the Augmented Dickey-Fuller test
adf_test_NASA <- adf.test(NASAdata_tidy_final_Q3_ts)

# Print ADF test results
print(adf_test_NASA)

# Perform KPSS test
kpss_test <- ur.kpss(NASAdata_tidy_final_Q3_ts, type = "tau")

# Print KPSS test results
print(kpss_test)


#NASA Data

#Diving data into two 10 year chunks, Jan 1993 to Dec 2002 (before)
# and Jan 2003 to Dec 2012

NASAdata_Q42013_before <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 1993") &
           Year_Mon <= as.yearmon("Dec 2002"))

NASAdata_Q42013_after <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2003") &
           Year_Mon <= as.yearmon("Dec 2012"))


# Perform Bartlett's test
bartlett_result <-
  bartlett.test(list(
    NASAdata_Q42013_before$Avg_Temp_Cel,
    NASAdata_Q42013_after$Avg_Temp_Cel
  ))

# Print the results
print(bartlett_result)


#Diving data into two 10 year chunks, Jan 2003 to Dec 2012 (before)
# and Jan 2013 to Jun 2023

NASAdata_Q42023_before <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2003") &
           Year_Mon <= as.yearmon("Dec 2012"))

NASAdata_Q42023_after <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2013"))


# Perform Bartlett's test
bartlett_result <-
  bartlett.test(list(
    NASAdata_Q42023_before$Avg_Temp_Cel,
    NASAdata_Q42023_after$Avg_Temp_Cel
  ))

# Print the results
print(bartlett_result)


#Visualization
# Calculate and compare variances
variance_group1_Q42013 <- var(NASAdata_Q42013_before$Avg_Temp_Cel)
variance_group2_Q42013 <- var(NASAdata_Q42013_after$Avg_Temp_Cel)
sd_group1_Q42013 <- sd(NASAdata_Q42013_before$Avg_Temp_Cel)
sd_group2_Q42013 <- sd(NASAdata_Q42013_after$Avg_Temp_Cel)
mean_group1_Q42013 <- mean(NASAdata_Q42013_before$Avg_Temp_Cel)
mean_group2_Q42013 <- mean(NASAdata_Q42013_after$Avg_Temp_Cel)


# Calculate and compare variances
variance_group1 <- var(NASAdata_Q42023_before$Avg_Temp_Cel)
variance_group2 <- var(NASAdata_Q42023_after$Avg_Temp_Cel)
sd_group1 <- sd(NASAdata_Q42023_before$Avg_Temp_Cel)
sd_group2 <- sd(NASAdata_Q42023_after$Avg_Temp_Cel)
mean_group1 <- mean(NASAdata_Q42023_before$Avg_Temp_Cel)
mean_group2 <- mean(NASAdata_Q42023_after$Avg_Temp_Cel)

NASAdata_tidy_final_check3 <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 1993"))

# Create a plot for both time periods
combined_plot <- ggplot() +
  geom_ribbon(
    data = data.frame(
      x = NASAdata_Q42013_before$Year_Mon,
      ymin = mean_group1_Q42013 - sd_group1_Q42013,
      ymax = mean_group1_Q42013 + sd_group1_Q42013
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = data.frame(
      x = NASAdata_Q42013_after$Year_Mon,
      ymin = mean_group2_Q42013 - sd_group2_Q42013,
      ymax = mean_group2_Q42013 + sd_group2_Q42013
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "red",
    alpha = 0.2
  ) +
  geom_line(
    data = NASAdata_tidy_final_check3,
    aes(x = Year_Mon, y = Avg_Temp_Cel),
    color = "black",
    size = 0.5,
    alpha = 0.5
  ) +
  geom_label(
    data = data.frame(
      x = as.yearmon("Jan 1993"),
      y = max(NASAdata_Q42013_before$Avg_Temp_Cel),
      label = sprintf("Variance: %.4f", variance_group1_Q42013)
    ),
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.2,
    color = "blue",
    size = 3
  ) +
  geom_label(
    data = data.frame(
      x = as.yearmon("Jan 2008"),
      y = max(NASAdata_Q42013_after$Avg_Temp_Cel),
      label = sprintf("Variance: %.4f", variance_group2_Q42013)
    ),
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.2,
    color = "red",
    size = 3
  ) +
  geom_label(
    data = data.frame(
      x = as.yearmon("Jan 2018"),
      y = max(NASAdata_Q42023_after$Avg_Temp_Cel),
      label = sprintf("Variance: %.4f", variance_group2)
    ),
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.2,
    color = "green",
    size = 3
  ) +
  geom_ribbon(
    data = data.frame(
      x = NASAdata_Q42023_before$Year_Mon,
      ymin = mean_group1 - sd_group1,
      ymax = mean_group1 + sd_group1
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = data.frame(
      x = NASAdata_Q42023_after$Year_Mon,
      ymin = mean_group2 - sd_group2,
      ymax = mean_group2 + sd_group2
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "red",
    alpha = 0.2
  ) +
  labs(x = "Year", y = "Average Temperature (°C)",
       title = "Temperature Variation Comparison") +
  theme_minimal()

# Display the combined plot
print(combined_plot)


##########Q5 and Q6#########################

#Q5

#MET Data
#Creating Pre 2007 modelling data
METdata_Mon_Q5 <- METdata_Mon_Q12 %>%
  filter(Year_Mon <= as.yearmon("Dec 2006"))

# Creating a subset of data with actual temperatures from 2007-2017
METdata_Mon_Q5_Post <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2007") &
           Year_Mon <= as.yearmon("Dec 2017"))

# Calculate average of 2006 monthly temperature
average_2006_temp <-
  mean(METdata_Mon_Q5$Avg_Temp_Cel[year(METdata_Mon_Q5$Year_Mon) == 2006])
# Add naive_forecast column to the data frame = Avg of 2006 temperatures
METdata_Mon_Q5_Post$naive_forecast <- average_2006_temp

# Predicting using ARIMA and LM + Residual for 2007 - 2017
METdata_Mon_Q5_msts <-
  msts(METdata_Mon_Q5$Avg_Temp_Cel, seasonal.periods = c(12, 48))
METdata_Mon_Q5_lm_msts <- tslm(METdata_Mon_Q5_msts ~ trend + season)
residualsArimaForecast <-
  forecast(
    auto.arima(METdata_Mon_Q5_lm_msts$residuals),
    h = 132,
    level = c(0.8, 0.90)
  )
residualsF <- as.numeric(residualsArimaForecast$mean)
regressionForecast <-
  forecast(METdata_Mon_Q5_lm_msts,
           h = 132,
           level = c(0.8, 0.90))
regressionF <- as.numeric(regressionForecast$mean)
ForecastR <- regressionF + residualsF
METdata_Mon_Q5_Post$Forecast <- ForecastR

# Predicting using SARIMA for 2007 - 2017
METdata_Mon_Q5_ts1 <-
  ts(METdata_Mon_Q5_Post$Avg_Temp_Cel,
     start = 1850,
     frequency = 12)
METdata_Q5_sarima1 <-
  auto.arima(METdata_Mon_Q5_ts1, seasonal = TRUE)
METdata_Q5_SARIMA1_pred <-
  forecast(METdata_Q5_sarima1, h = 132, level = c(0.8, 0.90))
METdata_Q5_SARIMA1R <- as.numeric(METdata_Q5_SARIMA1_pred$mean)
METdata_Mon_Q5_Post$Forecast_SARIMA <- METdata_Q5_SARIMA1R

# Calculate MAPE for Naive forecast, LM + Residual, SARIMA
mape_naive <-
  mean(abs((
    METdata_Mon_Q5_Post$Avg_Temp_Cel - METdata_Mon_Q5_Post$naive_forecast
  ) / METdata_Mon_Q5_Post$Avg_Temp_Cel
  )) * 100
mape_lm_residual <-
  mean(abs((
    METdata_Mon_Q5_Post$Avg_Temp_Cel - METdata_Mon_Q5_Post$Forecast
  ) / METdata_Mon_Q5_Post$Avg_Temp_Cel
  )) * 100
mape_sarima <-
  mean(abs((
    METdata_Mon_Q5_Post$Avg_Temp_Cel - METdata_Mon_Q5_Post$Forecast_SARIMA
  ) / METdata_Mon_Q5_Post$Avg_Temp_Cel
  )) * 100

# Calculate cumulative errors
cumulative_error_naive <-
  sum(abs(
    METdata_Mon_Q5_Post$Avg_Temp_Cel - METdata_Mon_Q5_Post$naive_forecast
  ))
cumulative_error_lm_residual <-
  sum(abs(
    METdata_Mon_Q5_Post$Avg_Temp_Cel - METdata_Mon_Q5_Post$Forecast
  ))

# Calculate CumRAE values
cumulative_relative_error_lm_residual <-
  cumulative_error_lm_residual / cumulative_error_naive
cumulative_relative_error_sarima <-
  cumulative_error_sarima / cumulative_error_naive

# Create a table of results
results_table <- data.frame(
  Method = c("Naive forecast", "LM + Residual"),
  MAPE = c(mape_naive, mape_lm_residual),
  Cumulative_Absolute_Error = c(cumulative_error_naive, cumulative_error_lm_residual),
  Cumulative_Relative_Absolute_Error = c(NA, cumulative_relative_error_lm_residual)
)

# Print the results table
print(results_table)

#Visualization
# Combine the relevant columns into a new dataframe for plotting
combined_data <- data.frame(
  Year_Mon = c(METdata_Mon_Q12$Year_Mon, METdata_Mon_Q5_Post$Year_Mon),
  Avg_Temp_Cel = c(
    METdata_Mon_Q12$Avg_Temp_Cel,
    METdata_Mon_Q5_Post$Avg_Temp_Cel
  ),
  naive_forecast = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q5_Post$naive_forecast),
  Forecast = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q5_Post$Forecast),
  Forecast_SARIMA = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q5_Post$Forecast_SARIMA)
)

# Filter out rows with missing values
combined_data <- combined_data[complete.cases(combined_data),]

# Create the line chart
ggplot(combined_data, aes(x = Year_Mon)) +
  #geom_line(data = METdata_Mon_Q5, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast),
    color = "green",
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(y = Forecast),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal()


#NASA Data
#Creating Pre 2007 modelling data
NASAdata_Mon_Q5 <- NASAdata_tidy_final %>%
  filter(Year_Mon <= as.yearmon("Dec 2006"))

# Creating a subset of data with actual temperatures from 2007-2017
NASAdata_Mon_Q5_Post <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2007") &
           Year_Mon <= as.yearmon("Dec 2017"))

# Calculate average of 2007 monthly temperatures
average_2006_temp <-
  mean(NASAdata_Mon_Q5$Avg_Temp_Cel[year(NASAdata_Mon_Q5$Year_Mon) == 2006])
# Add naive_forecast column to the data frame
NASAdata_Mon_Q5_Post$naive_forecast <- average_2006_temp

# Predicting using ARIMA and LM + Residual for 2007 - 2017
NASAdata_Mon_Q5_msts <-
  msts(NASAdata_Mon_Q5$Avg_Temp_Cel, seasonal.periods = c(12, 48))
NASAdata_Mon_Q5_lm_msts <-
  tslm(NASAdata_Mon_Q5_msts ~ trend + season)
residualsArimaForecast <-
  forecast(
    auto.arima(NASAdata_Mon_Q5_lm_msts$residuals),
    h = 132,
    level = c(0.8, 0.90)
  )
residualsF <- as.numeric(residualsArimaForecast$mean)
regressionForecast <-
  forecast(NASAdata_Mon_Q5_lm_msts,
           h = 132,
           level = c(0.8, 0.90))
regressionF <- as.numeric(regressionForecast$mean)
ForecastR <- regressionF + residualsF
NASAdata_Mon_Q5_Post$Forecast <- ForecastR

# Predicting using SARIMA for 2007 - 2017
NASAdata_Mon_Q5_ts1 <-
  ts(NASAdata_Mon_Q5$Avg_Temp_Cel,
     start = 1850,
     frequency = 12)
NASAdata_Q5_sarima1 <-
  auto.arima(NASAdata_Mon_Q5_ts1, seasonal = TRUE)
NASAdata_Q5_SARIMA1_pred <-
  forecast(NASAdata_Q5_sarima1,
           h = 132,
           level = c(0.8, 0.90))
NASAdata_Q5_SARIMA1R <- as.numeric(NASAdata_Q5_SARIMA1_pred$mean)
NASAdata_Mon_Q5_Post$Forecast_SARIMA <- NASAdata_Q5_SARIMA1R

# Calculate MAPE for Naive forecast, LM + Residual, SARIMA
mape_naive <-
  mean(abs((
    NASAdata_Mon_Q5_Post$Avg_Temp_Cel - NASAdata_Mon_Q5_Post$naive_forecast
  ) / NASAdata_Mon_Q5_Post$Avg_Temp_Cel
  )) * 100
mape_lm_residual <-
  mean(abs((
    NASAdata_Mon_Q5_Post$Avg_Temp_Cel - NASAdata_Mon_Q5_Post$Forecast
  ) / NASAdata_Mon_Q5_Post$Avg_Temp_Cel
  )) * 100
mape_sarima <-
  mean(abs((
    NASAdata_Mon_Q5_Post$Avg_Temp_Cel - NASAdata_Mon_Q5_Post$Forecast_SARIMA
  ) / NASAdata_Mon_Q5_Post$Avg_Temp_Cel
  )) * 100

# Calculate cumulative errors
cumulative_error_naive <-
  sum(abs(
    NASAdata_Mon_Q5_Post$Avg_Temp_Cel - NASAdata_Mon_Q5_Post$naive_forecast
  ))
cumulative_error_lm_residual <-
  sum(abs(
    NASAdata_Mon_Q5_Post$Avg_Temp_Cel - NASAdata_Mon_Q5_Post$Forecast
  ))
#cumulative_error_sarima <- sum(abs(NASAdata_Mon_Q5_Post$Avg_Temp_Cel - NASAdata_Mon_Q5_Post$Forecast_SARIMA))

# Calculate CumRAE values
cumulative_relative_error_lm_residual <-
  cumulative_error_lm_residual / cumulative_error_naive
cumulative_relative_error_sarima <-
  cumulative_error_sarima / cumulative_error_naive

# Create a table of results
results_table <- data.frame(
  Method = c("Naive forecast", "LM + Residual"),
  MAPE = c(mape_naive, mape_lm_residual),
  Cumulative_Absolute_Error = c(cumulative_error_naive, cumulative_error_lm_residual),
  Cumulative_Relative_Absolute_Error = c(NA, cumulative_relative_error_lm_residual)
)

# Print the results table
print(results_table)

#Visualization
# Combine the relevant columns into a new dataframe for plotting
combined_data <- data.frame(
  Year_Mon = c(NASAdata_tidy_final$Year_Mon, NASAdata_Mon_Q5_Post$Year_Mon),
  Avg_Temp_Cel = c(
    NASAdata_tidy_final$Avg_Temp_Cel,
    NASAdata_Mon_Q5_Post$Avg_Temp_Cel
  ),
  naive_forecast = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q5_Post$naive_forecast),
  Forecast = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q5_Post$Forecast),
  Forecast_SARIMA = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q5_Post$Forecast_SARIMA)
)

# Filter out rows with missing values
combined_data <- combined_data[complete.cases(combined_data),]

# Create the line chart
ggplot(combined_data, aes(x = Year_Mon)) +
  #geom_line(data = NASAdata_Mon_Q5, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast),
    color = "green",
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(y = Forecast),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal()


#Q6
#MET Data
#10 year time intervals
#Creating Pre 2000 modelling data
METdata_Mon_Q6 <- METdata_Mon_Q12 %>%
  filter(Year_Mon <= as.yearmon("Dec 1999"))

# Creating a subset of data with actual temperatures from 2000 for a 10 year time interval
METdata_Mon_Q6_10 <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2000") &
           Year_Mon <= as.yearmon("Dec 2010"))

# Calculate average of 1999 monthly temperatures
average_1999_temp <-
  mean(METdata_Mon_Q6$Avg_Temp_Cel[year(METdata_Mon_Q6$Year_Mon) == 1999])
# Add naive_forecast column to the data frame
METdata_Mon_Q6_10$naive_forecast <- average_1999_temp

# Predicting using ARIMA and LM + Residual for 2007 - 2017
METdata_Mon_Q6_msts <-
  msts(METdata_Mon_Q6$Avg_Temp_Cel, seasonal.periods = c(12, 48))
METdata_Mon_Q6_lm_msts <- tslm(METdata_Mon_Q6_msts ~ trend + season)
residualsArimaForecast <-
  forecast(
    auto.arima(METdata_Mon_Q6_lm_msts$residuals),
    h = 132,
    level = c(0.8, 0.90)
  )
residualsF <- as.numeric(residualsArimaForecast$mean)
regressionForecast <-
  forecast(METdata_Mon_Q6_lm_msts,
           h = 132,
           level = c(0.8, 0.90))
regressionF <- as.numeric(regressionForecast$mean)
ForecastR <- regressionF + residualsF
METdata_Mon_Q6_10$Forecast <- ForecastR

# Calculate MAPE for Naive forecast, LM + Residual, SARIMA
mape_naive <-
  mean(abs((
    METdata_Mon_Q6_10$Avg_Temp_Cel - METdata_Mon_Q6_10$naive_forecast
  ) / METdata_Mon_Q6_10$Avg_Temp_Cel
  )) * 100
mape_lm_residual <-
  mean(abs((METdata_Mon_Q6_10$Avg_Temp_Cel - METdata_Mon_Q6_10$Forecast) / METdata_Mon_Q6_10$Avg_Temp_Cel
  )) * 100

# Calculate cumulative errors
cumulative_error_naive <-
  sum(abs(
    METdata_Mon_Q6_10$Avg_Temp_Cel - METdata_Mon_Q6_10$naive_forecast
  ))
cumulative_error_lm_residual <-
  sum(abs(METdata_Mon_Q6_10$Avg_Temp_Cel - METdata_Mon_Q6_10$Forecast))

# Calculate CumRAE values
cumulative_relative_error_lm_residual <-
  cumulative_error_lm_residual / cumulative_error_naive

# Create a table of results
results_table <- data.frame(
  Method = c("Naive forecast", "LM + Residual"),
  MAPE = c(mape_naive, mape_lm_residual),
  Cumulative_Absolute_Error = c(cumulative_error_naive, cumulative_error_lm_residual),
  Cumulative_Relative_Absolute_Error = c(NA, cumulative_relative_error_lm_residual)
)

# Print the results table
print(results_table)

#Visualization
# Combine the relevant columns into a new dataframe for plotting
combined_data <- data.frame(
  Year_Mon = c(METdata_Mon_Q12$Year_Mon, METdata_Mon_Q6_10$Year_Mon),
  Avg_Temp_Cel = c(
    METdata_Mon_Q12$Avg_Temp_Cel,
    METdata_Mon_Q6_10$Avg_Temp_Cel
  ),
  naive_forecast = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q6_10$naive_forecast),
  Forecast = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q6_10$Forecast)
)

# Filter out rows with missing values
combined_data <- combined_data[complete.cases(combined_data),]

# Create the line chart
ggplot(combined_data, aes(x = Year_Mon)) +
  #geom_line(data = METdata_Mon_Q5, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast),
    color = "green",
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(y = Forecast),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal()

#20 year time intervals
# Creating a subset of data with actual temperatures from 2000 for a 10 year time interval
METdata_Mon_Q6_20 <- METdata_Mon_Q12 %>%
  filter(Year_Mon >= as.yearmon("Jan 2000") &
           Year_Mon <= as.yearmon("Dec 2020"))
#Adding the naive forecast column
METdata_Mon_Q6_20$naive_forecast <- average_1999_temp

# Predicting using ARIMA and LM + Residual for 2007 - 2017
METdata_Mon_Q6_msts <-
  msts(METdata_Mon_Q6$Avg_Temp_Cel, seasonal.periods = c(12, 48))
METdata_Mon_Q6_lm_msts <- tslm(METdata_Mon_Q6_msts ~ trend + season)
residualsArimaForecast <-
  forecast(
    auto.arima(METdata_Mon_Q6_lm_msts$residuals),
    h = 252,
    level = c(0.8, 0.90)
  )
residualsF <- as.numeric(residualsArimaForecast$mean)
regressionForecast <-
  forecast(METdata_Mon_Q6_lm_msts,
           h = 252,
           level = c(0.8, 0.90))
regressionF <- as.numeric(regressionForecast$mean)
ForecastR <- regressionF + residualsF
METdata_Mon_Q6_20$Forecast <- ForecastR

# Calculate MAPE for Naive forecast, LM + Residual, SARIMA
mape_naive <-
  mean(abs((
    METdata_Mon_Q6_20$Avg_Temp_Cel - METdata_Mon_Q6_20$naive_forecast
  ) / METdata_Mon_Q6_20$Avg_Temp_Cel
  )) * 100
mape_lm_residual <-
  mean(abs((METdata_Mon_Q6_20$Avg_Temp_Cel - METdata_Mon_Q6_20$Forecast) / METdata_Mon_Q6_20$Avg_Temp_Cel
  )) * 100

# Calculate cumulative errors
cumulative_error_naive <-
  sum(abs(
    METdata_Mon_Q6_20$Avg_Temp_Cel - METdata_Mon_Q6_20$naive_forecast
  ))
cumulative_error_lm_residual <-
  sum(abs(METdata_Mon_Q6_20$Avg_Temp_Cel - METdata_Mon_Q6_20$Forecast))

# Calculate CumRAE values
cumulative_relative_error_lm_residual <-
  cumulative_error_lm_residual / cumulative_error_naive

# Create a table of results
results_table <- data.frame(
  Method = c("Naive forecast", "LM + Residual"),
  MAPE = c(mape_naive, mape_lm_residual),
  Cumulative_Absolute_Error = c(cumulative_error_naive, cumulative_error_lm_residual),
  Cumulative_Relative_Absolute_Error = c(NA, cumulative_relative_error_lm_residual)
)

# Print the results table
print(results_table)

#Visualization
# Combine the relevant columns into a new dataframe for plotting
combined_data <- data.frame(
  Year_Mon = c(METdata_Mon_Q12$Year_Mon, METdata_Mon_Q6_20$Year_Mon),
  Avg_Temp_Cel = c(
    METdata_Mon_Q12$Avg_Temp_Cel,
    METdata_Mon_Q6_20$Avg_Temp_Cel
  ),
  naive_forecast = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q6_20$naive_forecast),
  Forecast = c(rep(NA, nrow(METdata_Mon_Q12)), METdata_Mon_Q6_20$Forecast)
)

# Filter out rows with missing values
combined_data <- combined_data[complete.cases(combined_data),]

# Create the line chart
ggplot(combined_data, aes(x = Year_Mon)) +
  #geom_line(data = METdata_Mon_Q5, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast),
    color = "green",
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(y = Forecast),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal()


#NASA Data
#10 year time intervals
#Creating Pre 2000 modelling data
NASAdata_Mon_Q6 <- NASAdata_tidy_final %>%
  filter(Year_Mon <= as.yearmon("Dec 1999"))

# Creating a subset of data with actual temperatures from 2000 for a 10 year time interval
NASAdata_Mon_Q6_10 <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2000") &
           Year_Mon <= as.yearmon("Dec 2010"))

# Calculate average of 1999 monthly temperatures
average_1999_temp <-
  mean(NASAdata_Mon_Q6$Avg_Temp_Cel[year(NASAdata_Mon_Q6$Year_Mon) == 1999])
# Add naive_forecast column to the data frame
NASAdata_Mon_Q6_10$naive_forecast <- average_1999_temp

# Predicting using ARIMA and LM + Residual for 2007 - 2017
NASAdata_Mon_Q6_msts <-
  msts(NASAdata_Mon_Q6$Avg_Temp_Cel, seasonal.periods = c(12, 48))
NASAdata_Mon_Q6_lm_msts <-
  tslm(NASAdata_Mon_Q6_msts ~ trend + season)
residualsArimaForecast <-
  forecast(
    auto.arima(NASAdata_Mon_Q6_lm_msts$residuals),
    h = 132,
    level = c(0.8, 0.90)
  )
residualsF <- as.numeric(residualsArimaForecast$mean)
regressionForecast <-
  forecast(NASAdata_Mon_Q6_lm_msts,
           h = 132,
           level = c(0.8, 0.90))
regressionF <- as.numeric(regressionForecast$mean)
ForecastR <- regressionF + residualsF
NASAdata_Mon_Q6_10$Forecast <- ForecastR

# Calculate MAPE for Naive forecast, LM + Residual, SARIMA
mape_naive <-
  mean(abs((
    NASAdata_Mon_Q6_10$Avg_Temp_Cel - NASAdata_Mon_Q6_10$naive_forecast
  ) / NASAdata_Mon_Q6_10$Avg_Temp_Cel
  )) * 100
mape_lm_residual <-
  mean(abs((
    NASAdata_Mon_Q6_10$Avg_Temp_Cel - NASAdata_Mon_Q6_10$Forecast
  ) / NASAdata_Mon_Q6_10$Avg_Temp_Cel
  )) * 100

# Calculate cumulative errors
cumulative_error_naive <-
  sum(abs(
    NASAdata_Mon_Q6_10$Avg_Temp_Cel - NASAdata_Mon_Q6_10$naive_forecast
  ))
cumulative_error_lm_residual <-
  sum(abs(
    NASAdata_Mon_Q6_10$Avg_Temp_Cel - NASAdata_Mon_Q6_10$Forecast
  ))

# Calculate CumRAE values
cumulative_relative_error_lm_residual <-
  cumulative_error_lm_residual / cumulative_error_naive

# Create a table of results
results_table <- data.frame(
  Method = c("Naive forecast", "LM + Residual"),
  MAPE = c(mape_naive, mape_lm_residual),
  Cumulative_Absolute_Error = c(cumulative_error_naive, cumulative_error_lm_residual),
  Cumulative_Relative_Absolute_Error = c(NA, cumulative_relative_error_lm_residual)
)

# Print the results table
print(results_table)

#Visualization
# Combine the relevant columns into a new dataframe for plotting
combined_data <- data.frame(
  Year_Mon = c(NASAdata_tidy_final$Year_Mon, NASAdata_Mon_Q6_10$Year_Mon),
  Avg_Temp_Cel = c(
    NASAdata_tidy_final$Avg_Temp_Cel,
    NASAdata_Mon_Q6_10$Avg_Temp_Cel
  ),
  naive_forecast = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q6_10$naive_forecast),
  Forecast = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q6_10$Forecast)
)

# Filter out rows with missing values
combined_data <- combined_data[complete.cases(combined_data),]

# Create the line chart
ggplot(combined_data, aes(x = Year_Mon)) +
  #geom_line(data = METdata_Mon_Q5, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast),
    color = "green",
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(y = Forecast),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal()

#20 year time intervals
# Creating a subset of data with actual temperatures from 2000 for a 10 year time interval
NASAdata_Mon_Q6_20 <- NASAdata_tidy_final %>%
  filter(Year_Mon >= as.yearmon("Jan 2000") &
           Year_Mon <= as.yearmon("Dec 2020"))
#Adding the naive forecast column
NASAdata_Mon_Q6_20$naive_forecast <- average_1999_temp

# Predicting using ARIMA and LM + Residual for 2007 - 2017
NASAdata_Mon_Q6_msts <-
  msts(NASAdata_Mon_Q6$Avg_Temp_Cel, seasonal.periods = c(12, 48))
NASAdata_Mon_Q6_lm_msts <-
  tslm(NASAdata_Mon_Q6_msts ~ trend + season)
residualsArimaForecast <-
  forecast(
    auto.arima(NASAdata_Mon_Q6_lm_msts$residuals),
    h = 252,
    level = c(0.8, 0.90)
  )
residualsF <- as.numeric(residualsArimaForecast$mean)
regressionForecast <-
  forecast(NASAdata_Mon_Q6_lm_msts,
           h = 252,
           level = c(0.8, 0.90))
regressionF <- as.numeric(regressionForecast$mean)
ForecastR <- regressionF + residualsF
NASAdata_Mon_Q6_20$Forecast <- ForecastR

# Calculate MAPE for Naive forecast, LM + Residual, SARIMA
mape_naive <-
  mean(abs((
    NASAdata_Mon_Q6_20$Avg_Temp_Cel - NASAdata_Mon_Q6_20$naive_forecast
  ) / NASAdata_Mon_Q6_20$Avg_Temp_Cel
  )) * 100
mape_lm_residual <-
  mean(abs((
    NASAdata_Mon_Q6_20$Avg_Temp_Cel - NASAdata_Mon_Q6_20$Forecast
  ) / NASAdata_Mon_Q6_20$Avg_Temp_Cel
  )) * 100

# Calculate cumulative errors
cumulative_error_naive <-
  sum(abs(
    NASAdata_Mon_Q6_20$Avg_Temp_Cel - NASAdata_Mon_Q6_20$naive_forecast
  ))
cumulative_error_lm_residual <-
  sum(abs(
    NASAdata_Mon_Q6_20$Avg_Temp_Cel - NASAdata_Mon_Q6_20$Forecast
  ))

# Calculate CumRAE values
cumulative_relative_error_lm_residual <-
  cumulative_error_lm_residual / cumulative_error_naive

# Create a table of results
results_table <- data.frame(
  Method = c("Naive forecast", "LM + Residual"),
  MAPE = c(mape_naive, mape_lm_residual),
  Cumulative_Absolute_Error = c(cumulative_error_naive, cumulative_error_lm_residual),
  Cumulative_Relative_Absolute_Error = c(NA, cumulative_relative_error_lm_residual)
)

# Print the results table
print(results_table)

#Visualization
# Combine the relevant columns into a new dataframe for plotting
combined_data <- data.frame(
  Year_Mon = c(NASAdata_tidy_final$Year_Mon, NASAdata_Mon_Q6_20$Year_Mon),
  Avg_Temp_Cel = c(
    NASAdata_tidy_final$Avg_Temp_Cel,
    NASAdata_Mon_Q6_20$Avg_Temp_Cel
  ),
  naive_forecast = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q6_20$naive_forecast),
  Forecast = c(rep(NA, nrow(
    NASAdata_tidy_final
  )), NASAdata_Mon_Q6_20$Forecast)
)

# Filter out rows with missing values
combined_data <- combined_data[complete.cases(combined_data),]

# Create the line chart
ggplot(combined_data, aes(x = Year_Mon)) +
  #geom_line(data = METdata_Mon_Q5, aes(x = Year_Mon, y = Avg_Temp_Cel), color = "black", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast),
    color = "green",
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(y = Forecast),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal()


ggplot(combined_data, aes(x = Year_Mon)) +
  geom_line(aes(y = Avg_Temp_Cel), color = "black") +
  geom_line(
    aes(y = naive_forecast, color = "Naive-forecast model"),
    linetype = "dashed",
    size = 1
  ) +
  geom_line(aes(y = Forecast, color = "Team Chernoff Model"),
            linetype = "dashed",
            size = 1) +
  labs(x = "Year_Mon", y = "Average Temperature (°C)", title = "Temperature Forecast Comparison") +
  theme_minimal() +
  scale_color_manual(values = c("green", "red"))

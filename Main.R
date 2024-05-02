library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)
library(zoo)
library(sf)
library(tidyr)
library(dplyr)
library(corrplot)
library(reshape)
df <- read.csv("C:\\Users\\<USERNAME>\\location\\to\\dataset\\bodyfat.csv")
head(df)

# Plotting the correlation matrix between all the feature columns
res <- cor(df)
round(res, 2)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE, mar = c(5, 2), main = "Hierarchical Clustered HeatMap of Feature Columns")

corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, mar = c(1, 1, 3, 1))
title(main = "Corr Plot of Feature Columns")


# Model for normal Linear Regression
model_normal <- lm(df$BodyFat ~ df$Abdomen)
summary(model_normal)

#################################################################################

## Our proposed Regression model

data <- subset(df, select = c(Abdomen, BodyFat))
head(data)

window_size <- 3

# Function to perform linear regression on a window of data
linear_regression_window <- function(window_data) {
  # Fit linear regression model
  model <- lm(BodyFat ~ Abdomen, data = window_data)
  
  # Get the coefficients (slope and intercept)
  a <- coef(model)[2]  # Slope (a)
  b <- coef(model)[1]  # Intercept (b)
  
  # Calculate RMSE for the window
  rmse <- summary(model)$sigma
  
  return(c(a, b, rmse))  # Return slope, intercept, and RMSE
}

# Initialize empty arrays for coefficients, RMSE, and residuals
a_values <- numeric(0)
b_values <- numeric(0)
rmse_values <- numeric(0)

# Iterate through the dataset using the sliding window
for (i in 1:(nrow(data) - window_size + 1)) {
  # Extract the current window
  window_data <- data[i:(i + window_size - 1), ]
  
  if (nrow(window_data) > 0) {
    # Calculate coefficients and RMSE for the window
    coefficients <- linear_regression_window(window_data)
    a_values <- c(a_values, coefficients[1])
    b_values <- c(b_values, coefficients[2])
    rmse_values <- c(rmse_values, coefficients[3])
    
    # Predict BodyFat values for the window data
    predicted_bodyfat <- coefficients[1] * window_data$Abdomen + coefficients[2]
  }
}

avg_a <- mean(a_values)
avg_b <- mean(b_values)
avg_rmse <- mean(rmse_values)
r_squared <- 1 - avg_rmse^2 / var(data$BodyFat)

# Calculate predicted BodyFat values for the entire dataset using average coefficients
predicted_bodyfat <- avg_a * data$Abdomen[-c(1, nrow(data))] + avg_b

# Calculate residuals for the entire dataset
residual_values <- data$BodyFat[-c(1, nrow(data))] - predicted_bodyfat

# Print the results
cat("Average slope (a):", avg_a, "\n")
cat("Average intercept (b):", avg_b, "\n")
cat("Average RMSE:", avg_rmse, "\n")
cat("R-SqauredMSE:", r_squared, "\n")
cat("Residuals:\n")
summary(residual_values)

# Scatter Plot with Regression Line for modified method
plot(df$Abdomen, df$BodyFat, xlab = "Abdomen", ylab = "BodyFat", main = "Scatter Plot with Regression Lines")
abline(model_normal, col = "blue")
abline(avg_b, avg_a, col = "red")
legend("topleft", legend = c("Linear Regression", "AWR Model"), col = c("blue", "red"), lty = 1)

# Residual Plot for normal linear regression model
plot(df$Abdomen, residuals(model_normal), xlab = "Abdomen", ylab = "Residuals", main = "Residual Plot for Linear Regression Model")

# Residual Plot for modified method
plot(df$Abdomen[1:(nrow(df) - window_size + 1)], residual_values, xlab = "Abdomen", ylab = "Residuals", main = "Residual Plot for AWR Model")

# Histogram of Residuals for normal linear regression model
hist(residuals(model_normal), xlab = "Residuals", main = "Histogram of Residuals from Linear Regression Model")

# Histogram of Residuals for modified method
hist(residual_values, xlab = "Residuals", main = "Histogram of Residuals from AWR Model")

# QQ Plot for normal linear regression model
qqnorm(residuals(model_normal), main = "QQ Plot for Linear Regression Model")
qqline(residuals(model_normal))

# QQ Plot for modified method
qqnorm(residual_values, main = "QQ Plot for AWR Model")
qqline(residual_values)

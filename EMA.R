# BDA400 Assignment 5
# Exponential Moving Average (EMA)
# Student: Parvaneh Rezaei

ema <- function(data, period) {
  
  # Check if data is long enough
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Calculate smoothing factor
  alpha <- 2 / (period + 1)
  
  # Initialize EMA vector
  ema_values <- numeric(length(data))
  
  # First EMA value
  ema_values[1] <- data[1]
  
  # Calculate remaining EMA values
  for (i in 2:length(data)) {
    ema_values[i] <- alpha * data[i] +
      (1 - alpha) * ema_values[i - 1]
  }
  
  return(ema_values)
}

# Test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
ema_result <- ema(data, period = 3)
print(ema_result)
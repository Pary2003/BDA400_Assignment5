# BDA400 Assignment 5
# Simple Moving Average (SMA)
# Student: Parvaneh Rezaei

sma <- function(data, period) {
  
  # Check if data is long enough
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize vector to store SMA values
  sma_values <- numeric()
  
  # Calculate SMA for each window
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}

# Test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
sma_result <- sma(data, period = 3)
print(sma_result)
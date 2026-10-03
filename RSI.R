# BDA400 Assignment 5
# Relative Strength Index (RSI)
# Student: Parvaneh Rezaei

rsi <- function(data, period) {
  
  # Calculate differences between consecutive values
  diff_values <- diff(data)
  
  # Initialize gains and losses
  gains <- rep(0, length(diff_values))
  losses <- rep(0, length(diff_values))
  
  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate initial average gain and loss
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  # Initialize RSI vector
  rsi_values <- rep(NA, length(data))
  
  # Calculate RSI using Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    
    rs <- avg_gain / avg_loss
    
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  
  return(rsi_values)
}

# Test
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

rsi_result <- rsi(data, period = 5)

print(rsi_result)
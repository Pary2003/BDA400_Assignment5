# BDA400 Assignment 5
# Standard Deviation (stdev)
# Student: Parvaneh Rezaei

stdev <- function(data) {
  
  # Calculate the mean
  mean_value <- sum(data) / length(data)
  
  # Calculate squared differences from the mean
  squared_differences <- (data - mean_value)^2
  
  # Calculate variance
  variance <- sum(squared_differences) / length(data)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}

# Test
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

stdev_result <- stdev(data)

print(stdev_result)

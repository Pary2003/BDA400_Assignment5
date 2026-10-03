# BDA400 Assignment 5
# Linear Regression
# Student: Parvaneh Rezaei

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  # Calculate the total number of elements in regressionSource
  n <- length(regressionSource)
  
  # Check if regressionLength is greater than the number of elements
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Calculate the starting index
  start_index <- max(1, n - regressionLength + regressionOffset)
  
  # Calculate the ending index
  end_index <- min(n, n - regressionOffset)
  
  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]
  
  # Calculate the index values
  index_values <- 1:length(source_subset)
  
  # Calculate sums
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  
  # Calculate means
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)
  
  # Calculate numerator and denominator
  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  
  denominator <- sum((index_values - mean_index)^2)
  
  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return slope, intercept, and predicted values
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}

# Test
data <- c(10, 20, 30, 40, 50, 60)

linreg_result <- linreg(
  data,
  regressionLength = 3,
  regressionOffset = 0
)

print(linreg_result)
# BDA400 Assignment 5
# Crossover Function
# Student: Parvaneh Rezaei

crossover <- function(arr1, arr2) {
  
  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize crossover signals
  crossover_signals <- rep("None", length(arr1))
  
  # First value has no previous point
  crossover_signals[1] <- "None"
  
  # Check for crossovers
  for (i in 2:length(arr1)) {
    
    if (arr1[i] > arr2[i] &&
        arr1[i - 1] <= arr2[i - 1]) {
      
      crossover_signals[i] <- "Up"
      
    } else if (arr1[i] < arr2[i] &&
               arr1[i - 1] >= arr2[i - 1]) {
      
      crossover_signals[i] <- "Down"
      
    } else {
      
      crossover_signals[i] <- "None"
    }
  }
  
  return(crossover_signals)
}

# Test
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

crossover_signals <- crossover(arr1, arr2)

print(crossover_signals)
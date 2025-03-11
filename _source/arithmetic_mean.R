arithmean <- function(x) {
  # Remove values that are 0 or less and NAs
  x_filtered <- x[x >= 0 & !is.na(x)]
  
  # Count non-NA, positive values
  n <- length(x_filtered)
  
  # Check if there are enough values to compute the mean
  if (n == 0) {
    return("No valid values to compute arithmetic mean")
  }
  
  # Compute mean and standard deviation ignoring NA values
  mean_val <- mean(x_filtered, na.rm = TRUE)
  sd_val <- sd(x_filtered, na.rm = TRUE)
  
  # Include the count in the output
  paste0("n = ", n, "; ", round(mean_val, 2), " +/- ", round(sd_val, 2))
}

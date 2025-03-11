medians <- function(x) {
  # Remove values that are negative and NAs
  x_filtered <- x[x >= 0 & !is.na(x)]
  
  # Count non-NA, positive values
  n <- length(x_filtered)
  
  # Check if there are enough values to compute statistics
  if (n == 0) {
    return("No valid values to compute median and IQR")
  }
  
  # Compute median and IQR ignoring NA values
  med <- median(x_filtered, na.rm = TRUE)
  iqr_low <- quantile(x_filtered, 0.25, na.rm = TRUE)
  iqr_up <- quantile(x_filtered, 0.75, na.rm = TRUE)
  
  # Include the count in the output
  paste0("n = ", n, "; ", round(med, 2), " (", round(iqr_low, 2), "-", round(iqr_up, 2), ")")
}

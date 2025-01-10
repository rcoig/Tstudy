medians <- function(x) {
  # Count non-NA values
  n <- sum(!is.na(x))
  
  # Compute median and IQR ignoring NA values
  med <- median(x, na.rm = TRUE)
  iqr_low <- quantile(x, 0.25, na.rm = TRUE)
  iqr_up <- quantile(x, 0.75, na.rm = TRUE)
  
  # Include the count in the output
  paste0("n = ", n, "; ", round(med, 2), " (", round(iqr_low, 2), "-", round(iqr_up, 2), ")")
}
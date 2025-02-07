geo95 <- function(x) {
  # Remove values that are 0 or less and NAs
  x_filtered <- x[x > 0 & !is.na(x)]
  
  # Count non-NA, positive values
  n <- length(x_filtered)
  
  # Check if there are enough values to compute the geometric mean
  if (n == 0) {
    return("No valid values to compute geometric mean")
  }
  
  aggr <- ci.mean(x_filtered, statistic = "geometric", alpha = 0.05, na.rm = TRUE)
  
  # Include the count in the output
  paste0("n = ", n, "; ", round(aggr$geomean, 2), " (", round(aggr$lower, 2), "-", round(aggr$upper, 2), ")")
}

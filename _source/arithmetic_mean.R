arithmean <- function(x) {
  # Count non-NA values
  n <- sum(!is.na(x))
  
  # Compute median and IQR ignoring NA values
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)

  # Include the count in the output
  paste0("n = ", n, "; ", round(mean, 2), " +/- ", round(sd, 2))
}
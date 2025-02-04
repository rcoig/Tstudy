geo <- function(x) 
{  # Count non-NA values
  n <- sum(!is.na(x))
  
  aggr <- ci.mean(x, statistic = "geometric", na.rm = TRUE)
  
  # Include the count in the output
  paste0("n = ", n, "; ", round(aggr$geomean, 2), " (", round(aggr$lower, 2), "-", round(aggr$upper,2), ")")}
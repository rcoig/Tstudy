geo <- function(x) 
{aggr <- ci.mean(x, statistic = "geometric", na.rm = TRUE)
paste0(round(aggr$geomean, 2), " (", round(aggr$lower, 2), "-", round(aggr$upper,2), ")")}
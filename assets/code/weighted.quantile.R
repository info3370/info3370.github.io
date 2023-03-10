# Function to estimate quantiles with sampling weights
# x is the variable being summarized (e.g., wealth)
# q is the quantile (e.g., median is q = 0.5)
# w is the survey weights
weighted.quantile <- function(x, q = .5, w = rep(1,length(x))) {
  # Sort the weights by x
  w <- w[order(x)]
  # Sort x by x
  x <- x[order(x)]
  # Calculate the cumulative distribution function
  # at each observed data point
  cdf <- cumsum(w) / sum(w)
  # Find the first index where the CDF exceeds the quantile cutoff
  first_index <- min(which(cdf > q))
  # Return the x-value at that index
  return(x[first_index])
}

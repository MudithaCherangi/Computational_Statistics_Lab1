myvar <- function(X) {
  n <- length(X)
  if (n <= 1) {
    stop("Sample size must be greater than 1.")
  }
  
  sum_xi_squared <- sum(X^2)
  sum_xi <- sum(X)
  myvar_x <- (1 / (n - 1)) * (sum_xi_squared - (1 / n) * sum_xi^2)
  
  return(myvar_x)
}

# Generate random data x
x <- rnorm(10000, mean = 10^8, sd = 1)

# Initialize vectors to store results
Y <- numeric(length(x))

# Calculate Yi for each subset Xi
for (i in 1:length(x)) {
  subset_x <- x[1:i]
  if (length(subset_x) > 1) {  # Check if sample size is greater than 1
    custom_variance <- myvar(subset_x)
    standard_variance <- var(subset_x)
    Y[i] <- custom_variance - standard_variance
  }
}

# Plot the dependence of Yi on i
plot(1:length(x), Y, type = "p", xlab = "Subset Size (i)", ylab = "Yi (Difference)")
abline(h = 0, col = "red")  # Add a reference line at Y = 0

#better variance estimator
better_var <- function(X) {
  n <- length(X)
  if (n <= 1) {
    stop("Sample size must be greater than 1.")
  }
  
  mean_x <- mean(X)
  better_var_x <- sum((X - mean_x)^2) / (n - 1)
  
  return(better_var_x)
}


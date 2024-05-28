data <- scan("history.txt")

sample_mean <- mean(data)
sample_size <- length(data)

sigma <- 5
confidence_level <- 0.95
Z <- qnorm((1 + confidence_level) / 2)

margin_of_error <- Z * sigma / sqrt(sample_size)
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

cat("Intervalul de încredere de 95% este [", lower_bound, ", ", upper_bound, "]\n")


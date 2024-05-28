selection_mean <- function(filename) {
  x <- scan(filename)
  m <- mean(x)
  n <- length(x)

  return(list(mean = m, sample_size = n))
}

result <- selection_mean("history.txt")

m <- result$mean
n <- result$sample_size
sigma <- 5

error_margin <- qnorm(0.975) * (sigma / sqrt(n))
confidence_interval <- c(m - error_margin, m + error_margin)

cat("Media de selecție:", m, "\n")
cat("Dimensiunea eșantionului:", n, "\n")
cat("Intervalul de încredere de 95%:", confidence_interval, "\n")

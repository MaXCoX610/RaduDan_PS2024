#II.6
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


#III.4
selection_stats <- function(filename) {
  x = scan(filename)
  m = mean(x)
  s = sd(x)
  n = length(x)
  return(list(mean = m, sd = s, size = n))
}

confidence_intervals <- function(stats, confidence_level) {
  m = stats$mean
  s = stats$sd
  n = stats$size
  error_margin = qnorm(1 - (1 - confidence_level) / 2) * s / sqrt(n)
  lower_bound = m - error_margin
  upper_bound = m + error_margin
  return(c(lower_bound, upper_bound))
}

stats <- selection_stats("history.txt")

confidence_95 <- confidence_intervals(stats, 0.95)
cat("Intervalul de incredere de 95%: [", confidence_95[1], ", ", confidence_95[2], "]\n")

confidence_99 <- confidence_intervals(stats, 0.99)
cat("Intervalul de incredere de 99%: [", confidence_99[1], ", ", confidence_99[2], "]\n")

#IV.1
test_proportion <- function(alpha, n, num_successes, p0) {
  p_hat = num_successes / n

  z = (p_hat - p0) / sqrt(p0 * (1 - p0) / n)

  z_critical = qnorm(1 - alpha)
  
  return(list(z = z, z_critical = z_critical))
}

#IV.2
# Parametrii problemei
alpha = 0.05
n = 150
num_successes = 20
p0 = 0.10

result = test_proportion(alpha, n, num_successes, p0)

cat("Scorul testului (z):", result$z, "\n")
cat("Valoarea critică (z_critical):", result$z_critical, "\n")

if (result$z > result$z_critical) {
  cat("Procentul componentelor defecte este semnificativ mai mare decât 10% (reținem ipoteza alternativă).\n")
} else {
  cat("Nu putem afirma că procentul componentelor defecte este semnificativ mai mare decât 10% (nu respingem ipoteza nulă).\n")
}


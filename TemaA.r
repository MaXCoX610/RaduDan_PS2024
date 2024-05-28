#A1
calc_prob_poisson <- function(lambda, k, m) {
  x <- k:(k + m)
  probs <- dpois(x, lambda)
  return(probs)
}

calc_prob_geometric <- function(p, k, m) {
  x <- k:(k + m)
  probs <- dgeom(x, p)
  return(probs)
}

calc_prob_binomial <- function(n, p, k, m) {
  x <- k:(k + m)
  probs <- dbinom(x, n, p)
  return(probs)
}

plot_poisson <- function(lambda, k, m) {
  x <- k:(k + m)
  probs <- dpois(x, lambda)
  plot(x, probs, type = "h", lwd = 2, col = "blue", 
       xlab = "x", ylab = "P(X=x)", main = paste("Poisson Distribution (lambda =", lambda, ")"))
}

plot_geometric <- function(p, k, m) {
  x <- k:(k + m)
  probs <- dgeom(x, p)
  plot(x, probs, type = "h", lwd = 2, col = "green", 
       xlab = "x", ylab = "P(X=x)", main = paste("Geometric Distribution (p =", p, ")"))
}

plot_binomial <- function(n, p, k, m) {
  x <- k:(k + m)
  probs <- dbinom(x, n, p)
  plot(x, probs, type = "h", lwd = 2, col = "red", 
       xlab = "x", ylab = "P(X=x)", main = paste("Binomial Distribution (n =", n, ", p =", p, ")"))
}

calc_k0_poisson <- function(lambda) {
  threshold <- 1 - 10^(-6)
  k0 <- 0
  cum_prob <- ppois(k0, lambda)
  while (cum_prob <= threshold) {
    k0 <- k0 + 1
    cum_prob <- ppois(k0, lambda)
  }
  return(k0)
}

# Parametrii
lambda <- 3.5
p <- 0.2
n <- 10
k <- 0
m <- 10

probs_poisson <- calc_prob_poisson(lambda, k, m)
probs_geometric <- calc_prob_geometric(p, k, m)
probs_binomial <- calc_prob_binomial(n, p, k, m)

print(probs_poisson)
print(probs_geometric)
print(probs_binomial)

plot_poisson(lambda, k, m)
plot_geometric(p, k, m)
plot_binomial(n, p, k, m)
k0 <- calc_k0_poisson(lambda)
print(k0)

#A2

process_grades <- function(filename) {

  data <- read.table(filename, header = TRUE)
  
  freq_absolute_P <- table(data$P)
  freq_absolute_S <- table(data$S)
  
  freq_relative_P <- prop.table(freq_absolute_P)
  freq_relative_S <- prop.table(freq_absolute_S)
  
  mean_P <- mean(data$P)
  mean_S <- mean(data$S)
  
  list(
    freq_absolute_P = as.vector(freq_absolute_P),
    freq_relative_P = as.vector(freq_relative_P),
    freq_absolute_S = as.vector(freq_absolute_S),
    freq_relative_S = as.vector(freq_relative_S),
    mean_P = mean_P,
    mean_S = mean_S
  )
}

result <- process_grades("note_PS.txt")
print(result)


process_and_plot <- function(filename) {
  data <- read.table(filename, header = TRUE)
  
  remove_outliers <- function(x) {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    x[x >= lower_bound & x <= upper_bound]
  }
  
  clean_P <- remove_outliers(data$P)
  clean_S <- remove_outliers(data$S)
  
  freq_clean_P <- table(cut(clean_P, breaks = seq(0, 10, by = 1)))
  freq_clean_S <- table(cut(clean_S, breaks = seq(0, 10, by = 1)))
  
  par(mfrow = c(1, 2))
  barplot(freq_clean_P, main = "Distribuția frecvențelor pentru P", xlab = "Intervale", ylab = "Frecvență", col = "blue")
  barplot(freq_clean_S, main = "Distribuția frecvențelor pentru S", xlab = "Intervale", ylab = "Frecvență", col = "green")
  
  list(clean_P = clean_P, clean_S = clean_S)
}

cleaned_data <- process_and_plot("note_PS.txt")
print(cleaned_data)

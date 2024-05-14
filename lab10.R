#I.2
estimate_area <- function(n = 10000){
  f <- function(x) -2*x^2 + 5*x - 2
  
  a <- 0.5
  b <- 2
  
  x <- runif(n, a, b)
  y <- runif(n, 0, 2)
  under_curve <- sum(y < f(x))
  estimated_area <- under_curve / n * (b - a) * 2
  exact_area <- integrate(f, a, b)$value
  relative_error <- abs(estimated_area - exact_area) / exact_area
  return(list(estimated_area = estimated_area, exact_area = exact_area, relative_error = relative_error))
}

#II.1.b
calculeaza_eroare1 <- function(estimare) {
  exact_val <- exp(4) - exp(1)
  
  eroare_absoluta <- abs(exact_val - estimare)
  
  eroare_relativa <- eroare_absoluta / abs(exact_val)
  
  cat("Valoarea exactă a integralei: ", exact_val, "\n")
  cat("Eroarea absolută: ", eroare_absoluta, "\n")
  cat("Eroarea relativă: ", eroare_relativa, "\n")
}

#II.1.d
calculeaza_eroare2 <- function(estimare) {
  exact_val <- -1/(4*Inf - 2) + 1/(4*1 - 2)
  
  eroare_absoluta <- abs(exact_val - estimare)
  
  eroare_relativa <- eroare_absoluta / abs(exact_val)
  
  cat("Valoarea exactă a integralei: ", exact_val, "\n")
  cat("Eroarea absolută: ", eroare_absoluta, "\n")
  cat("Eroarea relativă: ", eroare_relativa, "\n")
}

#II.2
estimate_integral <- function(lambda, N) {
  set.seed(123)
  x <- rexp(N, rate = lambda)
  
  f <- function(u) exp(-2*u^2)
  
  mean_f <- mean(f(x))
  
  integral_estimate <- mean_f / lambda
  
  return(integral_estimate)
}

#III.2
genereaza_valori <- function(n) {
  lambda1 <- 4
  lambda2 <- 12
  
  p <- 3/4
  
  X <- rexp(n, rate = ifelse(runif(n) < p, lambda1, lambda2))
  
  return(mean(X))
}

#IV.1
estimate_probability <- function(p_X = 0.3, p_Y = 0.5, error = 0.005, confidence = 0.95){
  n <- ceiling((1-confidence)/(error^2))
  count <- 0
  
  set.seed(123) # pentru reproducibilitate
  for(i in 1:n){
    X <- rgeom(1, p_X) + 1 # +1 pentru că în R distribuția geometrică începe de la 0
    Y <- rgeom(1, p_Y) + 1
    if(X < Y^2){
      count <- count + 1
    }
  }
  prob <- count/n
  
  return(prob)
}

# Apelăm funcțiile și afișăm rezultatele
results <- estimate_area()
print(paste("Aria estimată este ", results$estimated_area))
print(paste("Aria exactă este ", results$exact_area))
print(paste("Eroarea relativă este ", results$relative_error))

calculeaza_eroare2(log(3/4))
calculeaza_eroare1(51.87987)

result <- estimate_integral(1, 50000) # variabila 'result' nu a fost definită anterior
print(result)

genereaza_valori(10000) #un numar mare de valori pentru a obtine o estimare precisa

prob <- estimate_probability()
print(paste("Probabilitatea estimată este ", prob))


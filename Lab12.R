simuleaza_variabila <- function(valori, probabilitati) {
  if(length(valori) != length(probabilitati)) {
    stop("Lungimile vectorilor 'valori' și 'probabilitati' trebuie să fie egale.")
  }
  if(sum(probabilitati) != 1) {
    stop("Suma probabilităților trebuie să fie 1.")
  }
  return(sample(valori, size = 1, prob = probabilitati, replace = TRUE))
}


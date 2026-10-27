# Metodo de aceptacion y rechazo

# Variable aleatoria X
# X \in {1,2,...}
# Pj \in {0.11, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09, 0.09, 0.10, 0.10}
# plot(1:10, c(0.11, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09, 0.09, 0.10, 0.10), type = "s", ylim = c(0,0.15), xlab = "X = x", ylab = "Probabilidad", main ="Variable aleatoria X")
# abline(h = 0.10, col = "red") # qj = 0.10 constante por ser Uniforme (Se quiere generar otra distribucion a partir de esta)

# Implementacion
mar <- function(P, X){
  n <- length(P)
  Y <- 1:n             # Valores distribuciopn
  qY <- rep(1/n, n)    # Distribicion discreta uniforme
  cc <<- max(P/qY)
  val_Y <- floor(n*runif(1)) + 1
  while(runif(1) > P[val_Y]/(cc*qY[val_Y]))
    {
      val_Y <- floor(n*runif(1)) + 1
    }
  return(X[val_Y])
}

P <- c(0.17, 0.51, 0.05, 0.27)
X <- c(0L, 3L, 7L, 11L)

res <- sapply(1:1000, function(k){
  mar(P,X)
})

# Validación de probabilidades empíricas
table(res)/length(res)

# Aplicación: Serie Logaritmica de Fisher
p <- 0.95
size <- 300

res <- sapply(1:size, function(k){
  -p^k / (k*log(1-p))
})
sum(res)
plot(1:size, res, type = "l", col = "red")

res2 <- sapply(1:10000, function(k){
  mar(res, 1:size)
})

hist(res2)
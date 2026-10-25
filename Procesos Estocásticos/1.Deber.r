# Milena Palacios
# Deber 1

cat("Ejercicio 11")
# Considere la cadena de dos estados del ejercicio 8 con a = 0.15 y b = 0.35
a <- 0.15
b <- 0.35
cat("\n\n1) Construya la matriz P y verifique que cada fila suma 1")
P <- matrix(c(1-a, a, b, 1-b), nrow = 2, ncol = 2, byrow = TRUE)
cat("\nLa suma de la primera fila es:", sum(P[1,]))
cat("\nLa suma de la segunda fila es:", sum(P[2,]))

cat("\n\n2) Simule una trayectoria de longitud n = 200 iniciando en el estado 0 y grafíquela")
n <- 200
Xn <- numeric(n)
Xn[1] <- 0
for(i  in 2:n){
  Xn[i] = sample(c(0,1), 1, replace = TRUE, P[Xn[i-1]+1,])
}

print(plot(Xn, type = "b", xlab = "Tiempo(n)", ylab = "Estados", main = "Simulación de una trayectoria"))

cat("\n3) calcule Pⁿ para varios valores de n usando potencias de matrices (librería expm) y compárela con la fórmula cerrada obtenida analíticamente")
library(expm)

for(i in c(1,5,10,30)){
  cat("\n********* n = ", i, "*********")
  Pnt <- matrix(c((b+a*(1-a-b)^i)/(a+b), a*(1-(1-a-b)^i)/(a+b), b*(1-(1-a-b)^i)/(a+b), (a+b*(1-a-b)^i)/(a+b)), nrow = 2, ncol = 2, byrow = TRUE)
  cat("\n-- Pn teórica --\n")
  print(Pnt)
  
  Pne <- P %^% i
  cat("\n-- Pn experimental --\n")
  print(Pne)
}

cat("\n4) Estime empíricamente la distribución estacionaria")
cat("\nPi teórico:", c(b/(a+b), a/(a+b)))
n <- 100000
Yn <- numeric(n)
Yn[1] <- 0
for(i  in 2:n){
  Yn[i] = sample(c(0,1), 1, replace = TRUE, P[Yn[i-1]+1,])
}
c1 <- sum(Yn)/n
c0 <- (n - sum(Yn))/n

cat("\nPi experimental:", c(c0, c1))
# ------------------------------------------
cat("\n\nEjercicio 12")
# Simule un paseo aleatorio con  p = 0.6 y n = 500 pasos.
p <- 0.6
nA <- 500
B <- 5000
paseo_aleatorio <- function(p, n){
  A <- numeric(n) # trayectoria
  A[1] <- 0
  for(i  in 2:nA){
    ant <- A[i-1]
    A[i] <- sample(c(ant+1,ant-1), 1, replace = TRUE, c(p, 1-p))
  }
  Zn <- (tail(A,1) - nA*(p - (1-p)))/sqrt(4*p*(1-p)*nA)
  return(c(A, Zn))
}

res<- sapply(1:B, function(k){
  paseo_aleatorio(p,nA)
})
trayectorias <- res[1:nA,]
Zn_vals <- res[nA+1,]

hist(Zn_vals, probability = TRUE, main ="Histograma de Zn")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)

# Comparacion probabilidad empírica y aproximación normal local
k = 20
i = 250
cat("\nLa probabilidad empirica para P(X_i = k) es:", mean(trayectorias[i+1,]==k))
cat("\nAproximación normal local:", 2*dnorm(k, mean = i*(p- (1-p)), sd = sqrt(4*p*(1-p)*i)))



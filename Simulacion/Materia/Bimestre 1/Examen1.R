# Milena Palacios
# Ejercicio 1

# Sistema de Linea de Espera con cuatro servidores en serie
Tasa_estancia <- c(6, 8, 10, 5)

# Probabilidades de salto: el estado 5 es la salida!
PS <- matrix(c(
  0, 0.85, 0,    0,    0.15,    # De 1 puede ir a 2 o salir(5)
  0, 0,   0.70, 0,    0.30,     # De 2 puede ir a 3 o salir(5)
  0, 0,    0,   0.60,  0.40,    # De 3 puede ir a 4 o salir(5)
  0, 0,    0,   0,      1       # De 4 solo puede salir(5)
), nrow = 4, ncol=5, byrow = TRUE)

# a) Los posibles estados son 1,2,3,4, 5 (salida)
Torden <- function(x0 = 1){
  t <- 0
  estado <- x0
  T_hist <- c()
  Est_hist <- c()
  while(estado != 5){
    T_estancia <- rexp(1, rate = Tasa_estancia[estado])
    t <- t + T_estancia
    T_hist <- c(T_hist, t)
    Est_hist <- c(Est_hist, estado)
    estado <- sample(1:5, size = 1, prob = PS[estado, ])
    if(estado == 5)
    {
      break
    }
  }
  return(list(
    X = t,                   # Tiempo total de preparación
    K = length(Est_hist),    # Número de estaciones visitadas (1 a 4)
    T_hist = T_hist,
    Est_hist = Est_hist
  ))
}

# Retorna el tiempo total de preparacion X de una orden
Torden() 

# b.1) 
llegadas_por_turno <- rpois(10000, lambda = 4)
X_sim <- c()
K_sim <- c()
S_turno <- c()
idx <- 1
for (i in 1:10000) {
  n_ordenes <- llegadas_por_turno[i]
  suma_i <- 0
  if (n_ordenes > 0) {
    for (j in 1:n_ordenes) {
      res <- Torden(x0 = 1)
      X_sim[idx] <- res$X
      K_sim[idx] <- res$K
      suma_i <- suma_i + res$X
      idx <- idx + 1
    }
    S_turno[i] <- suma_i
  }
}

mean(X_sim)
var(X_sim)
quantile(X_sim, probs = 0.90)

# b.2) 
mean(K_sim[X_sim > 0.5] >= 3)

# b.3)


# Ejercicio 2
######## Procesos de Poisson NO Homogeneo
flambda <- function(t){
  return(5 + 15/((1+(t-5))^2))
}

#Twi: simulada con transformada inversa
Twi <- function(){
  return(-4*log(1-runif(1)))
}
#a)
ppnh <- function(TT, n = 3){ # n es el numero de particiones para el tiempo
  ti <- seq(0, TT, length.out = (n+1))[-1] # Genera la particion y elimino el primer elemento que es 0.
  lambda_i <- flambda(ti)
  t <- I <- 0
  J <- 1
  S <- c()
  X <- (-1/lambda_i[J])*log(runif(1))
  while(J <= n){
    if((t + X > ti[J]) && (J == n)){
      break
    } else if((t + X > ti[J]) && (J < (n+1))){
      X <- (X - ti[J] + t)*lambda_i[J]/lambda_i[J+1] # En el libro esta mal
      t <- ti[J]
      J <- J + 1
    } else if(t + X <= ti[J]){
      t <- t + X 
      if (runif(1) <= flambda(t)/lambda_i[J]){
        I <- I + 1
        aux <- t + Twi()
        if(aux <= TT){
          S[I] <- aux
        }
      }
      X <- (-1/lambda_i[J])*log(runif(1))
    } 
  }
  S <- sort(S)
  return(list(N10 = length(S), T8 = ifelse(S[8] !=0,S[8], 0) ))
}

# Para mejorar el algoritmo podramos tomar como cota el maximo de la funcion lambda evaluada en cada particion [ti, ti+1]
# b)

res <- sapply(1:10000, function(k){
  ppnh(TT=10)
})

# mean(res$N10)
# var(res$N10)

# p <- length(res$T8 <= 2.5)/length(res)










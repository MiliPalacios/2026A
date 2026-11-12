# Proceso de Poisson Homogéneos
pph <- function(T, lambda){
  t <- 0
  I <- 0
  S <- c()
  while(t < T){
    I <- I + 1
    S[I] <- t
     t <- t + (-1/lambda)*log(runif(1))
  }
  return(S)
}
res <- pph(10, lambda= 3)
plot(res,0:(length(res)-1), type="s", main = "Proceso de Poisson", ylab = "Num Eventos", xlab = "Tiempo", col = "red")

# Esperanza Numero de arribos en el periodo T = 10
mean(sapply(1:1000, function(k){
  length(pph(10,lambda = 3))
}))

# Segunda alternativa
pph2 <- function(T, lambda){
  N <- rpois(1, lambda = T*lambda)
  t <- sort(N*runif(N))
  return(t)
}

# Grafico del proceso de Poisson
res <- pph2(10, lambda = 3)
plot(res, 0:(length(res)-1), type="s", main = "Proceso de Poisson - proceso 2", ylab = "Evolución", xlab = "Tiempo", col = "red")

# Procesos de Poisson NO Homogeneo
flambda <- function(s){
  return(10 + s)
}
x <- seq(0,1,by = 0.01)
plot(x, flambda(x), type = "l", col = "red", ylim=c(0,11))
abline(v=0.5)
abline(h=0.5)

ppnh <- function(T, n){ # n es el numero de particiones para el tiempo
  #T <- 10
  #n <- 3
  t <- I <- 0
  J <- 1
  S <- c()
  ti <- seq(0, T, length.out = (n+1))[-1] # Genera la particion y elimino el primer elemento que es 0.
  lambda_i <- flambda(ti)
  X <- (-1/lambda_i[J])*log(runif(1))
  while(J!= (n+1)){
    if((t + X > ti[J]) && (J == (n+1))){
      break
    } else if((t + X > ti[J]) && (J < (n+1))){
      X <- (X - ti[J] + t)*lambda_i[J]*lambda_i[J+1]
      t <- ti[J]
      J <- J + 1
    } else if(t + X <= ti[J]){
      t <- t + X 
       if (runif(1) <= flambda(t)/lambda_i[J]){
          I <- I + 1
          S[I] <- t
          X <- (-1/lambda_i[J])*log(runif(1))
        }
      } 
    }
  return(list(S= S, ti = ti))
}

res <- ppnh(10,3)
resS <- res$S
resti <- res$ti
plot(resS,0:(length(resS)-1), type="s", main = "Proceso de Poisson No Homogeneo", ylab = "Num Eventos", xlab = "Tiempo", col = "red", xlim=c(0,10))
for(i in(1:length(resti))){
  abline(v = resti[i])
}

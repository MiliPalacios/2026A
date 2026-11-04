# Transformada Inversa: F DISTRIBUCION considerada debe ser invertible
# Se iguala F(X) = U y de despeja X. Luego se genera un numero aleatorio U y se construye X con la formula encontrada.
# Simulacion de X niforme en [a,b] a partir de U uniforme en [0,1]
# Sabemos que F(x) = (x-1)/(b-a), entonces F(x) = U implica que X = a + (b-a)U    (escala y traslada)

# Método de rechazo
# F DISTR. QUE SE QUIERE GENERARR
fx <- function(x){
  return(20*x*(1-x)^3)
}
plot(seq(0,1,by=0.01),fx(seq(0,1,by=0.01)),type='l', col ='red',
ylab='f(x)',xlab='x')
abline(h=2.1, col='blue') # h \aprrox c

# G DISTR. DE REFERENCIA (uniforme en [0,1])
gx <- function(x){
  return(1)
}

# Cálculo de la constante c COMO MAXIMO DE LA FUNCION f/g
optimize(f=function(x){fx(x)/gx(x)},interval=c(0,1),maximum = TRUE)

fx(0.25)

# Implementacion;no siempre se generaran v.a. VALIDAS!!!
dc <- function(){
  Y <- runif(1) # de la densidad g, podría ser exp, etc la que sea util
  U <- runif(1)
  c <- optimize(f=function(x){fx(x)/gx(x)},interval=c(0,1),maximum = TRUE)$objective
  if (U <=fx(Y)/(c*gx(Y))){
    return (Y)
  } else{
    return (NA)
  }
}

res <-replicate(1000,dc())
sum(is.na(res)) # Valores fallidos al generar la va
sum(!is.na(res)) # Valores validos de la distribución

##################################
# Metodo de aceptacion y rechazo #
##################################
fx <- function(x){
  return((2/sqrt(pi))*x^(1/2)*exp(-x))
}

x <- seq(0,10, by = 0.1)
plot(x, fx(x), type = "l")

# Funcion envolvente
g1 <- function(x){
  return(exp(-x))
}

plot(x, fx(x), type = "l", ylim = c(0,7))
par(new = TRUE)
# Constante
c1 <- optimize(f = function(x){fx(x)/g1(x)}, interval = c(0,30), maximum = TRUE)$objective  # tengo que generar c1 variables para aceptar una
plot(x, c1*g1(x), type = "l", col = "red", ylim = c(0,7))

gamma_sim <- function(x){
  # Generamos Y
  Y <- -log(runif(1))
  # Verificacion
  if(runif(1) <= (fx(Y)/(c1*g1(Y)))){
    return(Y)
  } else {
    return(NA)
  }
}

res <- sapply(1:10000, function(k){gamma_sim()})
length(res[is.na(res)])
length(res[!is.na(res)])

# Funcion envolvente 02
g2 <- function(x){
  return(0.5*exp(-0.5*x))
}

plot(x, fx(x), type = "l", ylim = c(0,0.5))
par(new = TRUE)
# Constante
plot(x, c1*g1(x), type = "l", col = "blue", ylim = c(0,0.5))
par(new = TRUE)
# Constante
c2 <- optimize(f = function(x){fx(x)/g2(x)}, interval = c(0,30), maximum = TRUE)$objective  # tengo que generar c1 variables para aceptar una
plot(x, c2*g2(x), type = "l", col = "red", ylim = c(0,0.5))

gamma_sim02 <- function(x){
  # Generamos Y
  Y <- -2*log(runif(1))
  # Verificacion
  if(runif(1) <= (fx(Y)/(c1*g1(Y)))){
    return(Y)
  } else {
    return(NA)
  }
}

res <- sapply(1:10000, function(k){gamma_sim02()})
length(res[is.na(res)])
length(res[!is.na(res)])
length(res)/length(res[is.na(res)]) # total/aceptadas
c2 # valor anterior es aproximacion de c2

# Envolvente exponencial generica
g3 <- function(x, lambda){
  return(lambda*exp(-lambda*x))
}
# Búsqueda del valor de lambda
res <- sapply(seq(0.01, 0.99, by = 0.01), function(k){
  optimize(f = function(x){fx(x)/g3(x, k)}, interval = c(0,30), maximum = TRUE)$objective
})
library(data.table)
TT <- data.table(lambda = seq(0.01, 0.99, by = 0.01), constante = res )
TT[which.min(TT$constante),]
plot(TT$lambda, TT$constante, type = "l", col = "blue")

# Generacion de variables aleatoria normales
normal_sim <- function(){
  Y1 <- -log(runif(1))
  Y2 <- -log(runif(1))
  if(Y2 - (Y1-1)^2/2 > 0){
    U <- runif(1)
    if(U <= 0.5){
      return(Y1)
    } else {
      return(-Y1)
    }
  } else {
    return(NA)
  }
}
res <- sapply(1:10000, function(k){        # se puede volver vector a una lista con el comando unlist(lista)
  normal_sim()
})
length(res[is.na(res)])
length(res[!is.na(res)])
length(res)/ length(res[!is.na(res)]) # Aproximacion de la constante de la distribucion geometrica de la aceptacion
mean(res[!is.na(res)])
sd(res[!is.na(res)])

# Para las otras normales hacer: mu + sigma*Z con Z normal(0,1) generada      

################
# Metodo Polar #
################
normal_polar <- function(){
V1 <- 2*runif(1) - 1
V2 <- 2*runif(1) - 1
S <- V1^2 + V2^2
  if(S <= 1){
    X <- sqrt((-2*log(S))/S)*V1
    Y <- sqrt((-2*log(S))/S)*V2
    return(c(X,Y))
  } else {
    return(c(NA, NA))
  }
}

res <- sapply(1:1000, function(k){
  normal_polar()
})
length(res)

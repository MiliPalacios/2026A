# Transformada Inversa: F DISTRIBUCION considerada debe ser invertible
# Se iguala F(X) = U y de despeja X. Luego se genera un numero aleatorio U y se construye X con la formula encontrada.
# Simulacion de X niforme en [a,b] a partir de U uniforme en [0,1]
# Sabemos que F(x) = (x-1)/(b-a), entonces F(x) = U implica que X = a + (b-a)U    (escala y traslada)

# Método de rechazo
fx <- function(x){
  return(20*x*(1-x)^3)
}
plot(seq(0,1,by=0.01),fx(seq(0,1,by=0.01)),type='l', col ='red',
ylab='f(x)',xlab='x')
abline(h=1, col='blue')

gx <- function(x){
  return(1)
}

# Cálculo de la constante c
optimize(f=function(x){fx(x)/gx(x)},interval=c(0,1),maximum = TRUE)

fx(0.25)

# Implementacion

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
# Milena Palacios
###############
# Ejercicio 1 #   (integral separada)
###############

fx <- 'x^2* exp(-x^2 - exp(x))'
aprox_fx <- function(k){
  y <- runif(k)
  x <- (1 / y) - 1
  # int_0^infty
  positivo <- eval(parse(text = fx)) / (y^2)
  # int_-infty^0
  x <- -1*x
  negativo <- eval(parse(text = fx)) / (y^2)
  # Sumemos la parte positiva y negativa
  res <- numeric(length(positivo))
  for(i in 1:length(positivo)){
    res[i] = positivo[i] + negativo[i]
  }
  return(res) # Retorna vector de valores
}

res <- aprox_fx(100000)
cat("\na) Aproximacion de la integral:", mean(res)) # Estimador
cat("\nb) Desviación del estimador:", sd(res)/sqrt(length(res))) # Desviacion del estimador
cat("\nc)Intervalo de confianza al 95%: [ ", quantile(res, 0.025), " , ", quantile(res, 0.975)," ]")

###############
# Ejercicio 2 #
###############
# Deducible: cantidad que debe superar para que pague la diferencia
simulacion <- function(){
  N <- sample(c(0,1,2,3,4,5), 1, replace = TRUE, prob = c(0.3 , 0.25 , 0.2 , 0.12 , 0.08 , 0.05)) # Numero de siniestros
  # Costo de cada siniestro 
  if(N == 0){ 
    X <- 0 # si no hay siniestro no deberia haber costo
  }else{
    X <- rexp(N, rate = 1/1500) 
  }
  deducible <- 300
  
  # a) Cantidad indemnizada para cada accidente
  if(N == 0){ 
    Y <- 0 # si no hay accidente no deberia haber indemnizacion
  }else{
    Y <- numeric(N)
    for(i in 1:N){
      Y[i] <- max(0, X[i] - deducible)
    }
  }
  return(sum(Y))
}
# b)
m <- 10000
res <- sapply(1:m, function(k){
  simulacion()
})
cat("b)\nS = ", mean(res)) # valor esperado de S
cat("\nVar = ", sd(res)) # Varianza de S
cat("\nPrecio Poliza = ", ((mean(res) + 50)/(1 - 0.08))*(1 + 0.15))

###############
# Ejercicio 3 #
###############

simuacion <- function(nclientesporhora , horas)
{
  # Productos adquiridos de cada cliente
  N <- rpois(nclientesporhora, lambda = 6) 
  # Productos devueltos por cada cliente
  D <- numeric(nclientesporhora)
  for(i in 1:length(D)){
    D[i] <- rbinom(1, N[i] ,prob = 0.12)
  }
  return(sum(D)) # Retorna Dtotal
}

# Cada hora entran 100 clientes
nclientesporhora <- 100
horas <- 1
res <- simuacion(nclientesporhora, horas)
cat("a) Variable agregada del total de devoluciones:", res)

cat("\nb) Simulacion de Monte Carlo")
m <- 10000
res <- sapply(1:m,function(j){
  simuacion(nclientesporhora, horas)
})

cat("\nDtotal:", mean(res))
cat("\nIntervalo de confianza al 95%: [ ", quantile(res, 0.025), " , ", quantile(res, 0.975)," ]")
cat("\nP(Dtotal >90) = ", length(res[res>90])/length(res))


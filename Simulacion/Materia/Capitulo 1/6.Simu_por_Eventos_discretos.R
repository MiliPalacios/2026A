# Sistema de línea de espera con un servidor
# Distribución para las llegadas: exponencial con parámetro 1/5
# Distribución para tiempos las salidas: exponencial con parámetro 1/5

# Funcion de tiempos de llegada
Tllegada <- function(){
  return(rexp(1, rate = 1/5))
}

sle1s <- function(){
  t <- NNA <- NND <- 0
  ES <- 0
  # Generamos el primer tiempo de llegada
  T0 <- Tllegada()
  TA <- T0
  TD <- Inf

}
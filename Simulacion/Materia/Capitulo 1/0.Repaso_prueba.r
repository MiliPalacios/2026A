###############
# Ejercicio 1 #
###############
dados <- function(){
  return(sample(1:6, size = 2, replace = TRUE)) # sacar los resultados de los dados equiprobables
}

juego <- function(Capital){
  # El máximo capital que puede acumular un jugador es de 500 dolares (caso contrario explota)
  num <- 0
  flag <- 1 # Para discernir si gano todo (1) o perdio todo (0)
  capital_hist <- c(Capital)
  while(Capital >= 8 && Capital <=500){
    res <- dados()
    num <- num + 1     # juego iniciado
    if(res[1] == res[2]){ # otro criterio: si el minimo es igual al maximo entonces son iguales (para no compara el resultado de 5 dados)
      Capital <- Capital + 50      # si gana
    } else {
      Capital <- Capital -8        # si pierde
    }
    capital_hist <- c(capital_hist, Capital)
  }
  if(Capital < 8){
    # cat("El capital es insuficiente para iniciar el juego")
    flag <- 0
  }
  return(list(Capital = capital_hist, Partidas = num, Alerta = flag))
}
# Grafico de la trayectoria de la cadena de Markov
res<-juego(100)
plot(1:length(res$Capital), res$Capital, type="s", col = "red", ylab = "Capital", xlab = "Juego", main = "Simulación de un juego")

# Simulacion de 1000 partidas
num_sim <-10
res <- lapply(1:num_sim, function(k){     # lapply retorna una lista
juego(100)
})
partidas <- sapply(1:num_sim, function(k){res[[k]]$Partidas})[sapply(1:num_sim, function(j){res[[j]]$Alerta}) == 0]
length(partidas)
mean(partidas) # Numero medio de partidas que perdio

###############
# Ejercicio 2 #
###############
# Inicia: 6 am 
# Finaliza: 6 pm 
# Minutos: 12 * 60 = 720

duracion <- function(ntelefono){   # Si la duracion es 0, la llamada es falsa
  if(ntelefono ==1){
    return(rpois(1, lambda = 2.5))
  } else if(ntelefono == 2){
    return(rpois(1, lambda = 3.1))
  } else{
    return(rpois(1, lambda = 1.2))
  }
}

tiempo_espera <- function(n){
  rexp(n, rate = 1/1.5)   # Retorna n numeros aleatorios con tasa 1/media
}

# Simulacion minuto a minuto
sim_telefono <- function(horas){
  # Numero de llamadas por minuto
  nllamadas <<- rpois(60*horas, lambda = 2.7)
  espera <- numeric(60*horas)
  espera[1] <- ifelse(nllamadas[1] > 3, nllamadas[1] - 3,0)
  telefonos <- matrix(0, nrow = 60*horas, ncol = 3)
  telefonos[1,sample(1:3, size = ifelse(nllamadas[1]>3, 3, nllamadas[1]), replace = FALSE)] <- 1
  for(t in 2:(60*horas)){
   # Esperan en minuto t = esperaban minuto (t-1) - los que se cansaron + las nuevas llamadas -  llamadas agarradas con telefonos libres
    espera[t] <- espera[t-1] - sum(tiempo_espera(espera[t-1]) < 1) + nllamadas[t]
  }
  return(list(nllamadas, telefonos, espera))
}

sim_telefono(horas = 0.25)


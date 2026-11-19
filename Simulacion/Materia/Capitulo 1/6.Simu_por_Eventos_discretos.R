# Sistema de línea de espera con un servidor
# Distribución para las llegadas: exponencial con parámetro 1/5
# Distribución para tiempos las salidas: exponencial con parámetro 1/5

# Funcion de tiempos de llegada
Tllegada <- function(){
  return(rexp(1, rate = 1/5))
}

# Funcion de tiempos de salida
Tservicio <- function(){
  return(rexp(1, rate = 1/3))
}

sle1s <- function(TT){
  t <- NNA <- NND <- 0
  ES <- 0
  Hist_ES <- c() # Historial de ES
  Hist_t <- c() # Historial de t
  A <- D <- c()
  # Generamos el primer tiempo de llegada
  T0 <- Tllegada()
  TA <- T0
  TD <- Inf
  while(t < TT || ES > 0){ # La simulacion para si se sobrepasa el tiempo o no hay individuos
    # Caso 1
    if(TA <= TD  & TA <= TT){
      t <- TA
      NNA <- NNA + 1
      ES <- ES + 1
      TA <- t + Tllegada()
      if(ES == 1){
        TD <- t + Tservicio()
      }
      A[NNA] <- t
      Hist_ES <- c(Hist_ES,ES)
      Hist_t <- c(Hist_t,t)
    }
    # Caso 2
    if(TD < TA &  TD <= TT){
      t <- TD
      ES <- ES - 1
      NND <- NND + 1
      if(ES == 0){
        TD <- Inf
      } else {
        TD <- t + Tservicio()
      }
      D[NND] <- t
      Hist_ES <- c(Hist_ES,ES)
      Hist_t <- c(Hist_t,t)
    }
    # Caso 3
    if(min(TA, TD)> TT & ES > 0){
      t <- TD
      ES <- ES - 1
      NND <- NND + 1
      if(ES > 0){
        TD <- t + Tllegada()
      }
      D[NND] <- t
      Hist_ES <- c(Hist_ES,ES)
      Hist_t <- c(Hist_t,t)
    }
    # Caso 4
    if(min(TA, TD) > TT & ES == 0){
      Tp <- max(t-TT, 0)   # Tiempo Extra: tiempo en atender a los individuos luego de haber cerrado
    }
  }
  return(list(TiemposArribo = A, TiemposSalida = D, TiempoExtra = Tp, Sistema  = Hist_ES, Eventos_tiempo = Hist_t))
}
set.seed(12345)
res <- sle1s(TT = 50)
Sistema <- res$Sistema
eventos <- res$Eventos_tiempo
plot(eventos, Sistema, type="s", col="red", ylab= "Clientes", xlab="Tiempo", main ="Clientes dentro del sistema")
abline(v=50, col='blue') # A partir de este umbral no entran al sistema 

# Sistema de Linea de Espera con dos servidores en serie
# Funcion de tiempos de llegada: depende del sector puede ser normal, exp o exp al otro lado
Tllegada <- function(){
  return(rexp(1, rate = 1/5))
}

# Funcion de tiempos de salida
Tservicio <- function(){
  return(rexp(1, rate = 1/3))
}

sle2s <- function(TT){
  t <- NNA <- NND <- 0
  A1 <- A2 <- D <- c()
  ES <- c(0,0)
  TA <- Tllegada()
  T1 <- T2 <- Inf
  while(t < TT || min(ES)>0){
    if(TA == min(TA, T1, T2)){ # Hay un arribo pero los nodos estan ocupados
      t <- TA
      NNA <- NNA + 1
      ES[1] <- ES[1] + 1
      TA <- t + Tllegada()
      if(ES[1] == 1){
        T1 <- t + Tservicio()
      }
      A1[NNA] <- t
    }
    if(T1 == min(TA, T1, T2)){ # Se desocupa el servidor 1 antes del arribo
      t <- T1
      ES[1] <- ES[1] - 1
      ES[2] <- ES[2] + 1
      if(ES[1] == 0){
        T1 <- Inf
      } else {
        T1 <- t  + Tservicio()
      }
      if(ES[2] == 1){
        T2 <- t  + Tservicio()
      }
      A2[NNA - ES[1]] <- t 
    }
    if(T2 == min(TA, T1, T2)){ # Se desocupa el servidor 2 antes del arribo
      t <- T2
      NND <- NND + 1
      ES[2] <- ES[2] - 1
      if(ES[2] == 0){
        T2 <- Inf
      } else {
        T2 <- t + Tservicio()
      }
      D[NND] <- t
    }
  }
  return(list(ArriboS1 = A1, ArriboS2 = A2, Salidas = D))
}

sle2s(TT = 50)

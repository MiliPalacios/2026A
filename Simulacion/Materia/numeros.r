#Generación de numeros pseudoaleatorios 
mcm <- function(a, m, x0, n){
  if(is.integer(a) & is.integer(m) & a > 0 & m> 0){
    res <- numeric(n+1)
    res[1] <- x0
    for(i in 2:(n+1)){
      res[i] <- ((a*res[i-1]) %% m)
    }
    return(res[-1]/m)
  } else {
    cat('Los valores de a y m no cumplen las condiciones (enteros positivos)')
  }
}

res <- mcm(a = 31L, m = 67L, x0= 12, n = 1000)
which(res == res[1])
hist(res,breaks = seq(0,1,by=0.05))

valores<-expand.grid(a=c(7L, 19L, 31L, 47L, 71L, 89L), m =c(67L, 321L, 577L, 800L), x0 = c(7,12,22,53))

valores$ciclo <- sapply(1:nrow(valores), function(k){
  res <- mcm(a= valores[k,1], m= valores[k,2], x0= valores[k,3], n=1000)
  which(res == res[1])[2] - 1
})
print(valores)
which.max(valores$ciclo)
which.min(valores$ciclo)


gcm <- function(a, m, c, x0, n){
  if(is.integer(a) & is.integer(m) & a > 0 & m> 0){
    res <- numeric(n+1)
    res[1] <- x0
    for(i in 2:(n+1)){
      res[i] <- ((a*res[i-1] + c) %% m) /m
    }
    return(res[-1])
  } else {
    cat('Los valores de a y m no cumplen las condiciones (enteros positivos)')
  }
}
res1<-gcm(a = 31L, m = 67L,c=0, x0= 12, n = 10)
res2<-gcm(a = 31L, m = 67L,c=1, x0= 12, n = 10)
which(res1[1]==res2)
which(res1[2]==res2)

fx <- 'exp(-x^2)'

# Aproximacion de integrales
#f(x) es ingresada por el usuario (x es la variable)
aprox_fx <- function(k){
  x <- runif(k)
  return (mean(eval(parse(text=fx))))
}
aprox_fx(10000)
#valor teorico 0.74682
res <- sapply(seq(10,1000,by=10),function(j){
  aprox_fx(j)
})
plot(seq(10,1000,by=10),res, type='l')
abline(h=0.74682, col='red')

#Funciones que convierten texto en expresiones evaluables: eval y  parse
funcion <-'x^2 - 4*x + 7'
x <- seq(0,1,by=0.2)
eval(parse(text=funcion))

# Integrales en un rango [a.b]
# Evalúe la siguiente integral \int_{-x}^{5} exp(-x^2+5x-8)

f2x <- 'exp(-x^2 + 5*x - 8)'
a <- -2
b <- 5

aprox_f2x <- function(k) {
  y <- runif(k)
  x <- a + (b - a) * y
  return(mean(eval(parse(text = f2x))) * (b - a))
}

aprox_f2x(100000)

mi_entorno <- list(x=seq(a,b,by=0.1))
mi_entorno$y <- eval(parse(text = f2x),envir = mi_entorno)
mi_entorno

plot(mi_entorno$x, mi_entorno$y, type = 'l', col ='red' , xlab = 'x', ylab = 'y')

# Aproximación de integrales en [o,inf)

# Aproximar \int_{0}^{inf} exp(-x^2)
f3x <- 'exp(-x^2)'

aprox_f3x <- function(k) {
  y <- runif(k)
  x <- (1 / y) - 1
  
  return(mean(eval(parse(text = f3x)) / (y^2)))
}

aprox_f3x(10000)

# Aproximación de pi mediante la simulación de pares ordenados en un cuadrado de area 4
aprox_pi <- function(k){
  u1 <- runif(k)
  u2 <- runif(k)
  return(mean((2*u1 -1)^2 + (2*u2 -1)^2 <= 1))
}
aprox_pi(100000)

n <- 100
df <- data.frame(id = 1:n, x=2*runif(n)-1 , y =2*runif(n)-1)
df$dentro <- (df$x^2 +df$y^2 <=1)
df

library(ggplot2)
library(gganimate)
# Dibujamos el círculo
angulo <- seq(0,2*pi, length.out=n)
circulo_df <- data.frame(x=cos(angulo), y=sin(angulo))

p <- ggplot() + geom_path(data=circulo_df, aes(x=x, y=y), color='green', linewidth=1.2) + geom_point(data=df,aes(x=x, y=y, color = dentro),size=1.5)

# Animacion
animacion <- p + transition_time(id) + shadow_mark(past = TRUE)
anim_save('aproximacion-pi.gif',animation = animacion,nframes = 100, fps = 20,renderer = gifski_renderer())
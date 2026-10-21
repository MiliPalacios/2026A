# Generacion de variables aleatorias discretas

# Inputs:
# probs: vector de probabilidades
# labels: etiquetas o valores que puede tomar la variable aleatoria

mti <- function(probs, labels){
  U <- runif(1)
  pos <- which(U < cumsum(probs))[1]
  return(labels[pos])
}

# Verificacion a partir de las probabilidades empiricas
n <- 1000000
probabilidades <- runif(20); probabilidades <- probabilidades/sum(probabilidades)
labels <- paste0(LETTERS[1:20], "001")         # Genera etiquetas tipo A001, B001, ...

system.time(table(sapply(1:n, function(k){  # system.time cuenta tiempo de ejecucion| table hace un conteo | sapply es como un for
  mti(probabilidades,labels)
}))/n)

# Ordenando las probabilidades en forma descendente
res <- order(probabilidades, decreasing = TRUE) # order devuelve las posiciones de los elementos para que el objeto este ordenado
probs <- probabilidades[res] # Ordenar probs en forma descendente
labels <- labels[res] # Ordenar labels en funcion de probs
system.time(table(sapply(1:n, function(k){  # system.time cuenta tiempo de ejecucion| table hace un conteo | sapply es como un for
  mti(probs,labels)
}))/n)

mti(c(0.40, 0.45, 0.15),c("Deficiente", "Buenas", "Excelentes"))

# Simulacion distribucion geometrica      
# Inputs:
# q: probabilidad de fracaso

dg <- function(q){
  return(floor(log(runif(1))/log(q)) + 1)
}

res <- sapply(1:n,function(k){
  dg(q = 0.37)
})
table(res)/n
hist(res)

# Simular las variables geomtricas
val_teoricos <- rgeom(n = 1000000, prob = 1-0.37) + 1 # Empieza desde 0 exitos (x = 0)

# Test de Kolmogorov - Smirnov: prueba de bondad de ajuste (nuestro dg se parece a dgeom??)
ks.test(res, val_teoricos)

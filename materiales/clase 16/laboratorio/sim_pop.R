################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de eventos discrteos en una población cerrada        # 
# 12 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo de este laboratorio es construir una simulación de eventos discretos 
# para representar trayectorias demográficas individuales. Esas trayectorias incluirán
# los eventos primer hijo y fallecimiento.


# cargamos nuestra función para simular tiempos al evento a partir
# de una función de riesgo - tasas condicionales por edad-

source("ste.R")

# cargamos los datos
fx <- read.csv(file.path("datos","fx.csv"))
mx <- read.csv(file.path("datos","mx.csv"))


############################
# POBLACIÓN INICIAL        #
############################
# Comenzamos por definir una población inicial con N individuos


# Ejercicio: describir el contenido de cada linea con #

N <- 10
pop <- data.frame(id = 1:N) # Generamos un numero de id diferente para identificar a cada persona, como la cedula de identidad

srb <- 0.515 # Probabilidad de que la persona sea hombre

# Asignar un sexo a cado uno de los integrantes de "pop" (1 = mujer, 2 = hombre)
# usando sample con argumento prob = c(1-srb, srb)

pop$sexo <- sample(x = 1:2,size = N, replace = TRUE, prob = c(1-srb, srb))

pop$edad <- 0 # Todas las personas inician con edad 0

pop$edad_mte <- Inf # Inicializamos las edades de muertes


# crear una variable en pop "t_mte" con la edad a la muerte
pop$t_mte <- ste(n = N, edades = mx$edad, lambda = mx$h, Haz = F)

# Ejercicio:
# Describir el contenido de las primeras tres filas de la tabla "pop"

# Cada fila representa a una persona
# La persona 1 es una mujer de edad 0, donde aun no conocemos la edad de muerte
# La persona 2 es una mujer de edad 0, donde aun no conocemos la edad de muerte
# La persona 3 es un hombre de edad 0, donde aun no conocemos la edad de muerte
  
############################
# SIMULACIÓN               #
############################
# Definimos el año de inicio y de fin de la simulación
ini <- 1900  
fin <- 2000  

# definimos el tiempo actual (reloj) como el año de inico
tiempo <- ini


# Ahora vamos a comenzar a simular el evento muerte
# Tenemos que escojer el tiempo de espera más corto

# Completar rl objeto "rid" (real id) para que tenga la poscición en la tabla 
# que corresponde a la persona que va a experimentar el evento y "t" para que
# tenga el tiempo al evento
rid <- which.min(pop$t_mte)
t <- pop[rid, ]$t_mte
  
# Ahora que sabemos que evento vamos a simular tenemos que
# actualizar el reloj

tiempo <- tiempo + t 

# Actualizamos las edades

pop$edad <- pop$edad + t

# Actualizamos lo tiempos de espera

pop$t_mte <- pop$t_mte - t

# Ahora podemos simular el evento, es decir actualizar la información en pop
# una vez sucedido el evento

pop[rid,]$t_mte <- Inf
pop[rid,]$edad_mte <- pop[rid,]$edad
pop[rid,]$edad <- NA

# Ejercicio:
# Describir los cambios que se hiceron a "pop"

# Ahora vamos a simular el próximo evento, para eso tenemos que volver a 
# repetir las operaciones anteriores, comenzando por encontrar el tiempo de 
# espera más corto

rid <- which.min(pop$t_mte)
t <- pop[rid, ]$t_mte
tiempo <- tiempo + t 
pop$edad <- pop$edad + t
pop$t_mte <- pop$t_mte - t
pop[rid,]$t_mte <- Inf
pop[rid,]$edad_mte <- pop[rid,]$edad
pop[rid,]$edad <- NA

# Es claro que esta operación necesita un loop
# Ejercicio:

# Construir un while loop para simular la muerte de todos los individuos en pop.
# Incluir un print de tiempo para monitorear la evolución de la simulación.
# Primero volvemos a crear pop y definir los argumentos iniciales
N <- 10
pop <- data.frame(id = 1:N)  
srb <- 0.515 
pop$sexo <-  sample(x = 1:2,size = N, replace = TRUE, prob = c(1-srb, srb))
pop$edad <- 0 
pop$edad_mte <- Inf 
pop$t_mte <- ste(n = N, edades = mx$edad, lambda = mx$h, Haz = F)
ini <- 1900  
fin <- 2000  

tiempo <- ini

while (tiempo < fin){
  rid <- which.min(pop$t_mte)
  t <- pop[rid, ]$t_mte
  if (t == Inf) {break}
  tiempo <- tiempo + t 
  pop$edad <- pop$edad + t
  pop$t_mte <- pop$t_mte - t
  pop[rid,]$t_mte <- Inf
  pop[rid,]$edad_mte <- pop[rid,]$edad
  pop[rid,]$edad <- NA
  print(tiempo)
} 

# Ejercicio:
# Escribir una función sim_m que tome como argumentos: 
# N, ini, fin, srb, mx
# que simule los eventos y devuelva el objeto "pop"

sim_m <- function(N, ini, fin, mx, srb){
  
  pop <- data.frame(id = 1:N)
  pop$sexo <-  sample(x = 1:2,size = N, replace = TRUE, prob = c(1-srb, srb))
  pop$edad <- 0 
  pop$edad_mte <- Inf 
  pop$t_mte <- ste(n = N, edades = mx$edad, lambda = mx$h, Haz = F)
  
  tiempo <- ini
  
  cat("inicio ok, comenzando loop\n")
  
  while (tiempo < fin){
    rid <- which.min(pop$t_mte)
    t <- pop[rid, ]$t_mte
    if (t == Inf) {break}
    tiempo <- tiempo + t 
    pop$edad <- pop$edad + t
    pop$t_mte <- pop$t_mte - t
    pop[rid,]$t_mte <- Inf
    pop[rid,]$edad_mte <- pop[rid,]$edad
    pop[rid,]$edad <- NA
    print(tiempo)
  } 
  
  return(pop)
}

sim_m(10, 1900, 2000, mx, srb)

################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Proyecciones de Población                                       # 
# 26 de Noviembre de 2024                                                      #   
################################################################################

options(scipen =  999)

# El objetivo es proyectar una población estructurada por edad, en este caso la
# población de Suecia, utilizando información sobre las probabilidades de 
# supreviviencia en el intervalo de la proyección y unas tasa específicas de 
# fecundidad por edad.

# Vamos a trabajar con una población cerrrada y compuesta sólo por mujeres.

# Cargamos los datos
sw <- read.table(file.path("datos","sweden_data_1993.txt"), header = F, stringsAsFactors =  F)

# Agregamos nombres a las columnas: "age", "p93", "L", "f"

names(sw) <- c("age", "p93", "L", "f")

# EJERCICIO: Describir el contenido de cada columna de la tabla

# La primeras columna tiene los limites superiores (que no pertenecen) de los intevalos de edades de la población.
# La segunda es la población que pertenece a cada intervalo en el año 1993. Luego están los años persona que aporta cada intervalo, y por último las tasas específicas de fecundidad para los intervalos de personas que contienen a mujeres en edad fértil.

# Definimos el sex ratio at birth
#
srb <- 1.05 

# Salvamos las columnas relevantes en objetos para facilitar la lectura del código

L <- sw$L
f <- sw$f
N <- sw$p93


# Guardamos el largo de L en un objeto "n"

n <- length(L)


# Calcular las probabilidades de supervivencia en cada intervalo en un vector "px"

px <- L[2:n]/L[1:(n-1)]
px <- exp(diff(log(L)))

# Agregar la probabilidad en el intervalos abierto (últimos dos intervalos)

t85 <- L[n]
t80<- L[n-1] + t85

px[n-1] <- px[n] <- t85/t80


# Calculamos un vector auxiliar con la población en 1998

p98_aux <- N * px  

# Calcular la población mayor de 5 en 1998 (usand la información en p98_aux)

sw$p98 <- NA
sw$p98[2:(n-1)] <- p98_aux[1:(n-2)]
sw$p98[n] <- p98_aux[n-1] + p98_aux[n]


# Definimos un vector vacio "B" para guardar en los próximos pasos los nacimientos
B <- vector()  

# Calcular los nacimientos entre t y t+n 
idf <- which(f>0)

for(i in idf){
  B[i - min(idf-1)] <- f[i] * 5 * ((N[i] + N[i-1]*px[i-1])/2)
}
B

# Calcular los nacimientos de mujeres 
Bx_f <- sum(B)/ (1 + srb)

# Calcular la población de edad 0-5 en 1998
sw$p98[1] <- Bx_f * L[1] / (5*100000)

sw

# EJERCICIO: Repetir el procedimiento para proyectar la población a 2003
# Reproducir la tabla presentada en clase con las poblaciones a cada año y los nacimientos 
# por edad

#######################
#  MATRIZ DE LESLIE   #
#######################

# El procedimiento anterior puede pensarse como la multiplicación de una matriz (la matriz de Leslie) por un vector con la población base. La subdiagonal inferior contiene las probabilidades de superviviencia y la primera fila contiene las tasas de fecundidad para cada intervalo de edad.

# El objetivo de esta sección es construir la matriz y utilizarla para obtener la proyección de una población base.

# Los valores de la tabla deben estar en base 1. Por eso divididmos las Ls sobre l0

L <- L/100000

# Definir una matriz de largo = ancho = length(L)

Leslie_mat <- matrix(0, n, n)

# Asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior

Leslie_mat <- diag(px[-n], n - 1)

Leslie_mat <- rbind(0, Leslie_mat)

Leslie_mat <- cbind(Leslie_mat, 0)

Leslie_mat[n, n] <- Leslie_mat[n, n-1]

# Asignar las probabilidad de supervivencia para los grupos de edad en el intervalo abierto en t+n

# Asignar la tasas de fecundidad en la primera fila

f <- f/2.05 # Tasa de maternidad, riesgo de nacimientos femeninos

for (i in 1:(n-1)) {
  Leslie_mat[1, i] <- L[1] * (f[i] + f[i +1]* L[i + 1]/ L[i]) / 2
}
Leslie_mat %*% sw$p93

# EJERCICIO:

# Crear una función "leslie" para construir la matriz que tome como parámetros f y L
# Proyectar la población femenina de Suecia en el 93 al 98, utilizando la matriz

leslie <- function(f, L) {
  
# Obtenemos las probabilidades de supervivencia
  
  n <- length(L)
  
  px <- L[2:n]/L[1:(n-1)]
  
  t85 <- L[n]
  t80<- L[n-1] + t85
  
  px[n-1] <- px[n] <- t85/t80
  
# Obtenemos la matriz de Leslie

  Leslie_mat <- matrix(0, n, n)
  
  # Asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior
  
  Leslie_mat <- diag(px[-n], n - 1)
  
  Leslie_mat <- rbind(0, Leslie_mat)
  
  Leslie_mat <- cbind(Leslie_mat, 0)
  
  Leslie_mat[n, n] <- Leslie_mat[n, n-1]
  
  for (i in 1:(n-1)) {
    Leslie_mat[1, i] <- L[1] * (f[i] + f[i +1] * L[i + 1]/ L[i]) / 2
  }
  return(Leslie_mat)
}
  
LesM <- leslie(f/2.05, L/100000)

sw$p98 <- LesM %*% sw$p93

sw

# Obtenemos la base L > 10 de la matriz de Leslie

#for (i in c(1:n)) {
 # while(L[i] > 10) {
  #  L[i] <- L[i] / 10
 # }
#}

# Ejercicio:
# Crear una función "p_pop" que tome las tasas específicas de fecundidad "f", los años persona "L"
# y un vector incial de población "N0" y proyecte la población hacia el futuro.
# agregar un parametro "iter" con el número de iteraciones / intervalos de proyección.

sw <- read.table(file.path("datos","sweden_data_1993.txt"), header = F, stringsAsFactors =  F)

names(sw) <- c("age", "p93", "L", "f")

srb <- 1.05 

L <- sw$L
f <- sw$f
N <- sw$p93


p_pop <- function(f, L, n0, iter) {
  
  leslie <- function(f, L) {
    
    # Obtenemos las probabilidades de supervivencia
    
    n <- length(L)
    
    px <- L[2:n]/L[1:(n-1)]
    
    t85 <- L[n]
    t80<- L[n-1] + t85
    
    px[n-1] <- px[n] <- t85/t80
    
    # Obtenemos la matriz de Leslie
    
    Leslie_mat <- matrix(0, n, n)
    
    # Asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior
    
    Leslie_mat <- diag(px[-n], n - 1)
    
    Leslie_mat <- rbind(0, Leslie_mat)
    
    Leslie_mat <- cbind(Leslie_mat, 0)
    
    Leslie_mat[n, n] <- Leslie_mat[n, n-1]
    
    for (i in 1:(n-1)) {
      Leslie_mat[1, i] <- L[1] * (f[i] + f[i +1] * L[i + 1]/ L[i]) / 2
    }
    return(Leslie_mat)
  }
  
  n <- length(L)
  pop <- matrix(0, n, iter + 1)
  pop[,1] <- n0
  LesM <- leslie(f/2.05, L/100000)
  
  for (i in 1:iter){
    pop[,(i+1)] <- LesM %*% pop[,i]
  }
  return(pop)
}
p_pop(f, L, N, 2)

################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Proyecciones de Población                                       # 
# 26 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo es proyectar una población estructurada por edad, en este caso la
# población de Suecia, utilizando información sobre las probabilidades de 
# supreviviencia en el intervalo de la proyección y unas tasa específicas de 
# fecundidad por edad.

# Vamos a trabajar con una población cerrrada y compuesta sólo por mujeres.

# Cargamos los datos
sw <- read.table(file.path("datos","sweden_data_1993.txt"),
                 header=F, stringsAsFactors = F)

# Agregamos nombres a las columnas: "age", "p93", "L", "f"

names(sw) <- c("age", "p93", "L", "f")

# Ejercicio: Describir el contenido de cada columna de la tabla

# La primer columna tiene los limites superiores (no incluidos) de los intervalos de la población
# La segunda columna nos dice la cantidad de poblacion que se encuentra entre el limite inferior y superior de la edad
# La tercera columna representan los años persona de las pesonas que pertenece al intervalo
# La cuarta son las tasas de fecundidad por intervalo



# definimos el sex ratio at birth
srb <- 1.05

# Salvamos las columnas relevantes en objetos para facilitar la lectura del código
L <- sw$L
f <- sw$f
N <- sw$p93


# guardamos el largo de L en un objeto "n"

n <- length(L)

# calcular las probabilidades de supervivencia en cada intervalo en un vector "px"
px <- L[2:n]/L[-n]
  
# agregar la probabilidad en el intervalos abierto (últimos dos intervalos)
t85 <- L[n]
t80 <- L[n-1] + t85
px[n-1] <- px[n] <- t85/t80


# calcular la población mayor de 5 en 1998
sw$p98 <- NA
sw$p98[2:(n-1)] <- p98_aux[1:(n-2)]
sw$p98[n] <- sum(p98_aux[(n-1):n])
  
  
# definimos un vector vacio "B" para guardar en los próximos pasos los nacimientos
B <- vector()  

  
# Calcular los nacimientos entre t y t+n y guardarlos en un vector "Bx"
idf <- which(f>0)

for(i in idf){
  B[i-min(idf-1)] <- f[i] * 5 * ((N[i] + N[i-1]* px[i-1])/2) #completar
  
  #((N[i] + N[i-1]* px[i-1])/2) promedio de las mujeres expuestas al riesgo
  
}

  
# calcular los nacimientos de mujeres 
Bx_f <- sum(B) / (1 + srb)
  
# calcular la población de edad 0-5 en 1998
sw$p98[1] <- (Bx_f * L[1]) / (5 * 100000)

  

# Ejercicio: repetir el procedimiento para proyectar la población a 2003
# Reproducir la tabla presentada en clase con las poblaciones a cada año y los nacimientos 
# por edad

#######################
#  MATRIZ DE LESLIE   #
#######################

# El procedimiento anterior puede pensarse como la multiplicación de una matriz (la matriz de Leslie)
# por un vector con la población base. La subdiagonal inferior contiene las probabilidades de superviviencia
# y la primera fila contiene las tasas de fecundidad para cada intervalo de edad

# El objetivo de esta sección es construir la matriz y utilizarla para obtener la proyección de una población base.

# los valores de la tabla deben estar en base 1. Por eso divididmos las Ls sobre l0
L <- L/100000

# definir una matriz de largo = ancho = length(L)

mat <- matrix(0, nrow = n, ncol = n)

# asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior
px <- L[2:n]/L[-n]


# asignar las probabilidad de supervivencia para los grupos de edad en el intervalo abierto en t+n

Leslie <- rbind(c(0),diag(px[-n], n-1))
Leslie <- cbind(Leslie, c(0))

# asignar la tasas de fecundidad en la primera fila

f <- sw$f


# Ejercicio:
# Crear una función "leslie" para construir la matriz que tome como parámetros f y L
# Proyectar la población femenina de Suecia en el 93 al 98, utilizando la matriz

leslie <- function(f, L)
{
  n <- length(L)
  
  #Llevamos los valores a base 1
  
  for (i in c(1:n))
  {
    while (L[i] > 10)
    {mar
      L[i] <- L[i] / 10
    }
  }
  
  f <- f/2.05
  
  # definir una matriz de largo = ancho = length(L)
  px <- exp(diff(log(L)))
  
  mat <- matrix(0, nrow = n, ncol = n)
  
  diag(mat[-1, -ncol(mat)]) <- px
  
  mat[n,n-1] <- mat[n,n] <- L[n] /(L[n-1] + L[n])
  
  
  for(i in 1:(n-1)){
    
    mat[1,i] <- L[1] * (f[i] + f[i+1] * L[i+1]/L[i]) / 2
    
  }
  
  mat %*% sw$p93
  
}


# Ejercicio:
# Crear una función "p_pop" que tome las tasas específicas de fecundidad "f", los años persona "L"
# y un vector incial de población "N0" y proyecte la población hacia el futuro.
# agregar un parametro "iter" con el número de iteraciones / intervalos de proyección.


  

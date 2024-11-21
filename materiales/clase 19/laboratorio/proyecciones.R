################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Proyecciones de Población                                       # 
# 21 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo es proyectar una población estructurada por edad, en este caso la
# población de Suecia, utilizando información sobre las probabilidades de 
# supreviviencia en el intervalo de la proyección y unas tasas específicas de 
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

options(scipen = 990)

# guardamos el largo de L en un objeto "n"
n <- length(L)

# calcular las probabilidades de supervivencia en cada intervalo en un vector "px"
px <- L[2:n]/L[-n]


# agregar la probabilidad en el intervalos abierto (últimos dos intervalos)

t85 <- L[n]
t80 <- L[n-1] + t85
px[n-1] <- px[n] <- t85/t80


# Calculamos un vector auxiliar con la población en 1998
  
p98_aux <- N * px  

# calcular la población mayor de 5 en 1998 (usand la información en p98_aux)


sw$p98 <- NA
sw$p98[2:(n-1)] <- p98_aux[1:(n-2)]
sw$p98[n] <- sum(p98_aux[(n-1):n])
  

# definimos un vector vacio "B" para guardar en los próximos pasos los nacimientos
B <- vector()  
  
# Calcular los nacimientos entre t y t+n 
idf <- which(f>0)

for(i in idf){
  B[i-min(idf-1)] <- f[i] * 5 * ((N[i] + N[i-1]*px[i])/2) #completar
}
  
  
# calcular los nacimientos de mujeres 
Bx_f <- 
  
# calcular la población de edad 0-5 en 1998
sw$p98[1] <- 

  

# Ejercicio: repetir el procedimiento para proyectar la población a 2003
# Reproducir la tabla presentada en clase con las poblaciones a cada año y los nacimientos 
# por edad


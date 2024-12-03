################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Proyecciones de Población                                                    # 
# 28 de Noviembre de 2024                                                      #   
################################################################################
# El objetivo ahora es incorporar a nuestra proyección la población masculina.
# Par esto vamos a modificar la matriz de Leslie para que devuelva los 
# nacimientos *totales* en el primer elemento del vector que resulta de
# multiplicar la matriz por la población en un período dado.

# Cargamos los datos
swf <- read.table("datos/sweden_data_1993_females.txt", header=F)
names(swf) <- c("age", "p93", "L", "f")
swm <- read.table("datos/sweden_data_1993_males.txt", header=F)
names(swm) <- c("age", "p93", "L")

# guardamos columnas en objetos
f <- swf$f
Lf <- swf$L
Lm <- swm$L
Nf <- swf$p93
Nm <- swm$p93

# Modificamos la matriz para que devuelva los nacimientos totales

leslie <- function(L, m) {
  
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
    Leslie_mat[1, i] <- (m[i] + m[i +1] * L[i + 1]/ L[i]) * 5 / 2
  }
  return(Leslie_mat)
}

# Proyectamos la población usando la matriz (en el próximo paso corregimos la 
# primera entrada)
Mlf <- leslie(Lf, f) 
swf$p98 <- Mlf %*% Nf
  
# Guardamos los nacimientos en un objeto


births <- swf$p98[1]

# Corregir la primera entrada para que contenga la población de mujeres en el
# primer grupo de edad en 1998
swf$p98[1] <- (births / 2.05) * Lf[1]/ (5*100000)
  
# Proyectar la población masculina
Mlm <- leslie(Lm, f)
swm$p98 <- Mlm %*% Nm

swm$p98[1] <- (births / 2.05) * 1.05 * Lm[1]/ (5*100000)

# EJERCICIO:

# Crear una función "p_pop" que proyecte la población por edad y sexo
# tomando los siguientes argumentos:
# las tasas específicas de fecundidad "f"
# los años persona "Lf" y "Lm"
# un vector incial de población femenina "Nf" y  masculina "Nm"
# un parametro "iter" con el número de iteraciones / intervalos de proyección
# y la tasa de masculinidad "srb"


p_pop <- function(f, Lf, Lm, Nf, Nm, iter, int, srb){
  
  # Construir una matriz para hombres y otra para mujeres
  
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
  
  hombre_leslie <- leslie(f*(1.05/2.05), Lm)
  mujer_leslie <- leslie(f/2.05, Lf)
  
  # Crear una matriz "pop" para guardar la población base y las proyecciones
  
  mat_hombres <- matrix(0, length(Lm), iter + 1)
  mat_mujeres <- matrix(0, length(Lf), iter + 1)
  
  mat_hombres[,1] <- Nm
  mat_mujeres[,1] <- Nf
  
  # Projectar la población de hombres y mujeres
  
  is <- seq(1,by=2, len=iter)
  
  for(i in is){
    
    # Proyectar usando la matriz
    
    hombre_leslie %*% mat_hombres[,i]
      
    # Guardar nacimientos
    
    
    # Calcular población en 1er grupo de edad   
    
    
  }
  
  return(pop)
  
}

sw_pop <- p_pop(f = f,
                Lf = Lf/100000,
                Lm = Lm/100000,
                Nf = Nf,
                Nm = Nm,
                iter = 2,
                int = 5,
                srb = 1.05)










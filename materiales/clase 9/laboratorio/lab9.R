################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 9no Laboratorio: Tablas de Mortalidad                                        # 
# 19 de Septiembre de 2024                                                     #   
################################################################################

# El objetivo es construir una tabla de mortalidad para una cohorte 

# Disponemos de datos con la edad exacta a la muerte dx (en años) para 
# 10 personas nacidas el 1° de enero de 1800
dx <- c(71.55, 1.22, 62.91,59.60,0.07,
       22.12,71.14,16.41, 64.05, 76.79)

sort(dx)

# numero de observaciones
N <- length(dx)

# vamos a realizar los cálculos para los intervalos
# que comienzan en las siguientes edades exactas x:
x <- c(0, 1, 5, 10, 20, 30, 40, 50, 60, 70)

# tamaño de los intervalos - ver diff()
n <- diff(x)
  
######################################  
# defunciones entre edad x, x+n      #
######################################
# Empezamos por generar la primera columna de nuestra tabla: ndx

# Intervalo en el que se registra cada defunción - ver findinterval()
di <- findInterval(sort(dx), x)
  
# defunciones en cada intervalo
ndx <- as.vector(table(factor(di, levels = 1:length(x))))
  
# Creamos la tabla con las primeras dos columnas:
# edad exacta y defunciones
lt <- cbind(x, ndx)
lt

#############################
# sobrevivientes a edad x   #
#############################
# Para esto nos puede ayudar calcular la suma acumulada 
# de defunciones a edad exacta x
lx <- N - c(0, cumsum(ndx)[-length(ndx)])
  
# añadimos la columna lx a la tabla
lt <- cbind(lt, lx)
lt

#######################################
# probabilidad de morir entre x, x+n  #
#######################################
nqx <- ndx / lx

##########################################################  
# probabilidad de sobrevivir entre la edad x a edad x+n  #
##########################################################
npx <- 1-nqx

# añadimos las columnas qx, px
lt <- cbind(lt, nqx, npx)
lt

#####################################################
# años persona vividos entre edad x, x+n            #
#####################################################
# comenzamos calculando los años persona aportados por cada fallecimiento en 
# el intervalo que sucede el fallecimiento
ap <- sort(dx) - x[di]
  
# sumamos estos años persona por intervalo - ver by()
sum_ap <- as.vector(by(ap, di, sum))

all_ap <- rep(0, length(x))
all_ap[unique(di)] <- sum_ap

Lx <- (lx - ndx)*n + all_ap
  
# añadimos la columna Lx
lt <- cbind(lt, Lx)
lt

####################################################
# anios persona vividos por encima de la edad x    #
####################################################
Tx <- c(sum(Lx), (sum(Lx) - cumsum(Lx))[1:length(Lx)-1])
  
# añadimos la columna Tx
lt <- cbind(lt, Tx)
lt

#################################
# Esperanza de vida a edad x    #
#################################
ex <- Tx/lx
  
# añadimos la columna ex
lt <- cbind(lt, ex)
lt

# Describa las dos últimas columnas en lt
# cuál es su significado?

Revisar con diapos.

# Tx es la sumatoria de los años persona por encima de la edad x, o sea la
# exposición al riesgo que experimentaran un conjunto de personas despues de 
# alcanzar cierta edad x.

# Ex es la esperanza de vida, es el promedio del tiempo que se espera que viva
# una persona habiendo alcanzado la edad x, atravesando las condiciones de 
# mortalidad sin cambios tomadas en cuenta en la tabla.
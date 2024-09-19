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
N <- 10

# vamos a realizar los cálculos para los intervalos
# que comienzan en las siguientes edades exactas x:
x <- c(0, 1, 5, 10, 20, 30, 40, 50, 60, 70)

# tamaño de los intervalos - ver diff()
n <- diff(x, lag=1)
  
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
lx <- N - c(0, cumsum(ndx[1:9]))
  
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
ap <- ((lx - ndx) * n2) + c(dx-x[di])
  
# sumamos estos años persona por intervalo - ver by()
sum_ap <- by
  

Lx <- 
  
# añadimos la columna Lx
lt <- cbind(lt, Lx)
lt

####################################################
# anios persona vividos por encima de la edad x    #
####################################################
Tx <- 
  
# añadimos la columna Tx
lt <- cbind(lt, Tx)
lt

#################################
# Esperanza de vida a edad x    #
#################################
ex <- 
  
# añadimos la columna ex
lt <- cbind(lt, ex)
lt

# Describa las dos últimas columnas en lt
# cuál es su significado?



################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de tiempos a distintos eventos demográficos          # 
# 31 de Octubre de 2024                                                        #   
################################################################################

# El objetivo de este módulo es simular tiempos de espera o duraciones a los
# eventos fallecimiento y primer hijo. Para esto partimos de unas tasas 
# específicas de mortalidad y unas tasas específicas de transición a la maternidad. 
# Se asume que el riesgo de los eventos es *constante* en cada intervalo (edades)
# Bajo este supuesto, las tasas de ocurrencia/exposición representan al riesgo del evento.
# Es decir que el vector de tasas específicas representa la función de riesgo del evento.

# Vamos a empezar por entender las relaciones entre las funciones de riesgo, riesgo 
# acumulado y función de supervivencia.

###############
#   DATOS     #
###############
# Cargamos los datos de las tasas específicas por edad del primer hijo  
# *condicionales*  de una cohorte
fert <- read.csv(file.path("datos","fx.csv"))
plot(fert)

# Describir lo que se observa en el gráfico. Que son estos valores? Que hay en 
# numerador y denominador para su cálculo?

# Estamos viendo la probabilidad de tener el primer hijo de una cohorte de mujeres
# que aun no tuvieron, en el denominador estan los años persona del año de mujeres 
# que aun no experimentaron el evento (nacimiento), en el numerador tenemos 
# la cantidad de primeros nacimientos


# Cargamos los datos de las tasas específicas de mortalidad de una cohorte 
mort <- read.csv(file.path("datos","mx.csv"))
plot(mort)

# Describir lo que se observa en el gráfico? Que son estos valores? Que hay en 
# numerador y denominador para su cálculo?

# En el grafico podemos ver las tasas de mortalidad especificas por edad.
# En el numerador tenemos las defunciones, y en el denominador los años persona o sea
# la exposición al riesgo del evento.

# Estas tasas van a representar nuestra función de riesgo constante a intervalos para 
# las variables aleatorias "Tiempo al Fallecimiento" y "Tiempo al Primer Hijo" 

#################################################################################
#   Función de riesgo, función de riesgo acumulado y función de supervivencia   #
#################################################################################

# input
x <- mort$edad
lambda <- mort$h

# Responder:
# Cuál es el riesgo de morir:
# 1) Entre edades 0-1
lambda[1]
# 0.200466

# 2) Entre edades 50-51
lambda[51]
# 0.0043350

# 3) Entre edades 100-101
lambda[101]
# 0.7123090

# Cual es el riesgo de morir:
# 1) antes de alcanzar la edad exacta 0
0

# 2) antes de alcanzar la edad exacta 1
sum(lambda[seq_len(1)])
# 0.2004660

# 3) antes de alcanzar la edad exacta 100
sum(lambda[seq_len(100)])
# 5.703166

# 4) antes de alcanzar la edad exacta 101
sum(lambda[seq_len(101)])
# 6.415475

# Ejercicio:
# Crear una función pw_H (piece-wise Hazard) con argumentos x, lambda
# que calcule el riesgo acumulado hasta desde edad 0 hasta la edad maxima
# en los datos

pw_h <- function (x, lambda) {
  
  k = sum(lambda[seq_len(x)])
  return(k)
} 

# función de riesgo acumulado



# Responder utilizando la función: 
# Cual es el riesgo de morir:
# 1) antes de alcanzar la edad exacta 0
pw_h(0, lambda)

# 2) antes de alcanzar la edad exacta 1
pw_h(1, lambda)

# 3) antes de alcanzar la edad exacta 100
pw_h(100, lambda)

# 4) antes de alcanzar la edad exacta 101
pw_h(101, lambda)

# Ejercicio: Graficar la función de riesgo acumulado.
# (computar la función en un vector H utilizando pw_H)

h = c(0 : 101)

H = as.vector(lapply(h, function(j) pw_h(j, lambda)))

# Profe
sup <- min(x):(max(x)+1)
H <- rep(NA, length(sup))

for (i in 1:length(sup)) {
  H[i] <- pw_h(i-1, lambda)
}
H
#

plot(sup, H, type="l", lwd=3, col=2)

# Ejercicio: Computar y graficar la función de supervivencia

# Responder: 
# Cual es la probabilidad de sobrevivir:
# 1) a edad exacta 0

# 2) a edad exacta 1

# 3) a edad exacta 100

# 4) a edad exacta 101

# Que significa que esta última probabilidad no sea 0?


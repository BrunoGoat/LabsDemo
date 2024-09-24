################################################################################
# Demografía IESTA                                                             #
# Daniel Ciganda                                                               # 
# 2ndo Laboratorio: Modelos del proceso reproductivo                           # 
# 22 de Agosto de 2024                                                         #   
################################################################################

# En el laboratorio anterior trabajamos el cálculo de las tásas específicas 
# de fecundidad por edad a partir de datos de trayectorias reproductivas.

# Ahora vamos a empezar a modelar esas trayectorias reproductivas 
# utilizando los conceptos que vimos en clase.

###############################################################
# 1ra Parte - Modelos del 1er Nacimiento                      #
###############################################################

source("plot_fun.R") # funciones para graficar

# Comenzamos modelando el tiempo al primer nacimiento utilizando
# la fórmula propuesta por Gini

# Método Gini
n <- 10^6
fi <- 0.2 # fecundabilidad: probabilidad mensual de concebir 
m <- 0:30 # observamos concepciones desde el 1er al 30vo mes desde el casamiento
gini_props <- ((1 -fi)^{m})*fi  #completar
gini <- as.data.frame(cbind(meses = m, gini_props))
plot(gini)

# Describa brevemente lo que se observa en el gráfico


# Gini sugiere que la probabilidad de tener un hijo es de 0.20 en el 
# momento/mes 0 y a medida que pasa el tiempo la probabilidad disminuye, por
# ejemplo, cae mas de la mitad 5 meses despues. Luego sigue cayendo y se mantiene 
# en una probabilidad muy cercana a 0.

# Ahora simulamos meses hasta la primera concepción a partir de una 
# distribución geométrica (distribución binomial negativa con un éxito)

meses <- rnbinom(n = n, size = 1, prob = fi) #completar

nbinom <- as.data.frame(prop.table(table(meses))) 
names(nbinom)[2] <- "nbinom_props"
con <- merge(gini, nbinom, by = "meses", all = T)
con <- con[con$meses %in% m,]

# comparamos ambos resultados
plot(con$meses, con$gini_props,
     xlab = "Meses después de la unión",
     ylab = "Proporción de nacimientos", ylim = c(0,0.22))
points(con$meses, con$nbinom_props, col = "red")
legend(10, 0.15, legend = c("Modelo Gini", "Distribución Geométrica"),
       lwd = c(1), col = c("black", "red"),
       cex=0.95, bty = "n",
       lty = c(0, 0),
       pch = c(1, 1),
       y.intersp = 1,
       x.intersp = 0.5)

# Describa brevemente lo que se observa en el gráfico

# En el nuevo grafico podemos ver que el modelo de Gini parece tener resultados
# muy similares a la simulación que hicimos con la distribución geometrica.


# Ahora que sabemos como simular el tiempo de espera hasta una 1era concepción 
# vamos a modelar la heterogeneidad (entre mujeres) con respecto al riesgo
# de concebir a partir de una distribución beta 

# Queremos obtener una distribución con la siguiente media y desvío estandar:
mu <- 0.2
sigma <- 0.13

# Calcular alpha y beta
alpha <- mu * ((mu * (1 - mu)) / (sigma^2) - 1)
beta <- (1 - mu) * ((mu * (1 - mu)) / (sigma^2) - 1)

# Simular valores a partir de nuestra distribución beta
fi_i <- rbeta(n, alpha, beta) # completar

# Graficar
hist(fi_i, breaks = 50, main = "", xlab = "Fecundabilidad")

# Describa brevemente lo que se observa en el gráfico

# Vemos como la tasa de fecundabilidad varia entre las diferentes mujeres,
# siendo lo mas frecuente al rededor de 0.14-0.2 y luego se aprecia una cola
# hacia la derecha en la grafica mostrando que hay mujeres con mayor fecundidad
# pero cada vez con menor frecuencia.

# Ejercicio:
# Simular proporciones de nacimientos entre los meses 10-30 a partir de una
# distibución binomial incorporando en el modelo la heterogeneidad con 
# respecto a la fecundabilidad.

# Comparar en un gráfico con los resultados obtenidos con el modelo
# sin heterogeneidad

h_meses <- sapply(fi_i, function(x) rnbinom(1, 1, x))
h_nbinom <- as.data.frame(prop.table(table(h_meses)))
names(h_nbinom) <- c("meses", "h_nbinom_props")
con <- merge(gini, nbinom, by = "meses")
con <- merge(con, h_nbinom, by = "meses")

plot(con$meses, con$gini_props, ylim = c(0,0.25),
     xlab = "Meses después de la unión",
     ylab = "Proporción de nacimientos")
points(con$meses, con$nbinom_props, col = "red")
points(con$meses, con$h_nbinom_props, col  = "blue")
legend(20, 0.18, legend = c("Modelo Gini",
                            "rbinom",
                            "Modelo con Heterogeneidad"),
       lwd = c(1), col = c("black", "red", "blue"),
       cex=0.95, bty = "n",
       lty = c(0, 0, 0),
       pch = c(1, 1, 1),
       y.intersp = 1,
       x.intersp = 0.5)

# Que diferencia presenta el modelo que considera la heterogeneidad 
# de las mujeres con respecto a su fecundabilidad?

# Se aprecia que en nuestro nuevo modelo con heterogeneidad la proporcion de
# los nacimientos es ligeramente menor a los realizados anteriormente, y que a 
# partir de aproximadamente pasados los 10 meses tras la union nuestro modelo 
# con heterogeneidad comienza a tener mayor proporción de nacimientos que
# nuestro modelo Gini y la geometrica.

# Por qué se observa esa diferencia?

# Se observa ya que ahora hay diferentes tasas de fecundabilidad en cada mujer,
# esto hace que en un principio haya muchas mujeres con una tasa menor a la 
# de 0.2 que propusimos en nuestros otros modelos.
# (Otras tienen mas pero no son suficientes como para compensar) 
# Eso explica la leve caida del principio, y cuando se compensa pasados los 10
# meses es debido a la alta fecundabilidad de las otras mujeres, ademas de que
# necesariamente al ser una densidad debe integrar uno bajo la curva por ende
# es lógico pensar que si hubo una disminucion, tendras que "compensar" esa
# área perdida.


###############################################################
# 2ra Parte - Modelos del proceso reproductivo                #
###############################################################

# La intención ahora es comenzar con una primera aproximación a un
# modelo del proceso reproductivo completo, utilizando los elementos 
# incorporados hasta ahora.

# El primer paso es modelar el inicio del proceso, es decir la edad a la union
# para esto utilizamos una distribución log-normal

n <- 8
mu <- 20 # media de la distribución de edad a la union
su <-   1.1 # desviación estandar de la distribución de edad a la union

# Simulamos el tiempo de espera a la primera union en MESES
# equivale a la edad a la unión ya que lo modelamos desde el nacimiento

# nota: utilizamos la notación wt = waiting time 
# para definir el tiempo de espera a un evento 
# wt_u = waiting time to union 

wt_u <- rlnorm(n, meanlog = log(mu^2/ sqrt(mu^2+su^2)),
               sdlog = sqrt(log(1 + su^2/mu^2))) * 12

# definimos el resto de los parámetros
ns <- 6 # período de no suceptibilidad
fi <- 0.2

# simulamos el tiempo de espera en meses a la 1era concepción
wt_1c <- rnbinom(n, 1, fi) 

# calculamos la edad al primer nacimiento
wt_1b <- wt_u + wt_1c + 9

# ponemos todo en un data frame y graficamos
hst <- as.data.frame(cbind(id = 1:n,
                           edad = wt_1b/12,
                           paridad = 1))

plot_hst(hst, c(0.5, n), n)

# describa brevemente lo que se observa en el gráfico 


# Ejercicio:
# Simular la edad al segundo nacimiento para este cohorte de mujeres. Graficar.

wt_2c <- wt_1b + ns + wt_1c # completar 
wt_2b <- wt_2c + 9 # completar 

hst <- as.data.frame(cbind(id = rep(1:n, 2),
                           edad = c(wt_1b, wt_2b)/12,
                           paridad = rep(1:2,each = n)))

plot_hst(hst, c(0.5, n), n)

##################################################
# función para generar historias reproductivas   #
##################################################

# La idea es poner los bloques trabajados hasta ahora en una función que 
# genere todas las trayectorias reproductivas de un chorte de n mujeres,
# desde la formación de la unión hasta el fin de la edad reproductiva.

# Ejercicio: completar los comentarios describiendo el contenido de la línea
# (dentro de la función)

# Cuáles son los argumentos que toma la función?

gen_hst <- function(n, fi, ns, mu, su){
  
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 
  # Esto describe el tiempo hasta la unión en meses
  
  wt_c <- lapply(1:50, function(x) rnbinom(n, 1, fi)) 
  # Esto simula el tiempo de concepción x mujer en tener de 1 a 50 hijos con
  # una distribución geometrica
  
  wt_b <- list()
  wt_b[[1]] <- wt_u + wt_c[[1]] + 9 
  #Aqui simulamos cuanto tiempo pasa hasta el primer nacimiento de cada mujer
  
  hst <- list()
  hst[[1]] <- as.data.frame(cbind(id = 1:n,
                                  edad = wt_b[[1]]/12,
                                  paridad = 1))
  
  for(i in 2:20){
    
    wt_b[[i]] <- wt_b[[i-1]] + ns + wt_c[[i]] + 9 
    # Tiempo en meses del nacimiento x, siendo x > 1
    
    nid <- which(wt_b[[i]]>50*12) 
    # Buscamos que mujeres tuvieron hijos despues de los 50 años
    
    wt_b[[i]][nid] <- NA 
    
    if(sum(is.na(wt_b[[i]])) == n){break} 
    
    #Nos fijamos si el tiempo de espera hasta el nacimiento del niño i es Na, 
    # o sea, si nació despues de los 50 años, al hacer la suma y compararla con
    # n estamos viendo si todos los nacimientos que ocurrieton en la iteracion
    # numero i ya superan los 50 años, con eso ya no tenemos informacion 
    # relevante para seguir en el bucle y salimos con break.
    
    hst[[i]] <- as.data.frame(cbind(id = rep(1:n,i),
                                    edad = unlist(wt_b)/12,
                                    paridad = rep(1:i, each = n)))
    hst[[i]] <- hst[[i]][!is.na(hst[[i]]$edad),]
  }
  
  return(hst)
  
}

# Ejercicio: Generar las trayectorias reproductivas de una cohorte de 
# 8 mujeres con las siguiente parametrización:
# fi = 0.2
# ns = 6
# mu_u = 20 
# sd_u = 1.1

ls_hst <- gen_hst(n = 8, fi = 0.2, ns = 6, mu = 20, su = 1.1) # completar
  
# graficamos
for (i in 1:length(ls_hst)){
  
  plot_hst(ls_hst[[i]], c(0.5, n),n)
  
  Sys.sleep(1)  
}

# n mayor
ls_hst <- gen_hst(n = 1000, fi = 0.2, ns = 6, mu = 20, su = 1.1)

# plot f(x)
plot_fx(ls_hst[[length(ls_hst)]])


# Que se observa en el gráfico? Por qué?

# Se observa que, hasta aproximadamente los 18 años, no hay concepción, 
# ya que se asume que no ocurre concepción hasta el matrimonio, el cual 
# empieza a darse en esa etapa. A partir de ese punto, la concepción comienza 
# a aumentar y, alrededor de los 22-23 años, se estabiliza. 
# Esto se debe a que se ha tomado una probabilidad de concepción constante, 
# lo cual no refleja la realidad.



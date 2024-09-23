################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 5to Laboratorio: Proceso Reproductivo y fecundidad                           # 
# 5 de Septiembre de 2024                                                      #   
################################################################################

# Vamos a trabajar con el último modelo que construimos, con fecundabilidad
# dependiente de la edad

r = 0.02094238 
x0 = 454.6911
n = 8
ns = 6
mu = 20
su = 1.1

gen_hst_t <- function(n, ns, x0, r, mu, su, id = vector(), wt_c = vector()){
  
  meses <- 1:(50*12) # vector con el tiempo t en meses
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(meses-x0))) # fi en t
  fi_t[max(meses)] <- 0 # nos aseguramos que haya riesgo 0 luego de 50 años de edad 
  plot(meses,fi_t)
  
  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12
  #plot(prop.table(table(round(rlnorm(10^6, meanlog = log(mu*12), sdlog = log(1.1)),0))))
  
  # describa lo que se observa en el gráfico
  # Vemos la distribucion de la probabilidad del tiempo de union, siendo los 
  # casos mas frecuentes al rededor de los 240 meses (20 años)
  
  # Tomamos la fecundabilidad en los meses luego del mes a la union para cada mujer en n
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)])
  
  # tomamos el máximo nr de meses por los que vamos a observar a las mujeres en nuestra muestra
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it, function(x) x[t])) # que mujeres experimentan una concepción en el mes t 
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # tiempo de espera a la concepción
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      # eliminamos el riesgo desde la concepción hasta el final del periodo de no susceptibilidad
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) 
      
    }
    
  }
  
  data <- as.data.frame(cbind(id = id, wt_c = wt_c))
  data <- data[order(data$id),]
  hst <- as.data.frame(cbind(id = data$id,
                             edad = (data$wt_c + 9)/12,
                             nac = rep(table(data$id), table(data$id)),
                             paridad = sequence(table(data$id))))
  return(hst)
}

#######################################################################  
# Modelos del proceso reproductivo - Efectos de los determinantes     #
# próximos en la fecundidad natural                                   #
#######################################################################

# El objetivo ahora es utilizar el modelo para estimar el efecto de distintos
# factores en la fecundidad de una cohorte. 

# Desde el punto de vista del análisis del modelo este ejercicio se conoce como
# "analisis de sensibilidad" en el que se busca estimar la contribución
# relativa de cada uno de los parámteros en la explicación de la variabilidad
# en el resultado.


# Empecemos por fijar una población de referencia sobre la que vamos a medir
# las diferencias.
# Para esto utilizamos la parametrización que obtuvimos al intentar ajustar 
# las tasas de los Huteritas.
# El objetivo es obtener las tasas específicas y la fecundidad total en esta
# población de referencia tomando el tamañano de la cohorte = 5000.
# Para esto vamos utilizar una función para graficar los resultados y obtener
# la fecundiad total asociada a cada parametrización del modelo.
# Esto nos va permitir calcular las diferencias en la fecundidad total en el
# análisis de sensibilidad.

r = 0.028
x0 = 420
n = 5000
ns = 11
mu = 18
su = 1.1

hst_ref <- gen_hst_t(n = n, ns = ns , x0= x0 , r = r, mu = mu, su = su , id = vector(), wt_c = vector())# completar

plot_sum_fx <- function(dat, lines = F, ...){
  fx <- table(factor(floor(dat$edad), levels = 10:50)) / max(dat$id)
  if(lines){
    lines(10:50, fx, ...)
  }else{
    plot(10:50, fx, ...)
  }
  return(sum(fx))
}

plot_sum_fx(hst_ref, ylim = c(0,0.7))

#######################################################################  
# ANALISIS DE SENSIBILIDAD                                            #
#######################################################################

# El objetivo ahora es observar el efecto sobre las tasas específicas
# de fecundidad y sobre el número promedio de hijos por mujer
# de la variación en el valor de los parámetros


# Variamos los parámetros relevantes de a 1 por vez, dejando el resto fijos.


#################################
# Período de no suceptibilidad  #
#################################
ns_vals <- seq(6, 24, 1)

hst_ns_var <- lapply(ns_vals, function(x) gen_hst_t(n = n, ns = x, x0 = x0,r = r,mu =  mu,su = su))# completar usando lapply()

ref_tfr <- plot_sum_fx(hst_ref, ylim = c(0, 0.7))
var_tfr_ns <- lapply(hst_ns_var, plot_sum_fx, lines = T, col = "red") #completar

# Describa lo que se observa en el gráfico.

# En la grafica podemos ver los diferentes valores de ns en las lineas rojas, 
# las tasas mas grandes de natalidad son los valores mas bajos de ns ya que con
# periodos de no suceptibilidad bajos hay mas tiempo para concepciones, en este 
# caso el valor mas bajo es 6, las tasas mas bajas son para los ns mas altos.


# Ahora calculamos la diferencia entre las cohortes simuladas
# con los distintos valores de ns y la cohorte de referencia 
# con respecto a la tasa global de fecundidad (tfr) 

tfr_dif_ns <- sapply(var_tfr_ns, function(x) x - ref_tfr)# completar usando sapply()
    
# y graficamos
plot(ns_vals, tfr_dif_ns)

# Describa lo que se observa en el gráfico 

# Vemos las distintas diferencias de la tasa global de fecundidad con respecto 
# a la tasa original cuando ns era igual a 11, con el menor ns (6) podemos ver
# que la tasa de fecundidad es mayor en 2 unidades y con el mayor ns (24) la 
# tfr es menor en 3 unidades



###############################
# media de la edad a la union #
###############################
mu_vals <- seq(18, 25, 0.25)

hst_mu_var <- lapply(mu_vals, function(x) gen_hst_t(n = n, ns = ns, x0 = x0,r = r,mu =  x,su = su))

plot_sum_fx(hst_ref, ylim = c(0, 0.7))

var_tfr_mu <- lapply(hst_mu_var, plot_sum_fx, lines = T, col = "red") #completar
  
# Describa lo que se observa en el gráfico

# En la grafica podemos ver los diferentes valores de mu en las lineas rojas,
# o sea las diferentes medias de edad en meses de las mujeres con medias mas
# bajas tendremos mas concepciones porque hay mas tiempo y mejor fecundabilidad
# (ya que depende negativamente de la edad)


# Ahora calculamos la diferencia con respecto a la tasa global de fecundidad (tfr)

tfr_dif_mu <- sapply(var_tfr_mu, function(x) x - ref_tfr)
  
plot(mu_vals, tfr_dif_mu)

# Describa lo que se observa en el gráfico

# Vemos como con mu's mayores cae la tfr por lo explicado anteriormente

########################################
# desvio estdr. dist. edad a la union #
#######################################
su_vals <- seq(1.1, 4, 0.2)

hst_su_var <- lapply(su_vals, function(x) gen_hst_t(n = n, ns = ns, x0 = x0,r = r,mu =  mu,su = x))
  
plot_sum_fx(hst_ref, ylim = c(0,0.7))
var_tfr_su <- lapply(hst_su_var, plot_sum_fx, lines = T, col = "red")

# Describa lo que se observa en el gráfico

# Podemos ver los diferentes desvios de la edad de union, cuando el desvio es 
# menor significa que todas las parejas se unen en un tiempo cercano unas de 
# otras y como nuestro modelo contempla las concepciones a partir de la union
# podemos ver como con un desvio bajo el aumento de la probabilidad tiene una
# gran pendiente debido a que todas las parejas comienzan a concebir al mismo 
# tiempo, con mayor desvio, unas parejas comienzan antes y otras despues 
# causando una pendiente no muy inclinada


# Ahora calculamos la diferencia con respecto a la tasa global de fecundidad (tfr)

tfr_dif_su <- sapply(var_tfr_su, function(x) x - ref_tfr)
  
plot(su_vals, tfr_dif_su)

# Describa lo que se observa en el gráfico

# Los diferentes valores del desvio parecen apensas afectar la tfr (<0.1) ya 
# las concepciones suceden en practicamente la misma medida con la unica 
# diferencia es que tan alejadas en el tiempo estan las concepciones de una 
# pareja y otra.


#################################################
# Punto de inflexión caída de la fecundabilidad #
#################################################
x0_vals <- seq(420, 300, -10)

hst_x0_var <- lapply(x0_vals, function(x) gen_hst_t(n = n, ns = ns, x0 = x,r = r,mu =  mu,su = su))
  
plot_sum_fx(hst_ref, ylim = c(0,0.7))
var_tfr_x0 <- lapply(hst_x0_var, plot_sum_fx, lines = T, col = "red")
  
# Describa lo que se observa en el gráfico

# Con una caida de la fecundabilidad mas temprana vemos como la probabilidad 
# comienza a caer antes en el tiempo con respecto a x0 mayores


# Ahora calculamos la diferencia con respecto a la tasa global de fecundidad (tfr)

tfr_dif_x0 <- sapply(var_tfr_x0, function(x) x - ref_tfr)
  
plot(x0_vals, tfr_dif_x0)

# Describa lo que se observa en el gráfico

# Vemos como con menores valores de x0 o sea caidas de la fecundabilidad mas 
# temprana afectan a la tfr debido a que se dispone de mucho menos probabilidad
# de lograr una concepcion durante mas tiempo.

#################################################
# Pendiente caída de la fecundabilidad          #
#################################################
r_vals <- seq(0.02, 0.1, 0.005)

hst_r_var <- lapply(r_vals, function(x) gen_hst_t(n = n, ns = ns, x0 = x0,r = 0.1,mu =  mu,su = su))
  
plot_sum_fx(hst_ref, ylim = c(0,0.7))
var_tfr_r <- lapply(hst_r_var, plot_sum_fx, lines = T, col = "red")
  
# Describa lo que se observa en el gráfico

# Vemos como una vez se llega a una edad avanzada con una r alta la fecundabilidad
# cae mucho y rapidamente disponiendo de menos tiempo para la concepcion
# por ende esto desencadena una caida de la tfr,y viceversa

# Ahora calculamos la diferencia con respecto a la tasa global de fecundidad (tfr)

tfr_dif_r <- sapply(var_tfr_r, function(x) x - ref_tfr)
  
plot(r_vals, tfr_dif_r)

# Describa lo que se observa en el gráfico

# Vemos que a mayor r, menor tfr por lo explicado anteriormente

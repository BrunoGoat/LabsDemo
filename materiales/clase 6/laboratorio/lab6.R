################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 6to  Laboratorio: Proceso Reproductivo y fecundidad                          # 
# 10 de Septiembre de 2024                                                     #   
################################################################################

# Definimos las funciones para graficar
source("plot_fun.R")

################
# 1era Parte   #
################################################################################
# Modelos del proceso reproductivo - fecundidad natural a fecundidad regulada  #
################################################################################

# El objetivo ahora es aproximarse al modelado de unas trayectorias reproductivas 
# en un contexto de fecundidad regulada. Para esto vamos a incorporar en el
# modelo que veniamos trabajando hasta hora dos elementos nuevos:

# Las preferencias individuales - Nr. Deseado de Hijos
# El uso de anticonceptivos

# La referencia empírica de nuestro modelo ya no será una población con régimen
# de fecundidad natural como la de los Huteritas, sino una población donde si 
# se aplican sistemáticamente métodos voluntarios de control de los nacimientos.
# Tomamos el ejemplo de España.
# Vamos a utilizar las tasas de fecundidad por edad y por cohorte en España
# disponibles en  la Human Fertility Database 

# Ir a www.humanfertility.org y descargar las tasas específicas de fecundiad
# de cohorte para España guardarlas con el nombre asfr_hfd.txt en la carpeta 
# "datos"

# Cargamos los datos
fx_es <- read.table(file.path("datos","asfr_hfd.txt"), skip = 2,
                    header=T, stringsAsFactors = F)

# Graficamos
plot_fx_hfd(dat = fx_es, cohorts = 1940:1970, type = "lines")


# Describir lo que se observa en el gráfico.

# A medida que las cohortes son mas recientes vemos como se traslada el maximo 
# de la fecundidad desde los 20 hasta al rededor de los 30, y el maximo de la 
# tasa de fecundidad es mucho mas bajo, es incluso la mitad. Se reduce el nivel 
# y hay un aplazamiento de la fecundidad.


#######################################################
# Preferencias individuales: Número Deseado de Hijos  #  
#######################################################

# Comenzamos por modelar las preferencias. Primero vamos a tener que
# hacernos una idea de como se distribuyen las preferencias del
# tamaño deseado de familia.
# Para esto vamos a explorar una distribución empírica, utilizando datos 
# de las "International Value Survey" donde se pregunta a las personas por su
# número ideal de hijos.

# cargamos los datos
obs_d <- read.table(file.path("datos","ideal_nr_chlidren_1990_ivs.csv"))

# explorar los datos
head(obs_d)
table(obs_d)
table(obs_d$ideal_nr, useNA = "always")

# Obtener un data.frame con los valores y las frecuencias 
# Obtener un data.frame con los valores y las frecuencias 

obs_d <- as.data.frame(table(obs_d$ideal_nr), stringsAsFactors = F)
names(obs_d) <- c("nr","freq")

# graficar las frequencias
plot(x = obs_d$nr , y = obs_d$freq , type = "p")

# graficar las proporciones
plot(x = obs_d$nr, y = obs_d$freq/sum(obs_d$freq) , type = "p")

# Describa brevemente lo que se observa en el gráfico

# En el grafico podemos ver las proporciones de las preferencias de las personas
# para diferentes cantidades de hijos, los mas repetidos son 1, 2, 3 

# Vamos a modelar el número deseado de hijos con una distirbución lognormal. 
# Para esto vamos a simular 1000 números aleatorios de una distribución lognromal.
# Con los siguientes parámetros:

mu_d <- 2.4 
sd_d <- 1.1

sim_d <- round(rlnorm(n = 5000, meanlog = log(mu_d^2/ sqrt(mu_d^2+sd_d^2)),
                      sdlog = sqrt(log(1 + sd_d^2/mu_d^2))),0)
table(sim_d)

# Comparar el ajuste de los datos simulados a los datos obervados
plot(c(0,obs_d$nr), c(0,obs_d$freq/sum(obs_d$freq)), type = "p")
lines(table(sim_d)/sum(table(sim_d)), type = "p",
       ylim = c(0, 0.5), xlim = c(0,10), col = "red")


# Describir los que se observa en el gráfico
# Vemos que ajusta bastante bien con nuestro grafico inicial

# Ahora que comprobamos que la distribución lognormal ajusta bien los datos del 
# número ideal de hijos, podemos utilizar esta distribución para asignar
# el número deseado de hijos a las mujeres/parejas en nuestro modelo de
# simulación (abajo)

#######################################################
# Prácticas Anticonceptivas                           #  
#######################################################
# Para modelar el efecto del uso de anticonceptivos vamos a introducir 
# un parámetro "c".

# Ejercicio: Explorar el modelo y completar los comentarios describiendo 
# las operaciones que se realizan en cada caso.
# Para explorar los objetos se pueden definir los argumentos con estos valores:
n <- 10
ns <- 11
x0 <- 420
r <-  0.04
mu <- 22
su <- 1.1
mu_d <- 2.4 # Media del numero deseado de hijos
sd_d <- 1.1 # Desvio del numero deseado de hijos
c <- 0.1

gen_hst_d <- function(n, ns, x0, r, mu, su, mu_d, sd_d, c){
  
  id <-  vector()
  wt_c <- vector()
  k <- rep(0,n)
  edad <- 1:600
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(edad-x0)))
  fi_t[589:600] <- 0
  
  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 #Se calcula el tiempo hasta la union en meses utilizando la distribucion log normal
  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) # n vectores de los cuales se desprende la probabilidad de concebir mes a mes de cada mujer
  
  dk <- round(rlnorm(n, meanlog = log(mu_d^2/ sqrt(mu_d^2+sd_d^2)),
                        sdlog = sqrt(log(1 + sd_d^2/mu_d^2))),0) # Se simulan n numeros deseados de hijos (en numeros enteros ya que no tiene sentido por ejemplo 2.5 hijos)
  
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk)) #Cada uno de los elementos de fi_it y vamos a crear un subset (aplicando la funcion [, esto crea el subset), y el subset se hace en la t, con esto extraemos la probabilidad de concebir de cada mujer en el momento t.
    
    #Mientras la condicion sea falsa, multiplicamos por 1, cuando es verdadera (la pareja alcanza el numero deseado de hijos), la pareja comienza a usar anticonceptivos, entonces en el modelo se empieza a multiplicar por c, reduciendo en gran medida la probabilidad de concebir pero no hasta 0 ya que el anticonceptivo no es 100% efectivo.
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # guardamos los diferentes edades hasta la concepcion en meses
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) # eliminamos la posibilidad desde que hubo concepción hasta que termine el periodo de no susceptibilidad
      
      k <- as.vector(table(factor(id, levels = 1:n))) #Cantidad efectiva de hijos de cada mujer en cada momento t
      
    }
    
  }
  
  # data
  hst <- as.data.frame(cbind(id = id, wt_c = wt_c))
  hst <- hst[order(hst$id),]
  hst <- as.data.frame(cbind(id = hst$id,
                             edad = (hst$wt_c + 9)/12,
                             nac = rep(table(hst$id), table(hst$id)),
                             paridad = sequence(table(hst$id))))
  return(hst)
}

# Ejercicio: Describir como se modela el número ideal de hijos y el efecto 
# de las prácticas anticonceptivas. 

# El numero ideal de hijos se modela con una log normal debido a que ajusta bien a la realidad 
# como demostramos al principio del practico. 

# El efecto de las practicas anticonceptivas disminuye en gran proporcion la probabilidad de tener
# un hijo una vez es alcanzado el numero deseado.


# Que significa que en el nuevo modelo tenemos dos probabilidades de concebir:
# fecundabilidad y fecundabilidad residual?

# Ahora nuestra fecundabilidad es cuando se dan los nacimientos deseados sin utilizar metodos anticonceptivos
# en cambio la fecundabilidad residual son los nacimientos que ocurren por fallos de la anticoncepción


# Ejercicio: Utilizando el modelo del proceso reproductivo en régimen de 
# fecundidad regulada:
# aproximar las tasas específicas de fecundidad por edad de la cohorte de 1940 
# en España y de la cohorte de 1970. Comparar los datos observados con los
# simulados (utilizando las funciones plot_fx_hfd y plot_fx) y
# describir los cambios introducidos en los parámetros del modelo para aproximar
# una y otra distribución de tasas específicas. con n = 3000


#n <- 10
#ns <- 11
#x0 <- 420
#r <-  0.04
#mu <- 22
#su <- 1.1
#mu_d <- 2.4 # Media del numero deseado de hijos
#sd_d <- 1.1 # Desvio del numero deseado de hijos
#c <- 0.1

#gen_hst_d <- function(n, ns, x0, r, mu, su, mu_d, sd_d, c)

plot_fx_hfd(dat = fx_es, cohorts = 1940, type = "lines")

Sim_40 <- gen_hst_d(n=3000, ns=12, x0=400, r=0.03 , mu= 24, su=4, mu_d=1.7, sd_d=1.1, c=0.1)
plot_fx(dat = Sim_40 ,points =  T)

plot_fx_hfd(dat = fx_es, cohorts = 1970, type = "lines")

Sim_70 <- gen_hst_d(n=3000, ns=12, x0=400, r=0.03 , mu= 30, su=9, mu_d=0.8, sd_d=1, c=0.1)
plot_fx(dat = Sim_70 ,points =  T)


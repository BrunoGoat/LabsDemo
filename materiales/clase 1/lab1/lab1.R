################################################################################
# Demografía 2024 Licenciatura en Estadística                                  #
# Daniel Ciganda                                                               # 
# 1er Laboratorio: Proceso Reproductivo y fecundidad                           # 
# 20 de Agosto de 2024                                                         #
################################################################################

# El objetivo del laboratorio es entender como podemos modelar una serie
# de tasas específicas de fecundidad por edad f(x), para una cohorte.
# 
# Para esto vamos a aprender a simular trayectorias reproductivas utilizando
# una serie de ideas básicas como fecundabilidad, período de no-suceptibilidad
# y fecundidad natural

# Comenzamos graficando las f(x) de las cohortes de Huteritas entre 1900 y 1905.
# Esta es la referencia contra la que compararemos nuestros datos simulados.

source("plot_fun.R") # funciones para graficar

asfrs_ht <- read.csv(file.path("datos", "asfrs_ht.csv"), header = T) # datos

plot_asfr(asfrs_ht)

# Describir lo que se observa en la gráfica. Como se construye el indicador?

# Se observa que la tasa comienza a crecer mas pronunciadamente a partir de los
# 16 años llegando al maximo alrededor de los 24-28 años, y una posterior caida
# mas relevante a partir de los 36-40 donde incluso la grafica cambia de 
# concavidad, a partir de los 50 resulta ser casi nula.

# El indicador se construye con los nacimientos asociados a las mujeres, 
# dividido el numero total de mujeres, todo asociado a un intervalo

# Ahora vamos a generar una serie de datos ficticios de una cohorte de mujeres
# a la que se observa desde la edad 10 a la edad 50, sin truncamiento (sin 
# salida de la muestra por muerte u otros motivos).

# Necesitamos simular el tamaño de la cohorte, el nr. de hijos de cada mujer 
# y la edad de la madre a cada nacimiento.

n <- 8 # tamaño de la cohorte
nac <- c(2,1,2,4,1,3,2,1) # nr de nacimientos
edades <- c(34.3,37.2,38.5,21.8,24.3,26.5,28.5,32.3,
            38.3,20.1,21.5,23.4,27.7,22.4,25.6,15.9) # edades exactas

# organizamos la información en una tabla con las historias reproductivas "hst"
hst <- as.data.frame(cbind(id = rep(1:n, nac),
                           nac = rep(nac, nac),
                           edad = edades,
                           paridad = sequence(nac)))

# Describir brevemente el contenido de hst

# En id podemos diferenciar a cada mujer, en nac podemos ver el total de hijos,
# en edad podemos ver a que edad tuvo exactamente a cada uno y en paridad el 
# orden en que los tuvo.


# ahora vamos a agregar un vector con edades exactas pero enteras, para
# poder luego acumular en el intervalo entre edades exactas y enteras.
edad <- 10:50 
hst <- merge(hst, as.data.frame(edad), by = "edad", all= T)

# visualizamos las historias reproductivas de nuestra cohorte de 8 mujeres
plot_hst(dat = hst, ylim = c(0.5, n), n = n) 

# Calculamos la fecundidad acumulada a cada edad E(x) 

hst$cum_nac <- cumsum(!is.na(hst$nac)) # completar
hst$cum_fec <- hst$cum_nac/n

plot_cum_fec(hst)

# describa brevemente lo que se observa en el gráfico.

# El mayor crecimiento en la tasa se da en el periodo entre los 22 y 30 años,
# posteriormente crece mas despacio hasta los 38, donde ya no crece.


# Cuál es la fecundidad total alcanzada por esta cohorte de mujeres?

# La fecundidad total que se alcanza por la cohorte de mujeres es de 2


# Calculamos las tasas de fecundidad por edad f(x), en intervalos de un año 

edades_b <- floor(hst$edad[!is.na(hst$id)])
fx <- table(factor(edades_b, levels= edad))/n
plot(as.data.frame(fx))
sum(fx)

plot(edad, fx)

# describa brevemente lo que se observa en el gráfico.

# Vemos que entre los 20 y 28 años aproximadamente, la fecundabilidad se 
# mantiene constante, exceptuando a los 21 donde hubo mas nacimientos, entre los
# 30 y 40 hay otras subidas de la fecundabilidad, o sea, nacimienots, y a partir
# de los 40 años se vuelve nula.

# Que información nos aporta la sumatoria de las f(x)?

# La sumatoria de las f(x) es equivalente a la tasa de fecundidad total que
# ya mencionamos.



# La serie de f(x) dista bastante, en forma, de la referencia de los Huteritas
# esto de debe a que sólo tenemos 8 mujeres en la cohorte.
# Intentemos incrementando el n, por ej. a 1500 mujeres

n <- 1500

# Utilicemos la distribución uniforme para simular los nacimientos totales de cada
# una de estas mujeres asumiendo que todas tuvieron al menos un hijo y que el nr.
# máximo de nacimientos es 8 -- ver runif()

nac <- round(runif(n, 1, 8))# completar
plot(nac)

n_hst <- as.data.frame(cbind(id = rep(1:n, nac),
                             nac = rep(nac, nac),
                             paridad = sequence(nac)))# completar
  
# Definimos una función para generar edades a cada nacimiento
# Mas adelante estudiaremos la forma correcta para simular edades a cada
# nacimiento, en este momento necesitamos una función sencilla e intuituiva
# que nos evite tener que definir manualmente un vector de n edades

comp_edades <- function(nac){
  ed <- vector()
  for(i in 1:nac){
    if(i == 1){
      ed[i] <- round(runif(1, 12, 20), 5) 
    }else{
      ed[i] <- round(runif(1, ed[i-1]+1, min(ed[i-1]+5, 50)), 5) 
    }
  }
  return(ed)
}

n_hst$edad <- unlist(sapply(nac, comp_edades))

# Que supuestos hacemos sobre los primeros nacimientos

# Suponemos que los primeros nacimientos para las mujeres, se dan cuando estas 
# tienen entre 12 a 20 años máximo.

# y que supuestos hacemos sobre los siguientes nacimientos en "comp_edades"

# Estamos asumiendo que para cada nacimiento i este se dará entre un minimo de
# un año y un máximo de 5 años posterior al anterior nacimiento. Todo esto es válido
# hasta que la mujer tenga 50 años donde ya no tiene mas hijos.


# agregamos el vector con edades enteras
n_hst <- merge(n_hst, as.data.frame(edad), by = "edad", all= T) 


# Fecundidad acumulada a edad x
n_hst$cum_nac <- cumsum(!is.na(n_hst$nac))  # completar 
n_hst$cum_fec <- n_hst$cum_nac/n  # completar 

plot_cum_fec(n_hst)

# describa brevemente lo que se observa en el gráfico.

# Vemos la fecundidad acumulada, donde en este caso crece a partir de los 12
# años de manera muy pronunciada hasta aprox los 25-28 donde cambia la concavidad
# pero sigue creciendo, es a los 40 cuando comienza a estancarse y deja de crecer.

# tasas de fecundidad por edad f(x)
edades_b2 <- floor(n_hst$edad[!is.na(hst$id)])
fx <- table(factor(edades_b2, levels=edad))/n # completar
plot(edad, fx)

# describa brevemente lo que se observa en el gráfico.
# En que edades es mayor la fecundidad de esta cohorte?

# A partir de los 13 años, la fecundidad comienza a aumentar de manera 
# sostenida hasta alcanzar su punto máximo alrededor de los 20 años.
# Luego, se mantiene en un nivel elevado hasta los 30 años, y a partir de ahí,
# disminuye progresivamente hasta casi desaparecer alrededor de los 40 años.

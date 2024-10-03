################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 11vo Laboratorio: Tablas de Mortalidad                                       # 
# 1 de Octubre de 2024                                                         #   
################################################################################

###########################################################
# TABLA DE MORTALIDAD DE PERíODO                          #
###########################################################

source("plot_fun.R")

# Cargamos los datos
Mx_all <- read.csv(file.path("datos","Mx_1x1_Denmark.txt"),
                   sep = "", skip = 1, header = T)

# Extraer las tasas de mortalidad por edad Mx para hombres de 0 a 95 años en 
# Dinamarca en 1900 utilizando la función plot_Mx con argumento return_data = T,
# también especificar log_escale = F

nMx <- plot_Mx(dat = Mx_all, anios = 1900,edades = 0:95,
               sex = "Male", return_data = T, log_escale = F)


# traemos la función "compute_lt" trabajada en el práctico anterior y corregimos
# los años-persona en el intervalo abierto 

compute_lt <- function(nMx, x, sex, tabla = T) {
  
  get_na0 <- function(nMx, sex){
    
    if(sex=="M"){
      
      if (nMx[1] < 0.023){
        
        na0 <- 0.14929 - 1.99545 * nMx[1]
      }else{
        
        if(nMx[1] >= 0.023 & nMx[1] < .08307){
          
          na0 <- .02832 + 3.26021 * nMx[1]
          
        }else{
          
          na0 <- 0.29915
        }
      }
    }
    
    if(sex=="F"){
      
      if (nMx[1] <  0.01724){
        
        na0 <- .14903 - 2.05527 * nMx[1]
      }else{
        
        if(nMx[1] >= 0.01724 & nMx[1] < 0.06891){
          
          na0 <- 0.04667 + 3.88089 * nMx[1]
          
        }else{
          
          na0 <- .31411
        }
      }  
      
    }
    
    return(na0)
    
  }
  
  # Definimos el número de intervalos
  nmax <- length(nMx)
  
  # Definimos los factores de separación nax 
  
  # creamos un vector vacio para guardar los nax
  nax <- vector()
  
  # definimos a0 con la ayuda de la función "get_na0"
  na0 <- get_na0(nMx, sex)
  
  # asignamos los factores a cada intervalo
  nax[1] <- na0
  nax[2:nmax] <- 0.5
  
  # convertimos las nMx en nqx
  nqx <- (1*nMx)/(1+(1-nax)*nMx)
  
  # nos aseguramos que la probabilidad en el último intervalo sea 1
  nqx[nmax] <- 1
  
  # Construimos las lx
  lx <- c(1,cumprod((1-nqx)))
  
  # Obtenemos las defunciones
  ndx <- -diff(lx)
  
  # creamos un vector con los sobrevivientes en x+n
  lxn <- lx[-1]
  
  # Obtenemos los años persona en el intervalo nLx
  nLx <- lxn + (nax*ndx)
  
  nLx[nmax] <- lx[nmax] / nMx[nmax]
  
  # Calculamos los años persona por encima de x
  
  Tx <- rev(cumsum(rev(nLx)))
  
  # Calculamos la esperanza de vida a edad x
  ex <- Tx/lx[1:nmax]
  
  
  # Creamos la tabla
  if (tabla) {
    lt <- data.frame(x, nax = round(nax, 4),
                     nMx = round(nMx,4),
                     nqx = round(nqx[1:nmax], 4), lx = round(lx[1:nmax],4),
                     ndx = round(ndx, 4), nLx = round(nLx, 4), Tx = round(Tx, 2),
                     ex = round(ex, 2))
    return(lt)
  }
  
  else { 
    return(ex[1])
  }
  
  
}


#####################################################
# Validación                                        #
#####################################################
# El paquete`demogR` contiene una función para calcular una tabla de vida
# vamos a usar esta función para validar los resultados obtenidos 
# con los cálculos anteriores

# Cargamos el paquete
library(demogR)

# La función recibe como inputs un vector de defunciones por edad y un vector 
# con la exposición al riesgo en cada tramo de edad (años-persona).
# A partir de estos inputs calcula las tasas de mortalidad,
# convierte esas tasa en probabilidades de morir y calcula el resto de la tabla.

# Cargamos los datos - obtenidos de Human Mortality Database -

ndx_all <- read.csv(file.path("datos","Deaths_1x1.txt"),
                     sep = "", skip = 1, header = T)# defunciones
nkx_all <- read.csv(file.path("datos","Exposures_1x1.txt"),
                    sep = "", skip = 1, header = T)# exposición
  
# Obtenemos las defunciones / exposición al riesgo para hombres, edad 0:95 en 1900
ndx <- ndx_all[ndx_all$Year==1900 & ndx_all$Age %in% 0:95, "Male"]
nkx <- nkx_all[nkx_all$Year==1900 & nkx_all$Age %in% 0:95, "Male"]
  


# Calculamos la tabla - `type` refiere a los factores de separación,
# la opción "cd" es la más similar al procedimiento que utilizamos nosotros

ltd <- life.table(x = 0:95, nDx = ndx, nKx = nkx, iwidth = 1, width12 = c(1,1), type = "cd") 

# Comparamos resultados
plot(compute_lt(nMx, 0:95, sex = "M", tabla = T)$ex)
points(ltd$ex,col = "red")

#####################################################
# Análisis de la Evolución de la Esperanza de Vida  #
#####################################################

# Obtener las tasas de mortalidad para los hombes de edades 0 a 95
# desde 1900 a 2021. El argumento "as_list" de la función "plot_Mx" tiene
# que estar en "T" para obtener una lista con las tasas para cada año
# en cada elemento de la lista

nMxc_M <- plot_Mx(dat=Mx_all, anios = 1900:2021, sex = "Male", edades = 0:95, as_list = T,  return_data = T, log_escale = F)

# Obtener los mismos resultados para Mujeres
nMxc_F <- plot_Mx(dat=Mx_all, anios = 1900:2021, sex = "Female", edades = 0:95, as_list = T,  return_data = T, log_escale = F)

# Obtener la esperanza de vida al nacer para cada año 


exc_M <- lapply(nMxc_M, function(x) compute_lt(x, 0:95, sex = "M", tabla = F))
exc_F <- lapply(nMxc_F, function(x) compute_lt(x, 0:95, sex = "F", tabla = F))

plot(1900:2021, exc_F)
points(1900:2021, exc_M, col = "red")

# Describir los resultados 

# La esperenza de vida al nacer aumenta con el pasar de los años y en casi todos los
# casos la esperanza de vida al nacer de las mujeres es mayor que la de los hombres.

# Aproximadamente en 1950 se acentua mucho mas esta diferencia entre los generos.

# Luego de los 90 parecen crecer en ambos generos de igual medida.

# Al principio, entre 1900 y 1920, los servicios de la salud eran bastante peores y
# dispares entre clases sociales y además se llevaba un peor registro a la hora de 
# tomar los datos.




# TAREA TIPS
# PARTE 1 INC 2.
# HACER UN GRAFICO PARECIDO A EL DE FIGURE 1 TASAS DE FEC POR EDAD DE COH 1940 - 1970 ESPAÑA CLASE 7

# INCISO 3 ELEGIR FECUNDIDAD NATURAL O REGULADA, CALIBRAR PARAMETROS PARA QUE SE PAREZCA A EL AÑO SELECCIONADO.

# iNC 4, LO MAS IMPORTANTE, NO SER ESCUETO ACA

# INC 5, HACER VARIAR 2 PARAMETROS, UN ANALISIS DE SENSIBILIDAD, LO HICIMOS EN 2 LABS, E 
# INTERPRETAR EL RESULTADO Y EL CONTEXTO HISTORICO SUMA

# PARTE 2 
# iNC 1, SACAR LA S DE HTTP SI NO ANDA

# HALLAR UNA TABLA EN PAG.
defunciones
cruces con varibales de defunciones

# TITULO CUALQUIERA 
cruce de linea edad en años al facllecer
columna año de ocurrencia de la defuncion
variables de conrtros sexo
procentakje anbsoluto 

tabla


#EDAD EN AÑOS AL FALLECER
#AÑO DE OCURRENCIA
#VARIABLE DE CONTROL SEXO
#ABSOLUTOS

#DESCARGAR COMO EXCEL

# EN EL INST DE EST.
#REVISION 2013, TOTAL PAIS,. PON POR SEX Y EDAD1996 2050, APARECE UN EXCEL

# 30 DE JUNIO PORQUE CON ESO YA MEDIMOS LA UNIFORMIDAD DE NCAIMIENTOS Y MUERTES, ENTRADAS Y SALIDAS EN LA POBLACION

# GUARDAMOS ESE EXCEL, INC 3 DEFUNCIONES EN EL NUMERADOR SOBRE AÑOS PERSONA DENOMINADOR

# INC 4 USAR LA FUNCION QUE HICIMOS EN EL LAB 11, 

# iNC 5 ESTO: PERO CON LOS DATOS CHIDOS, GRAFICAMOS ESPERANZA DE VIDA AL NACER AL PASAR DEL TIEMPO
exc_M <- lapply(nMxc_M, function(x) compute_lt(x, 0:95, sex = "M", tabla = F))
exc_F <- lapply(nMxc_F, function(x) compute_lt(x, 0:95, sex = "F", tabla = F))

plot(1900:2021, exc_F)
points(1900:2021, exc_M, col = "red")

# sE RECOMIENDA USAR LaTex .tec, compila a un pdf y se entrega eso :P

# Para darle un plus al trabajo, la bibliografia 

# library(readxl) esto lee excel
# def_m es la chida para trabajar, son los datros para los hombres lo mismo con def_f, son defunciuoens

# dataos de años persona, saque fotos con el principio del trabjo 

# http://colo1.msp.gub.uy/redbin/RpWebEngine.exe/Portal?BASE=VITAL_DEF1&lang=esp
################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 10mo Laboratorio: Tablas de Mortalidad                                       # 
# 24 de Septiembre                                                             #   
################################################################################

###########################################################
# TABLA DE MORTALIDAD DE PERíODO                          #
###########################################################

# El objetivo es construir una tabla de mortalidad utilizando la información 
# más comunmente disponible en países que cuentan con sistemas de estadísticas
# vitales y censos de población.

########################
# EXPLORAR LOS DATOS   #
########################

# Vamos a utilizar los datos de las tasas específicas de mortalidad por edad,
# Mx, para Dinamarca
# Estos datos se encuentran disponibles en la Human Mortality Database 
# www.mortality.org

# Cargamos los datos
Mx_all <- read.csv(file.path("datos","Mx_1x1_Denmark.txt"),
                   sep = "", skip = 1, header = T)

# Graficamos las Mx utilizando la función plot_Mx para los anios
# 1900:2005 en escala logarítmica para hombres y mujeres 
# (sex = "Male" / "Female") de edades 0 a 95.

source("plot_fun.R")
plot_Mx(dat = Mx_all, anios = 1900:2005,edades = 0:95, sex = "Male")
plot_Mx(dat = Mx_all, anios = 1900:2005,edades = 0:95, sex = "Female")

# Describir lo que se observa en el gráfico. Contestando: 

# Qué indicador estamos utilizando? Qué hay en numerador/denominador?
Revisar


# Cuáles son las diferencias más imprtantes entre hombres y mujeres?
# Vemos que entre hombres y mujeres la mortalidad de las mujeres entre los 20
# y 30 años disminuyó en mayor medida al pasar el tiempo que la de los hombres,
# que tambien disminuyó, además vemos como en los años más recientes la 
# mortalidad de las mujeres tiene grandes caidas en lo que parece ser la 
# infancia y adolescencia a diferencia de los hombres.

# Cuál es la tendencia en el tiempo?
# La principal tendencia es el desplazamiento hacia abajo de la mortalidad en 
# practicamente todas las edades, menos en la de nacimiento ya que esas muertes
# no son evitables una vez se dieron ciertas condiciones en el embarazo.
# Además, ya fueron mencionadas otras particularidades en el inciso anterior.


#######################################################
# CONSTRUCCION DE LA TABLA DE MORTALIDAD DE PERÏODO   #
#######################################################

# Extraer las tasas de mortalidad por edad Mx para hombres de 0 a 95 años 
# en Dinamarca en 1900 utilizando la función plot_Mx con argumento
# return_data = T, también especificar log_escale = F

nMx <- plot_Mx(dat = Mx_all, anios = 1900,sex='Male',edades = 0:95, return_data = T, log_escale = F)


# Describir que hace la siguiente función:
# Definimos los supuestos de ax para cada genero, en el intervalo 0, 1, 
# segun la mortalidad infantil.

get_na0 <- function(nMx, males){
  
  if(males){
    
    if (nMx[1] < 0.023){
      
      na0 <- 0.14929 - 1.99545 * nMx[1]
    }else{
      
      if(nMx[1] >= 0.023 & nMx[1] < .08307){
        
        na0 <- .02832 + 3.26021 * nMx[1]
        
      }else{
        
        na0 <- 0.29915
      }
    }
  }else{
    
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

# Definir las edades
x <- c(0:95)

# Definimos el número de intervalos
nmax <- length(x)

# Definimos los factores de separación nax 
# creamos un vector vacio para guardar los nax
nax <- vector()

# definimos a0 con la ayuda de la función "get_na0"
na0 <- get_na0(nMx, T)

# asignamos los factores a cada intervalo
nax[1] <- na0
nax[2:nmax] <- 0.5

# convertimos las nMx en nqx
nqx <- nMx/(1+(1-nax)*nMx)

# nos aseguramos que la probabilidad en el último intervalo sea 1
nqx[nmax] <- 1

# Construimos las lx - ver cumprod() - Nota: l0 = 1
(1-nqx) #Probabilidad de sobrevivir

lx <- c(1,cumprod((1-nqx)))

# Obtenemos las defunciones
ndx <- nqx * lx[1:length(lx)-1]
# -diff(lx) alternativa

# creamos un vector con los sobrevivientes en x+n
lxn <- lx[2:length(lx)]
# lxn <- lx[-1] alternativa

# Obtenemos los años persona en el intervalo nLx
nLx <- lxn + (nax*ndx)

# Calculamos los años persona por encima de x

Tx <- c(sum(nLx), (sum(nLx) - cumsum(nLx))[1:length(nLx)-1])

# rev(cumsum(rev(nLx)))

# Calculamos la esperanza de vida a edad x
ex <- Tx/lx

# Creamos la tabla
lt <- data.frame(x, nax = round(nax, 4),
                 nMx = round(nMx,4),
                 nqx = round(nqx[1:nmax], 4), lx = round(lx[1:nmax],4),
                 ndx = round(ndx, 4), nLx = round(nLx, 4), Tx = round(Tx, 
                                                                      2), ex = round(ex, 2))
lt

# Ejercicio:

# crear una función llamada "compute_lt" que va a tomar los siguientes argumentos:
# 1 - nMx: Un vector de tásas específicas de mortalidad por edad observadas. 
# 2 - x: Un vector con las edades de inicio de cada intervalo
# 3 - sex: Define si el análisis está hecho para hombres ("M") o mujeres ("F")
# 4 - tabla: si TRUE la función devuelve toda la tabla, si FALSE devuelve sólo el valor de la esperanza de 
# vida al nacer (hacer TRUE por default)

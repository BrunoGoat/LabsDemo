
##################################################
###           SCRIPT - PRIMERA PARTE           ###
##################################################

##### INCISO UNO #####
# Para el inciso uno, los datos que elegí descargar fueron los de japón.

# Incluyo además las diferentes funciones que utilizaremos para graficar.
source("plot_fun.R")

fx_jap <- read.table(file.path("datos","FertilityRates.txt"), skip = 2,
                       header=T, stringsAsFactors = F)
fx_jap1 <- fx_jap


##### INCISO DOS #####
# Utilizamos la funcion plot_fx_hfd previamente cargada para graficar la 
# evolución de las tasas de fertilidad a lo largo del tiempo.

# Si bien, los datos comienzan a partir de 1892, se tiene muy poca información 
# sobre ese y los proximos años, por ende utilizaremos 1930 como primer año de 
# cohorte y terminaremos en 1985 donde si bien, los datos de ese y los ulitmos años
# no llegan al final de nuestro grafico (se observa en la presencia de las caidas 
# inmediatas de fecundidad a 0, debido a que esa cohorte aún no llega a la edad
# solicitada por nuestro gráfcio) pudimos recaudar los años mas relevantes
# para la fertilidad de esas últimas cohortes.

plot_fx_hfd(dat = fx_jap1, cohorts = 1930:1985, type = "lines")

##### INCISO TRES #####
plot_fx_hfd(dat = fx_jap1, cohorts = 1980, type = "lines")

# El modelo seleccionado para la simular las trayectorias reproductivas es el
# de fecundidad regulada.

gen_hst_d <- function(n, ns, x0, r, mu, su, mu_d, sd_d, c){
  
  id <-  vector()
  wt_c <- vector()
  k <- rep(0,n)
  edad <- 1:600
  
  fi_t <- 0.2 / (1 + exp(r*(edad-x0)))
  fi_t[589:600] <- 0
  
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 
  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) 
  
  dk <- round(rlnorm(n, meanlog = log(mu_d^2/ sqrt(mu_d^2+sd_d^2)),
                     sdlog = sqrt(log(1 + sd_d^2/mu_d^2))),0)
  
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk)) 
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t)
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) 
      
      k <- as.vector(table(factor(id, levels = 1:n))) 
      
    }
    
  }
  
  hst <- as.data.frame(cbind(id = id, wt_c = wt_c))
  hst <- hst[order(hst$id),]
  hst <- as.data.frame(cbind(id = hst$id,
                             edad = (hst$wt_c + 9)/12,
                             nac = rep(table(hst$id), table(hst$id)),
                             paridad = sequence(table(hst$id))))
  return(hst)
}

Sim_80 <- gen_hst_d(n = 10000, ns = 11, x0 = 450, r = 0.05, mu = 30, 
                    su = 5.5, mu_d = 1.5, sd_d = 0.5, c = 0.002)

plot_fx(dat = Sim_80 , points =  T)

##### INCISO CUATRO #####

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
    
    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk)) 
    #Vamos a cada uno de los elementos de fi_it y vamos a crear un subset 
    #(aplicando la funcion [, esto crea el subset), y el subset se hace en t, 
    # con esto extraemos la probabilidad de concebir de cada mujer en el 
    # momento t.
    
    # Mientras la condicion k >= dk sea falsa, multiplicamos por 1, cuando es 
    # verdadera (la pareja alcanza el numero deseado de hijos), la pareja 
    # comienza a usar anticonceptivos, entonces en el modelo se empieza a 
    # multiplicar por c, que es la probabilidad de que falle el anticonceptivo,
    # reduciendo en gran medida la probabilidad de concebir pero no hasta 0 
    # ya que el anticonceptivo no es 100% efectivo.
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # guardamos los diferentes edades hasta la concepcion en meses
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) 
      # eliminamos los nacimientos que ocurrieron durante el embarazo y el 
      # periodo de no suceptibilidad
      
      k <- as.vector(table(factor(id, levels = 1:n))) 
      #Cantidad efectiva de hijos de cada mujer en cada momento t
      
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

##### INCISO CINCO #####

# Analisis de sensibilidad para mu - (Edad promedio de union de parejas) 

# Creamos nuestrro gráfico de la simulación de 1980.
Sim_80_plot <- plot_fx(Sim_80, lines  = T)
Sim_80_plot <- plot_fx2(Sim_80, lines  = T)

# Diferentes valores para mu
mu_vals <- seq(23, 30, 1)

# Generamos las trayectorias reproductivas con los diferentes valores de mu

mu_var <- lapply(mu_vals, function(x) gen_hst_d(n = 1000, ns = 11, x0 = 450, 
                                                r = 0.035, mu = x, su = 5.5, 
                                                mu_d = 1.5, sd_d = 0.5, c = 0.002))

colors <- rainbow(length(mu_vals))
var_tfr_c <- lapply(1:length(mu_vals), function(x) plot_fx2(dat = mu_var[[x]],
                                                          lines = T,
                                                          col = colors[x]))
add_legend(mu_vals, colors)


# Analisis de sensibilidad para c - (Probabilidad de que falle el anticonceptivo)

# Creamos nuestrro gráfico de la simulación de 1980.
Sim_80_plot <- plot_fx(Sim_80, lines  = T)
Sim_80_plot <- plot_fx2(Sim_80, lines  = T)

# Diferentes valores para c 
c_vals <- seq(0.002, 0.042, 0.01)

# Generamos las trayectorias reproductivas con los diferentes valores de c

c_var <- lapply(c_vals, function(x) gen_hst_d(n = 1000, ns = 11, x0 = 450, 
                                              r = 0.035, mu = 30, su = 5.5, 
                                              mu_d = 1.5, sd_d = 0.5, c = x))

colors <- rainbow(length(c_vals))
var_tfr_c <- lapply(1:length(c_vals), function(x) plot_fx2(dat = c_var[[x]],
                                                            lines = T,
                                                            col = colors[x]))
add_legend(c_vals, colors)


##################################################
###           SCRIPT - SEGUNDA PARTE           ###
##################################################

##### INCISO TRES #####

#install.packages("readxl")
library(readxl)

# Defunciones
def_raw <- as.data.frame(read_excel(file.path("datos","reporte.xlsx"), skip = 14))# cargar datos defunciones
def_m <- def_raw[1:115, 3:(ncol(def_raw)-1)]# subset hombres
names(def_m)[1] <- "edad" 
str(def_m)
def_m <- apply(def_m, 2, as.numeric)# convertir todas las columnas a "numeric"
def_m[is.na(def_m)] <- 0 # NAs a 0
def_m <- as.data.frame(def_m) # hacer data.frame


def_f <- def_raw[119:236, 3:(ncol(def_raw)-1)]
names(def_f)[1] <- "edad" 
str(def_f)
def_f <- apply(def_m, 2, as.numeric)
def_f[is.na(def_f)] <- 0
def_f <- as.data.frame(def_f)

# Años persona
exp_raw <- as.data.frame(read_excel(file.path("datos","Total_pais_poblacion_por_sexo_y_edad_1996-2050.xls"), skip = 4))# cargar datos
exp_m <- exp_raw[99:189, 1:26]# subset hombres
str(exp_m)
names(exp_m)[1] <- "edad"
exp_m$edad <- as.numeric(exp_m$edad) #columna edad a numeric
exp_m$edad[length(exp_m$edad)] <- 90

# mujeres
exp_f <- exp_raw[193:283, 1:26]
str(exp_f)
names(exp_f)[1] <- "edad"
exp_f$edad <- as.numeric(exp_f$edad)
exp_f$edad[length(exp_f$edad)] <- 90


# Intervalo abierto 90+ defunciones
open_int_m <- apply(def_m[def_m$edad >= 90, 2:ncol(def_m)], 2, sum, na.rm = T)# seleccionar la filas y columnas correspondientes y hacer una suma por columnas con apply()  
open_int_f <- apply(def_f[def_f$edad >= 90, 2:ncol(def_f)], 2, sum, na.rm = T)

def_m <- rbind(def_m[def_m$edad %in% 0:89,], c(90, open_int_m))
def_f <- rbind(def_f[def_f$edad %in% 0:89,], c(90, open_int_f))

# Obtener las tasas 
nMx_m <-  def_m[, -1]/exp_m[, -1]# HOMBRES

nMx_f <-  def_f[, -1]/exp_f[, -1]# MUJERES

##### INCISO CUATRO #####

# Formatear los datos para el plot
nMx_m <- reshape(nMx_m,
                 direction = "long",
                 varying = list(names(nMx_m)),
                 v.names = "Male",
                 timevar = "Year",
                 times = 1996:2020)

nMx_m <- cbind(def_m[,1], nMx_m[,1:2])
names(nMx_m)[1] <- "Age" 

nMx_f <- reshape(nMx_f,
                 direction = "long",
                 varying = list(names(nMx_f)),
                 v.names = "Female",
                 timevar = "Year",
                 times = 1996:2020)

nMx_f <- cbind(def_f[,1], nMx_f[,1:2])
names(nMx_f)[1] <- "Age" 

# graficar 
library(ggplot2)

plot_Mx <- function(dat, anios, sex, edades, smooth = F,
                    spar_val = 0.3, log_escale = T, return_data = F, as_list =F, save = F){
  dat[dat$Age == "110+", ] <- "110"
  dat[,2] <- as.numeric(dat[,2])
  dat <- dat[dat$Age %in% edades & dat$Year %in% anios, c("Year","Age",sex)] 
  dat[,1] <- as.numeric(dat[,1])
  dat[,3] <- as.numeric(dat[,3])
  
  
  if(log_escale){
    dat[,3] <- log(dat[,3])
    ylims <- c(-13, 1)
  }else{ylims <- c(0, 1)}
  
  if(smooth){
    split_dat <- split(dat, dat$Year)
    
    sm_y <- lapply(split_dat, function(x) smooth.spline(x[,3], spar = spar_val)$y)
    
    dat <- cbind(dat[,1:2], unlist(sm_y))
    
  }
  
  names(dat) <- c("Year","Age","Mx")
  
  p <- ggplot(dat, aes(x = Age, y = Mx,
                       group = as.factor(Year),
                       colour = Year))+
    geom_line() +  scale_colour_gradient(low = "orange", high = "red")+
    theme_bw() +
    ylim(ylims)+
    ylab("M(x)") + xlab("Edad")+
    theme(legend.position = c(0.85, 0.3),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  if(save){
    pdf(file.path("..","..","..","imagenes", "asfr.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  
  print(p)
  
  if(return_data){
    if(as_list){
      
      split_dat <-  split(dat, dat$Year)
      Mx_list <- lapply(split_dat, function(x) x[,3])  
      return(Mx_list)
    }else{
      return(dat[,3])}
  }
}

summary(nMx_m)
plot_Mx(dat = nMx_m, anios = 1996:2020, edades = 0:90, sex = "Male")

summary(nMx_f)
plot_Mx(dat = nMx_f, anios = 1996:2020, edades = 0:90, sex = "Female")

# eliminar anio 1996

nMx_m <- nMx_m[nMx_m$Year!=1996,]
nMx_f <- nMx_f[nMx_f$Year!=1996,]

plot_Mx(dat = nMx_m, anios = 1997:2020, edades = 0:90, sex = "Male")
plot_Mx(dat = nMx_f, anios = 1997:2020, edades = 0:90, sex = "Female")

# definir función compute_lt

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
  
  nmax <- length(nMx)
  
  nax <- vector()
  
  na0 <- get_na0(nMx, sex)
  
  nax[1] <- na0
  nax[2:nmax] <- 0.5
  
  nqx <- (1*nMx)/(1+(1-nax)*nMx)
  
  nqx[nmax] <- 1
  
  lx <- c(1,cumprod((1-nqx)))
  
  ndx <- -diff(lx)
  
  lxn <- lx[-1]
  
  nLx <- lxn + (nax*ndx)
  
  nLx[nmax] <- lx[nmax] / nMx[nmax]
  
  Tx <- rev(cumsum(rev(nLx)))
  
  ex <- Tx/lx[1:nmax]
  
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


# obtener tasas con función plot_Mx

nMxt_m <- plot_Mx(dat = nMx_m, anios = 1997:2020, edades = 0:90, sex = "Male",
                  return_data = T, log_escale = F, as_list = T)

nMxt_f <- plot_Mx(dat = nMx_f, anios = 1997:2020, edades = 0:90, sex = "Female",
                  return_data = T, log_escale = F, as_list = T)

tab_m <- lapply(nMxt_m, function(n) compute_lt(n, x = 0:90, sex = "M", tabla = T))
tab_f <- lapply(nMxt_f, function(n) compute_lt(n, x = 0:90, sex = "F", tabla = T))

# calcular la esperanza de vida al nacer para cada serie de Mx con compute_lt y lapply

##### INCISO CINCO #####

esp_m <- lapply(nMxt_m, function(n) compute_lt(n, x = 0:90, sex = "M", tabla = F))

esp_f <- lapply(nMxt_f, function(n) compute_lt(n, x = 0:90, sex = "F", tabla = F))

# graficar

plot(1997:2020, esp_f, ylim = c(70, 82), col = "violet")
points(1997:2020, esp_m, col = "blue")


source("plot_fun.R")

# Parte 1, Inciso 1

fx_jap <- read.table(file.path("datos","FertilityRates.txt"), skip = 2,
                       header=T, stringsAsFactors = F)
fx_jap1 <- fx_jap

# Sacamos la primer columna con el Año
# fx_jap1 <- fx_jap[-1]

# Reordenamos las columnas
# fx_jap1 <- fx_jap1[, c("Cohort", "Age", "ASFR")]

# Pasamos las columnas Cohort y Age a numeros ya que vienen como caracter
# fx_jap1$Cohort <- as.numeric(fx_jap1$Cohort)
# fx_jap1$Age <- as.numeric(fx_jap1$Age)

# Sacamos los datos NA
# fx_jap1 <- fx_jap1[!is.na(fx_jap1$Cohort), ]

# Parte 1, Inciso 2

# Graficamos las tasas de fecundiad por año de Japon.
# Los datos comienzan a partir de 1892, sin embargo se tienen muy pocas 
# observaciones durante ese y los proximos años, por ende utilizaremos 1930 como
# primer año de cohorte.
plot_fx_hfd(dat = fx_jap1, cohorts = 1930:1985, type = "lines")

# Parte 1, Inciso 3 y 4 EXPLICACION IMPORTANTE AGREGAR
plot_fx_hfd(dat = fx_jap1, cohorts = 1980, type = "lines")

# Modelo para simular trayectorias reproductivas de una cohorte (con fecundidad regulada)
# se elige este modelo ya que tiene mas sentido para representar este año, en 1980 ya 
# era mas normal usar anticonceptivos.

# n = Tamaño de la cohorte
# ns = Periodo de no suceptibilidad
# x0 = Punto de inflexión caída de la fecundabilidad
# r = Pendiente caída de la fecundabilidad 
# mu = Edad media de la union
# su = Desvio de distribución edad en la union
# mu_d = Media de hijos deseados
# sd_d = Desvio de distribución de hijos deseados
# c = Probabilidad de que falle el anticonceptivo


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

plot_fx_hfd(dat = fx_jap1, cohorts = 1980, type = "lines")

Sim_80 <- gen_hst_d(n = 10000, ns = 11, x0 = 450, r = 0.05, mu = 30, 
                    su = 5.5, mu_d = 1.5, sd_d = 0.5, c = 0.002 )

plot_fx(dat = Sim_80 , points =  T)

# Parte 1, Inciso 5

# Analisis de sensibilidad para mu
Sim_80_plot <- plot_fx(Sim_80, lines  = T)
Sim_80_plot <- plot_fx2(Sim_80, lines  = T)

# Variaciones en la edad media de la union


# CAMBIAR NOMBRES DE COSASSSSSSSSSSSS
mu_vals <- seq(23, 30, 1)

# Aplicamos la funcion para cada valor de mu

hst_mu_var <- lapply(mu_vals, function(x) 
                    gen_hst_d(n = 1000, ns = 11, x0 = 450, r = 0.035, mu = x, 
                              su = 5.5, mu_d = 1.5, sd_d = 0.5, c = 0.002))

colors <- rainbow(length(mu_vals))
var_tfr_c <- lapply(1:length(mu_vals), function(x) plot_fx2(dat = hst_mu_var[[x]],
                                                          lines = T,
                                                          col = colors[x]))
add_legend(mu_vals, colors)

# Analisis de sensibiliad para
Sim_80_plot <- plot_fx(Sim_80, lines  = T)
Sim_80_plot <- plot_fx2(Sim_80, lines  = T)

# Variaciones en la edad media de la union

# CAMBIAR NOMBRES DE COSASSSSSSSSSSSS
c_vals <- seq(0.002, 0.01, 0.002)

# Aplicamos la funcion para cada valor de c


hst_c_var <- lapply(c_vals, function(x) 
                    gen_hst_d(n = 1000, ns = 11, x0 = 450, r = 0.035, mu = 30, 
                              su = 5.5, mu_d = 1.5, sd_d = 0.5, c = x))

colors <- rainbow(length(c_vals))
var_tfr_c <- lapply(1:length(c_vals), function(x) plot_fx2(dat = hst_c_var[[x]],
                                                            lines = T,
                                                            col = colors[x]))
add_legend(mu_vals, colors)

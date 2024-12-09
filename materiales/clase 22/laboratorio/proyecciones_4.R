################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Proyecciones de Población                                       # 
# 03 de Diciembre de 2024                                                      #   
################################################################################
# El objetivo es proyectar una población estructurada por edad y sexo,
# en este caso, la población de Suecia, utilizando información sobre los
# cocientes de supervivencia en el intervalo de proyección y las tasas específicas 
# de fecundidad por edad.


# Función para preparar los datos para el gráfico de la pirámide de población
reshape_long <- function(dat, ini, int){
  
  dat <- as.data.frame(dat)
  
  # Extraer el año de los nombres de las columnas
  yrs <- unique(sub('^[fm]_','', names(dat)))
  
  # Verificar que solo hay un año en los datos
  if(length(yrs) != 1){
    stop("Los datos contienen múltiples años")
  }
  
  dat0 <- reshape(data = dat,
                  varying = names(dat),
                  direction = "long",
                  sep = "_")
  names(dat0) <- c("yr", "count", "count")
  ages_lon <- rbind(dat0[,1:2], dat0[,c(1,3)])
  ages_lon$group <- c(rep("females", nrow(dat0)), rep("males", nrow(dat0)))
  ages_lon$age <- rep(seq(0,(nrow(dat)*int)-int,int),2)
  totals_v <- apply(dat, 2, sum)
  ages_lon$totals <- rep(totals_v, each = length(seq(0,(nrow(dat)*int)-int,int)))
  ages_lon$pct <- ages_lon$count/ages_lon$totals

  ages_lon$pct[ages_lon$group == "males"] <- -ages_lon$pct[ages_lon$group == "males"]
  
  return(ages_lon)
}

# Función para graficar la estructura por edad y sexo de la población 
# (pirámide poblacional) 
pd_plot_base <- function(dat, pt = FALSE) {
  
  # Preparar datos
  ages <- dat$age[dat$group == "females"]
  pct_females <- dat$pct[dat$group == "females"]
  pct_males <- dat$pct[dat$group == "males"]
  
  # Establecer límites simétricos para el eje x
  max_pct <- max(abs(c(pct_females, pct_males)))
  xlim <- c(-max_pct, max_pct)
  ylim <- c(0.5, length(ages)+0.5)
  
  # Suprimir ejes y área de trazado predeterminados
  plot(0, 0, type = "n", xlim = xlim, ylim = ylim,
       xlab = "Porcentaje de la Población", ylab = "Edad",
       main = paste("Estructura por Edad de la Población de Suecia:", 
                    unique(dat$yr)),
       cex.main = 0.8, cex.axis = 0.9, cex.lab = 0.9, axes = FALSE)
  
  # Añadir eje y personalizado (edades)
  axis(2, at = 1:length(ages), labels = ages, las = 1, cex.axis = 0.9)
  
  # Añadir eje x personalizado con etiquetas positivas
  x_ticks <- pretty(xlim, n = 5)
  axis(1, at = x_ticks, labels = abs(x_ticks * 100), cex.axis = 0.9)
  box()
  
  # Graficar mujeres
  for (i in 1:length(ages)) {
    rect(0, i - 0.4, pct_females[i], i + 0.4, col = "#D55E00", border = NA)
  }
  
  # Graficar hombres
  for (i in 1:length(ages)) {
    rect(0, i - 0.4, pct_males[i], i + 0.4, col = "#009E73", border = NA)
  }
  
  # Añadir leyenda
  legend("topright", legend = c("Mujeres", "Hombres"), 
         fill = c("#D55E00", "#009E73"), bty = "n", cex = 0.8)
}

# Función para la matriz de Leslie modificada
leslie <- function(L, m) {
  n = length(L)
  M = matrix(0, n, n)
  
  # Probabilidades de supervivencia
  px <- exp(diff(log(L)))
  diag(M[-1,-ncol(M)]) <- px
  M[n,n-1] <- M[n,n] <- L[n]/(L[n-1] + L[n])
  
  # Tasas de fecundidad
  for(i in 1:(n-1)) {
    if(m[i] != 0 | m[i+1] != 0) {
      M[1,i] <- (m[i] + m[i+1] * L[i+1]/L[i])*5/2
    }
  }
  return(M)
}

# Función para proyectar la población y graficar resultados 
p_pop_base <- function(f, Lf, Lm, Nf, Nm, iter, ini, int, srb){
  
  # Variables necesarias para graficar el crecimiento de la población:
  
  # Población Total en cada año
  N <- rep(NA, iter+1)
  
  # Años
  Y <- as.numeric(seq(ini, ini + (ini*iter), int))
  
  # data frame con Población Total por año (una entrada por fila)
  gw <- as.data.frame(cbind(N,Y))
    
  # Construir matrices para mujeres y hombres
  Mf <- leslie(L = Lf, m = f)
  Mm <- leslie(L = Lm, m = f)
  
  # Crear una matriz "pop" para guardar la población base y las proyecciones
  pop <- matrix(NA, length(Lf), 2*iter + 2)
  pop[,1] <- Nf
  pop[,2] <- Nm
  
  # Asignar nombres de columnas a 'pop'
  colnames(pop) <- character(ncol(pop))
  colnames(pop)[1:2] <- c(paste0('f_', ini), paste0('m_', ini))
  
  # Obtener la estructura por edad de la población base
  dat_long <- reshape_long(dat = pop[,1:2], ini = ini, int = int)
  
  # Calcular la población total en el año base y guardar como primer elemento
  # del vector "gw"
  gw$N[1] <- sum(pop[,1:2])
  
  # Graficar el tamaño de la población base y la pirámide
  par(mfrow = c(1, 2))
  
  # Gráfico de crecimiento poblacional
  plot(gw$Y, gw$N, type = "b", ylim = c(4e6, 12e6), 
       xlab = "Año", ylab = "Población",
       main = "Crecimiento de la Población")
  
  # Gráfico de pirámide
  pd_plot_base(dat = dat_long)
  
  # Proyectar la población de hombres y mujeres
  is <- seq(1,by=2,len=iter)
  
  for(i in is){
    
    pop[,i+2] <- Mf %*% pop[,i]
    pop[,i+3] <- Mm %*% pop[,i+1]
    
    # Guardar nacimientos
    births <- pop[1,i+2]
    
    # Calcular población en el primer grupo de edad
    pop[1,i+2] <- births / (1+srb) * Lf[1] / int
    pop[1,i+3] <- births * srb / (1+srb) * Lm[1] / int
    
    # Calcular el índice para el año actual
    idx <- which(is == i) + 1
    current_year <- gw$Y[idx]
    
    # Asignar nombres de columnas
    colnames(pop)[i+2] <- paste0('f_', current_year)
    colnames(pop)[i+3] <- paste0('m_', current_year)
    
    Sys.sleep(0.1)
    dat_long <- reshape_long(dat = pop[,c(i+2,i+3)], ini, int)
    
    gw$N[idx] <- sum(pop[,c(i+2,i+3)])  
    
    # Graficar el crecimiento poblacional y la pirámide actualizados
    par(mfrow = c(1, 2))
    
    # Gráfico de crecimiento poblacional
    plot(gw$Y, gw$N, type = "b", ylim = c(4e6, 12e6), xlab = "Año", ylab = "Población",
         main = "Crecimiento de la Población")
    
    # Gráfico de pirámide 
    pd_plot_base(dat_long)
  
  }
  
  
  return(pop)
}

# Cargar los datos (nuevamente, para mayor claridad)
swf <- read.table("datos/sweden_data_1993_females.txt", header=F)
names(swf) <- c("age", "p93", "L", "f")
swm <- read.table("datos/sweden_data_1993_males.txt", header=F)
names(swm) <- c("age", "p93", "L")

Lf <- swf$L
f <- swf$f
Lm <- swm$L
Nf <- swf$p93
Nm <- swm$p93

# Establecer el número de iteraciones
iter <- 20  # Ajustar según sea necesario

# Proyectar la población 
sw_pop <- p_pop_base(f = f,
                     Lf = Lf/100000,
                     Lm = Lm/100000,
                     Nf = Nf,
                     Nm = Nm,
                     iter = iter,
                     ini = 1993, 
                     int = 5,
                     srb = 1.05)


#Apuntes 

# TRABAJO 2
# 
# ELEGIR UN PAIS, DESCARGAR, TASAS CONDICIONALES, 
# GRAFICAMOS LAS TASAS Y EXPLICAMOS QUE ESTAMOS MIRANDO
# 

# 
# GRAFICAR MX EN FUNCION D ELA EDAD
# TOMAR DE 0 A 100 para el GRAFICO KOPEN MAIER 
# INTERESA LA DESCRIPCION DETALLA ADA YCOMO PODEMOS QUE HACE LA FUNCIOJN, NO A PRACTICO SI NO LA IDEA
# metoDO DE TRANSFORMADA DINVERSAM RIESGO ACUMULADO, DESCRIPCION CONCEPTUAL, NO "FOR" "LOOP", CONCEPCTUAL!!
# 
# QUE ES LA COMPARACION DE KOPEN MAIER
# LO MISMO PARA LA FECUNDIDAD, ACA EN EL GRAFICO NO HAY CORRESPONDENCIA EN CIERTOS PUNTOS
# 
# LAS TASAS CONDICIONALES NOS INTERESA SOLO M1X RIESGO DEL PRIMER HIJO
# 
# 
# 
# 
# PARTE 2 
# TASAS NO CONDICIONALES
# 
# MODIFICAR LA FUNCION PARA QUE CAMBIEN LAS LS Y LAS XS COMPARAMAS LA POBLACION DE 1900 CON LA DE 2022
# CON LA POBLACION ESPERADA EL CAMBIO ES POR LA MIGRACION, DESCRIPCION DE LA PIRAMIDE CONTEXTUAL
# 
# SUECIA NO
# 
# 
# TASAS ESPECIFICAS YEAR - AGE- ASFR ASFRS
# 
# POBLACION 
# 
# YEAR AGE FEMALE MALE TOTAL 
# 
# TABLAS DE VIDAd period 1x1, LIFE TABLES
# 
# year age MX QX AX LX DX LX TX EX
# 

#MAX(MINIMOS de pop,f,lf,lm) 
#
#dat[[1]][.females]

#igual que la utlima pero cambia las ls y las fs, tenemos que iterar en el  for para tomar esta vez no un vector si no una lista
#
# nos tiene que quedar una piramide que se mueva con diferentes mortalidades y fecundidades
# 
# como chequeo, agarro las ultima poblacion, del humnamortality, y la grafico con pd_plot
# 
# y cual 

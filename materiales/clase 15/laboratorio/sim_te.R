################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de tiempos a distintos eventos demográficos          # 
# 11 de Noviembre de 2024                                                      #   
################################################################################
 
# En el laboratorio anterior trabajamos en una función "ste" para simular los
# tiempos de espera a un evento a partir de una función de riesgo, bajo el supuesto de
# riesgo constante a intervalos. Los argumentos de la función son: n, edades, lambda (riesgo) y
# Haz que cuando T devuelve la función de riesgo acumulado H además de los tiempos simulados.

ste <- function(n, edades, lambda, Haz = F){
  
  inf <- seq(0, length(edades)-1,1)
  sup <- seq(1, length(edades))
  
  H.pw <- function(t, inf, sup, lambda){  
    p1 <-  pmax((t-inf), 0)
    p2 <-  pmin(p1, sup-inf)
    return(sum(lambda*p2)) 
  }
  
  H <- rep(NA, length(edades))
  x <- min(inf):max(sup)
  
  for (i in 1:length(H)){
    H[i] <- H.pw(x[i], inf, sup, lambda)
  }
  
  f <- function(t, inf, sup, lambda, u){
    res <- H.pw(t, inf, sup, lambda) + log(u)
    return(res)
  }
  
  root <- function(n, inf, sup, lambda){
    u <- runif(n, min = 0, max = 1) 
    times <- rep(NA, n) 
    
    for(i in 1:n){
      result <- uniroot(f, interval=c(0, length(lambda)),
                        u=u[i], inf=inf, sup=sup, lambda=lambda) 
      times[i] <- result$root
    }
    return(times)
  }
  
  t <- root(n, inf, sup, lambda)
  
  
  if(Haz){ return(list(t, H))}else{ return(t) }
  
}

####################
#    VALIDACIÓN    #
####################
# Para validar el procedimiento que aplicamos para simular los tiempos de espera
# podemos comparar la función de superviviencia teórica - la función a partir de
# la cual simulamos los datos -, con la que obtenemos a partir de los datos simulados.
# Para esto utilizamos el paquete "survival" que nos permite obtener una estimación de 
# la función de supervivencia a partir de datos individuales utilizando el estimador
# Kaplan-Meier.
library(survival)

# definimos el número de tiempos que vamos a generar
n <- 10000
# Cargamos las tasas de mortalidad por edad para una cohorte
mort <- read.csv(file.path("datos","mx.csv"))
# simulamos los tiempos
te <- ste(n, edades = mort$edad, lambda = mort$h, Haz = T)
# definimos la variable que indica que observaciones experimentaron el evento
eventos <- te[[1]] < Inf 
# guardamos el Riesgo Acumulado teórico
H <- te[[2]] 
# Graficamos la función de supervivencia estimada a partir de los tiempos simulados
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
# comparamos con la teórica
lines(mort$edad, exp(-H), lwd=3, col=2, lty=2)

# Repetimos el procedimiento para el caso del evento primer hijo.
fert <- read.csv(file.path("datos","fx.csv"))
te <- ste(n, edades = fert$edad, lambda = fert$h, Haz = T)

# La función devuelve un error en este caso. 

# Ejercicio: La idea es encontrar el motivo por el cual nuestra función 
# no se comporta de la manera esperada en este caso y ajustarla

# Para esto es importante recordar que:

# H(t_i) = -log(u_i) 

# además:
# S(t) = exp(-H(t))

# Por tanto:

# H(t) = -log(S(t))

# Que representan entonces las uniformes que estamos simulando para encontrar los
# tiempos de espera?




# Graficar la función de supervivencia teórica del evento primer hijo

edades <- fert$edad
inf <- seq(0,length(edades)-1,1)
sup <- seq(1,length(edades))
lambda <- fert$h

H.pw <- function(t, inf, sup, lambda){  
  
  p1 <-  pmax(t-inf, 0) # 
  p2 <-  pmin(p1, sup-inf) #
  
  return(sum(lambda*p2)) # Nos devuelve la suma del riesgo hasta t, devuelve el riesgo acumulado hasta t
  
}

# Que devuelve la función en este caso?
# Nos devuelve el riesgo acumulado hasta la edad t, en este caso 101

# plot
x <- min(inf):max(sup)
H <- rep(NA, length(x))

for (i in 1:length(x)){
  H[i] <- H.pw(x[i], inf, sup, lambda)
}
H
plot(x, H, type="l", lwd=3, col=2)

# Graficar la función de supervivencia
S <- exp(-H)
plot(x, S, typ="l",lwd=3, col=2, ylim=c(0,1))

# Que diferencia hay entre esta función y la que obteniamos en el caso del evento muerte?


# Ejercicio: Ajustar la función de acuerdo a los resultados obtenidos arriba y validar


ste2 <- function(n, edades, lambda, Haz = F){
  
  inf <- seq(0, length(edades)-1,1)
  sup <- seq(1, length(edades))
  
  H.pw <- function(t, inf, sup, lambda){  
    p1 <-  pmax((t-inf), 0)
    p2 <-  pmin(p1, sup-inf)
    return(sum(lambda*p2)) 
  }
  
  H <- rep(NA, length(edades))
  x <- min(inf):max(sup)
  
  for (i in 1:length(H)){
    H[i] <- H.pw(x[i], inf, sup, lambda)
  }
  
  f <- function(t, inf, sup, lambda, u){
    res <- H.pw(t, inf, sup, lambda) + log(u)
    return(res)
  }
  
  root <- function(n, inf, sup, lambda){
    u <- runif(n, min = exp(-H)[length(exp(-H))], max = 1) 
    times <- rep(NA, n) 
    
    for(i in 1:n){
      result <- uniroot(f, interval=c(0, length(lambda)),
                        u=u[i], inf=inf, sup=sup, lambda=lambda) 
      times[i] <- result$root
    }
    return(times)
  }
  
  t <- root(n, inf, sup, lambda)
  
  
  if(Haz){ return(list(t, H))}else{ return(t) }
  
}
# Funcion modificada

ste2 <- function(n, edades, lambda, Haz = F){
  
  inf <- seq(0, length(edades)-1,1)
  sup <- seq(1, length(edades))
  
  H.pw <- function(t, inf, sup, lambda){  
    p1 <-  pmax((t-inf), 0)
    p2 <-  pmin(p1, sup-inf)
    return(sum(lambda*p2)) 
  }
  
  H <- rep(NA, length(edades))
  x <- min(inf):max(sup)
  
  for (i in 1:length(H)){
    H[i] <- H.pw(x[i], inf, sup, lambda)
  }
  
  f <- function(t, inferior, superior, nivel, u){
    return(H.pw(t, inf=inferior, sup=superior, lambda=nivel) + log(u))
  }
  
  ne <- exp(-H)[length(exp(-H))]
  nn <- n - sum(runif(n) < ne)
  
  if(nn == 0){
    
    t <- rep(Inf, n)
    
    return(t); break()
    
  }
  
  root <- function(nn, inf, sup, lambda){
    u <- runif(nn, min=exp(-H)[length(exp(-H))])
    times <- rep(NA, nn)
    for(i in 1:nn){
      result <- uniroot(f, interval=c(0, length(lambda)),
                        u=u[i], inferior=inf, superior=sup, nivel=lambda)
      times[i] <- result$root
    }
    return(times)
  }
  
  t_e <- root(nn, inf, sup, lambda)
  
  if(n-length(t_e)!=0){
    
    t <- sample(c(t_e, rep(Inf, n-length(t_e))))
    
  }else{
    
    t <- t_e
    
  }
  
  if(min(edades)!=0){ 
    t <- t + min(edades)  
  }
  
  if(Haz){ return(list(t, H)) }else{ return(t) }
  
}


####################
#    VALIDACIÓN    #
####################
n <- 10000
mort <- read.csv(file.path("datos","mx.csv"))
te <- ste2(n, edades = mort$edad, lambda = mort$h, Haz = T)
eventos <- te[[1]] < Inf 
H <- te[[2]] 
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
lines(mort$edad, exp(-H), lwd=3, col=2, lty=2)

fert <- read.csv(file.path("datos","fx.csv"))
te <- ste2(n, edades = fert$edad, lambda = fert$h, Haz = T)
eventos <- te[[1]] < Inf 
H <- te[[2]]
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
lines(fert$edad, exp(-H), lwd=3, col=2, lty=2)

# Ejercicio: Modificar la función para que retorne la duración desde el nacimiento en ambos casos

ste(1, mort$edad, mort$h)

ste2(1, mort$edad, mort$h)

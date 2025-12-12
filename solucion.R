# solucion trivial ejemplo
remove(list=ls())
setwd("/.")
dir()
# cargamos funciones con soluciones triviales. 
# Cada equipo debera definir en su archivo de solucion_teamName.R todas las
# funciones incluidas abajo

#############################################################################################
# MODIFICAR DESDE AQUI...


teamName <- "ChinaGrande"
# integrante 1: 
# integrante 2:
# integrante 3:

##########################
# seccion 2 - predicciones
##########################

# Funcion para predecir UN activo para el periodo test en el momento t
getPred <- function(x_train, x_test_past){
  # INPUTS: x_train serie de tiempo de todo periodo train para UN activo
  #       : x_test_past serie de tiempo de periodo test que va desde 0 hast t-1
  
  # OUTPUTS: mu_hat: prediccion del activo para el momento t del periodo test
  #          se_hat: desviación estándar de la predicción. 
  
  
  # INSERTAR ESTIMACION DE MODELO DE SERIES DE TIEMPO PARA MOMENTO T PARA UN ACTIVO
  
  # Entrenamos el modelo con todo el pasado
  
  modelo <- auto.arima(x_train)
  
  datos_actuales <- c(x_train,x_test_past)
  
  nuevo_ajuste <- Arima(datos_actuales, model = modelo)
  
  # Pronóstico a un paso
  
  fc <- forecast::forecast(nuevo_ajuste, h = 1)
  
  # Valor esperado (media) del pronóstico
  
  mu_hat <- as.numeric(fc$mean[1])
  
  # Estimamos la desviación estándar a partir del intervalo del 80%
  
  z80 <- qnorm(0.8)
  se_from_up <- (fc$upper[,"80%"][1] - fc$mean[1]) / z80
  se_from_lo <- (fc$mean[1] - fc$lower[,"80%"][1]) / z80
  se_hat_aux <- pmax(se_from_up, se_from_lo)
  se_hat <- as.numeric(se_hat_aux)
  
  return(list(mu_hat=mu_hat, se_hat=se_hat))
}

# Función para maximizar el retorno total en función del parámetro gamma

getGamma <- function(mu_hat, se_hat, Xtrain, getSigma, getAlpha, crit){
  
  # Partimos los datos de train en 80% entrenamiento y 20% validación
  
  i80 <- as.integer(0.8*length(Xtrain[,1]))
  
  Xsub <- Xtrain[1:i80,]
  Xval <- Xtrain[(i80+1):length(Xtrain[,1]),]
  
  # Tomamos el gamma que nos de el máximo rendimiento total en la fase de
  # validación
  
  gamma_list <- seq(1,20,length.out=100)
  R_list <- c()
  
  for (i in 1:length(gamma_list)) {
    
    alpha_hat <- getAlpha_ts(mu_hat, se_hat, gamma_list[i], getSigma, getAlpha, Xsub, Xval)
    passChecks <- getChecks(alpha_hat, mode=c(crit))
    R_list[i] <- getRet(alpha_hat, Xval, passChecks)
  }
  
  return(gamma_list[which.max(R_list)])
}


#####################################
# seccion 3 - utilidad media-varianza
###########################################################
# 3.1 utilidad media-varianza, alfa_i positiva o negativa
###########################################################
gammaMV <- 5

# Funcion para estimar la matriz de covarianzas entre los rendimientos de los 5
# activos a partir de las desviaciones estándares (que vendran de su modelo Arima)
# y el historico de rendimientos (que pueden usar para estimar correlaciones) 
getSigmaMV <- function(sig, Xpast){
  # INPUT: sig, vector en R^5 con desviaciones estandar de 5 activos al momento t
  #        Xpast, matriz en R^(T x 5) con rendimientos de 5 activos desde t=0 hasta t-1
  # OUTPUT: Sigma: matriz en 5 x 5 con covarianzas entre activos
  
  # Creamos la matriz de correlación R
  
  R <- cor(Xpast)
  
  # Creamos la matriz diagonal D
  
  D <- diag(sig)
  
  #Creamos la matriz de covarianza
  
  Sigma <- D %*% R %*% D
  
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad Media-Varianza CON posiciones cortas

# Resolvemos el problema de optimización cuadrática con los multiplicadores
# de Lagrange

getAlphaMV <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-MV sujeto a que alpha sume a 1.
  
  # Estamos ante un problema de optimización cuadrática con restricciones lineales
  # min Q x = b sujeto a sum(x) = 1
  # donde Q = gamma*Sigma, b = mu y x = alpha
  # Para resolverlo utilizamos la función solve.QP de la librería quadprog
  
  # Definimos las matrices del problema
  
  Q <- gamma*Sigma
  b <- mu
  
  # Definimos las restricciones como una matriz que indica el término que
  # multiplica a cada incógnita de nuestro sistema en las restricciones
  # (solve.QP asume que todas las restricciones son lineales)
  
  A <- matrix(1, nrow=length(mu), ncol=1)
  d <- 1
  
  # Resolvemos el problema cuadrático mediante solve.QP
  
  sol <- solve.QP(Q, b, A, d, meq=1)
  
  # Nota: El parámetro meq=1 se utiliza para indicar que la primera  
  # restricción es de igualdad
  
  alpha <- sol$solution
  return(alpha)
} 

############################################
# 3.2 utilidad media-varianza, alfa_i positiva 
############################################

gammaMVPos <- 18.4646

# Funcion para estimar la matriz de covarianzas entre los rendimientos de los 5
# activos a partir de las desviaciones estándares (que vendran de su modelo Arima)
# y el historico de rendimientos (que pueden usar para estimar correlaciones) 
getSigmaMVPos <- function(sig, Xpast){
  # INPUT: sig, vector en R^5 con desviaciones estandar de 5 activos al momento t
  #        Xpast, matriz en R^(T x 5) con rendimientos de 5 activos desde t=0 hasta t-1
  # OUTPUT: Sigma: matriz en 5 x 5 con covarianzas entre activos
  
  # Creamos la matriz de correlación R
  
  R <- cor(Xpast)
  
  # Creamos la matriz diagonal D
  
  D <- diag(sig)
  
  #Creamos la matriz de covarianza
  
  Sigma <- D %*% R %*% D
  
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad Media-Varianza SIN posiciones cortas


# Resolvemos el problema de optimización cuadrática con las condiciones KKT 
# (función solve.QP de la librería quadprog)

getAlphaMVPos <- function(mu,Sigma,gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-MV sujeto a que alpha sume a 1 y alpha_i>0.
  
  # Estamos ante un problema de optimización cuadrática con restricciones lineales
  # min Q x = b sujeto a sum(x) = 1 y x_i > 0 para todo i
  # donde Q = gamma*Sigma, b = mu y x = alpha
  # Para resolverlo utilizamos la función solve.QP de la librería quadprog
  
  # Definimos las matrices del problema
  
  Q <- gamma*Sigma
  b <- mu
  
  # Definimos las restricciones como una matriz que indica el término que
  # multiplica a cada incógnita de nuestro sistema en las restricciones
  
  A_ig <- matrix(1, nrow=length(mu), ncol=1)
  A_des <- diag(length(mu))
  
  A <- cbind(A_ig, A_des)
  d <- c(1, rep(0, length(mu)))
  
  # Resolvemos el problema cuadrático mediante solve.QP
  
  sol <- solve.QP(Q, b, A, d, meq=1)
  
  alpha <- sol$solution
  
  return(alpha)
}


################################################
# seccion 4 - 
# utilidad log, alfa_i positiva o negativa
################################################
gammaLog = 10 # INSERTAR VALOR EN REALES

# Funcion para estimar la matriz de covarianzas entre los rendimientos de los 5
# activos a partir de las desviaciones estándares (que vendran de su modelo Arima)
# y el historico de rendimientos (que pueden usar para estimar correlaciones) 
getSigmaLog <- function(sig, Xpast){
  # INPUT: sig, vector en R^5 con desviaciones estandar de 5 activos al momento t
  #        Xpast, matriz en R^(T x 5) con rendimientos de 5 activos desde t=0 hasta t-1
  # OUTPUT: Sigma: matriz en 5 x 5 con covarianzas entre activos
  
  # Creamos la matriz de correlación R
  
  R <- cor(Xpast)
  
  # Creamos la matriz diagonal D
  
  D <- diag(sig)
  
  #Creamos la matriz de covarianza
  
  Sigma <- D %*% R %*% D
  
  return(Sigma)
}

# Función de restricción

restNorm <- function(alpha,...) {
  return(sum(alpha))
}

minFunc <- function(alpha, gamma, mu_hat, Sigma_hat) {
  return(-Ulog(alpha, gamma, mu_hat, Sigma_hat))
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad log CON posiciones cortas

# Resolvemos el problema con un método de Programación Cuadrática Secuencial 
# y penalizaciones (función solnp de la librería Rsolnp)

getAlphaLog <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-log sujeto a que alpha sume a 1.
  
  N <- length(mu)
  alpha0 <- rep(1/N, N)
  
  res <- solnp(pars = alpha0,    # Puntos de inicio
               fun = minFunc,    # Función a minimizar
               eqfun = restNorm, # Función de restricción de igualdad
               eqB = c(1),       # El valor al que debe igualar (suma = 1)
               gamma = gamma,
               mu_hat = mu,
               Sigma_hat = Sigma 
  )                
  
  # --- 5. RESULTADOS ---
  alphas <- res$pars
  
  return(alphas)
}

# ... HASTA AQUI
#############################################################################################


###############################################################
# Evaluación de soluciones
###############################################################
library(rstudioapi)
library(quadprog)
library(Rsolnp)

setwd(dirname(getActiveDocumentContext()$path))

source("funciones/eval_funcs.R")

X <- read.csv("data/stock_returns_train_2.csv")
X <- ts(X/100)

# Validation mode - para que se evaluen asi mismos con el 
Xtrain <- window(X, start=1,end=8*12) # el start-end es un ejemplo, pueden cambiarlo
Xtest <- window(X, start=8*12+1,end=10*12)

# Test mode - no tendran el archivo stock_returns_test.csv asi que esto lo 
# ejecutaremos una vez entreguen soluciones
#Xtrain <- X
#Xtest <- ts(read.csv("stock_returns_test.csv"))


#seccion 2 - predicciones
set.seed(43)
res <- getPred_ts(Xtrain, Xtest, getPred)
mu_hat = res$mu_hat
se_hat = res$se_hat

# MAPE

for (i in 1:5) {
  plot(as.data.frame(Xtest)[,i], ty="l")
  lines(mu_hat[,i], col="blue", ty="l")
}


rmse <- sqrt(mean((Xtest-mu_hat)^2))
evals <- c(rmse=rmse)

evals

# seccion 3 - utilidad media varianza
# seccion 3.1 - utilidad media-varianza, alfa_i positiva o negativa

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaMV, getSigmaMV, getAlphaMV, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode="sum1")
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retMV=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaMV, getSigmaMV, passChecks, Umv)
evals <- c(evals,  Umv=Umv_rel)

# seccion 3.2 - utilidad media-varianza, alfa_i positiva

#gammaMVPos <- getGamma(mu_hat, se_hat, Xtrain, getSigmaMVPos, getAlphaMVPos, c("sum1","pos"))

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaMVPos, getSigmaMVPos, getAlphaMVPos, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode=c("sum1","pos"))
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retMVPos=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaMVPos, getSigmaMVPos, passChecks, Umv)
evals <- c(evals,  UmvPos=Umv_rel)

# seccion 4 -
# utilidad log, alfa_i positiva o negativa

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaLog, getSigmaLog, getAlphaLog, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode="sum1")
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retLog=ret)
print(passChecks)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaLog, getSigmaLog, passChecks, Ulog)
evals <- c(evals,  UmvPosInt=Umv_rel)

evals

print('Gammas elegidos')
print('')
cat('GammaMV =', gammaMV)
print('')
cat('GammaMVPos =', gammaMVPos)
print('')
cat('GammaLog =', gammaLog)
print('')

print('Seccion 2')
cat('Rmse = ',evals[1])
print('')

print('Seccion 3.1')
cat('R = ',evals[2], '; Uv = ', evals[3])
print('')

print('Seccion 3.2')
cat('R = ',evals[4], '; Uv = ', evals[5])
print('')

print('Seccion 4')
cat('R = ',evals[6], '; Uv = ', evals[7])
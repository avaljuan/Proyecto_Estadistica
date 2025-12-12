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

#####################################
# seccion 3 - utilidad media-varianza
###########################################################
# 3.1 utilidad media-varianza, alfa_i positiva o negativa
###########################################################
gammaMV <- 5 #INSERTAR VALOR EN REALES

getGamma <- function(mu_hat, se_hat, Xtrain, Xtest, getSigma, getAlpha, Ufunc, crit){
  
  gamma_list <- seq(1,10,length.out=100)
  U_list <- c()
  
  for (i in 1:length(gamma_list)) {
    
    alpha_hat <- getAlpha_ts(mu_hat, se_hat, gamma_list[i], getSigma, getAlpha, Xtrain, Xtest)
    passChecks <- getChecks(alpha_hat, mode=c(crit))
    
    U_list[i] <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gamma_list[i], getSigma, passChecks, Ufunc)
  }
  
  return(gamma_list[which.max(U_list)])
}

getSharpe <- function(alpha_hat, Xtest, passChecks, rf = 0){
  # retornos de portafolio por periodo
  port_ret <- rowSums(alpha_hat * Xtest)
  
  # nos quedamos sólo con los periodos válidos
  port_ret <- port_ret[passChecks]
  
  # si la sd es 0 o muy pequeña, devolvemos NA para evitar problemas
  sd_ret <- sd(port_ret)
  if (length(port_ret) < 2 || sd_ret == 0) return(NA)
  
  sharpe <- (mean(port_ret - rf)) / sd_ret
  return(sharpe)
}

getGamma_Sharpe <- function(mu_hat, se_hat, Xtrain,
                            getSigma, getAlpha, crit, rf = 0){
  i80 <- as.integer(0.8*length(Xtrain[,1]))
  
  
  
  Xsub <- Xtrain[1:i80,]
  Xval <- Xtrain[(i80+1):length(Xtrain[,1]),]
  
  

  gamma_list <- seq(1, 10, length.out = 50)  # por ejemplo
  SR_list <- rep(NA, length(gamma_list))
  
  for (i in seq_along(gamma_list)) {
    gamma_i <- gamma_list[i]
    
    # calculamos los pesos a lo largo del test
    alpha_hat <- getAlpha_ts(mu_hat, se_hat, gamma_i,
                             getSigma, getAlpha, Xsub, Xval)
    
    passChecks <- getChecks(alpha_hat, mode = crit)
    
    SR_list[i] <- getSharpe(alpha_hat, Xval, passChecks, rf)
  }
  
  best_idx <- which.max(SR_list)
  return(gamma_list[best_idx])
}

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
  
  #INSERTAR CONSTRUCCION DE MATRIZ DE COVARIANZAS 
  Sigma <- D %*% R %*% D
  
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad Media-Varianza CON posiciones cortas

getAlphaMV <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-MV sujeto a que alpha sume a 1.
  
  # INSERTAR PASOS DE OPTIMIZACION
  # Problema: Max (alpha^T * mu - (gamma/2) * alpha^T * Sigma * alpha)
  # Equivalente a QP: Min ((gamma/2) * alpha^T * Sigma * alpha - mu^T * alpha)
  # Sujeto a: sum(alpha) = 1
  
  # Matriz cuadrática (D en quadprog es 2 * término cuadrático si no hay factor 1/2, 
  # pero solve.QP minimiza 1/2 x^T D x. Nuestra función tiene gamma/2.
  # Por lo tanto, pasamos gamma * Sigma como Dmat).
  
  Dmat <- gamma * Sigma
  
  # Vector lineal (-d^T b en solve.QP). Nosotros tenemos -mu^T alpha.
  dvec <- mu
  
  # Restricciones: Sum(alpha) = 1
  # Amat^T * alpha >= bvec (o = bvec si meq > 0)
  Amat <- matrix(1, nrow=length(mu), ncol=1) # Columna de 1s
  bvec <- 1
  
  # Resolvemos QP (meq=1 indica que la primera restricción es de igualdad)
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
  
  alpha <- sol$solution
  return(alpha)
} 

############################################
# 3.2 utilidad media-varianza, alfa_i positiva 
############################################

gammaMVPos <- 5 #INSERTAR VALOR EN REALES

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
  
  #INSERTAR CONSTRUCCION DE MATRIZ DE COVARIANZAS
  
  Sigma <- D %*% R %*% D
  
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad Media-Varianza SIN posiciones cortas
getAlphaMVPos <- function(mu,Sigma,gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-MV sujeto a que alpha sume a 1 y alpha_i>0.
  
  # INSERTAR PASOS DE OPTIMIZACION
  n_assets <- length(mu)
  
  # Matriz y vector objetivo iguales al caso anterior
  Dmat <- gamma * Sigma
  dvec <- mu
  
  # Restricciones:
  # 1. Sum(alpha) = 1 (Igualdad)
  # 2. alpha_i >= 0   (Desigualdad, matriz identidad)
  
  A_sum <- matrix(1, nrow=n_assets, ncol=1)
  A_pos <- diag(n_assets)
  Amat <- cbind(A_sum, A_pos) # Combinamos restricciones
  
  bvec <- c(1, rep(0, n_assets)) # 1 para la suma, 0s para la positividad
  
  # meq=1: la primera restricción (suma) es igualdad, el resto desigualdad
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
  
  alpha <- sol$solution
  return(alpha)
}


################################################
# seccion 4 - 
# utilidad log, alfa_i positiva o negativa
################################################
gammaLog = 5 # INSERTAR VALOR EN REALES

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
  
  #INSERTAR CONSTRUCCION DE MATRIZ DE COVARIANZAS 
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
getAlphaLog <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-log sujeto a que alpha sume a 1.
  
  N <- length(mu)
  alpha0 <- rep(1/N, N)
  
  res <- solnp(pars = alpha0,                 # Puntos de inicio
               fun = minFunc,       # Función a minimizar
               eqfun = restNorm, # Función de restricción de igualdad
               eqB = c(1),      # El valor al que debe igualar (suma = 1)
               LB = rep(-1, N),
               UB = rep(2, N),
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

# seccion 3 - utilidad media varianza
# utilidad media-varianza, alfa_i positiva o negativa

crit <- "sum1"
#gammaMV <- getGamma(mu_hat, se_hat, Xtrain, Xtest, getSigmaMV, getAlphaMV, Umv, crit)
gammaMV <- getGamma_Sharpe(mu_hat, se_hat, Xtrain, getSigmaMV, getAlphaMV, crit)

#gammaMV <- 3

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaMV, getSigmaMV, getAlphaMV, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode=crit)
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retMV=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaMV, getSigmaMV, passChecks, Umv)
evals <- c(evals,  Umv=Umv_rel)

# utilidad media-varianza, alfa_i positiva

crit <- c("sum1","pos")
#gammaMVPos <- getGamma(mu_hat, se_hat, Xtrain, Xtest, getSigmaMVPos, getAlphaMVPos, Umv, crit)
gammaMVPos <- getGamma_Sharpe(mu_hat, se_hat, Xtrain,getSigmaMVPos, getAlphaMVPos, crit)

#gammaMVPos <- 3

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaMVPos, getSigmaMVPos, getAlphaMVPos, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode=crit)
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retMVPos=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaMVPos, getSigmaMVPos, passChecks, Umv)
evals <- c(evals,  UmvPos=Umv_rel)

# seccion 4 -
# utilidad log, alfa_i positiva o negativa

crit <- "sum1"
#gammaLog <- getGamma(mu_hat, se_hat, Xtrain, Xtest, getSigmaLog, getAlphaLog, Ulog, crit)
gammaLog <- getGamma_Sharpe(mu_hat, se_hat, Xtrain, getSigmaLog, getAlphaLog, crit)

#gammaLog <- 3

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaLog, getSigmaLog, getAlphaLog, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode=crit)
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retLog=ret)
print(passChecks)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaLog, getSigmaLog, passChecks, Ulog)
evals <- c(evals,  UmvPosInt=Umv_rel)

print('Gammas elegidos')
print('')
cat('GammaMV =', gammaMV )
print('')
cat('GammaMVPos =', gammaMVPos )
print('')
cat('GammaLog =', gammaLog)
print('')

print('Seccion 2')
cat('Rmse = ',evals[1])
print('')
print('Seccion 3.1')
cat('R = ',evals[2], '; Uv = ', evals[3], '\n')
print('Seccion 3.2')
cat('R = ',evals[4], '; Uv = ', evals[5], '\n')
print('')
print('Seccion 4')
cat('R = ',evals[6], '; Uv = ', evals[7])
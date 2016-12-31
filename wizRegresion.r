# Paquetes que vamos a utilizar
require(kknn)

# Importamos el fichero con los datos para el trabajo
wizmir <- read.csv2("./wizmir/wizmir.dat", header = F, sep = ",", comment.char = "@", dec = ".")
# Nombres de las variables
names(wizmir) <- c("Max_temperature", "Min_temperature", "Dewpoint", "Precipitation", "Sea_level_pressure", "Standard_pressure", "Visibility", "Wind_speed", "Max_wind_speed", "Mean_temperature")

# Creamos varias funciones que utilizaremos a lo largo del código

## Funciones para crear una tabla donde poder comparar los resultados

percentage <- function(x){
  x * 100
}

## Función para calcular la moda

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Función para calcular el error RMSE (Error cuadrático medio)

RMSE <- function(x){
  # Formula
  yprime=predict(x,wizmir)
  sqrt(sum(abs(Mean_temperature-yprime)^2)/length(yprime))
}

# Función para calcular el error al usar knn
knnRMSE <- function(x){
  yprime <- x$fitted.values
  sqrt(sum((Mean_temperature-yprime)^2)/length(yprime)) #RMSE
}

# Estructura de datos para guardar los valores obtenidos de un modelo.
df <- data.frame(
  r.squared = numeric(0),
  porCiento_R = numeric(0),
  adj.r.squared = numeric(0),
  porCientoR2 = numeric(0),
  RMSE = numeric(0))

# Función para crear las filas del data.frame

createRow <- function(x){
  valueList <- c(round(summary(x)$r.squared, digits = 4),
                 percentage(summary(x)$r.squared),
                 round(summary(x)$adj.r.squared, digits = 4),
                 percentage(summary(x)$adj.r.squared),
                 rmse)
}

# Función para poner el nombre a cada fila del data.frame df.

putName <- function(nameRow, df){
  aux <- row.names(df)
  aux[length(aux)] <- nameRow
  row.names(df) <- aux
  df
}


# Miramos el tipo de objeto que tenemos y el tipo de sus variables.

## Características generales de wizmir
attach(wizmir)
is.data.frame(wizmir)
dim(wizmir)
str(wizmir)

### Existen valores "NA"
table(is.na(wizmir))

### Tipos de datos de las variables
is.double(Max_temperature)
is.double(Min_temperature)
is.double(Dewpoint)
is.double(Precipitation)
is.double(Sea_level_pressure)
is.double(Standard_pressure)
is.double(Visibility)
is.double(Wind_speed)
is.double(Max_wind_speed)
is.double(Mean_temperature)

## Resumen del data.frame en el que se nos muestra el valor máximo y mínimo registrado,
## el primer cuartil, la mediana, el tercer cuartil y la media de cada variable.
summary(wizmir)

# Calculamos algunas métricas que no salen en el "summary"

## Desviación estandar para cada una de las variables
allSd <- apply(wizmir, 2, sd)
## Mediana de cada una de las variables
allMedian <- apply(wizmir, 2, median)
## Rango intercuartílico
allIQR <- apply(wizmir, 2, IQR)

# Gráfica de todos con todos
plot(wizmir)

# Análisis de cada una de las variables por separado

## Max_temperature

###  Medidas de centralidad

mean(Max_temperature)
median(Max_temperature)
getMode(Max_temperature)

### Medidas de dispersión

var(Max_temperature)
sd(Max_temperature)
max(Max_temperature)
min(Max_temperature)
range(Max_temperature)
quantile(Max_temperature)

### Gráfica de Max_temperature respecto a Mean_temperature, la salida.
plot(Max_temperature, Mean_temperature)

## Min_temperature

###  Medidas de centralidad

mean(Min_temperature)
median(Min_temperature)
getMode(Min_temperature)

## Medidas de dispersión

var(Min_temperature)
sd(Min_temperature)
max(Min_temperature)
min(Min_temperature)
range(Min_temperature)
quantile(Min_temperature)

### Gráfica de Max_temperature respecto a Mean_temperature, la salida.
plot(Min_temperature, Mean_temperature)

# Dewpoint

###  Medidas de centralidad

mean(Dewpoint)
median(Dewpoint)
getMode(Dewpoint)

## Medidas de dispersión

var(Dewpoint)
sd(Dewpoint)
max(Dewpoint)
min(Dewpoint)
range(Dewpoint)
quantile(Dewpoint)

### Gráfica de Dewpoint respecto a Mean_temperature, la salida.
plot(Dewpoint, Mean_temperature)

# Precipitation

###  Medidas de centralidad

mean(Precipitation)
median(Precipitation)
getMode(Precipitation)

## Medidas de dispersión

var(Precipitation)
sd(Precipitation)
max(Precipitation)
min(Precipitation)
range(Precipitation)
quantile(Precipitation)

### Gráfica de Precipitation respecto a Mean_temperature, la salida.
plot(Precipitation, Mean_temperature)

# Sea_level_pressure

###  Medidas de centralidad

mean(Sea_level_pressure)
median(Sea_level_pressure)
getMode(Sea_level_pressure)

## Medidas de dispersión

var(Sea_level_pressure)
sd(Sea_level_pressure)
max(Sea_level_pressure)
min(Sea_level_pressure)
range(Sea_level_pressure)
quantile(Sea_level_pressure)

### Gráfica de Sea_level_pressure respecto a Mean_temperature, la salida.
plot(Sea_level_pressure, Mean_temperature)

# Standard_pressure

###  Medidas de centralidad

mean(Standard_pressure)
median(Standard_pressure)
getMode(Standard_pressure)

## Medidas de dispersión

var(Standard_pressure)
sd(Standard_pressure)
max(Standard_pressure)
min(Standard_pressure)
range(Standard_pressure)
quantile(Standard_pressure)

### Gráfica de Standard_pressure respecto a Mean_temperature, la salida.
plot(Standard_pressure, Mean_temperature)

# Visibility

###  Medidas de centralidad

mean(Visibility)
median(Visibility)
getMode(Visibility)

## Medidas de dispersión

var(Visibility)
sd(Visibility)
max(Visibility)
min(Visibility)
range(Visibility)
quantile(Visibility)

### Gráfica de Visibility respecto a Mean_temperature, la salida.
plot(Visibility, Mean_temperature)

# Wind_speed

###  Medidas de centralidad

mean(Wind_speed)
median(Wind_speed)
getMode(Wind_speed)

## Medidas de dispersión

var(Wind_speed)
sd(Wind_speed)
max(Wind_speed)
min(Wind_speed)
range(Wind_speed)
quantile(Wind_speed)

### Gráfica de Wind_speed respecto a Mean_temperature, la salida.
plot(Wind_speed, Mean_temperature)

# Max_wind_speed

###  Medidas de centralidad

mean(Max_wind_speed)
median(Max_wind_speed)
getMode(Max_wind_speed)

## Medidas de dispersión

var(Max_wind_speed)
sd(Max_wind_speed)
max(Max_wind_speed)
min(Max_wind_speed)
range(Max_wind_speed)
quantile(Max_wind_speed)

### Gráfica de Max_wind_speed respecto a Mean_temperature, la salida.
plot(Max_wind_speed, Mean_temperature)

# Mean_temperature (Variable de salida)

###  Medidas de centralidad

mean(Mean_temperature)
median(Mean_temperature)
getMode(Mean_temperature)

## Medidas de dispersión

var(Mean_temperature)
sd(Mean_temperature)
max(Mean_temperature)
min(Mean_temperature)
range(Mean_temperature)
quantile(Mean_temperature)

# Regresión lineal

## Max_temperature

lm1 <- lm(Mean_temperature~Max_temperature, data = wizmir)
summary(lm1)
plot(Mean_temperature~Max_temperature, wizmir)
abline(lm1, col="red")
confint(lm1)
rmse <- RMSE(lm1)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm1)
# Almacenamiento de los valores en el data.frame df
df[1,] <- valueList
df <- putName("lm1",df)

## Min_temperature

lm2 <- lm(Mean_temperature~Min_temperature, data = wizmir)
summary(lm2)
plot(Mean_temperature~Min_temperature, wizmir)
abline(lm2, col="red")
confint(lm2)
rmse <- RMSE(lm2)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm2)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm2",df)

## Dewpoint

lm3 <- lm(Mean_temperature~Dewpoint, data = wizmir)
summary(lm3)
plot(Mean_temperature~Dewpoint, wizmir)
abline(lm3, col= "red")
confint(lm3)
rmse <- RMSE(lm3)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm3)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm3",df)

## Sea_level_pressure

lm4 <- lm(Mean_temperature~Sea_level_pressure, data = wizmir)
summary(lm4)
plot(Mean_temperature~Sea_level_pressure, wizmir)
abline(lm4, col = "red")
confint(lm4)
rmse <- RMSE(lm4)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm4)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm4",df)

## Standard_pressure

lm5 <- lm(Mean_temperature~Standard_pressure, data = wizmir)
summary(lm5)
plot(Mean_temperature~Standard_pressure, data = wizmir)
abline(lm5, col = "red")
confint(lm5)
rmse <- RMSE(lm5)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm5)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm5",df)

## Resultados

df

# Modelo lineal múltiple

## Max_temperature y Min_temperature (Mejor aproximación por separado)

lm6 <- lm(Mean_temperature~Max_temperature+Min_temperature, data = wizmir)
summary(lm6)
rmse <- RMSE(lm6)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm6)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm6",df)

## Modelo con todas las variables

lm7 <- lm(Mean_temperature~., data = wizmir)
summary(lm7)
rmse <- RMSE(lm7)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm7)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm7",df)

## Eliminación de las variables no relevantes

lm8 <- lm(Mean_temperature~.-Precipitation-Wind_speed, data = wizmir)
summary(lm8)
rmse <- RMSE(lm8)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm8)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm8",df)

lm9 <- lm(Mean_temperature~.-Precipitation-Wind_speed-Standard_pressure-Max_wind_speed, data = wizmir)
summary(lm9)
rmse <- RMSE(lm9)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm9)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm9",df)

lm10 <- lm(Mean_temperature~.-Precipitation-Wind_speed-Standard_pressure-Max_wind_speed-Dewpoint, data = wizmir)
summary(lm10)
rmse <- RMSE(lm10)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm10)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm10",df)

## Resultados

df

# Interacciones (Todos con todos)

## Max_temperature * Min_temperature
lm11 <- lm(Mean_temperature~Max_temperature*Min_temperature, data = wizmir)
summary(lm11)
rmse <- RMSE(lm11)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm11)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm11",df)

# Max_temperature * Dewpoint
lm12 <- lm(Mean_temperature~Max_temperature*Dewpoint, data = wizmir)
summary(lm12)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm12)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm12",df)

# Max_temperature * Sea_level_pressure
lm13 <- lm(Mean_temperature~Max_temperature*Sea_level_pressure, data = wizmir)
summary(lm13)
rmse <- RMSE(lm13)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm13)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm13",df)

# Max_temperature * Standard_pressure
lm14 <- lm(Mean_temperature~Max_temperature*Standard_pressure, data = wizmir)
summary(lm14)
rmse <- RMSE(lm14)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm14)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm14",df)

# Min_temperature * Dewpoint
lm15 <- lm(Mean_temperature~Min_temperature*Dewpoint, data = wizmir)
summary(lm15)
rmse <- RMSE(lm15)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm15)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm15",df)

# Min_temperature * Sea_level_pressure
lm16 <- lm(Mean_temperature~Min_temperature*Sea_level_pressure, data = wizmir)
summary(lm16)
rmse <- RMSE(lm16)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm16)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm16",df)

# Min_temperature * Standard_pressure
lm17 <- lm(Mean_temperature~Min_temperature*Standard_pressure, data = wizmir)
summary(lm17)
rmse <- RMSE(lm17)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm17)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm17",df)

# Dewpoint * Sea_level_pressure
lm18 <- lm(Mean_temperature~Dewpoint*Sea_level_pressure, data = wizmir)
summary(lm18)
rmse <- RMSE(lm18)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm18)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm18",df)

# Dewpoint * Standard_pressure
lm19 <- lm(Mean_temperature~Dewpoint*Standard_pressure, data = wizmir)
summary(lm19)
rmse <- RMSE(lm19)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm19)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm19",df)

# Sea_level_pressure * Standard_pressure
lm20 <- lm(Mean_temperature~Sea_level_pressure*Standard_pressure, data = wizmir)
summary(lm20)
rmse <- RMSE(lm20)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm20)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm20",df)

## Resultados (Tabla con todos los r2 ajustados)

df

# Regresión no lineal

## Polinómica para Max_temperature

nlm1 <- lm(Mean_temperature~poly(Max_temperature, 19, raw = T), data = wizmir)
summary(nlm1)

# Rebajo el grado a 4
nlm1 <- lm(Mean_temperature~poly(Max_temperature, 4, raw = T), data = wizmir)
summary(nlm1)
rmse <- RMSE(nlm1)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm1)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm1",df)

## Polinómica para Min_temperature

nlm2 <- lm(Mean_temperature~poly(Min_temperature, 19, raw = T), data = wizmir)
summary(nlm2)

# Rebajo el grado a 4
nlm2 <- lm(Mean_temperature~poly(Min_temperature, 4, raw = T), data = wizmir)
summary(nlm2)
rmse <- RMSE(nlm2)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm2)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm2",df)

## Polinómica para Dewpoint
nlm3 <- lm(Mean_temperature~poly(Dewpoint, 20, raw = T), data = wizmir)
summary(nlm3)

# Rebajo el grado a 5
nlm3 <- lm(Mean_temperature~poly(Dewpoint, 5, raw = T), data = wizmir)
summary(nlm3)
rmse <- RMSE(nlm3)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm3)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm3",df)

## Polinómica para Sea_level_pressure
nlm4 <- lm(Mean_temperature~poly(Sea_level_pressure, 15, raw = T), data = wizmir)
summary(nlm4)

# Grado 3
nlm4 <- lm(Mean_temperature~poly(Sea_level_pressure, 3, raw = T), data = wizmir)
summary(nlm4)
rmse <- RMSE(nlm4)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm4)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm4",df)

## Polinómica para Standard_pressure, directamente a grado 4
nlm5 <- lm(Mean_temperature~poly(Standard_pressure, 4, raw = T), data = wizmir)
summary(nlm5)
rmse <- RMSE(nlm5)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm5)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm5",df)

## Polinómica unión entre Max_temperature y Min_temperature

nlm6 <- lm(Mean_temperature~Max_temperature +Min_temperature +I(Max_temperature * Min_temperature) +I(Max_temperature^2) +I(Max_temperature^2 * Min_temperature),data = wizmir)
summary(nlm6)
rmse <- RMSE(nlm6)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm6)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm6",df)

# Resultados obtenidos hasta el momento
df

dfBest <- df[df$porCientoR2 > 99 & df$porCiento_R > 99,]
# Modelo ordenado de menos a mayor RMSE
dfBest[order(dfBest$RMSE),]


# k-nn

# knn para los mejores modelos hasta el momento
# La función kknn tiene por defecto:
#  k = 7 (número de vecinos), distance = 2 (euclidea), kernel = "optimal“ y scale=TRUE

## Max_temperature + Min_temperature (lm6)
knn1 <- kknn(Mean_temperature~Max_temperature + Min_temperature, wizmir, wizmir)
rmse <- knnRMSE(knn1)

# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn1",df)

## Mean_temperature ~ . (lm7)
knn2 <- kknn(Mean_temperature ~ ., wizmir, wizmir)
# Error RMSE knn
rmse <- knnRMSE(knn2)

# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn2",df)

##  Mean_temperature ~ . - Precipitation - Wind_speed (lm8)
knn3 <- kknn( Mean_temperature ~ . - Precipitation - Wind_speed, wizmir, wizmir)
# Error RMSE knn
rmse <- knnRMSE(knn3)
# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn3",df)

## Mean_temperature ~ . - Precipitation - Wind_speed - Standard_pressure - Max_wind_speed (lm9)
knn4 <- kknn(Mean_temperature ~ . - Precipitation - Wind_speed - Standard_pressure - Max_wind_speed, wizmir, wizmir)
# Error RMSE knn
rmse <- knnRMSE(knn4)
# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn4",df)

## Mean_temperature ~ . - Precipitation - Wind_speed - Standard_pressure -
# Max_wind_speed - Dewpoint (lm10)
knn5 <- kknn(Mean_temperature ~ . - Precipitation - Wind_speed - Standard_pressure - Max_wind_speed - Dewpoint, wizmir, wizmir)
rmse <- knnRMSE(knn5)
# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn5",df)

## Mean_temperature ~ Max_temperature * Min_temperature (lm11)
knn6 <- kknn(Mean_temperature ~ Max_temperature * Min_temperature, wizmir, wizmir)
rmse <- knnRMSE(knn6)
# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn6",df)

## Mean_temperature ~ Max_temperature + Min_temperature + I(Max_temperature * Min_temperature) +
# I(Max_temperature^2) + I(Max_temperature^2 * Min_temperature) ()
knn7 <- kknn(Mean_temperature ~ Max_temperature + Min_temperature + I(Max_temperature * Min_temperature) + I(Max_temperature^2) + I(Max_temperature^2 * Min_temperature), wizmir, wizmir)
rmse <- knnRMSE(knn7)
# Almacenamiento del error RMSE en el data.frame df
df <- rbind(df, c(NA,NA,NA,NA,rmse))
df <- putName("knn7",df)

# Visualización de df
df

# Mejores RMSE
dfBest <- df[df$RMSE < 2,]
# Modelo ordenado de menos a mayor RMSE
dfBest[order(dfBest$RMSE),]


# Función cogida de las transparencias de clase
nombre <- "./wizmir/wizmir"
run_lm_fold <- function(i, x, tt = "test", z) {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  if(substr(z, start=1, stop=1) == "k"){
    z <- substr(z, start = 2, stop = nchar(z))
    print("Entro en el if del knn con ")
    print(z)
    fitMulti=kknn(z,x_tra,test)
    yprime=fitMulti$fitted.values
  }
  else{
    fitMulti = lm(z,x_tra)
    yprime=predict(fitMulti,test)
  }
  #fitMulti=lm(Y~.,x_tra)

  #fitMulti = lm(Y~X1 + X2, x_tra)
  #yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
# lm6 (Mean_temperature~Max_temperature + Min_temperature)
lmMSEtrain_lm6<-mean(sapply(1:5,run_lm_fold,nombre,"train", "Y~X1 + X2"))
lmMSEtest_lm6<-mean(sapply(1:5,run_lm_fold,nombre,"test", "Y~X1 + X2"))

# knn1 (misma fórmula)
lmMSEtrain_knn1<-mean(sapply(1:5,run_lm_fold,nombre,"train", "kY~X1 + X2"))
lmMSEtest_knn1<-mean(sapply(1:5,run_lm_fold,nombre,"test", "kY~X1 + X2"))

# lm7 (Mean_temperature~.)
lmMSEtrain_lm7<-mean(sapply(1:5,run_lm_fold,nombre,"train", "Y~."))
lmMSEtest_lm7<-mean(sapply(1:5,run_lm_fold,nombre,"test", "Y~."))

# lm8 Mean_temperature ~ . - Precipitation - Wind_speed
lmMSEtrain_lm8<-mean(sapply(1:5,run_lm_fold,nombre,"train", "Y~. - X4 - X8"))
lmMSEtest_lm8<-mean(sapply(1:5,run_lm_fold,nombre,"test", "Y~. - X4 - X8"))

# knn6 (Mean_temperature ~ Max_temperature * Min_temperature)
lmMSEtrain_knn6<-mean(sapply(1:5,run_lm_fold,nombre,"train", "kY~X1 * X2"))
lmMSEtest_knn6<-mean(sapply(1:5,run_lm_fold,nombre,"test", "kY~X1 * X2"))

# knn7 (Mean_temperature ~ Max_temperature + Min_temperature +     I(Max_temperature *
# Min_temperature) + I(Max_temperature^2) +     I(Max_temperature^2 * Min_temperature))
lmMSEtrain_knn7<-mean(sapply(1:5,run_lm_fold,nombre,"train", "kY~X1 + I(X1*X2) + I(X1^2) + I(X1^2 * X2)"))
lmMSEtest_knn7<-mean(sapply(1:5,run_lm_fold,nombre,"test", "kY~X1 + I(X1*X2) + I(X1^2) + I(X1^2 * X2)"))

# Introducimos los resultados obtenidos en un data.frame.

dfMSE <- data.frame(training_error = numeric(0),
                    test_error = numeric(0))

dfMSE[1,] <- c(lmMSEtrain_lm6, lmMSEtest_lm6)
dfMSE <- putName("lm6", dfMSE)
dfMSE <- rbind(dfMSE, c(lmMSEtrain_knn1, lmMSEtest_knn1))
dfMSE <- putName("knn1", dfMSE)
dfMSE <- rbind(dfMSE, c(lmMSEtrain_lm7, lmMSEtest_lm7))
dfMSE <- putName("lm7", dfMSE)
dfMSE <- rbind(dfMSE, c(lmMSEtrain_lm8, lmMSEtest_lm8))
dfMSE <- putName("lm8", dfMSE)
dfMSE <- rbind(dfMSE, c(lmMSEtrain_knn6, lmMSEtest_knn6))
dfMSE <- putName("knn6", dfMSE)
dfMSE <- rbind(dfMSE, c(lmMSEtrain_knn7, lmMSEtest_knn7))
dfMSE <- putName("knn7", dfMSE)
dfMSE

# Fórmula del mejor ajuste encontrado
lm8$call$formula

#leemos la tabla con los errores medios de test (código sacado de los apuntes)
resultados <- read.csv("./wizmir/regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]

# Regresión linear
tablatst["wizmir", "out_test_lm"] <- dfMSE["lm7","test_error"]
tablatst["wizmir", "out_test_kknn"] <- dfMSE["knn1","test_error"]

# Comparativa por pares de LM y KNN (Wilcoxon’s test) ##lm (other) vs knn (ref)
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]

wilc_1_2 <- cbind(ifelse(difs<0, abs(difs)+0.1, 0+0.1), ifelse(difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

# Se aplica el test y se interpretan los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman

tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)

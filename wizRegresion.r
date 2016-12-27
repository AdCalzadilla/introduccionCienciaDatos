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

putName <- function(nameRow){
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
df <- putName("lm1")

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
df <- putName("lm2")

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
df <- putName("lm3")

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
df <- putName("lm4")

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
df <- putName("lm5")

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
df <- putName("lm6")

## Modelo con todas las variables

lm7 <- lm(Mean_temperature~., data = wizmir)
summary(lm7)
rmse <- RMSE(lm7)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm7)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm7")

## Eliminación de las variables no relevantes

lm8 <- lm(Mean_temperature~.-Precipitation-Wind_speed, data = wizmir)
summary(lm8)
rmse <- RMSE(lm8)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm8)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm8")

lm9 <- lm(Mean_temperature~.-Precipitation-Wind_speed-Standard_pressure-Max_wind_speed, data = wizmir)
summary(lm9)
rmse <- RMSE(lm9)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm9)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm9")

lm10 <- lm(Mean_temperature~.-Precipitation-Wind_speed-Standard_pressure-Max_wind_speed-Dewpoint, data = wizmir)
summary(lm10)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm10)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm10")

## Resultados

df

# Interacciones (Todos con todos)

## Max_temperature * Min_temperature
lm11 <- lm(Mean_temperature~Max_temperature*Min_temperature, data = wizmir)
summary(lm11)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm11)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm11")

# Max_temperature * Dewpoint
lm12 <- lm(Mean_temperature~Max_temperature*Dewpoint, data = wizmir)
summary(lm12)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm12)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm12")

# Max_temperature * Sea_level_pressure
lm13 <- lm(Mean_temperature~Max_temperature*Sea_level_pressure, data = wizmir)
summary(lm13)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm13)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm13")

# Max_temperature * Standard_pressure
lm14 <- lm(Mean_temperature~Max_temperature*Standard_pressure, data = wizmir)
summary(lm14)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm14)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm14")

# Min_temperature * Dewpoint
lm15 <- lm(Mean_temperature~Min_temperature*Dewpoint, data = wizmir)
summary(lm15)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm15)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm15")

# Min_temperature * Sea_level_pressure
lm16 <- lm(Mean_temperature~Min_temperature*Sea_level_pressure, data = wizmir)
summary(lm16)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm16)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm16")

# Min_temperature * Standard_pressure
lm17 <- lm(Mean_temperature~Min_temperature*Standard_pressure, data = wizmir)
summary(lm17)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm17)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm17")

# Dewpoint * Sea_level_pressure
lm18 <- lm(Mean_temperature~Dewpoint*Sea_level_pressure, data = wizmir)
summary(lm18)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm18)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm18")

# Dewpoint * Standard_pressure
lm19 <- lm(Mean_temperature~Dewpoint*Standard_pressure, data = wizmir)
summary(lm19)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm19)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm19")

# Sea_level_pressure * Standard_pressure
lm20 <- lm(Mean_temperature~Sea_level_pressure*Standard_pressure, data = wizmir)
summary(lm20)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(lm20)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("lm20")

## Resultados (Tabla con todos los r2 ajustados)

df

# Regresión no lineal

## Polinómica para Max_temperature

nlm1 <- lm(Mean_temperature~poly(Max_temperature, 19, raw = T), data = wizmir)
summary(nlm1)

# Rebajo el grado a 4
nlm1 <- lm(Mean_temperature~poly(Max_temperature, 4, raw = T), data = wizmir)
summary(nlm1)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm1)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm1")

## Polinómica para Min_temperature

nlm2 <- lm(Mean_temperature~poly(Min_temperature, 19, raw = T), data = wizmir)
summary(nlm2)

# Rebajo el grado a 4
nlm2 <- lm(Mean_temperature~poly(Min_temperature, 4, raw = T), data = wizmir)
summary(nlm2)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm2)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm2")

## Polinómica para Dewpoint
nlm3 <- lm(Mean_temperature~poly(Dewpoint, 20, raw = T), data = wizmir)
summary(nlm3)

# Rebajo el grado a 5
nlm3 <- lm(Mean_temperature~poly(Dewpoint, 5, raw = T), data = wizmir)
summary(nlm3)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm3)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm3")

## Polinómica para Sea_level_pressure, directamente a grado 5
nlm4 <- lm(Mean_temperature~poly(Sea_level_pressure, 15, raw = T), data = wizmir)
summary(nlm4)

nlm4 <- lm(Mean_temperature~poly(Sea_level_pressure, 3, raw = T), data = wizmir)
summary(nlm4)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm4)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm4")

## Polinómica para Standard_pressure, directamente a grado 4
nlm5 <- lm(Mean_temperature~poly(Standard_pressure, 4, raw = T), data = wizmir)
summary(nlm5)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm5)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm5")

## Polinómica unión entre Max_temperature y Min_temperature

nlm6 <- lm(Mean_temperature~Max_temperature +Min_temperature +I(Max_temperature * Min_temperature) +I(Max_temperature^2) +I(Max_temperature^2 * Min_temperature),data = wizmir)
summary(nlm6)

# Lista para ir introduciendo los resultados del r2.
valueList <- createRow(nlm6)

# Almacenamiento de los valores en el data.frame df
df <- rbind(df, valueList)
df <- putName("nlm6")

# Resultados obtenidos hasta el momento
df

dfBest <- df[df$porCientoR2 > 99 & df$porCiento_R > 99,]
# Modelo ordenado de menos a mayor RMSE
dfBest[order(dfBest$RMSE),]


# k-nn

# knn en problemas de regresión

# Obtención del modelo para el conjunto de datos entero
#knn1 <- kknn(Mean_temperature ~ ., wizmir, wizmir) # Por defecto k = 7, distance = 2, kernel = "optimal“ # y scale=TRUE
#names(knn1)
#knn1$fitted.values
# Visualización
#plot(Mean_temperature~Max_temperature)
#points(Max_temperature,knn1$fitted.values,col="blue",pch=20)
#summary(knn1)

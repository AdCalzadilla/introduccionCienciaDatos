# Importamos el fichero con los datos para el trabajo
wizmir <- read.csv2("./wizmir/wizmir.dat", header = F, sep = ",", comment.char = "@", dec = ".")
# Nombres de las variables
names(wizmir) <- c("Max_temperature", "Min_temperature", "Dewpoint", "Precipitation", "Sea_level_pressure", "Standard_pressure", "Visibility", "Wind_speed", "Max_wind_speed", "Mean_temperature")

# Creamos varias funciones que utilizaremos a lo largo del código

## Funciones para crear una tabla donde poder comparar los resultados

percentage <- function(x){
  x * 100
}

createTable <- function(x){
  df <- data.frame(cbind(x), stringsAsFactors = F)
  porcentaje <- sapply(df[,1], as.numeric)
  porcentaje <- sapply(df[,1], percentage)
  df <- cbind(df, porcentaje)
  df
}

## Función para calcular la moda

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
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
# Lista para ir introduciendo los resultados del r2 ajustado.
adj.r.squared <- list(lm1=round(summary(lm1)$adj.r.squared, digits = 4))

## Min_temperature

lm2 <- lm(Mean_temperature~Min_temperature, data = wizmir)
summary(lm2)
plot(Mean_temperature~Min_temperature, wizmir)
abline(lm2, col="red")
confint(lm2)
adj.r.squared <- c(adj.r.squared, lm2=round(summary(lm2)$adj.r.squared, digits = 4))

## Dewpoint

lm3 <- lm(Mean_temperature~Dewpoint, data = wizmir)
summary(lm3)
plot(Mean_temperature~Dewpoint, wizmir)
abline(lm3, col= "red")
confint(lm3)
adj.r.squared <- c(adj.r.squared, lm3=round(summary(lm3)$adj.r.squared, digits = 4))

## Sea_level_pressure

lm4 <- lm(Mean_temperature~Sea_level_pressure, data = wizmir)
summary(lm4)
plot(Mean_temperature~Sea_level_pressure, wizmir)
abline(lm4, col = "red")
confint(lm4)
adj.r.squared <- c(adj.r.squared, lm4=round(summary(lm4)$adj.r.squared, digits = 4))

## Standard_pressure

lm5 <- lm(Mean_temperature~Standard_pressure, data = wizmir)
summary(lm5)
plot(Mean_temperature~Standard_pressure, data = wizmir)
abline(lm5, col = "red")
confint(lm5)
adj.r.squared <- c(adj.r.squared, lm5=round(summary(lm5)$adj.r.squared, digits = 4))

## Conclusiones (Tabla con todos los r2 ajustados)

createTable(adj.r.squared)

# Modelo lineal múltiple

## Max_temperature y Min_temperature (Mejor aproximación por separado)

lm6 <- lm(Mean_temperature~Max_temperature+Min_temperature, data = wizmir)
summary(lm6)
adj.r.squared <- c(adj.r.squared, lm6=round(summary(lm6)$adj.r.squared, digits = 4))

## Modelo con todas las variables

lm7 <- lm(Mean_temperature~., data = wizmir)
summary(lm7)
adj.r.squared <- c(adj.r.squared, lm7=round(summary(lm7)$adj.r.squared, digits = 4))

## Eliminación de las variables no relevantes

lm8 <- lm(Mean_temperature~.-Precipitation-Wind_speed, data = wizmir)
summary(lm8)
adj.r.squared <- c(adj.r.squared, lm8=round(summary(lm8)$adj.r.squared, digits = 4))

lm9 <- lm(Mean_temperature~.-Precipitation-Wind_speed-Standard_pressure-Max_wind_speed, data = wizmir)
summary(lm9)
adj.r.squared <- c(adj.r.squared, lm9=round(summary(lm9)$adj.r.squared, digits = 4))

lm10 <- lm(Mean_temperature~.-Precipitation-Wind_speed-Standard_pressure-Max_wind_speed-Dewpoint, data = wizmir)
summary(lm10)
adj.r.squared <- c(adj.r.squared, lm10=round(summary(lm10)$adj.r.squared, digits = 4))

## Conclusiones (Tabla con todos los r2 ajustados)

createTable(adj.r.squared)

# Interacciones (Todos con todos)

## Max_temperature * Min_temperature
lm11 <- lm(Mean_temperature~Max_temperature*Min_temperature, data = wizmir)
summary(lm11)
adj.r.squared <- c(adj.r.squared, lm11=round(summary(lm11)$adj.r.squared, digits = 4))

# Max_temperature * Dewpoint
lm12 <- lm(Mean_temperature~Max_temperature*Dewpoint, data = wizmir)
summary(lm12)
adj.r.squared <- c(adj.r.squared, lm12=round(summary(lm12)$adj.r.squared, digits = 4))

# Max_temperature * Sea_level_pressure
lm13 <- lm(Mean_temperature~Max_temperature*Sea_level_pressure, data = wizmir)
summary(lm13)
adj.r.squared <- c(adj.r.squared, lm13=round(summary(lm13)$adj.r.squared, digits = 4))

# Max_temperature * Standard_pressure
lm14 <- lm(Mean_temperature~Max_temperature*Standard_pressure, data = wizmir)
summary(lm14)
adj.r.squared <- c(adj.r.squared, lm14=round(summary(lm14)$adj.r.squared, digits = 4))

# Min_temperature * Dewpoint
lm15 <- lm(Mean_temperature~Min_temperature*Dewpoint, data = wizmir)
summary(lm15)
adj.r.squared <- c(adj.r.squared, lm15=round(summary(lm15)$adj.r.squared, digits = 4))

# Min_temperature * Sea_level_pressure
lm16 <- lm(Mean_temperature~Min_temperature*Sea_level_pressure, data = wizmir)
summary(lm16)
adj.r.squared <- c(adj.r.squared, lm16=round(summary(lm16)$adj.r.squared, digits = 4))

# Min_temperature * Standard_pressure
lm17 <- lm(Mean_temperature~Min_temperature*Standard_pressure, data = wizmir)
summary(lm17)
adj.r.squared <- c(adj.r.squared, lm17=round(summary(lm17)$adj.r.squared, digits = 4))

# Dewpoint * Sea_level_pressure
lm18 <- lm(Mean_temperature~Dewpoint*Sea_level_pressure, data = wizmir)
summary(lm18)
adj.r.squared <- c(adj.r.squared, lm18=round(summary(lm18)$adj.r.squared, digits = 4))

# Dewpoint * Standard_pressure
lm19 <- lm(Mean_temperature~Dewpoint*Standard_pressure, data = wizmir)
summary(lm19)
adj.r.squared <- c(adj.r.squared, lm19=round(summary(lm19)$adj.r.squared, digits = 4))

# Sea_level_pressure * Standard_pressure
lm20 <- lm(Mean_temperature~Sea_level_pressure*Standard_pressure, data = wizmir)
summary(lm20)
adj.r.squared <- c(adj.r.squared, lm20=round(summary(lm20)$adj.r.squared, digits = 4))

## Conclusiones (Tabla con todos los r2 ajustados)

createTable(adj.r.squared)

# Regresión no lineal

## Polinómica para Max_temperature

nlm1 <- lm(Mean_temperature~poly(Max_temperature, 19, raw = T), data = wizmir)
summary(nlm1)

# Rebajo el grado a 4
nlm1 <- lm(Mean_temperature~poly(Max_temperature, 4, raw = T), data = wizmir)
summary(nlm1)
adj.r.squared <- c(adj.r.squared, nlm1=round(summary(nlm1)$adj.r.squared, digits = 4))

## Polinómica para Min_temperature

nlm2 <- lm(Mean_temperature~poly(Min_temperature, 19, raw = T), data = wizmir)
summary(nlm2)

# Rebajo el grado a 4
nlm2 <- lm(Mean_temperature~poly(Min_temperature, 4, raw = T), data = wizmir)
summary(nlm2)
adj.r.squared <- c(adj.r.squared, nlm2=round(summary(nlm2)$adj.r.squared, digits = 4))

## Polinómica para Dewpoint
nlm3 <- lm(Mean_temperature~poly(Dewpoint, 20, raw = T), data = wizmir)
summary(nlm3)

# Rebajo el grado a 5
nlm3 <- lm(Mean_temperature~poly(Dewpoint, 5, raw = T), data = wizmir)
summary(nlm3)
adj.r.squared <- c(adj.r.squared, nlm3=round(summary(nlm3)$adj.r.squared, digits = 4))

## Polinómica para Sea_level_pressure, directamente a grado 5
nlm4 <- lm(Mean_temperature~poly(Sea_level_pressure, 15, raw = T), data = wizmir)
summary(nlm4)

nlm4 <- lm(Mean_temperature~poly(Sea_level_pressure, 3, raw = T), data = wizmir)
summary(nlm4)
adj.r.squared <- c(adj.r.squared, nlm4=round(summary(nlm4)$adj.r.squared, digits = 4))

## Polinómica para Standard_pressure, directamente a grado 4
nlm5 <- lm(Mean_temperature~poly(Standard_pressure, 4, raw = T), data = wizmir)
summary(nlm5)
adj.r.squared <- c(adj.r.squared, nlm5=round(summary(nlm5)$adj.r.squared, digits = 4))

## Polinómica unión entre Max_temperature y Min_temperature

nlm6 <- lm(Mean_temperature~Max_temperature +Min_temperature +I(Max_temperature * Min_temperature) +I(Max_temperature^2) +I(Max_temperature^2 * Min_temperature),data = wizmir)
summary(nlm6)
adj.r.squared <- c(adj.r.squared, nlm6=round(summary(nlm6)$adj.r.squared, digits = 4))

# Resultados obtenidos hasta el momento
createTable(adj.r.squared)

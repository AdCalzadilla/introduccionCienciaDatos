# Importamos el fichero con los datos para el trabajo
wizmir <- read.csv2("./wizmir/wizmir.dat", header = F, sep = ",", comment.char = "@", dec = ".")
# Nombres de las variables
names(wizmir) <- c("Max_temperature", "Min_temperature", "Dewpoint", "Precipitation", "Sea_level_pressure", "Standard_pressure", "Visibility", "Wind_speed", "Max_wind_speed", "Mean_temperature")

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

## Función para calcular la moda

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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

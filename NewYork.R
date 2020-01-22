#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Proyecto bicicletas New York +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++Jose Luis Apellániz+++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The data includes:
# Trip Duration (seconds)
# Start Time and Date
# Stop Time and Date
# Start Station Name
# End Station Name
# Station ID
# Station Lat/Long
# Bike ID
# User Type (Customer = 24-hour pass or 3-day pass user; Subscriber = Annual Member)
# Gender (Zero=unknown; 1=male; 2=female)
# Year of Birth

#This data has been processed to remove trips that are taken by staff as they service and inspect the system, 
#trips that are taken to/from any of our “test” stations (which we were using more in June and July 2013), 
#and any trips that were below 60 seconds in length (potentially false starts or users trying to re-dock a bike 
#to ensure it's secure).
# LED ####
{
cat("\014")
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
library(dplyr)
library(tibble)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.- Importación de datos: parsing y formatos de variables (continuas, categóricas, 
# fechas, etc.)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Import Data ####
{
df = data.frame()
for (i in 1:6) {
  filecsv = paste0("dat/JC-20190", i,"-citibike-tripdata.csv")
  df1= read.csv(filecsv,fileEncoding = "UTF-8")
  df = rbind(df,df1)
}
summary(df)
str(df)
}
# Clean/Prepare Data ####
# Cambio de formatos de variables
{
# Campos tipo fecha.
df$starttime = as.POSIXct(df$starttime, format="%Y-%m-%d %H:%M:%OS")
head(format(df$starttime, "%Y-%m-%d %H:%M:%OS4"))
df$stoptime = as.POSIXct(df$stoptime, format="%Y-%m-%d %H:%M:%OS")
head(format(df$stoptime, "%Y-%m-%d %H:%M:%OS4"))


# Campo gender. Lo factorizamos y cambiamos los levels.
df$gender = as.factor(df$gender)
levels(df$gender)
df$gender = factor(df$gender, levels = c("0", "1", "2", "unknown", "male", "female"))
df$gender[df$gender == "0"] = "unknown"
df$gender[df$gender == "1"] = "male"
df$gender[df$gender == "2"] = "female"
table(df$gender)
# df$gender = factor(df$gender, levels = c("unknown", "male", "female"))
df$gender = droplevels(df$gender)
str(df)
summary(df)
}
# Las estaciones: los valores de id, name y long-lat siempre son los mismos para las
# estaciones. Eliminamos las columnas redundantes y creamos una tabla de estaciones aparte.
{
dfStationsStart = unique(df[, c(4,5,6,7)])
dfStationsEnd = unique(df[, c(8,9,10,11)])
colnames(dfStationsStart) = c("station.id", "station.name", "station.latitude", "station.longitude")
colnames(dfStationsEnd) = c("station.id", "station.name", "station.latitude", "station.longitude")

dfStations = right_join(dfStationsStart, dfStationsEnd, by = "station.id")
# dfStations tiene 88 estaciones cuando el máximo debia ser 85
df %>% count(df$start.station.id)
df %>% count(df$end.station.id)


# Comprobamos duplicidades y vemos que hay una estación con dos pares de coordenadas diferentes
# Se trata de la estación Sip Ave
dfStationsFactor = as.data.frame(as.factor(table(dfStations$station.name.x)))
colnames(dfStationsFactor)  = "Repeticiones"
dfStationsFactor = rownames_to_column(dfStationsFactor)
dfStationsFactor = arrange(dfStationsFactor,desc(Repeticiones))

# Comprobamos cuales son las coordenadas más utilizadas para desechar las que lo son menos.
dfSipAveStart = df[df$start.station.name == "Sip Ave",]
table(dfSipAveStart$start.station.latitude)
table(dfSipAveStart$start.station.longitude)

dfSipAveEnd = df[df$end.station.name == "Sip Ave",]
table(dfSipAveEnd$end.station.latitude)
table(dfSipAveEnd$end.station.longitude)
#Las coordenadas que más se repiten son
# Longitud: -74.0639126300812
# Latitud: 40.7308970978618
# Cambiamos las otras coordenadas por estas.
dfStations$station.latitude.x[dfStations$station.id==3195] = 40.7308970978618
dfStations$station.latitude.y[dfStations$station.id==3195] = 40.7308970978618

dfStations$station.longitude.x[dfStations$station.id==3195] = -74.0639126300812
dfStations$station.longitude.y[dfStations$station.id==3195] = -74.0639126300812

dfStations = unique(dfStations) # Eliminamos las repetidas
dfStations = subset(dfStations, select= c(-2,-3,-4)) # Eliminamos las columnas redundantes
colnames(dfStations) = c("station.id", "station.name", "station.latitude", "station.longitude")

write.csv(dfStations, "dat/stations.csv")

rm(dfSipAveStart)
rm(dfSipAveEnd)
rm(dfStationsStart)
rm(dfStationsEnd)
rm(dfStationsFactor)

# Ahora elimino las columnas redundantes de la tabla df
str(df)
df = df[,c(-5,-6,-7,-9,-10,-11)]
str(df)
summary(df)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.- Estadísticos básicos para identificación de valores faltantes.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Missing Values ####
{
# Utilizamos función de R Bloggers para calcular el PORCENTAJE de valores
# perdidos de cada variable
pMissed = function(x) {sum(is.na(x))/length(x)*100}
apply(X = df, MARGIN = 2, FUN = pMissed)

# Son pocos casos en las variables start.time y stop.time. Las eliminamos del df
df = df[complete.cases(df$starttime), ]

apply(X = df, MARGIN = 2, FUN = pMissed) # Desaparecen todos los NAs
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.- Identificación de valores extremos 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Tratamiento de valores extremos - Outliers 
# Vemos la distribución de las variables numéricas
# Outliers ####
{
par(las=2,mfrow=c(1,2))
boxplot(df$tripduration, xlab = "Trip duration")
boxplot(df$birth.year, xlab = "Birth year")

par(las=2, mfrow=c(1,1))
library(ggplot2)
plot(prop.table(table(df$birth.year)))
g = ggplot(df,aes(df$birth.year))
g+geom_density()
}
{
outliers_age = boxplot.stats(df$birth.year)$out
boxplot(df$birth.year,boxwex=0.1)

x = df$birth.year
qnt = quantile(x, probs = c(.25, .75), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = NA
x[x > (qnt[2] + H)] = NA

plot(prop.table(table(x)))
g =ggplot(df,aes(x))
g+geom_density()

# Comprobamos eliminación de outliers
df$birth.year %>% summary()
x %>% summary()

df$birth.year = x

}
{
# Outliers de tripduration.
df11 = df

plot(prop.table(table(df11$tripduration)))
g = ggplot(df,aes(df11$tripduration))
g+geom_density()

outliers_age = boxplot.stats(df11$tripduration)$out
boxplot(df11$tripduration,boxwex=0.1)
# creamos un campo nuevo difftime como diferencia entre stoptime y starttime para
# ver si sigue la misma distribucón que tripduration
for (i in 1:nrow(df11)) {
      df11$difftime[i] = as.numeric(difftime(df11$stoptime[i], df11$starttime[i], units = "secs"))
  }
}
{
summary(df11$tripduration)
summary(df11$difftime)

# Son prácticamente idénticos luego no podemos usar la diferencia entre stop y start para
# imputar los outliers

x = df11$tripduration
qnt = quantile(x, probs = c(.25, .75), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = NA
x[x > (qnt[2] + H)] = NA

plot(prop.table(table(x)))
g = ggplot(df,aes(x))
g+geom_density()

df11$tripduration %>% summary()
x %>% summary()

df$tripduration = x

summary(df)

rm(df11)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.- Imputación de valores faltantes.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Imputación de NA's ####
{
library(VIM)
par(mfrow=c(1,1))
aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=FALSE, 
                  labels=names(df), cex.axis=.7, gap=3,
                  ylab=c("Histograma de valores perdidos","Patron"))
}

{
# +++++ IMPUTACIÓN MULTIPLE (con paquete "mice") +++++++++++++++++++++++++++++++
# Vamos a utilizar el método de Predictive mean matching (PMM)
# 10 iteraciones y 5 imputaciones múltiples
# Solo pueden ser varibles numéricas
str(df)
temp = mice::mice(df[,-c(2,3,7,9)],
            m = 5, maxit = 10,
            method = "pmm", seed = 1234)
summary(temp) # Resumen del proceso

# Eligimos uno de los "m" (5) conjuntos generados:
df41 = mice::complete(temp, 1) # Por ejemplo, el 1. O bien 2, 3, 4 o 5 (a elegir)

# Comparar el data frame original con el elegido imputado:
summary(df)
summary(df41) # Ya no hay NAs, los hemos imputado
}
{
par(mfrow = c(2,1))
plot(y = df$tripduration, x = df$birth.year,
     main = "Trip duration(dataset con NA)",
     xlab = "Año", ylab = "secs", col = "red"); grid()
plot(y = df41$tripduration, x = df$birth.year,
     main = "Trip duration(dataset sin NA)",
     xlab = "Año", ylab = "secs", col = "green"); grid()

df$tripduration <- df41$tripduration
df$start.station.id <- df41$start.station.id
df$end.station.id <- df41$end.station.id
df$bikeid <- df41$bikeid
df$birth.year <- df41$birth.year


readr::write_csv(df,"dat/bike_nyc_1_6_2019.csv")
}

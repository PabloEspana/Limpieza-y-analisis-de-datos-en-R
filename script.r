
# Cargamos los paquetes R que vamos a usar
library(ggplot2)
library(dplyr)

# Cargamos el juego de datos
datosAdult <- read.csv('adult.data', stringsAsFactors = FALSE, header = FALSE)

# Nombres de los atributos
names(datosAdult) <- c("age","workclass","fnlwgt","education",
     "educationNum","maritalStatus","occupation", "relationship","race","sex",
    "capitalGain","capitalLoss","hourPerWeek","nativeCountry","income")

# Verificamos la estructura del juego de datos
str(datosAdult)

# Estadísticas de valores vacíos
colSums(is.na(datosAdult))

colSums(datosAdult=="")

# Tomamos valor "United-States" para los valores vacíos de la variable "nativeCountry"
datosAdult$'nativeCountry'[datosAdult$'nativeCountry'==""]=" United-States"

# Tomamos la media para valores vacíos de la variable "hour-per-week"
datosAdult$'hourPerWeek'[is.na(datosAdult$'hourPerWeek')] <- mean(datosAdult$'hourPerWeek', na.rm=T)

# Tomamos valor "Female" para valor vacío de la variable "sex"
datosAdult$sex[datosAdult$sex==""]=" Female"

# Tomamos valor ">50K" para valor vacío de la variable "income"
datosAdult$income[datosAdult$income==""]=" >50K"

# Discretizamos cuando tiene sentido y en función de cada variable para ello comprobamos
# para qué variables tendría sentido un proceso de discretización

apply(datosAdult, 2, function(x) length(unique(x)))

# Discretizamos las variables con pocas clases
cols<-c("race", "sex", "income")
for (i in cols){
  datosAdult[,i] <- as.factor(datosAdult[,i])
}

# Después de los cambios, analizamos la nueva estructura del juego de datos
str(datosAdult)

# Visualizamos la relación entre las variables "sex" y "race":
ggplot(data=datosAdult[],aes(x=sex,fill=race))+geom_bar()+ylab("Frecuencia")

# Visualizamos la relación entre las variables "sex" y "maritalStatus":
ggplot(data=datosAdult[],aes(x=sex,fill=maritalStatus))+geom_bar()+geom_bar(position="fill")+ylab("Frecuencia")

# Matrices de porcentages de frecuencia. 
# Podemos ver que la probabilidad de estar divorciado siendo del sexo masculino es de 8.20 %

t<-table(datosAdult[]$sex,datosAdult[]$maritalStatus)
for (i in 1:dim(t)[1]){
    t[i,]<-t[i,]/sum(t[i,])*100
}
t

# Visualizamos la relación entre las variables "sex" y "education":
ggplot(data=datosAdult[],aes(x=sex,fill=education))+geom_bar()+geom_bar(position="fill")+ylab("Frecuencia")

# Trabajando con 3 variables en un mismo gráfico de frecuencias. Agregaos la variable "nativeCountry"
ggplot(data = 
       datosAdult[],aes(x=sex,fill=education)
      )+geom_bar(position="fill")+facet_wrap(~nativeCountry)+ylab("Frecuencia")



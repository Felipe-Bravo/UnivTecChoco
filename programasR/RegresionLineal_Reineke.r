# Felipe Bravo (Univesidad de Valladolid), fbravo [at] pvs.uva.es
# Universidad Técnica del Chocó, 2020
# Regresión lineal

# En este ejemplo aprenderemos a
  # dibujar una línea de Reineke
  # añadir líneas de autoaclareo, de ocupación total del rodal y
  # de inicio de la competencia
  # añadir una malla y texto dentro del gráfico
  # ajustar la línea de Reineke para un conjunto de datos de una plantación de 
  # Pinus halepensis en España 

###############

# Establecemos nuestro directorio de trabajo
#setwd('C:/Tu_directorio_de_Trabajo_paraR')

setwd('C:/datosR')

# Definir los variables básicas y la línea de autoaclareo (línea de Reineke)


Dg<-c(1:60)
Nmax <-(1:2500)
Nmax <-exp(11.9358)*Dg^(-1.605) # Linea de densidad máxima según Hernéndez y Arrechea, 2010
N60<-0.60*Nmax #límite inferior de autoaclareo (Long, 1985)
N35<-0.35*Nmax #límite inferior de la ocupación completa de la estación (Long, 1985)
N25<-0.25*Nmax #Inicio de la competencia (cierre del dosel arbóreo) (Long, 1985)

# Dibujamos la línea de máxima densidad
plot (Dg, Nmax, log=("xy"), type="l", lwd=2, col="black"
      ,ylab="Trees per ha"
      ,xlab="Quadratic mean diameter (cm)"
      ,main="P. halepensis (Aragón, Spain)"
      ,ylim= c(100,5000)
      ,xlim= c (5, 50)
)
lines(Dg,Nmax,type="l",lwd=2, col="black")
lines(Dg,N60, type="l",lty=2, col="black")
lines(Dg,N35, type="l", lty=2, col="black")
lines (Dg,N25, type="l", lty=2, col="black")

# Ahora vamos a añadir una malla al gráfico anterior

# líneas verticales
abline(v=5, lty=4, col="grey")
abline(v=10, lty=4, col="grey")
abline(v=15, lty=4, col="grey")
abline(v=20, lty=4, col="grey")
abline(v=25, lty=4, col="grey")
abline(v=30, lty=4, col="grey")
abline(v=35, lty=4, col="grey")
abline(v=40, lty=4, col="grey")
abline(v=45, lty=4, col="grey")

# líneas horizontales
abline(h=200, lty=4, col="grey")
abline(h=500, lty=4, col="grey")
abline(h=1000, lty=4, col="grey")
abline(h=1500, lty=4, col="grey")
abline(h=2000, lty=4, col="grey")

# Ahora añadimos un texto explicativo
# los dos primeros dígitos de cada línea
# indican la posición del texto

text(15,4000,"SDImax", col="red")
text(25,400, "35% SDImax", col="green")
text(20,1000,"60% SDImax", col="blue")
text(35, 100, "25% SDImax", clo="black")

##################################

# Ahora vamos a ajustar una línea de Reineke
# usaremos datos del Inventario Forestal Nacional (IFN) de España
# los datos están en este archivo Pnig_34_en_noSDI.csv
# el IFN de España se puede consultar en esta dirección: https://forestexplorer.gsic.uva.es

# importamos los datos (deben estar en el directorio de trabajo)
newdata <- read.csv2("Pnig_34_eng_noSDI.csv")
# comprobamos la estructura de los datos
names(newdata)
head(newdata)
tail(newdata)
View (newdata)

# calculamos ahora el SDI (índice de densidad del rodal) y su valor máximo
newdata$SDI <- newdata$N*(25/newdata$DG)**(-1.605)
names(newdata)
max(newdata$SDI)

# Creamos el archivo newdata2 donde solo estarán las observaciones
# con un valor de SDI que sea el 80% o más del límite superior
newdata2 <- subset(newdata, SDI >= 0.8*889.8126)

# creamos ahora nuevas variables mediante transformación logarítmica
# en el archivo newdata2
newdata2$LOGN <- log(newdata2$N)
newdata2$LOGDG <- log(newdata2$DG)

# comprobamos si las variables están creadas
names(newdata2)

# dibujamos las variables transformadas

plot(newdata2$LOGN, newdata2$LOGDG)

# ajustamos la línea de Reineke mediante regresión lineal
modelo <- lm(newdata2$LOGN~ newdata2$LOGDG)

names(modelo) #permite ver la información almacenada en el objeto modelo
summary(modelo) #muestra los principales parámetros de modelo

confint(modelo, level=0.95) #muestra el intervalo de confianza de los parámetros con una probabilidad del 95%

# calculamos ahora el intervalo de confianza (para el valor del parámetro)
predict (modelo, data.frame(LOGDG), interval="confidence") 

# calculamos ahora el intervalo de predicción (para el valor de la predicción)
predict (modelo, data.frame(LOGDG), interval="prediction") 


# representamos el modelo ajustado sobre las observaciones que lo han generado

attach(newdata2)
plot(x = LOGDG, y = LOGN, main = "LogN vs LogDg", pch = 20, col = "grey30")
abline(modelo, lwd = 3, col = "red")

# Estudiamos ahora los resíduos del modelo
par(mfrow = c(1,2))
plot(modelo)

# para identificar las observaciones influyentes
# dibujamos los residuos estundentizados y valores predichos

par(mfrow = c(1,1))

plot(x = modelo$fitted.values, y = abs(rstudent(modelo)),
     main = "Residuos estudentizados (valores absolutos)vs valores predichos", 
     pch = 20,      col = "grey30")
abline(h = 1.44, col = "red") # valores de los residuos estudentizados por encima del
# cuadrado de 1,2 (ver figura 3: raíz de residuos estandarizados)indican observaciones influyentes


plot(hatvalues(modelo), main = "Medición de leverage", pch = 20)


# Ajustemos ahora un modelo lineal múltiple

# AB ~  SDI + H0 + D0

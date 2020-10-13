# Felipe Bravo (Univesidad de Valladolid), fbravo [at] pvs.uva.es
# Universidad T�cnica del Choc�, 2020
# Regresi�n lineal

# En este ejemplo aprenderemos a
  # dibujar una l�nea de Reineke
  # a�adir l�neas de autoaclareo, de ocupaci�n total del rodal y
  # de inicio de la competencia
  # a�adir una malla y texto dentro del gr�fico
  # ajustar la l�nea de Reineke para un conjunto de datos de una plantaci�n de 
  # Pinus halepensis en Espa�a 

###############

# Establecemos nuestro directorio de trabajo
#setwd('C:/Tu_directorio_de_Trabajo_paraR')

setwd('C:/datosR')

# Definir los variables b�sicas y la l�nea de autoaclareo (l�nea de Reineke)


Dg<-c(1:60)
Nmax <-(1:2500)
Nmax <-exp(11.9358)*Dg^(-1.605) # Linea de densidad m�xima seg�n Hern�ndez y Arrechea, 2010
N60<-0.60*Nmax #l�mite inferior de autoaclareo (Long, 1985)
N35<-0.35*Nmax #l�mite inferior de la ocupaci�n completa de la estaci�n (Long, 1985)
N25<-0.25*Nmax #Inicio de la competencia (cierre del dosel arb�reo) (Long, 1985)

# Dibujamos la l�nea de m�xima densidad
plot (Dg, Nmax, log=("xy"), type="l", lwd=2, col="black"
      ,ylab="Trees per ha"
      ,xlab="Quadratic mean diameter (cm)"
      ,main="P. halepensis (Arag�n, Spain)"
      ,ylim= c(100,5000)
      ,xlim= c (5, 50)
)
lines(Dg,Nmax,type="l",lwd=2, col="black")
lines(Dg,N60, type="l",lty=2, col="black")
lines(Dg,N35, type="l", lty=2, col="black")
lines (Dg,N25, type="l", lty=2, col="black")

# Ahora vamos a a�adir una malla al gr�fico anterior

# l�neas verticales
abline(v=5, lty=4, col="grey")
abline(v=10, lty=4, col="grey")
abline(v=15, lty=4, col="grey")
abline(v=20, lty=4, col="grey")
abline(v=25, lty=4, col="grey")
abline(v=30, lty=4, col="grey")
abline(v=35, lty=4, col="grey")
abline(v=40, lty=4, col="grey")
abline(v=45, lty=4, col="grey")

# l�neas horizontales
abline(h=200, lty=4, col="grey")
abline(h=500, lty=4, col="grey")
abline(h=1000, lty=4, col="grey")
abline(h=1500, lty=4, col="grey")
abline(h=2000, lty=4, col="grey")

# Ahora a�adimos un texto explicativo
# los dos primeros d�gitos de cada l�nea
# indican la posici�n del texto

text(15,4000,"SDImax", col="red")
text(25,400, "35% SDImax", col="green")
text(20,1000,"60% SDImax", col="blue")
text(35, 100, "25% SDImax", clo="black")

##################################

# Ahora vamos a ajustar una l�nea de Reineke
# usaremos datos del Inventario Forestal Nacional (IFN) de Espa�a
# los datos est�n en este archivo Pnig_34_en_noSDI.csv
# el IFN de Espa�a se puede consultar en esta direcci�n: https://forestexplorer.gsic.uva.es

# importamos los datos (deben estar en el directorio de trabajo)
newdata <- read.csv2("Pnig_34_eng_noSDI.csv")
# comprobamos la estructura de los datos
names(newdata)
head(newdata)
tail(newdata)
View (newdata)

# calculamos ahora el SDI (�ndice de densidad del rodal) y su valor m�ximo
newdata$SDI <- newdata$N*(25/newdata$DG)**(-1.605)
names(newdata)
max(newdata$SDI)

# Creamos el archivo newdata2 donde solo estar�n las observaciones
# con un valor de SDI que sea el 80% o m�s del l�mite superior
newdata2 <- subset(newdata, SDI >= 0.8*889.8126)

# creamos ahora nuevas variables mediante transformaci�n logar�tmica
# en el archivo newdata2
newdata2$LOGN <- log(newdata2$N)
newdata2$LOGDG <- log(newdata2$DG)

# comprobamos si las variables est�n creadas
names(newdata2)

# dibujamos las variables transformadas

plot(newdata2$LOGN, newdata2$LOGDG)

# ajustamos la l�nea de Reineke mediante regresi�n lineal
modelo <- lm(newdata2$LOGN~ newdata2$LOGDG)

names(modelo) #permite ver la informaci�n almacenada en el objeto modelo
summary(modelo) #muestra los principales par�metros de modelo

confint(modelo, level=0.95) #muestra el intervalo de confianza de los par�metros con una probabilidad del 95%

# calculamos ahora el intervalo de confianza (para el valor del par�metro)
predict (modelo, data.frame(LOGDG), interval="confidence") 

# calculamos ahora el intervalo de predicci�n (para el valor de la predicci�n)
predict (modelo, data.frame(LOGDG), interval="prediction") 


# representamos el modelo ajustado sobre las observaciones que lo han generado

attach(newdata2)
plot(x = LOGDG, y = LOGN, main = "LogN vs LogDg", pch = 20, col = "grey30")
abline(modelo, lwd = 3, col = "red")

# Estudiamos ahora los res�duos del modelo
par(mfrow = c(1,2))
plot(modelo)

# para identificar las observaciones influyentes
# dibujamos los residuos estundentizados y valores predichos

par(mfrow = c(1,1))

plot(x = modelo$fitted.values, y = abs(rstudent(modelo)),
     main = "Residuos estudentizados (valores absolutos)vs valores predichos", 
     pch = 20,      col = "grey30")
abline(h = 1.44, col = "red") # valores de los residuos estudentizados por encima del
# cuadrado de 1,2 (ver figura 3: ra�z de residuos estandarizados)indican observaciones influyentes


plot(hatvalues(modelo), main = "Medici�n de leverage", pch = 20)


# Ajustemos ahora un modelo lineal m�ltiple

# AB ~  SDI + H0 + D0

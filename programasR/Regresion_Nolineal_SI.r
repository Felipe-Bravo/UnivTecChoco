# Felipe Bravo (Univesidad de Valladolid), fbravo [at] pvs.uva.es
# Universidad Técnica del Chocó, 2020
# Regresión no lineal

# En este ejemplo aprenderemos a
  # Ajustar un modelo anármofico de curvas de calidad (modelo de Hossfel I)
  # Dibujar la curva guía y añadir las líneas de las curvas de calidad

###############

# Establecemos nuestro directorio de trabajo
#setwd('C:/Tu_directorio_de_Trabajo_paraR')

setwd('C:/datosR')

# Importamos los datos

DATA.SI<- read.csv2("DATA4_SiteIndex.csv", header=TRUE, sep=";", dec=".")

head(DATA.SI, 10)

# Dejamos solo los datos que provienen de análisis de tronco. Borramos los datos 
# de parcelas temprales (H0_Age)

library(lattice)
DATA.SI <-subset(DATA.SI, type !="H0_Age")

# Exploramos los datos para ver que los datos de parcelas temporales
# han sido eliminados

head(DATA.SI, 10)

# Gráfico básico de los datos

# Creamos y exportamos el gráfico de altura dominante frente a edad
# se exporta en formato png y se indica el tamaño (ancho y altura)

png('stemanalysis.png', width = 683, height = 495)

xyplot(Height~Age,data=DATA.SI,groups=Plot,type="l", col="grey", 
       main="Pinus sylvestris 
       Alto Valle del Ebro, España", ylab="H0 (m)", xlab="Edad (años)",)

dev.off()


# Ajuste no lineal del modelo de Hossfeld I

# Definimos el modelo de Hossfeld I 
Hossfeld <- Height~Age^2/(a+b*Age)^2

# Ajustamos el modelo que acabamos de definir
# Los valores semilla o de comienzo es obtienen de un ajuste lineal previo
# o de la literatura

nlmod <- nls(Hossfeld, data=DATA.SI, start=list(a=5.7319, b=0.17166)) 

# Obtenemos los resultados del ajuste no lineal
summary(nlmod)

# calculamos el promedio de los residuos
mean(residuals(nlmod), na.rm=TRUE)

# análisis gráfico de los residuos
par(mfrow=c(1,3), mar=c(4,4,2,1), las=1)
plot(fitted(nlmod), residuals(nlmod, type="pearson"),
     xlab = "Valores predichos", ylab = "Residuos estandarizados")
abline(h=0, col="red")
qqnorm(residuals(nlmod, type="pearson"))
qqline(residuals(nlmod, type="pearson"))
plot(fitted(nlmod), DATA.SI$Height, xlab="Valores predichos", ylab = "Valores observados")
abline(0, 1, col="red")

# Dibujamos ahora la curva guía (el modelo  no lineal que acabamos de ajustar)
par(mfrow=c(1,1))
X<-c(1:140)
Y<-c(X^2/(4.842670+0.178942*X)^2)
plot (X,Y,lwd=1, type="l", col = "red", lty=1, ljoin=10 
      , main="Curva guía | Hossfeld I"
      , ylab="Altura dominante (m)", xlab = "Edad (años)")

# Tras calcular los parámetros para cada una de las calidades de estación
# SI igual a 14, 17, 20, 23 y 26 metros
# Dibujamos las curvas de calidad - Hosffeld I (manteniendo b constante)

# definimos el eje X

X<-c(1:120)

# defimos las 5 curvas de calidad


Y<-c(X^2/(8.83192419+0.178942*X)^2)# SI=14
K<-c(X^2/(6.3593625+0.178942*X)^2)# SI=17
Z<-c(X^2/(4.46647977+0.178942*X)^2)# SI=20
G<-c(X^2/(2.95724141+0.178942*X)^2)# SI=23
H<-c(X^2/(1.71741351+0.178942*X)^2)# SI=26

# Dibujamos las curvas de calidad (la más alta primero)
plot (X,H,lwd=1, type="l", col = "red", lty=1, ljoin=10, 
      main="Curvas de calidad, 
      Hossfeld I, b común",
      ylab="Altura dominante (m)", xlab = "Edad (años)")
# añadimos las otras calidades
lines(X,K,lwd=1, col = "blue")
lines(X,Z,lwd=1, col = "green")
lines(X,G,lwd=1, col = "black")
lines(X,Y,lwd=1, col = "yellow")


# Finalmente vamos a hacer un gráficon conjunto de los análisis de troncos
# con las curvas de calidad


xyplot(Height~Age,data=DATA.SI,groups=Plot,type="l", col="grey",
       main="Análisis de tronco (Ps), Alto Valle del Ebro (Spain)
       Hossfeld I b común", ylab="Altura dominante (m)", xlab="Edad (años)",
       panel=function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.lines(X, K, col=1, lwd=2)
         panel.lines(X, Z, col=1, lwd=2)
         panel.lines(X, G, col=1, lwd=2)
         panel.lines(X, Y, col=1, lwd=2)
         panel.lines(X, H, col=1, lwd=2)})









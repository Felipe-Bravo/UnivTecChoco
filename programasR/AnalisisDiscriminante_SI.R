# Felipe Bravo (Univesidad de Valladolid), fbravo [at] pvs.uva.es
# Universidad T�cnica del Choc�, 2020
# An�lisis discriminante

# En este ejemplo aprenderemos a
  # Desarrollar un an�lisis discriminante para clasificar estaciones forestales


###############

# Establecemos nuestro directorio de trabajo
#setwd('C:/Tu_directorio_de_Trabajo_paraR')

setwd('C:/datosR')

# Importamos los datos
suelo <- read.table("DATOS2.txt", header=TRUE, sep="\t", dec=".") 

head(suelo)

Realizar el an�lisis discriminante

install.packages(MASS)
library(MASS)

disc <- lda(SI4 ~ ., data=suelo)
# An�lisis discriminante lineal

Disc
# muestra el resultado

plot(disc)
# dibuja la dispersi�n de datos frente a las funciones lineales #definidas (en este caso 3)

clas <- predict(disc,suelo[,-1])
plot(clas$x,pch=as.character(suelo[,1]),col=as.numeric(clas$class)+1)

table(suelo[,1],clas$class)

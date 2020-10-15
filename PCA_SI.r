# Felipe Bravo (Univesidad de Valladolid), fbravo [at] pvs.uva.es
# Universidad Técnica del Chocó, 2020
# PCA - Análisis de Componentes Principales

# En este ejemplo aprenderemos a
  # Reducir la dimensionalidad de una base de datos mediante PCA


###############

# Establecemos nuestro directorio de trabajo
#setwd('C:/Tu_directorio_de_Trabajo_paraR')

setwd('C:/datosR')

# Importamos los datos
suelo <- read.table("DATOS2.txt", header=TRUE, sep="\t", dec=".") 

head(suelo)


# realizamos el pca

pc <- princomp(suelo[,-1],cor=TRUE)

plot (pc, type="l") #gráfico con la varianza que recoge cada componente

pc$loadings # peso de cada componente principal
pc$scores # valor de cada componente principal para cada observación

biplot(pc)#gráfico de componentes principales con los pesos de cada variable

plot(pc$scores[,1],pc$scores[,2])#Dibuja las observaciones sobre los dos primeros ejes
text(pc$scores[,1],pc$scores[,2],suelo[,1],pos=4) # Añade la etiqueta del Índice de sitio

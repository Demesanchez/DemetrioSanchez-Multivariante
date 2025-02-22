# DENDOGRAMA

#Paqueter�as nesesar�as 

library(cluster.datasets)
library(tinytex)

# Base de datos 

data("all.mammals.milk.1956")
AMM=all.mammals.milk.1956

#Usaremos el data set de **"all.mammals.milk.1956"**, el cual contiene datos sobre la leche de diferentes especies de animales.


## Revisi�n de la base de datos  
#Dimensi�n
dim(AMM)

#Esta base cuenta con 25 observaciones y 6 variables 

#Datos faltantes 
AnyNA(AMM)

#La b�squeda sale negativa as� que proseguimos con el Dendograma 

#Tipo de variables 

str(AMM)

#Encontramos que la base esta conformada por 5 variables num�ricas y una car�cter donde se encuentra registrado el nombre de los animales, en las num�ricas teneos la cantidad de prote�na, nivel de agua, grasa, lactosa, los minerales de la lech

# C�lculo de la matriz de distancias de Mahalonobis
dist.AMM<-dist(AMM[,2:6])

#Calculamos la distancia de Mahalanobis para las variables que comprende de  la dos a la seis, variables num�ricas. 

#Con la distancia de Mahalanobis podemos calcular la similitud que existe entre las variables teniendo en cuenta la correlaci�n que hay entre ellas. 

## Redondeo   


round(as.matrix(dist.AMM)[1:6, 1:6],3)

#Realizamos un redondeo de los c�lculos de la distancia de Mahalanobis y los convertimos a una matriz, proyectamos e indicamos que solo usaremos a los primeros 6 individuos as� que especificamos la selecci�n de las 6 filas y 6 columnas pertenecientes a dichos individuos.  


# Calculo del dendrograma

dend.AMM<-as.dendrogram(hclust(dist.AMM))

#Se calcula el Dendograma para nuestras observaciones elegidas donde  usaremos el m�todo de agrupaci�n por Cl�sters **"hclust"**, el cual nos ofrece una agrupaci�n jer�rquica.

# Graficaci�n  del dendrograma

#Creamos un vector para las etiquetas que le asignaremos al Dendograma para el cual necesitaremos la librer�a "dendextend"
library(dendextend)

L=labels(dend.AMM)

labels(dend.AMM)=AMM$name[L]

#Graficamos el Dendograma cambiamos el tama�o de las etiquetas y aplicamos color a las etiquetas para que resalten.

dend.AMM %>%
  set(what="labels_col", "blue") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de mam�feros")
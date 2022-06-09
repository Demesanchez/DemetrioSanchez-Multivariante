# DENDOGRAMA

#Paqueterías nesesarías 

library(cluster.datasets)
library(tinytex)

# Base de datos 

data("all.mammals.milk.1956")
AMM=all.mammals.milk.1956

#Usaremos el data set de **"all.mammals.milk.1956"**, el cual contiene datos sobre la leche de diferentes especies de animales.


## Revisión de la base de datos  
#Dimensión
dim(AMM)

#Esta base cuenta con 25 observaciones y 6 variables 

#Datos faltantes 
AnyNA(AMM)

#La búsqueda sale negativa así que proseguimos con el Dendograma 

#Tipo de variables 

str(AMM)

#Encontramos que la base esta conformada por 5 variables numéricas y una carácter donde se encuentra registrado el nombre de los animales, en las numéricas teneos la cantidad de proteína, nivel de agua, grasa, lactosa, los minerales de la lech

# Cálculo de la matriz de distancias de Mahalonobis
dist.AMM<-dist(AMM[,2:6])

#Calculamos la distancia de Mahalanobis para las variables que comprende de  la dos a la seis, variables numéricas. 

#Con la distancia de Mahalanobis podemos calcular la similitud que existe entre las variables teniendo en cuenta la correlación que hay entre ellas. 

## Redondeo   


round(as.matrix(dist.AMM)[1:6, 1:6],3)

#Realizamos un redondeo de los cálculos de la distancia de Mahalanobis y los convertimos a una matriz, proyectamos e indicamos que solo usaremos a los primeros 6 individuos así que especificamos la selección de las 6 filas y 6 columnas pertenecientes a dichos individuos.  


# Calculo del dendrograma

dend.AMM<-as.dendrogram(hclust(dist.AMM))

#Se calcula el Dendograma para nuestras observaciones elegidas donde  usaremos el método de agrupación por Clústers **"hclust"**, el cual nos ofrece una agrupación jerárquica.

# Graficación  del dendrograma

#Creamos un vector para las etiquetas que le asignaremos al Dendograma para el cual necesitaremos la librería "dendextend"
library(dendextend)

L=labels(dend.AMM)

labels(dend.AMM)=AMM$name[L]

#Graficamos el Dendograma cambiamos el tamaño de las etiquetas y aplicamos color a las etiquetas para que resalten.

dend.AMM %>%
  set(what="labels_col", "blue") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de mamíferos")
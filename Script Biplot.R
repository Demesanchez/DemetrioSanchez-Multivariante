# BIPLOT


#Se cargan las librer�as necesesarias: 
 
library(MultBiplotR)
library(readxl)
library(knitr)


#Se carga la matriz de datos:
  
  vinos <- read_excel("C:/Users/Demetrio Sanchez/Downloads/vinos.xls")
BD<-as.data.frame(vinos)[,-1]



## Exploracion de matriz


#Dimensi�n de la matriz.
dim(BD)



colnames(BD)



str(BD)



BD$denomina = as.factor(BD$denomina)
BD$grupo = as.factor(BD$grupo)


## Gr�ficos de exploraci�n


BX1<-BoxPlotPanel(BD[,4:9], nrows=2, groups=BD$denomina)
BX1



BX2<-BoxPlotPanel(BD[,4:9], nrows=2, groups=BD$grupo)
BX2


##  Filtrado de variables

### 1.- Selecci�n de variables num�ricas


X<-BD[,4:21]


### 2.- Generaci�n del scatter plot


PL1<-plot(X[,1:9])



PL2<-plot(X[,10:18])


##   Reducci�n de la dimensionalidad

### 1.- ACP

acpvino<-PCA.Analysis(X,Scaling = 5)
summary(acpvino)



### 2.- Contenido del objeto acpvino


names(acpvino)


### 3.- Generaci�n del gr�fico sin caja


acp1<-plot(acpvino, ShowBox=FALSE)


### Screeplot con barras 


acp2<-princomp(X, cor=TRUE, score=TRUE)
plot(acp2)


### Gr�fico circular de correlaci�n


acp3<-plot(acpvino, CorrelationCircle=TRUE, 
           ShowAxis=TRUE,  CexInd=1.5)


### Agregar grupos al biplot definido por usuario


acpvino1<-AddCluster2Biplot(acpvino, ClusterType="us", 
                            Groups = BD$grupo)


### Grafico con poligonos

#CexInd= tama�o de los argumentos


acp4<-plot(acpvino1, PlotClus=TRUE, 
           ClustCenters=TRUE, margin=0.05, 
           CexInd=0.7, ShowBox=TRUE)


### Gr�fico con elipses


acp5<-plot(acpvino1, PlotClus=TRUE, ClustCenters=TRUE, 
           margin=0.05, CexInd=0.7, TypeClus="el", 
           ShowBox=F)


### Gr�fico con estrellas


acp6<-plot(acpvino1, PlotClus=TRUE, ClustCenters=TRUE, 
           margin=0.05, CexInd=0.7, TypeClus="st", 
           ShowBox=TRUE)


##  Aplicacion del Biplot


#Predeterminado JK


bipvino<-PCA.Biplot(X, Scaling = 5)
summary(bipvino)


### Valores propios


bipvino$EigenValues


### Screeplot


SC<-barplot(bipvino$EigenValues)


### Vectores propios


bipvino$EV


### Tabla de inercias


Inercias<-data.frame(paste("Eje",1:length(bipvino$EigenValues)),
                     bipvino$EigenValues, bipvino$Inertia, 
                     bipvino$CumInertia)
colnames(Inercias)<-c("Eje", "Valor Propio", 
                      "Inercia", "Inercia acumulada")
kable(Inercias)


### Tabla contribuci�n de columnas


kable(bipvino$ColContributions)

###  Gr�ficos Biplot


plot(bipvino, ShowBox=TRUE)


#  Prolongaci�n de vectores linea recta


BP1<-plot(bipvino, mode="s", 
          margin=0.1, ShowBox=TRUE)


#  Prolongaci�n de vectores con flechas y linea punteada


BP2<-plot(bipvino, mode="ah", margin=0.05, 
          ShowBox=TRUE)


### Gr�fico circular correlaciones 


GC<-CorrelationCircle(bipvino)


### Gr�fico contribuciones de los vectores

#  Calidad de representacion eje 1, 2 y 1+2


ColContributionPlot(bipvino, AddSigns2Labs = FALSE)


#  Proyecci�n individuos sobre una variable  donde dp= selecciona la variable


BP3<-plot(bipvino, dp=2, mode="s", 
          ColorVar=c("blue", rep("grey",17)),
          ShowBox=TRUE)


#  Proyecci�n de ind sobre todas las variables con *PredPoints= individuo*
  
  
  BP4<-plot(bipvino, PredPoints=1, mode="s", 
            ColorVar=1:18, ShowBox=TRUE)

# Cluster Jer�rquico con datos originales con el metodo *ward.D*
  
  bipvino=AddCluster2Biplot(bipvino, NGroups=4, 
                            ClusterType="hi", 
                            method="ward.D", 
                            Original=TRUE)


#Cluster aplicado al biplot


clusBP<-plot(bipvino, PlotClus=TRUE,ShowAxis=TRUE)
clusBP

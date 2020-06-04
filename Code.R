#PROYECTO FINAL DE INTELIGENCIA ARTIFICIAL
#JULIANA BALCERO TORRES & DIANA BARBOZA PRIMERA
#2020-10

#PAQUETES Y LIBRERÍAS
install.packages("ggplot2")
install.packages("dplyr")
install.packages("purrr")
install.packages("factoextra")
install.packages("DataExplorer")

library(dplyr)
library(ggplot2)
library(purrr)
library(cluster)
library(factoextra)
library(DataExplorer)

#LIMPIEZA DE DATOS
Data <- read.table("DataSet1.csv",
                   sep=",",
                   header = TRUE,
                   encoding = "UTF-8")

Data <- select(Data, 
               -Municipio.de.residencia,
               -Año.desmovilizacion,
               -Tipo.de.Desmovilizacion,
               -Ingreso.No.ingreso,
               -Maximo.Nivel.FpT.Reportado,
               -Linea.de.FpT.para.el.Max..Nivel,
               -Clasificacion.Componente.Especifico)

Data <- as.data.frame(Data)
Data$Año.de.Independizacion.Ingreso <- as.numeric(Data$Año.de.Independizacion.Ingreso)
Data <- Data %>% 
  filter(Año.de.Independizacion.Ingreso>2.015) %>% 
  filter(N..de.Hijos>=0) %>% 
  filter(Total.Integrantes.grupo.familiar>=0)

Data <- na.omit(Data)
Original_Data <- Data
Data <- dummify(Data,maxcat=33)

#ANÁLISIS EXPLORATORIO
summary(Data)
head(Data)

#DETERMINANDO EL NÚMERO DE CLÚSTER
#MÉTODO DEL CODO

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = Data, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

elbow_plot <- ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Elbow plot")

(elbow_plot)

#CLÚSTER

dist_variables <- dist(Data)

model1 <- kmeans(x = Data, centers = 4)

#Caracterización de los grupos

Original_DataClustered <- Original_Data %>% mutate(cluster=model1$cluster)

c1 <- Original_DataClustered %>% filter(cluster==1)
c2 <- Original_DataClustered %>% filter(cluster==2)
c3 <- Original_DataClustered %>% filter(cluster==3)
c4 <- Original_DataClustered %>% filter(cluster==4)

summary(c1)
summary(c2)
summary(c3)
summary(c4)

#Representar los desmovilizados en un espacio de dimensión reducido

extra <- as.matrix(dist_variables)

graph <- clusplot(extra,
         model1$cluster, 
         main = "2D Plot", 
         shade = T, 
         labels = 4,
         lines = 2)
(graph)


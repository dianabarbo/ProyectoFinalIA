#PROYECTO FINAL DE INTELIGENCIA ARTIFICIAL
#JULIANA BALCERO TORRES & DIANA BARBOZA PRIMERA
#2020-10

#PAQUETES Y LIBRERÍAS
install.packages("dplyr")
install.packages("purrr")
install.packages("factoextra")

library(dplyr)
library(ggplot2)
library(purrr)
library(cluster)
library(factoextra)

#LIMPIEZA DE DATOS
Data <- read.table("DataSet.csv",sep=",",header = TRUE)

#Cómo vamos a manejar las tildes?

#ANÁLISIS EXPLORATORIO
summary(Data)
head(Data)

#DETERMINANDO EL NÚMERO DE CLÚSTER
#MÉTODO DEL CODO

tot_withinss <- map_dbl(1:9, function(k){
  model <- kmeans(x = Data, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:9,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Elbow plot")

#CLÚSTER


#ANÁLISIS DE SALIDAS


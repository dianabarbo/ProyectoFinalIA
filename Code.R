#PROYECTO FINAL DE INTELIGENCIA ARTIFICIAL
#JULIANA BALCERO TORRES & DIANA BARBOZA PRIMERA
#2020-10

#PAQUETES Y LIBRERÍAS
install.packages("dplyr")
install.packages("purrr")
install.packages("factoextra")
install.packages("DataExplorer") #

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
                   encoding = "UTF-8",)
Data <- select(Data, 
               -Municipio.de.residencia,
               -Año.desmovilizacion,
               -Tipo.de.Desmovilizacion,
               -Ingreso.No.ingreso,
               -Maximo.Nivel.FpT.Reportado,
               -Linea.de.FpT.para.el.Max..Nivel)

Data <- as.data.frame(Data)
Data$Año.de.Independizacion.Ingreso <- as.numeric(Data$Año.de.Independizacion.Ingreso)
Data <- Data %>% filter(Año.de.Independizacion.Ingreso>2.015)
Data <- na.omit(Data)
Data <- dummify(Data,maxcat=33)

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

elbow_plot <- ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Elbow plot")

(elbow_plot)

#CLÚSTER

dist_variables <- dist(Data)

#------ Caracterización de los "posibles grupos"

model1 <- kmeans(x = Data, centers = 4)
model1$centers

cluster1 <- model1$cluster
cluster1

Original_DataClustered <- Data %>% 
  mutate (cluster = cluster1)
Original_DataClustered

#------ Determinar el patrón de cada grupo (individuo "representante" del mismo)

patterns <- model1$centers
patterns

#------Representar los ecosistemas en un espacio de dimensión reducido

clusplot(Original_DataClustered,
         model1$cluster, 
         main = "2D Plot", 
         shade = T, 
         labels = 4,
         lines = 2)

clusplot(Original_DataClustered,
         model1$cluster, 
         main = "2D Plot", #Title
         shade = T, 
         labels = 2,
         lines = 0)

# ---------------------------------------------------------------------------

#EXTRAS

#--- Silhouette Analysis

sil_width <- map_dbl(2:10,  function(k){
  model <- pam(Data, k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2:10) +
  ggtitle("Silhouette Analysis")

#--- OPTIMAL K = 2 based on comparision between two methods analized before.

MeanCLusters <- Original_DataClustered %>% 
  group_by(cluster) %>% 
  summarise(mean_colif_total = mean(Colif_total),
            mean_colif_fecal=mean(Colif_fecal),
            mean_Estrep_fecal=mean(Estrep_fecal),
            mean_Cont_mineral=mean(Cont_mineral),
            mean_conduct = mean(Conductivitat),
            mean_Solids_susp = mean(Solids_susp),
            mean_DQO =mean(DQO_M))

as.data.frame(MeanCLusters)

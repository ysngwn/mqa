library(glm2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(scales)
library(factoextra)
library(cluster)


doutores <- read.csv('doutores_processado_1000.csv')
summary(doutores)

doutores_filtered <- doutores[, c('DuracaoDoutorado', 'Publicacoes', 'IdadeAcademica', 'HistoricoFiliacoesEmIESInternacional', 'UltimaFiliacaoEmIES')] 
scale(doutores_filtered)
summary(doutores_filtered)


# matriz de correlacao
matcor <- cor(doutores_filtered)
print(matcor, digits = 2)
corrplot(matcor, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black")


# gower
gower_dist <- daisy(doutores_filtered, metric = "gower")

# Perform K-Medoids clustering
kmedoids <- pam(gower_dist, k = 3)

# Plotting
clusplot(kmedoids, main = "Cluster Plot", color = TRUE, shade = TRUE, labels = 2, lines = 0)

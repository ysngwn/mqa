library(glm2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(scales)
library(factoextra)
library(cluster)


doutores <- read.csv('doutores_processado.csv')
summary(doutores)

doutores_filtered <- doutores[, c('DuracaoDoutorado', 'Publicacoes', 'IdadeAcademica', 'HistoricoFiliacoesEmIESInternacional', 'UltimaFiliacaoEmIES')] 
# scale(doutores_filtered)
summary(doutores_filtered)


#doutores_filtered2$DuracaoDoutorado <- rescale(doutores_filtered$DuracaoDoutorado)
#doutores_filtered2$Publicacoes <- rescale(doutores_filtered$Publicacoes)
#doutores_filtered2$IdadeAcademica <- rescale(doutores_filtered$IdadeAcademica)
#summary(doutores_filtered2)


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
fviz_cluster(list(cluster = kmedoids$cluster, data = doutores_filtered), geom = "point", ellipse.type = "norm")

library(glm2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(scales)
library(factoextra)
library(cluster)


doutores <- read.csv('doutores_processado.csv')
summary(doutores)

doutores_filtered <- doutores[, c('DuracaoDoutorado', 'Publicacoes', 'IdadeAcademica', 'HistoricoFiliacoesEmIESInternacional', 'UltimaFiliacaoEmIES')] 
# scale(doutores_filtered)
summary(doutores_filtered)


#doutores_filtered2$DuracaoDoutorado <- rescale(doutores_filtered$DuracaoDoutorado)
#doutores_filtered2$Publicacoes <- rescale(doutores_filtered$Publicacoes)
#doutores_filtered2$IdadeAcademica <- rescale(doutores_filtered$IdadeAcademica)
#summary(doutores_filtered2)


# matriz de correlacao
matcor <- cor(doutores_filtered)
print(matcor, digits = 2)
corrplot(matcor, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black")


# gower
gower_dist <- daisy(doutores_filtered, metric = "gower")

# Perform K-Medoids clustering
kmedoids <- pam(gower_dist, k = 4)

# Plotting
fviz_cluster(list(cluster = kmedoids$cluster, data = doutores_filtered), geom = "point", ellipse.type = "norm")

# clusplot(kmedoids, main = "Cluster Plot using PAM", color = TRUE, shade = TRUE, labels = 2, lines = 0)
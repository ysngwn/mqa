library(glm2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(scales)
library(factoextra)
library(cluster)
library(FactoMineR)
library("data.table")    
library(psych)

doutores <- fread("doutores_processado_2000_com_todos_int.csv", 
                  select = c("DuracaoDoutorado", "Publicacoes", "IdadeAcademica", "HistoricoFiliacoesEmIESInternacional", "UltimaFiliacaoEmIES", "ExclusividadeDoutorado", "Genero")) 
summary(doutores)
head(doutores)


doutores10 <- filter(doutores, DuracaoDoutorado == 1.0)
summary(doutores10)


matcor <- cor(doutores)
print(matcor)
corrplot(matcor, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black")
KMO(doutores)

fit<-princomp(doutores,cor=TRUE)
summary(fit)
screeplot(fit)
plot(fit,type="lines")

PCAdente<-principal(doutores, nfactors=2,
                    n.obs=30,rotate="varimax", scores=TRUE)
PCAdente

biplot(PCAdente)


#################################### PCA



scale(doutores)
data.pca <- princomp(doutores)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")




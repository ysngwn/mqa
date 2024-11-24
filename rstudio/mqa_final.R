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

doutores <- fread("doutores_processado_1000.csv", 
                  select = c("DuracaoDoutorado", "Publicacoes", "IdadeAcademica", "HistoricoFiliacoesEmIESInternacional", "UltimaFiliacaoEmIES")) 
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



scale(doutores_UltimaFiliacaoEmIES_1)
doutores_UltimaFiliacaoEmIES_1$UltimaFiliacaoEmIES <- NULL
data.pca <- princomp(doutores_UltimaFiliacaoEmIES_1)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")




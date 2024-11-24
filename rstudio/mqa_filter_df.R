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

# por duracao doutorado
doutores_DuracaoDoutorado_1 <- filter(doutores, DuracaoDoutorado == 1.0)
doutores_DuracaoDoutorado_2 <- filter(doutores, DuracaoDoutorado == 2.0)
doutores_DuracaoDoutorado_2_5 <- filter(doutores, DuracaoDoutorado == 2.5)
doutores_DuracaoDoutorado_3 <- filter(doutores, DuracaoDoutorado == 3.0)
doutores_DuracaoDoutorado_3_5 <- filter(doutores, DuracaoDoutorado == 3.5)
doutores_DuracaoDoutorado_4 <- filter(doutores, DuracaoDoutorado == 4.0)
doutores_DuracaoDoutorado_4_5 <- filter(doutores, DuracaoDoutorado == 4.5)
doutores_DuracaoDoutorado_5 <- filter(doutores, DuracaoDoutorado == 5.0)
doutores_DuracaoDoutorado_6 <- filter(doutores, DuracaoDoutorado == 6.0)
doutores_DuracaoDoutorado_7 <- filter(doutores, DuracaoDoutorado == 7.0)
doutores_DuracaoDoutorado_8 <- filter(doutores, DuracaoDoutorado == 8.0)
doutores_DuracaoDoutorado_9 <- filter(doutores, DuracaoDoutorado == 9.0)
doutores_DuracaoDoutorado_10 <- filter(doutores, DuracaoDoutorado == 10.0)

# por idade academica 
doutores_IdadeAcademica_0 <- filter(doutores, IdadeAcademica < 10.0)
doutores_IdadeAcademica_10 <- filter(doutores, IdadeAcademica >= 10.0 & IdadeAcademica < 20.0)
doutores_IdadeAcademica_20 <- filter(doutores, IdadeAcademica >= 20.0 & IdadeAcademica < 30.0)
doutores_IdadeAcademica_30 <- filter(doutores, IdadeAcademica >= 30.0 & IdadeAcademica < 40.0)
doutores_IdadeAcademica_40 <- filter(doutores, IdadeAcademica >= 40.0 & IdadeAcademica < 50.0)
doutores_IdadeAcademica_50 <- filter(doutores, IdadeAcademica >= 50.0 & IdadeAcademica < 60.0)

# por publicacoes
doutores_Publicacoes_0 <- filter(doutores, Publicacoes < 50)
doutores_Publicacoes_50 <- filter(doutores, Publicacoes >= 50 & Publicacoes < 100)
doutores_Publicacoes_100 <- filter(doutores, Publicacoes >= 100 & Publicacoes < 150)
doutores_Publicacoes_150 <- filter(doutores, Publicacoes >= 150 & Publicacoes < 200)
doutores_Publicacoes_200 <- filter(doutores, Publicacoes >= 200 & Publicacoes < 250)
doutores_Publicacoes_250 <- filter(doutores, Publicacoes >= 250 & Publicacoes < 300)
doutores_Publicacoes_300 <- filter(doutores, Publicacoes >= 300 & Publicacoes < 400)

# por HistoricoFiliacoesEmIESInternacional
doutores_HistoricoFiliacoesEmIESInternacional_0 <- filter(doutores, HistoricoFiliacoesEmIESInternacional == 0)
doutores_HistoricoFiliacoesEmIESInternacional_1 <- filter(doutores, HistoricoFiliacoesEmIESInternacional == 1)

# por UltimaFiliacaoEmIES
doutores_UltimaFiliacaoEmIES_0 <- filter(doutores, UltimaFiliacaoEmIES == 0)
doutores_UltimaFiliacaoEmIES_1 <- filter(doutores, UltimaFiliacaoEmIES == 1)


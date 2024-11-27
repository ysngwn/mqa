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
library(gridExtra)
library(rpart)
library(rpart.plot)

doutores <- fread("doutores_processado_2000_com_todos_int.csv", 
  select = c("DuracaoDoutorado", "Publicacoes", "IdadeAcademica", "HistoricoFiliacoesEmIESInternacional", "UltimaFiliacaoEmIES", "GrandesAreas", "ExclusividadeDoutorado", "Genero")) 


####################################################################################################


unique(doutores$DuracaoDoutorado)
unique(doutores$Publicacoes)
unique(doutores$IdadeAcademica)

summary(doutores$DuracaoDoutorado)
summary(doutores$Publicacoes)
summary(doutores$IdadeAcademica)

# por DuracaoDoutorado
doutores_DuracaoDoutorado_1 <- filter(doutores, DuracaoDoutorado < 2.0 )
doutores_DuracaoDoutorado_2 <- filter(doutores, DuracaoDoutorado >= 2.0 | DuracaoDoutorado < 4.0)
doutores_DuracaoDoutorado_4 <- filter(doutores, DuracaoDoutorado >= 4.0 | DuracaoDoutorado < 6.0)
doutores_DuracaoDoutorado_7 <- filter(doutores, DuracaoDoutorado >= 7.0 | DuracaoDoutorado < 9.0)
doutores_DuracaoDoutorado_9 <- filter(doutores, DuracaoDoutorado >= 9.0)

count(doutores_DuracaoDoutorado_1) # 
count(doutores_DuracaoDoutorado_2) # 
count(doutores_DuracaoDoutorado_4) # 
count(doutores_DuracaoDoutorado_7) # 
count(doutores_DuracaoDoutorado_9) # 

# por IdadeAcademica
doutores_IdadeAcademica_0 <- filter(doutores, IdadeAcademica < 5.0)
doutores_IdadeAcademica_5 <- filter(doutores, IdadeAcademica >= 5.0 & IdadeAcademica < 10.0)
doutores_IdadeAcademica_10 <- filter(doutores, IdadeAcademica >= 10.0 & IdadeAcademica < 15.0)
doutores_IdadeAcademica_15 <- filter(doutores, IdadeAcademica >= 15.0 & IdadeAcademica < 20.0)
doutores_IdadeAcademica_20 <- filter(doutores, IdadeAcademica >= 20.0)

count(doutores_IdadeAcademica_0) # 
count(doutores_IdadeAcademica_5) # 
count(doutores_IdadeAcademica_10) # 
count(doutores_IdadeAcademica_15) # 
count(doutores_IdadeAcademica_20) # 

# por Publicacoes
doutores_Publicacoes_0 <- filter(doutores, Publicacoes < 10)
doutores_Publicacoes_50 <- filter(doutores, Publicacoes >= 50 & Publicacoes < 100)
doutores_Publicacoes_100 <- filter(doutores, Publicacoes >= 100 & Publicacoes < 150)
doutores_Publicacoes_150 <- filter(doutores, Publicacoes >= 150 & Publicacoes < 200)
doutores_Publicacoes_200 <- filter(doutores, Publicacoes >= 200 & Publicacoes < 250)
doutores_Publicacoes_250 <- filter(doutores, Publicacoes >= 250 & Publicacoes < 300)
doutores_Publicacoes_300 <- filter(doutores, Publicacoes >= 300 & Publicacoes < 400)

count(doutores_Publicacoes_0) # 
count(doutores_Publicacoes_50) # 
count(doutores_Publicacoes_100) # 
count(doutores_Publicacoes_150) # 
count(doutores_Publicacoes_200) # 
count(doutores_Publicacoes_250) # 
count(doutores_Publicacoes_300) # 

# por HistoricoFiliacoesEmIESInternacional
doutores_HistoricoFiliacoesEmIESInternacional_0 <- filter(doutores, HistoricoFiliacoesEmIESInternacional == 0)
doutores_HistoricoFiliacoesEmIESInternacional_1 <- filter(doutores, HistoricoFiliacoesEmIESInternacional == 1)

count(doutores_HistoricoFiliacoesEmIESInternacional_0) # 
count(doutores_HistoricoFiliacoesEmIESInternacional_1) # 

# por UltimaFiliacaoEmIES
doutores_UltimaFiliacaoEmIES_0 <- filter(doutores, UltimaFiliacaoEmIES == 0)
doutores_UltimaFiliacaoEmIES_1 <- filter(doutores, UltimaFiliacaoEmIES == 1)

count(doutores_UltimaFiliacaoEmIES_0) # 
count(doutores_UltimaFiliacaoEmIES_1) # 

# por ExclusividadeDoutorado
doutores_ExclusividadeDoutorado_0 <- filter(doutores, ExclusividadeDoutorado == 0)
doutores_ExclusividadeDoutorado_1 <- filter(doutores, ExclusividadeDoutorado == 1)

count(doutores_ExclusividadeDoutorado_0)
count(doutores_ExclusividadeDoutorado_1)

# por Genero
doutores_Genero_0 <- filter(doutores, Genero == 0)
doutores_Genero_1 <- filter(doutores, Genero == 1)

count(doutores_Genero_0)
count(doutores_Genero_1)


####################################################################################################



# IES INTERNACIONAL

doutores_HistoricoFiliacoesEmIESInternacional_0
doutores_HistoricoFiliacoesEmIESInternacional_1

# Quem tem ultima filiacao em IES
count(filter(doutores_HistoricoFiliacoesEmIESInternacional_0, UltimaFiliacaoEmIES == 0)) # 329
count(filter(doutores_HistoricoFiliacoesEmIESInternacional_0, UltimaFiliacaoEmIES == 1)) # 225

count(filter(doutores_HistoricoFiliacoesEmIESInternacional_1, UltimaFiliacaoEmIES == 0)) # 217
count(filter(doutores_HistoricoFiliacoesEmIESInternacional_1, UltimaFiliacaoEmIES == 1)) # 229

# IES Internacional por anos de doutorado


####################################################################################################

# Ultima Filiacao em IES

doutores_UltimaFiliacaoEmIES_0
doutores_UltimaFiliacaoEmIES_1

count(filter(doutores_UltimaFiliacaoEmIES_0, HistoricoFiliacoesEmIESInternacional == 0))
count(filter(doutores_UltimaFiliacaoEmIES_0, HistoricoFiliacoesEmIESInternacional == 1))

count(filter(doutores_UltimaFiliacaoEmIES_1, HistoricoFiliacoesEmIESInternacional == 0))
count(filter(doutores_UltimaFiliacaoEmIES_1, HistoricoFiliacoesEmIESInternacional == 1))

#count(filter(doutores_DuracaoDoutorado_1, HistoricoFiliacoesEmIESInternacional == 0)) # 8
#count(filter(doutores_DuracaoDoutorado_2, HistoricoFiliacoesEmIESInternacional == 0)) # 34
#count(filter(doutores_DuracaoDoutorado_3, HistoricoFiliacoesEmIESInternacional == 0)) # 104
#count(filter(doutores_DuracaoDoutorado_4, HistoricoFiliacoesEmIESInternacional == 0)) # 265
#count(filter(doutores_DuracaoDoutorado_5, HistoricoFiliacoesEmIESInternacional == 0)) # 100
#count(filter(doutores_DuracaoDoutorado_6, HistoricoFiliacoesEmIESInternacional == 0)) # 23
#count(filter(doutores_DuracaoDoutorado_7, HistoricoFiliacoesEmIESInternacional == 0)) # 9
#count(filter(doutores_DuracaoDoutorado_8, HistoricoFiliacoesEmIESInternacional == 0)) # 4
#count(filter(doutores_DuracaoDoutorado_9, HistoricoFiliacoesEmIESInternacional == 0)) # 4
#count(filter(doutores_DuracaoDoutorado_10, HistoricoFiliacoesEmIESInternacional == 0)) # 0

mean(doutores_DuracaoDoutorado_1$HistoricoFiliacoesEmIESInternacional == 1) # 0.38
mean(doutores_DuracaoDoutorado_2$HistoricoFiliacoesEmIESInternacional == 1) # 0.49
mean(doutores_DuracaoDoutorado_3$HistoricoFiliacoesEmIESInternacional == 1) # 0.45
mean(doutores_DuracaoDoutorado_4$HistoricoFiliacoesEmIESInternacional == 1) # 0.46
mean(doutores_DuracaoDoutorado_5$HistoricoFiliacoesEmIESInternacional == 1) # 0.40
mean(doutores_DuracaoDoutorado_6$HistoricoFiliacoesEmIESInternacional == 1) # 0.44
mean(doutores_DuracaoDoutorado_7$HistoricoFiliacoesEmIESInternacional == 1) # 0.31
mean(doutores_DuracaoDoutorado_8$HistoricoFiliacoesEmIESInternacional == 1) # 0.20
mean(doutores_DuracaoDoutorado_9$HistoricoFiliacoesEmIESInternacional == 1) # 0
mean(doutores_DuracaoDoutorado_10$HistoricoFiliacoesEmIESInternacional == 1) # 1





####################################################################################################

# Distribuição de variáveis em doutores_HistoricoFiliacoesEmIESInternacional_0

duracao_0 <- ggplot(doutores_HistoricoFiliacoesEmIESInternacional_0, aes(x = DuracaoDoutorado)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Duração do Doutorado", x = "Duração (anos)", y = "Frequência")

publicacoes_0 <- ggplot(doutores_HistoricoFiliacoesEmIESInternacional_0, aes(x = Publicacoes)) +
  geom_histogram(binwidth = 5, fill = "green", alpha = 0.7) +
  labs(title = "Publicações", x = "Número de Publicações", y = "Frequência")


duracao_1 <- ggplot(doutores_HistoricoFiliacoesEmIESInternacional_1, aes(x = DuracaoDoutorado)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Duração do Doutorado", x = "Duração (anos)", y = "Frequência")

publicacoes_1 <- ggplot(doutores_HistoricoFiliacoesEmIESInternacional_1, aes(x = Publicacoes)) +
  geom_histogram(binwidth = 5, fill = "green", alpha = 0.7) +
  labs(title = "Publicações", x = "Número de Publicações", y = "Frequência")



grid.arrange(
  grid.arrange(duracao_0, publicacoes_0, ncol = 2),
  grid.arrange(duracao_1, publicacoes_1, ncol = 2),
  ncol = 1
)


####################################################################################################


# Converter a variável alvo em fator (caso necessário)
#doutores_DuracaoDoutorado_4$HistoricoFiliacoesEmIESInternacional <- as.factor(doutores_DuracaoDoutorado_4$HistoricoFiliacoesEmIESInternacional)

# Dividir o dataset em treino e teste
set.seed(123)
trainIndex <- sample(1:nrow(doutores_DuracaoDoutorado_2), size = 0.8 * nrow(doutores_DuracaoDoutorado_2))
train <- doutores_DuracaoDoutorado_2[trainIndex, ]
test <- doutores_DuracaoDoutorado_2[-trainIndex, ]

# Construir a árvore de decisão
modelo_arvore <- rpart(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES + Genero + ExclusividadeDoutorado,
                       data = train, method = "class")

# Visualizar o modelo
print(modelo_arvore)

# Plotar a árvore
rpart.plot(modelo_arvore, type = 3, extra = 104, fallen.leaves = TRUE,
           main = "Árvore de Decisão para Prever Mobilidade Internacional")


# Fazer previsões nos dados de teste
predicoes <- predict(modelo_arvore, newdata = test, type = "class")

# Matriz de confusão
library(caret)
confusionMatrix(predicoes, test$HistoricoFiliacoesEmIESInternacional)


# Ajuste de hiperparâmetros
modelo_arvore_tuned <- rpart(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES,
                             data = train, method = "class",
                             control = rpart.control(cp = 0.01, maxdepth = 5))

# Visualizar a árvore ajustada
rpart.plot(modelo_arvore_tuned, type = 3, extra = 104, fallen.leaves = TRUE,
           main = "Árvore de Decisão Ajustada")


#####################################################################################


freq_mobilidade <- table(doutores$HistoricoFiliacoesEmIESInternacional)
print(freq_mobilidade)


desempenho <- doutores[, .(
  DuracaoMedia = mean(DuracaoDoutorado, na.rm = TRUE),
  PublicacoesMedia = mean(Publicacoes, na.rm = TRUE),
  IdadeAcademicaMedia = mean(IdadeAcademica, na.rm = TRUE)
), by = HistoricoFiliacoesEmIESInternacional]
print(desempenho)

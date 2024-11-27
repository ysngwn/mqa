# IDLattes
# DuracaoDoutorado
# Genero
# ExclusividadeDoutorado
# Publicacoes
# IdadeAcademica
# HistoricoFiliacoesEmIESInternacional
# UltimaFiliacaoEmIES
# PrimeiraAreaNumeric


library(data.table)
library(ggplot2)
library(nnet)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(factoextra)
library(FactoMineR)
library(factoextra)



doutores_com_outlier <- fread("doutores_processado_2000_final.csv", 
                  select = c("DuracaoDoutorado", "Genero", "ExclusividadeDoutorado", "Publicacoes", "IdadeAcademica", "HistoricoFiliacoesEmIESInternacional", "UltimaFiliacaoEmIES", "PrimeiraAreaNumeric"))


# REMOÇÃO DE OUTLIERS
# remoção de dados abaixo de 1.5 * IQR do Q1 e acima de 1.5 * IQR do Q3
# para as variáveis IdadeAcademica, DuracaoDoutorado, Publicacoes

remove_outliers <- function(data, column_name) {
  quartiles <- quantile(data[[column_name]], probs = c(0.25, 0.75), na.rm = TRUE)
  IQR_value <- IQR(data[[column_name]], na.rm = TRUE)
  
  lower_bound <- quartiles[1] - 1.5 * IQR_value
  upper_bound <- quartiles[2] + 1.5 * IQR_value
  
  filtered_data <- subset(data, data[[column_name]] > lower_bound & data[[column_name]] < upper_bound)
  
  return(filtered_data)
}

doutores <- remove_outliers(doutores_com_outlier, "IdadeAcademica")
doutores <- remove_outliers(doutores_com_outlier, "DuracaoDoutorado")
doutores <- remove_outliers(doutores, "Publicacoes")

dim(doutores_com_outlier) # 2062
dim(doutores) # 1856



#########################################################################
# ANÁLISE DE DADOS
#########################################################################


# ANÁLISE DE FREQUÊNCIA DE CADA VARIÁVEL

table(doutores$DuracaoDoutorado)
ggplot(doutores, aes(x = DuracaoDoutorado)) + geom_bar()
table(doutores$Genero)
ggplot(doutores, aes(x = Genero)) + geom_bar()
table(doutores$ExclusividadeDoutorado)
ggplot(doutores, aes(x = ExclusividadeDoutorado)) + geom_bar()
table(doutores$Publicacoes)
ggplot(doutores, aes(x = Publicacoes)) + geom_bar()
table(doutores$IdadeAcademica)
ggplot(doutores, aes(x = IdadeAcademica)) + geom_bar()
table(doutores$HistoricoFiliacoesEmIESInternacional)
ggplot(doutores, aes(x = HistoricoFiliacoesEmIESInternacional)) + geom_bar()
table(doutores$UltimaFiliacaoEmIES)
ggplot(doutores, aes(x = UltimaFiliacaoEmIES)) + geom_bar()
table(doutores$PrimeiraAreaNumeric)
ggplot(doutores, aes(x = PrimeiraAreaNumeric)) + geom_bar()

# 'CIENCIAS BIOLOGICAS': 0, 
# 'CIENCIAS EXATAS E DA TERRA': 1, 
# 'CIENCIAS AGRARIAS': 2, 
# 'ENGENHARIAS': 3, 
# 'CIENCIAS DA SAUDE': 4, 
# 'CIENCIAS SOCIAIS APLICADAS': 5, 
# 'CIENCIAS HUMANAS': 6, 
# 'LINGUISTICA LETRAS E ARTES': 7, 
# 'OUTROS': 8


# ANÁLISE DE FREQUENCIA DE HistoricoFiliacoesEmIESInternacional POR VARIÁVEIS

table(doutores$DuracaoDoutorado, doutores$HistoricoFiliacoesEmIESInternacional)
ggplot(doutores, aes(x = DuracaoDoutorado, color = as.factor(HistoricoFiliacoesEmIESInternacional))) +
  geom_density(size = 1.2) +
  labs(
    title = "Duração Doutorado x Filiação em IES Internacional",
    x = "Duração Doutorado",
    y = "Densidade",
    color = "Filiação em\nIES Internacional\n(0 = Não, 1 = Sim)"
  ) +
  theme_minimal() # ???????? tive duvida
table(doutores$Genero, doutores$HistoricoFiliacoesEmIESInternacional)
table(doutores$ExclusividadeDoutorado, doutores$HistoricoFiliacoesEmIESInternacional)
table(doutores$Publicacoes, doutores$HistoricoFiliacoesEmIESInternacional)
ggplot(doutores, aes(x = Publicacoes, color = as.factor(HistoricoFiliacoesEmIESInternacional))) +
  geom_density(size = 1.2) +
  labs(
    title = "Publicações x Filiação em IES Internacional",
    x = "Quantidade de Publicações",
    y = "Densidade",
    color = "Filiação em\nIES Internacional\n(0 = Não, 1 = Sim)"
  ) +
  theme_minimal() # publicacao이 적을 수록 IES internacional이 아님. Publicacao 올라갈 수록 반대
table(doutores$IdadeAcademica, doutores$HistoricoFiliacoesEmIESInternacional)
ggplot(doutores, aes(x = IdadeAcademica, color = as.factor(HistoricoFiliacoesEmIESInternacional))) +
  geom_density(size = 1.2) +
  labs(
    title = "Idade Acadêmica x Filiação em IES Internacional",
    x = "Idade Acadêmica",
    y = "Densidade",
    color = "Filiação em\nIES Internacional\n(0 = Não, 1 = Sim)"
  ) +
  theme_minimal() # 위와 동일
table(doutores$UltimaFiliacaoEmIES, doutores$HistoricoFiliacoesEmIESInternacional)
table(doutores$PrimeiraAreaNumeric, doutores$HistoricoFiliacoesEmIESInternacional)


# COMPARAÇÃO DE DESEMPENHO ACADÊMICO POR HistoricoFiliacoesEmIESInternacional

desempenho <- doutores[, .(
  DuracaoMedia = mean(DuracaoDoutorado, na.rm = TRUE),
  PublicacoesMedia = mean(Publicacoes, na.rm = TRUE),
  IdadeAcademicaMedia = mean(IdadeAcademica, na.rm = TRUE)
), by = HistoricoFiliacoesEmIESInternacional]
print(desempenho)

desempenho2 <- doutores[, .(
  DuracaoMedia = mean(DuracaoDoutorado, na.rm = TRUE),
  PublicacoesMedia = mean(Publicacoes, na.rm = TRUE),
  IdadeAcademicaMedia = mean(IdadeAcademica, na.rm = TRUE)
), by = .(PrimeiraAreaNumeric, HistoricoFiliacoesEmIESInternacional)]
setorder(desempenho2, PrimeiraAreaNumeric, HistoricoFiliacoesEmIESInternacional)
print(desempenho2)

desempenho3 <- doutores[, .(
  DuracaoMedia = mean(DuracaoDoutorado, na.rm = TRUE),
  PublicacoesMedia = mean(Publicacoes, na.rm = TRUE),
  IdadeAcademicaMedia = mean(IdadeAcademica, na.rm = TRUE)
), by = .(ExclusividadeDoutorado, HistoricoFiliacoesEmIESInternacional)]
setorder(desempenho3, ExclusividadeDoutorado, HistoricoFiliacoesEmIESInternacional)
print(desempenho3)

desempenho4 <- doutores[, .(
  PublicacoesMedia = mean(Publicacoes, na.rm = TRUE),
  IdadeAcademicaMedia = mean(IdadeAcademica, na.rm = TRUE)
), by = .(DuracaoDoutorado, HistoricoFiliacoesEmIESInternacional)]
setorder(desempenho4, DuracaoDoutorado, HistoricoFiliacoesEmIESInternacional)
print(desempenho4)



#########################################################################
# Regressão Logistica
#########################################################################



set.seed(123)

# Teste e treino
train_indices <- sample(1:nrow(doutores), size = 0.7 * nrow(doutores))
doutores_treino <- doutores[train_indices, ]
doutores_teste <- doutores[-train_indices, ]

# Variaveis categoricas
doutores_treino$PrimeiraAreaNumeric <- as.factor(doutores_treino$PrimeiraAreaNumeric)
doutores_treino$Genero <- as.factor(doutores_treino$Genero)
doutores_treino$ExclusividadeDoutorado <- as.factor(doutores_treino$ExclusividadeDoutorado)
doutores_treino$UltimaFiliacaoEmIES <- as.factor(doutores_treino$UltimaFiliacaoEmIES)

doutores_teste$PrimeiraAreaNumeric <- as.factor(doutores_teste$PrimeiraAreaNumeric)
doutores_teste$Genero <- as.factor(doutores_teste$Genero)
doutores_teste$ExclusividadeDoutorado <- as.factor(doutores_teste$ExclusividadeDoutorado)
doutores_teste$UltimaFiliacaoEmIES <- as.factor(doutores_teste$UltimaFiliacaoEmIES)

# Modelo
logit <- glm(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Genero + ExclusividadeDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES + PrimeiraAreaNumeric, 
             family = binomial(link = "logit"), 
             data = doutores_treino)
summary(logit)

# Predict
previsoes <- predict(logit, newdata = doutores_teste, type = "response")
previsoes_classes <- ifelse(previsoes > 0.5, 1, 0)

# Matriz de confusao
real_classes <- doutores_teste$HistoricoFiliacoesEmIESInternacional
matriz_confusao_caret <- confusionMatrix(factor(previsoes_classes), factor(real_classes))
print(matriz_confusao_caret)



#########################################################################
# Arvore de Decisao
#########################################################################



set.seed(123)

# Treino e teste
trainIndex <- sample(1:nrow(doutores), size = 0.7 * nrow(doutores))
train <- doutores[trainIndex, ]
test <- doutores[-trainIndex, ]

# Arovre
modelo_arvore <- rpart(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Genero + ExclusividadeDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES + PrimeiraAreaNumeric,
                       data = train, method = "class")

print(modelo_arvore)
rpart.plot(modelo_arvore, type = 3, extra = 104, fallen.leaves = TRUE,
           main = "Árvore de Decisão para Prever Mobilidade Internacional")


# Predict
predicoes <- predict(modelo_arvore, newdata = test, type = "class")


# Matriz de confusao
test$HistoricoFiliacoesEmIESInternacional <- factor(test$HistoricoFiliacoesEmIESInternacional)
predicoes <- factor(predicoes, levels = levels(test$HistoricoFiliacoesEmIESInternacional))

confMatrix <- confusionMatrix(predicoes, test$HistoricoFiliacoesEmIESInternacional)
print(confMatrix)



#########################################################################
# AF / PCA
#########################################################################



# Pré-processamento: Selecionar variáveis numéricas e normalizar
numerical_data <- doutores[, .(DuracaoDoutorado, Publicacoes, IdadeAcademica)]
numerical_data <- scale(numerical_data)

pca=PCA(numerical_data, graph=TRUE)

autovalores=get_eigenvalue(pca)
autovalores
fviz_eig(pca, addlabels=TRUE, ylim = c(0,50))

variaveis=get_pca_var(pca)
head(variaveis$coord)




#########################################################################
# K-Means
#########################################################################


doutores_com_outlier <- fread("doutores_processado_2000_final.csv", 
                              select = c("DuracaoDoutorado", "Genero", "ExclusividadeDoutorado", "Publicacoes", "IdadeAcademica", "HistoricoFiliacoesEmIESInternacional", "UltimaFiliacaoEmIES", "PrimeiraAreaNumeric")
                              , nrows= 300)

doutores <- remove_outliers(doutores_com_outlier, "IdadeAcademica")
doutores <- remove_outliers(doutores, "DuracaoDoutorado")
doutores <- remove_outliers(doutores, "Publicacoes")

fviz_nbclust(doutores, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res=kmeans(doutores, 4, nstart=25)
print(km.res)



fviz_cluster(km.res, data=doutores,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=FALSE,
             ggtheme=theme_minimal()
)


# CONCLUSAO: 
# HISTORICO IES INTERN TENDE A TER MAIS PUBLICACOES E FICAR MAIS TEMPO NA ACADEMIA

# PODIA TER SIDO FEITO:
# COMPARAR POR QUANTIDADE DE IES INTERNACIONAL

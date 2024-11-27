# IDLattes
# DuracaoDoutorado
# Genero
# ExclusividadeDoutorado
# Publicacoes
# IdadeAcademica
# HistoricoFiliacoesEmIESInternacional
# UltimaFiliacaoEmIES
# PrimeiraAreaNumeric

# PODIA TER SIDO FEITO:
# COMPARAR POR QUANTIDADE DE IES INTERNACIONAL

library(data.table)
library(ggplot2)
library(nnet)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

doutores_com_outlier <- fread("doutores_2000_com_area_numerico.csv", 
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
doutores <- remove_outliers(doutores_com_outlier, "Publicacoes")

dim(doutores_com_outlier) # 2062
dim(doutores) # 1960



#########################################################################
# ANÁLISE DE DADOS
#########################################################################



# ANÁLISE DE FREQUÊNCIA DE CADA VARIÁVEL

table(doutores$DuracaoDoutorado)
ggplot(doutores, aes(x = DuracaoDoutorado)) + geom_bar()
table(doutores$Genero)
table(doutores$ExclusividadeDoutorado)
table(doutores$Publicacoes)
ggplot(doutores, aes(x = Publicacoes)) + geom_bar()
table(doutores$IdadeAcademica)
ggplot(doutores, aes(x = IdadeAcademica)) + geom_bar()
table(doutores$HistoricoFiliacoesEmIESInternacional)
table(doutores$UltimaFiliacaoEmIES)
table(doutores$PrimeiraAreaNumeric)

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
  theme_minimal() # 
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

doutores$PrimeiraAreaNumeric <- as.factor(doutores$PrimeiraAreaNumeric)
doutores$Genero <- as.factor(doutores$Genero)
doutores$ExclusividadeDoutorado <- as.factor(doutores$ExclusividadeDoutorado)
doutores$UltimaFiliacaoEmIES <- as.factor(doutores$UltimaFiliacaoEmIES)

logit <- glm(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Genero + ExclusividadeDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES + PrimeiraAreaNumeric, family=binomial(link="logit"), data=doutores) 
summary(logit)


#########################################################################
# Arvore de Decisao
#########################################################################

# Dividir o dataset em treino e teste
set.seed(123)
trainIndex <- sample(1:nrow(doutores), size = 0.8 * nrow(doutores))
train <- doutores[trainIndex, ]
test <- doutores[-trainIndex, ]

# Construir a árvore de decisão
modelo_arvore <- rpart(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Genero + ExclusividadeDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES + PrimeiraAreaNumeric,
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
modelo_arvore_tuned <- rpart(HistoricoFiliacoesEmIESInternacional ~ DuracaoDoutorado + Genero + ExclusividadeDoutorado + Publicacoes + IdadeAcademica + UltimaFiliacaoEmIES + PrimeiraAreaNumeric,
                             data = train, method = "class",
                             control = rpart.control(cp = 0.01, maxdepth = 5))

# Visualizar a árvore ajustada
rpart.plot(modelo_arvore_tuned, type = 3, extra = 104, fallen.leaves = TRUE,
           main = "Árvore de Decisão Ajustada")



#########################################################################
# AF / PCA
#########################################################################


# Pré-processamento: Selecionar variáveis numéricas e normalizar
numerical_data <- doutores[, .(DuracaoDoutorado, Publicacoes, IdadeAcademica)]
numerical_data <- scale(numerical_data)

# Aplicar PCA
pca_result <- prcomp(numerical_data, center = TRUE, scale. = TRUE)
summary(pca_result)
biplot(pca_result)

# Aplicar Análise Fatorial
# Definir o número de fatores (por exemplo, 2)
num_factors <- 2
factor_result <- factanal(numerical_data, factors = num_factors, rotation = "varimax")
print(factor_result)

# Visualização dos resultados da PCA
pca_data <- as.data.table(pca_result$x)
pca_data[, HistoricoFiliacoesEmIESInternacional := doutores$HistoricoFiliacoesEmIESInternacional]

ggplot(pca_data, aes(x = PC1, y = PC2, color = HistoricoFiliacoesEmIESInternacional)) +
  geom_point() +
  labs(title = "PCA dos Doutorandos", x = "Componente Principal 1", y = "Componente Principal 2")

# Visualização dos resultados da Análise Fatorial
factor_loadings <- as.data.table(factor_result$loadings[, 1:num_factors])
print(factor_loadings)

#########################
cargaFatorada <- TRUE
source('01.ETL.R')
source('02.PrepararDataFrames.R')

#########################
# Naive Bayes
# Instalar pacote caso não exista: install.packages('e1071');
library("e1071")

modelo = naiveBayes(train.treino[-c(1,2)], train.treino$Survived)
previsao = predict(modelo, train.teste[-c(1,2)])

resultados <- addResultado(resultados, "Naive Bayes", 1 - mean(train.teste$Survived != previsao))

#########################
# Árvore de decisão
# Instalar pacote caso não exista: install.packages('rpart');
library("rpart")

modelo_rp = rpart(formula, data=train.treino, method="anova")
previsao <- predict(modelo_rp, train.teste)
previsao <- ifelse(previsao > 0.5, 1, 0)

resultados <- addResultado(resultados, "Árvore de Decisão", 1 - mean(train.teste$Survived != previsao))

#########################
# Floresta Aletória
# Instalar pacote caso não exista: install.packages('randomForest');
library("randomForest")

modelo_rf <- randomForest(formula, data=train.treino)
previsao <- predict(modelo_rf, train.teste)

resultados <- addResultado(resultados, "Floresta Aletória", 1 - mean(train.teste$Survived != previsao))

#########################
cargaFatorada <- FALSE
source('01.ETL.R')
source('02.PrepararDataFrames.R')

#########################
# K-Média
# Instalar pacote caso não exista: install.packages('stats');
library("stats")

res <- kmeans(train[-c(1,2)], 2)
previsao = (res$cluster - 1)
if (getMode(previsao) != 0)
  previsao <- ifelse(previsao == 1, 0, 1)

resultados <- addResultado(resultados, "K-Média", 1 - mean(train$Survived != previsao))

#########################
# K-Vizinhos
# Instalar pacote caso não exista: install.packages('class');
library("class")

sample <- sample(1:nrow(train), size=nrow(train)*0.5)
previsao <- knn(train[sample, -c(1,2)], train[-sample, -c(1,2)], train[sample, 2], k=2)
if (getMode(previsao) != 0)
  previsao <- ifelse(previsao == 1, 0, 1)

resultados <- addResultado(resultados, "K-Vizinhos", 1 - mean(train[-sample,]$Survived != previsao))

#########################
# Regressão Linear
modelo_lm <- lm(formula, data=train.treino)
previsao <- predict(modelo_lm, train.teste)
previsao <- ifelse(previsao > 0.5, 1, 0)

resultados <- addResultado(resultados, "Regressão Linear", 1 - mean(train.teste$Survived != previsao))

#########################
# GLM
modelo_glm <- glm(formula, family=binomial(link = "logit"), data=train.treino)
predicao <- predict(modelo_glm, newdata=train.teste, type="response")
predicao <- ifelse(predicao > 0.5, 1, 0)

resultados <- addResultado(resultados, "Generalized linear model", 1 - mean(train.teste$Survived != previsao))

#########################
# Conversão de escala
treino.scale <- doScale(train.treino[-1])
teste.scale <- doScale(train.teste[-c(1,2)])

# Rede Neural
# Instalar pacote caso não exista: install.packages('neuralnet');
library("neuralnet")
modelo_nn = neuralnet(formula, data=treino.scale, hidden=10, linear.output=T, stepmax=1e6)

predicao <- neuralnet::compute(modelo_nn, teste.scale)
predicao <- predicao$net.result
predicao <- ifelse(predicao > 0.5, 1, 0)

resultados <- addResultado(resultados, "Rede Neural", 1 - mean(train.teste$Survived != previsao))

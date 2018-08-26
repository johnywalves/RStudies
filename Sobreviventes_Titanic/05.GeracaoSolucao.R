# Selecionar a pasta de trabalho e limpar variáveis de ambiente
setwd(".")
rm(list = ls())

# Carregar dados para entrega
cargaFatorada <- TRUE
source('01.ETL.R')
source('02.PrepararDataFrames.R')

# Floresta Aletória
# Instalar pacote caso não exista: install.packages('randomForest');
library("randomForest")
modelo_rf <- randomForest(formula, data=train)
test$Survived <- predict(modelo_rf, test)
test$Survived <- as.numeric(test$Survived) - 1

# Escrever arquivo
write.csv(test[c("PassengerId", "Survived")], file="solution.csv", row.names=FALSE)
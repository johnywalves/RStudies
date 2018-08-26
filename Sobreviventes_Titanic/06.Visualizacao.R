# Selecionar a pasta de trabalho e limpar variáveis de ambiente
setwd(".")
rm(list = ls())

# Carregar dados para entrega
cargaFatorada <- TRUE
source('01.ETL.R')
source('02.PrepararDataFrames.R')

# Tidyverse - R packages
# Instalar pacote caso não exista: install.packages('randomForest');
library("tidyverse")

# Descrição de Sobrevivência
train$Sub = ifelse(train$Survived == 1, "Sim" , "Não") 
# Palheta de Cores
cbPalette <- c("#FF6A00", "#069B87")

# Comparação Idades x Sobrevivência
ggplot(train, aes(Age, fill = Sub, colour = Sub)) +
  labs(x = "Idade", y = "Quantidade", fill = "Sobreviventes", colour = "Sobreviventes") +
  geom_density(position = "identity", alpha = 0.2) +  
  xlim(0, max(train$Age)) + 
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette)

# Comparação Valor Passagem x Sobrevivência
ggplot(train, aes(Fare, fill = Sub, colour = Sub)) +
  labs(x = "Passagem", y = "Quantidade", fill = "Sobreviventes", colour = "Sobreviventes") +
  geom_density(alpha = 0.3) +  
  xlim(0, 150) + 
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette)

# Comparação Classe x Sobrevivência
ggplot(train, aes(Pclass, fill = Sub, colour = Sub)) +
  geom_histogram(binwidth = 0.5,alpha = 0.2) +
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette)

# Comparação Familia x Sobrevivência
ggplot(train, aes(as.numeric(Familia), fill=Sub, colour=Sub)) +
  labs(title = "", 
       x="Tamanho da Família", y="Quantidade", 
       fill="Sobreviventes", colour="Sobreviventes") +
  geom_bar(position='dodge', alpha = 1) +
  scale_x_continuous(breaks=c(1:11)) + 
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette)

# Comparação Deck x Sobrevivência
ggplot(train, aes(as.numeric(Deck), fill = Sub, colour = Sub)) +
  geom_bar(position='dodge', alpha = 1) +
  scale_x_continuous(breaks=c(1:11)) + 
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette)
  
# Comparação Número Cabines x Sobrevivência
ggplot(train, aes(as.character(NCabin), fill = Sub, colour = Sub)) +
  geom_bar(position='dodge', alpha = 0.2) +
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette)

#########################################################
#    Visualizar as importancias dos parâmetros
#########################################################
importance <- importance(modelo_rf)

varImportance <- data.frame(Variables = row.names(importance), 
        Importance = round(importance[,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
        mutate(Rank = paste0('#',dense_rank(desc(importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip() + 
  theme_minimal()

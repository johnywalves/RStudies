# Carregar bases de dados
test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Base combinada
test$Survived <- NA
combi <- rbind(train, test)
rm('train','test')

# Tamanho de Família
combi$Familia <- combi$SibSp + combi$Parch + 1
# Tipo Família
combi$TipoFamilia[combi$Familia == 1] <- 'Solteiro/a'
combi$TipoFamilia[combi$Familia < 5 & combi$Familia > 1] <- 'Pequena'
combi$TipoFamilia[combi$Familia > 4] <- 'Grande'

# Conversão em Fator
combi$Embarked <- as.factor(combi$Embarked)
combi$Sex <- as.factor(combi$Sex)
combi$TipoFamilia <- as.factor(combi$TipoFamilia)

# Compreender Títulos
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {trimws(strsplit(x, split = "[.,]")[[1]][2])})
# Organizar títulos 
rareTitle <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
combi$Title[combi$Title == 'Mlle'] <- 'Miss' 
combi$Title[combi$Title == 'Ms'] <- 'Miss'
combi$Title[combi$Title == 'Mme'] <- 'Mrs' 
combi$Title[combi$Title %in% rareTitle] <- 'Rare Title'
combi$Title <- as.factor(combi$Title)
rm(rareTitle)

# Ajuste de Sobrenome
combi$Surname <- sapply(combi$Name, FUN=function(x) {trimws(strsplit(x, split = "[.,]")[[1]][1])})

# Ajustar a passagem
combi$Fare[is.na(combi$Fare)] <- median(combi$Fare[combi$Pclass == 3 & combi$Embarked == "S"], na.rm=TRUE)

# Ajustar as idades
library("rpart")
modelo_idade <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Familia + TipoFamilia + Title, data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(modelo_idade, combi[is.na(combi$Age),])
rm(modelo_idade)

# Definições de prioridades - reconhecimento de crianças
combi$Kid <- ifelse(combi$Age <= 16, 1, 0)

# Definições de prioridades - reconhecimento de mães
combi$Mother <- ifelse(combi$Sex == 'female' & combi$Parch > 0 & combi$Age > 18 & combi$Title != 'Miss', 1, 0)

# Correção de porto de embarque
combi$Embarked[combi$Embarked == ""] <- NA
combi$Embarked[is.na(combi$Embarked)] <- "C"

# Reconhecimento de deck
combi$Deck <- substr(combi$Cabin, 1, 1)
combi$Deck <- as.factor(combi$Deck)

# Número de Cabines utilizadas
combi$NCabin <- sapply(as.character(combi$Cabin), FUN=function(x) {ifelse(x=="",0,length(strsplit(x, split = " ")[[1]]))})

# Definição dos American Lines 
combi$Line <- ifelse(combi$Ticket == "LINE", 1, 0)

# Definição outros tickets
ticketLimpo <- gsub("[./]", "", toupper(combi$Ticket))
combi$PC <- ifelse(substr(ticketLimpo, 1, 2) == "PC", 1, 0)
combi$CA <- ifelse(substr(ticketLimpo, 1, 2) == "CA", 1, 0)
combi$WC <- ifelse(substr(ticketLimpo, 1, 2) == "WC", 1, 0)
rm(ticketLimpo)

# Encontrar a tripulação
combi$Crew <- ifelse(combi$Fare == 0 & combi$Deck == "", 1, 0)

if (cargaFatorada) {
  #
  combi$Survived <- as.factor(combi$Survived)
  combi$Kid <- as.factor(combi$Kid)
  combi$Mother <- as.factor(combi$Mother)
  combi$NCabin <- as.factor(combi$NCabin)
  combi$Line <- as.factor(combi$Line)
  combi$PC <- as.factor(combi$PC)
  combi$CA <- as.factor(combi$CA)
  combi$WC <- as.factor(combi$WC)
} else {
  # Campos lidar com números
  combi$Sex <- as.integer(match(combi$Sex, levels(combi$Sex)))
  combi$Title <- as.integer(match(combi$Title, levels(combi$Title)))
  combi$Deck <- as.integer(match(combi$Deck, levels(combi$Deck)))
  combi$Embarked <- as.integer(match(combi$Embarked, levels(combi$Embarked)))
}
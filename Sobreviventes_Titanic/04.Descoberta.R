# Selecionar a pasta de trabalho e limpar variáveis de ambiente
setwd(".")
rm(list = ls())

# Função de moda
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Função de escala
doScale <- function(df) {
  maxs <- apply(df, 2, max)
  mins <- apply(df, 2, min)
  
  scaled <- as.data.frame(
    as.matrix(
      scale(df,
            center = mins, 
            scale = maxs - mins)))
  
  rownames(scaled) <- rownames(df)
  colnames(scaled) <- colnames(df)
  return(scaled)
}

# Função de acumular resultados
resultados <- data.frame()
addResultado <- function(resultados, metodo, margemErro) {

  registro <- data.frame(c(metodo), c(margemErro * 100))
  colnames(registro) <- c('Metodo', 'Margem')
    
  if (nrow(resultados) == 0) {
    resultados <- registro
  } else {
    resultados <- rbind(resultados, registro)
  }
  return(resultados)
}

#set.seed(4)
source('03.Aprendizagem.R')
#set.seed(8)
#source('03.Aprendizagem.R')
#set.seed(15)
#source('03.Aprendizagem.R')
#set.seed(16)
#source('03.Aprendizagem.R')
#set.seed(23)
#source('03.Aprendizagem.R')
#set.seed(42)
#source('03.Aprendizagem.R')

#########################
# Apresentar resultados
print(aggregate(Margem ~ Metodo, resultados, mean))

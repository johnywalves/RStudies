# 
setwd('.')
rm(list=ls())

# Código da Série
# Explicação da base de dados - https://www.imdb.com/interfaces/
# Base de dados - https://datasets.imdbws.com/
codSerie <- 'tt1751105' # My Little Poney: Friendship is Magic

# Importar rating (classificação de episódios) 
ratings = read.table(gzfile("title.ratings.tsv.gz"), header = TRUE)
episode = read.table(gzfile("title.episode.tsv.gz"), header = TRUE)

# Selecionar as dez melhores séries
episodeRating <- cbind(episode[match(ratings$tconst, episode$tconst),], ratings)
episodeRating <- episodeRating[!is.na(episodeRating$parentTconst),]
episodeRating <- episodeRating[(episodeRating$seasonNumber != '\\N'),]
episodeRating <- episodeRating[(episodeRating$numVotes > 50),]
# Ordenar por episódios
episodeRating <- episodeRating[order(episodeRating$seasonNumber, episodeRating$episodeNumber),] 
# Converter número da temporada e episódio
episodeRating$seasonNumber <- as.integer(as.character(episodeRating$seasonNumber))
episodeRating$episodeNumber <- as.integer(as.character(episodeRating$episodeNumber))

# Média da pontuação dos 
meanRating <- episodeRating[,c('parentTconst', 'averageRating')]
meanRating <- aggregate(averageRating ~ parentTconst, meanRating, mean)
meanRating <- meanRating[order(-meanRating$averageRating),]

# Cálculos das médias A, B e C
mediaCurvaA <- mean(meanRating[1:(nrow(meanRating)*0.2),]$averageRating)
mediaCurvaB <- mean(meanRating[1:(nrow(meanRating)*0.3),]$averageRating)
mediaCurvaC <- mean(meanRating[1:(nrow(meanRating)*0.5),]$averageRating)
# Cálculo da média da série
mediaSerie <- meanRating[meanRating$parentTconst == codSerie,]$averageRating

# Filtar os episódios da série e remover atributos duplicados
episodeRatingSerie = episodeRating[episodeRating$parentTconst == codSerie,-5]

# Organizar e ordenar episodios por sequência
library(stringr)
episodeRatingSerie$episodeNumber <- str_pad(episodeRatingSerie$episodeNumber, 2, pad = "0")
episodeRatingSerie$seasonNumber <- str_pad(episodeRatingSerie$seasonNumber, 2, pad = "0")
episodeRatingSerie$episode <- paste(episodeRatingSerie$seasonNumber, episodeRatingSerie$episodeNumber, sep = "")
# Ordenar
episodeRatingSerie <- episodeRatingSerie[order(episodeRatingSerie$episode),]
episodeRatingSerie$ordemEpisode <- seq(1,nrow(episodeRatingSerie))

# Definir cores as linhas
rainbow <- rainbow(3)

# Geração de gráfico por episódios
library(ggplot2)
ggplot(episodeRatingSerie, aes(x = ordemEpisode, y = averageRating)) +
  labs(x = "Episódio", y = "Média de pontuação") +
  geom_line(color=rainbow[1]) +
  geom_segment(color=rainbow[2], size=0.8, aes(x=1, xend=nrow(episodeRatingSerie), y=mediaSerie, yend=mediaSerie), alpha = 0.5) +  
  theme_minimal()

# Médias de ranking por temporada
mediaTemporada = aggregate(episodeRatingSerie$averageRating, by=list(Category=episodeRatingSerie$seasonNumber), mean)
mediaTemporada$Category = as.integer(mediaTemporada$Category)
colnames(mediaTemporada) <- c('seasonNumber', 'averageRating')

# Geração de gráfico por temporadas
ggplot(mediaTemporada, aes(x=seasonNumber, y=averageRating)) +
    labs(x = "Temporada", y = "Média de pontuação") +
    geom_line(color=rainbow[1]) +
    
    geom_segment(color=rainbow[2], size=1, aes(x=1, xend=nrow(mediaTemporada), y=mediaSerie, yend=mediaSerie), alpha = 0.5) +
    geom_segment(color=rainbow[3], size=1, aes(x=1, xend=nrow(mediaTemporada), y=mediaCurvaA, yend=mediaCurvaA), alpha=0.5) +
    geom_segment(color=rainbow[4], size=1, aes(x=1, xend=nrow(mediaTemporada), y=mediaCurvaB, yend=mediaCurvaB), alpha=0.5) +  
    geom_segment(color=rainbow[5], size=1, aes(x=1, xend=nrow(mediaTemporada), y=mediaCurvaC, yend=mediaCurvaC), alpha=0.5) +    
    
    geom_text(aes(x=1.1, y=mediaSerie, label='Série'), hjust=0, vjust=0.55, size=4, colour='red', alpha = 0.5) +
    geom_text(aes(x=1.1, y=mediaCurvaA, label='A'), hjust=0, vjust=0.55, size=4, colour='red', alpha = 0.5) +
    geom_text(aes(x=1.1, y=mediaCurvaB, label='B'), hjust=0, vjust=0.55, size=4, colour='red', alpha = 0.5) +
    geom_text(aes(x=1.1, y=mediaCurvaC, label='C'), hjust=0, vjust=0.55, size=4, colour='red', alpha = 0.5) + 
    
    theme_minimal()

# Importar títulos das séries 
# quote padrão é o "'" e comment.char padrão é o "#", mas alguns nomes possuem eles
titles = read.table(gzfile("title.basics.tsv.gz"), sep="\t", quote="", comment.char="", header = TRUE)

#codSerie <- 'tt0407362' # Battlestar Galactica 
#codSerie <- 'tt0411008' # Lost
#codSerie <- 'tt0944947' # Game of Thrones 
#codSerie <- 'tt0903747' # Breaking Bad
#codSerie <- 'tt0141842' # The Sopranos

# Listagem das séries
codSeries <- c('tt0407362', 'tt0411008', 'tt0944947', 'tt0903747', 'tt0141842')

# Carregar pontuação da série
grupoSeries <- episodeRating[episodeRating$parentTconst %in% codSeries,]
grupoSeries <- aggregate(grupoSeries$averageRating, by=list(seasonNumber=grupoSeries$seasonNumber, parentTconst=grupoSeries$parentTconst), mean)
colnames(grupoSeries)[3] <- 'averageRating'

# Capturar somente as colunas  títulos das séries 
titlesSeries <- titles[titles$tconst %in% as.factor(codSeries), c('tconst', 'primaryTitle')]

grupoSeries <- cbind(titlesSeries[match(grupoSeries$parentTconst, titlesSeries$tconst),], grupoSeries)

ggplot(grupoSeries, aes(x=seasonNumber, y=averageRating)) +
    labs(x = "Temporada", y = "Média de pontuação", colour = "Séries") +  
    geom_line(aes(group = primaryTitle, colour = primaryTitle)) +
    theme_minimal()

# Quais as melhores de todos os tempos
melhoresSeries <- head(meanRating)
melhoresTitles <- titles[titles$tconst %in% as.factor(melhoresSeries$parentTconst), c('tconst', 'primaryTitle')]
melhoresSeries <- cbind(melhoresTitles[match(melhoresSeries$parentTconst, melhoresTitles$tconst),], melhoresSeries)

print(melhoresSeries)

# Selecionar a pasta de trabalho e limpar ambiente
setwd(".")
rm(list = ls())

# Carregar bibliotecas
# Instalar pacote caso n?o exista: install.packages('tidyverse');
library('tidyverse')

# Carregar conjunto de dados
tarantino <- read.csv('tarantino.csv')

# Somente as mortes do Kill Bill (Vol. 1 & 2)
mortes <- 
  tarantino %>% 
  filter(type == 'death' & (movie == 'Kill Bill: Vol. 1' | movie == 'Kill Bill: Vol. 2'))

# Contagem de Mortes
qtdMortes <- nrow(as.data.frame(mortes$minutes_in))
mortes$qtd <- seq(1, qtdMortes)

# Adição de 112 minutos (tempo do volume 1 para o volume 2)
# Fonte: https://www.imdb.com/title/tt0266697/ (Duração de 1h51 + 1min para in?io do pr?ximo)
mortes[(mortes$movie == 'Kill Bill: Vol. 2'),]$minutes_in <- mortes[(mortes$movie == 'Kill Bill: Vol. 2'),]$minutes_in + 112

# Geração de Gráfico
graph <- ggplot(mortes, aes(x=qtd, y=minutes_in)) + 
  labs(title = "Mortes por tempo em Kill Bill (Vol. 1 & 2)", x = "Número de mortes", y = "Tempo de filme (minutos)") +
  geom_point(color='#db0d0d', size = 1.5, alpha = 0.5) +
  geom_segment(color='#0d74db', size = 1, aes(x=0, xend=qtdMortes, y=112, yend=112), alpha = 0.5) +
  theme_minimal()

# Salvar arquivo do Gráfico
ggsave(filename = 'KillBillDeath.jpg', graph,
       width = 9.38, height = 5.47, dpi = 95, units = 'in', device = 'jpg')
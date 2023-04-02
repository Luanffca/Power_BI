

# Mini-projeto 2 
# Limpeza e Transformação de dados com Power Query e Linguagem R
setwd("C:/Users/luan/Documents/DATA SCIENCE ACADEMY/caps12")
getwd()


# Instala os pacotes 
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")

# Carregado Pacotes
library(dplyr)
library(data.table)
library(ggplot2)

# Carregando Dados
dados_iris <- iris
View(dados_iris)

# Tabela 1 - Sumarizar os dados com as médidas de cada coluna de um dataset.
library(dplyr)
medias_iris <- summarize(group_by(dados_iris, Species),
                         media_sepal_length = mean(Sepal.Length),
                         media_sepal_width = mean(Sepal.Width),
                         media_petal_length = mean(Petal.Length),
                         media_petal_width = mean(Petal.Width))
View(medias_iris)

#Tarefa 2 -Extrair o valor inteiro de uma das colunasdecimais.

library(data.table)
dados_iris_id <- data.table(dados_iris)
View(dados_iris_id)
dados_iris_id$Sepal.Length <- as.integer(dados_iris_id$Sepal.Length)
View(dados_iris_id)

# Para executar no Power Query
library(data.table)
dados_iris_id <- data.table(dados_iris)
dados_iris_id$Sepal.Length <- as.integer(dados_iris_id$Sepal.Length)

#Tarefa 3 –Construir um gráfico mostrando a relação de duas variáveis numéricas para as 3 categoriasde uma variável categórica.

library(ggplot2)
ggplot(data = dados_iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(color = Species), size = 3) + 
  ggtitle("Largura e Comprimento das Pétalas") +
  labs(x = "Largura da Pétala", y = "Comprimento da Pétala") +
  theme_bw() + 
  theme(title = element_text(size = 15, color = "turquoise4"))


# Cores disponíveis na Liguagem R
colors()


















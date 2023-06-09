

# Mini-projeto 2 
# Limpeza e Transforma��o de dados com Power Query e Linguagem R
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

# Tabela 1 - Sumarizar os dados com as m�didas de cada coluna de um dataset.
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

#Tarefa 3 �Construir um gr�fico mostrando a rela��o de duas vari�veis num�ricas para as 3 categoriasde uma vari�vel categ�rica.

library(ggplot2)
ggplot(data = dados_iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(color = Species), size = 3) + 
  ggtitle("Largura e Comprimento das P�talas") +
  labs(x = "Largura da P�tala", y = "Comprimento da P�tala") +
  theme_bw() + 
  theme(title = element_text(size = 15, color = "turquoise4"))


# Cores dispon�veis na Liguagem R
colors()


















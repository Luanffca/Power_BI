
# Criando Visualização de Dados no Power BI com Linguagem R

# Caminho do arquivo
setwd("C:/Users/luan/Documents/DATA SCIENCE ACADEMY/caps12")
getwd()

# Carregando o dataset
vendas <- read.csv("vendas.csv", fileEncoding = "windows-1252")

# Carrega o ggplot2
library(ggplot2)

#Cria o gráfico
?qplot
qplot(Valor, Custo, data = vendas)

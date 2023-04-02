# Estatística Básica

# Parte 3 - Medidas De Posição Relativa

# Definindo a pasta de trabalho
setwd("C:/Users/luan/Documents/DATA SCIENCE ACADEMY/caps12")
getwd()

# Carregando o dataset
vendas <- read.csv("vendas.csv", fileEncoding = "windows-1252")

# Resumo dos dados
head(vendas)  # Primeiras linhas
tail(vendas)  # Ultimas linhas
view(vendas)
View(vendas)
View(Vendas)

# Medidas de Tendência Central
summary(vendas$Valor)
summary(vendas[c('Valor', 'Custo')])


# Explorando variáveis numéricas
mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.99))
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20))
IQR(vendas$Valor)  #Diferença entre Q3 e Q1
range(vendas$Valor)
summary(vendas$Valor)
diff(range(vendas$Valor))

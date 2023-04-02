
# LISTA DE EXERCÍCIOS 01

setwd("C:/Users/luan/Documents/DATA SCIENCE ACADEMY/caps12")
getwd()
notas <- read.csv("notas.csv", fileEncoding = "windows-1252")

# Resumo do dataset
View(notas)
str(notas)
head(notas)  # Primeiras linhas
tail(notas)  # Ultimas linhas

#Exercício 1: Apresente um resumo de tipos de dados e estatísticasdo dataset.
str(notas)
summary(notas[c('TurmaA', 'TurmaB')])

#Exercício 2: Qual a média de cada turma?
mean(notas$TurmaA)
mean(notas$TurmaB)

#Exercício 3: Qual turma apresentou maior variabilidade de notas? 
sd(notas$TurmaA)
sd(notas$TurmaB)

#Exercício 4 - Calcule o coeficiente de variação das 2 turmas.
media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <- sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

cvA <- sd_ta / media_ta * 100
cvB <- sd_ta / media_tb * 100

cvA
cvB

#Exercício 5 - Qual nota apareceu mais vezes em cada turma?

calculaModa <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

calculaModa(notas$TurmaA)
calculaModa(notas$TurmaB)



# LISTA DE EXERC�CIOS 01

setwd("C:/Users/luan/Documents/DATA SCIENCE ACADEMY/caps12")
getwd()
notas <- read.csv("notas.csv", fileEncoding = "windows-1252")

# Resumo do dataset
View(notas)
str(notas)
head(notas)  # Primeiras linhas
tail(notas)  # Ultimas linhas

#Exerc�cio 1: Apresente um resumo de tipos de dados e estat�sticasdo dataset.
str(notas)
summary(notas[c('TurmaA', 'TurmaB')])

#Exerc�cio 2: Qual a m�dia de cada turma?
mean(notas$TurmaA)
mean(notas$TurmaB)

#Exerc�cio 3: Qual turma apresentou maior variabilidade de notas? 
sd(notas$TurmaA)
sd(notas$TurmaB)

#Exerc�cio 4 - Calcule o coeficiente de varia��o das 2 turmas.
media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <- sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

cvA <- sd_ta / media_ta * 100
cvB <- sd_ta / media_tb * 100

cvA
cvB

#Exerc�cio 5 - Qual nota apareceu mais vezes em cada turma?

calculaModa <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

calculaModa(notas$TurmaA)
calculaModa(notas$TurmaB)


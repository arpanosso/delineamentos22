## Carregando os pacotes
library(tidyverse)
library(readxl)
library(ExpDes.pt)
library(skimr)
library(VCA)
library(nlme)

## 1) Carregar os dados que estão no arquivo aula5.xlsx

## 2) Qual o tipo de primitivo de cada coluna?

## 3) Faça uma estatística descritiva rápida das colunas

## 4) Quantas famílias, genótipos e blocos temos no experimento?

## 5) Converta as colunas familia, bloco e genotipo para fatores

## 6) Crie um gráfico de colunas com as médias de cada familia

## 7) Altere o gráfico para que as médias seja apresentadas em ordem
## crescente.

## 8) Altere as cores do gráfico para aquelas de sua preferência.

## 9) Inverta as coordenadas cartesianas.

## 10) Crie a a vairável `agrupamento`, a partir da seguinte regra:
## genótipo de de 1 a 5 - A
## genótipo de 16 a 28 - B
## genótipo de 29 a 49 - C
## genótipo de 50 a 54 - D

## 11) Faça a contagem de subject, ordenando-os

## 12) Agrupe as categorias com menores frequências em uma única categoria

## Exemplos de Aula - Análises
## Entrada de dados
familia <- aula5 %>% pull(familia) %>%  as_factor()
bloco <- aula5 %>% pull(bloco) %>%  as_factor()
genotipo <- aula5 %>% pull(genotipo) %>%  as_factor()
genotipo_b <- aula5 %>% pull(genotipo_b) %>%  as_factor()
y <- aula5 %>% pull(resp)

# BLOCOS COM FAMÍLIAS E PROGENIES  DENTRO DE FAMÍLIAS
## a)	ANOVA, usando as progenies:
modelo_01 <- aov(y ~ bloco + familia  + Error(bloco/familia) + genotipo:bloco )
summary(modelo_01)

a5<- as.data.frame(aula5)
a5$familia <- as_factor(a5$familia)
a5$bloco <- as_factor(a5$bloco)
fitVCA(resp~bloco + familia/bloco, a5)

## b)	ANOVA,usando média das progênies, em cada combinação de bloco e familia:
modelo_02 <- aov(y ~ bloco + familia  + Error(familia/bloco) )
summary(modelo_02)
fitVCA(resp~bloco + familia + familia/bloco, a5)

## Componentes de veriância utilizando o delineamento em quadrado latino
aula5_ql <- read.table("data/aula5_ql.txt",
                       h=TRUE)
head(aula5_ql)

## Análise, delineamento quadrado latino
linha <- aula5_ql %>% pull(LINHA) %>% as_factor()
coluna  <- aula5_ql %>% pull(COLUNA) %>% as_factor()
trat  <- aula5_ql %>% pull(TRAT) %>% as_factor()
y  <- aula5_ql %>% pull(RESP)

# definindo o modelo
m0 <- aov(y ~ linha + coluna + trat)
anova(m0)

## usando o Exp.Des.pt
ExpDes.pt::dql(trat, linha, coluna, y, quali = TRUE,
               mcomp = "tukey")

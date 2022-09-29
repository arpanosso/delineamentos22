## Carregando os pacotes
library(ExpDes.pt)
library(tidyverse)
library(readxl)
library(skimr)
library(nlme)
library(VCA)

# Exercícios Práticos -----------------------------------------------------
## 1) Carregar os dados que estão no arquivo aula5.xlsx
aula5 <- read_excel("data/aula5.xlsx")

## 2) Qual o tipo de primitivo de cada coluna?
glimpse(aula5)

## 3) Faça uma estatística descritiva rápida das colunas
skim(aula5)

## 4) Quantas famílias, genótipos e blocos temos no experimento?
aula5 %>% pull(familia) %>% unique()
aula5 %>% pull(bloco) %>% unique()
aula5 %>% pull(genotipo) %>% unique()

## 5) Construa uma coluna contendo a média geral do experimento (media_G)
## e outra (media_f ) contendo a média por familia
aula5 %>%
  mutate(media_g = mean(resp)) %>%
  group_by(familia) %>%
  mutate(media_f = mean(resp)) %>%
  group_by(familia, bloco) %>%
  mutate(media_fb = mean(resp))

## 6) Converta as colunas familia, bloco e genotipo para fatores
aula5 <- aula5 %>%
  mutate(
    familia = as_factor(familia),
    bloco = as_factor(bloco),
    genotipo = as_factor(genotipo)
  )
glimpse(aula5)

## 7) Crie um gráfico de colunas com as médias de cada familia
aula5 %>%
  group_by(familia) %>%
  summarise(
    media = mean(resp)
  ) %>%
  ggplot(aes(x=familia,y=media)) +
  geom_col()

## 8) Altere o gráfico para que as médias seja apresentadas em ordem
## crescente.
aula5 %>%
  group_by(familia) %>%
  summarise(
    media = mean(resp)
  ) %>%
  mutate(
    familia = fct_reorder(familia,media)
  ) %>%
  ggplot(aes(x=familia,y=media)) +
  geom_col()

## 9) Altere as cores do gráfico para aquelas de sua preferência.
pt<-aula5 %>%
  group_by(familia) %>%
  summarise(
    media = mean(resp)
  ) %>%
  mutate(
    familia = fct_reorder(familia,media)
  ) %>%
  ggplot(aes(x=familia,y=media,fill=familia)) +
  geom_col() +
  #scale_fill_manual(
  #  values = c("red","blue","green")
  #  ) +
  scale_fill_viridis_d()
pt

## 10) Inverta as coordenadas cartesianas.
pt +
  coord_flip()

## 11) Crie a a vairável `agrupamento`, a partir da seguinte regra:
## genótipo de de 1 a 5 - A
## genótipo de 16 a 28 - B
## genótipo de 29 a 49 - C
## genótipo de 50 a 54 - D
aula5 <- aula5 %>%
  mutate(
    genotipo = as.numeric(genotipo),
    agrupamento = case_when(
      genotipo <= 5 ~ "A",
      genotipo <= 28 ~ "B",
      genotipo <= 49 ~ "C",
      genotipo <= 54 ~ "D"
    )
  )

## 12) Faça a contagem de subject, ordenando-os
aula5 %>%
  count(agrupamento, sort=TRUE)

## 13) Agrupe as categorias com menores frequências em uma única categoria
aula5 %>%
  mutate(
    agrupamento = fct_lump(agrupamento, n=2)
  ) %>%
  count(agrupamento, sort=TRUE)

# Exemplo de Aula Teórica -------------------------------------------------
## Entrada de dados
aula5 <- read_excel("data/aula5.xlsx")
familia <- aula5 %>% pull(familia) %>%  as_factor()
bloco <- aula5 %>% pull(bloco) %>%  as_factor()
genotipo <- aula5 %>% pull(genotipo) %>%  as_factor()
y <- aula5 %>% pull(resp)

# BLOCOS COM FAMÍLIAS E PROGENIES  DENTRO DE FAMÍLIAS
## a)	ANOVA, usando as progenies:
modelo_01 <- aov(y ~ bloco + familia  +
                   Error(bloco/familia) + genotipo:bloco )
summary(modelo_01)

# Manipulação para criação de fatores
a5 <- aula5 %>%
  mutate(
    bloco = as_factor(bloco),
    familia = as_factor(familia)
  ) %>%  data.frame
fitVCA(resp~bloco + familia + familia/bloco, a5)

## b)	ANOVA,usando média das progênies, em cada combinação de bloco e familia:
a5_media <- aula5 %>%
  group_by(familia, bloco) %>%
  summarize(media = mean(resp)) %>%
  mutate(
    familia = as_factor(familia),
    bloco = as_factor(bloco)
  ) %>%
  data.frame()
modelo_02 <- aov(media ~ bloco + familia, data=a5_media  )
summary(modelo_02)
fitVCA(media~bloco + familia, a5_media, "anova")

# Exercicio 5 - item 6 ----------------------------------------------------
## Componentes de variância utilizando o delineamento em quadrado latino
aula5_ql <- read.table("data/aula5_ql.txt",
                       h=TRUE)
head(aula5_ql)

### ITEM 5 do Exercício 05
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

aula5_ql <- aula5_ql %>%
  mutate(
    LINHA = as_factor(LINHA),
    COLUNA = as_factor(COLUNA),
    TRAT = as_factor(TRAT)
  )

## Cálculo dos componentes da variância
fitVCA(RESP ~ LINHA + COLUNA + TRAT, aula5_ql)

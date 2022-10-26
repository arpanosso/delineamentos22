## Carregando os pacotes exigidos
library(tidyverse)
library(emmeans)

## 1) Lendo o banco de dados que está em aula8-superficie.xlsx
## passe as colunas do tipo character para fator
dados <- readxl::read_xlsx("data/aula8-superficie.xlsx")

## 2) Criar as variáveis auxiliares para Falta de ajuste (FA = TRAT)
dados <- dados %>%
  mutate(
    FA = TRAT
  )

## 3) Crie o gráfico para os estudos das interações

## Monte o modelo para estudo da superficie de resposta N N2 K K2
m2 <- lm(RESP ~ N + N2 + K + K2 + NK + FA,
            data = dados %>%
            mutate(N2=N^2, K2=K^2, NK=N*K, FA=as_factor(FA))
          )
anova(m2)


## 4) Extrair a soma de  quadrados dos resíduos, seus graus de liberdade e
## calcular CoefVar, Desvio Padrão e Media Geral
